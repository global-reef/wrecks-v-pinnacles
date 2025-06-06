plot_species_heatmap_publication <- function(fish_long, 
                                             functional_groups_df = NULL,
                                             fg_palette = "BuGn",
                                             classification_palette = "BuGn",
                                             log_breaks = c(1, 10, 100, 1000, 3000)) {
  library(dplyr)
  library(ggplot2)
  library(ggtext)
  library(RColorBrewer)
  
  # Step 1: If functional_groups_df not supplied, stop
  if (is.null(functional_groups_df)) {
    stop("Please supply a data.frame mapping Species to Functional_Group as functional_groups_df")
  }
  
  # Step 2: Define unique functional groups and classifications
  fg_levels <- unique(functional_groups_df$Functional_Group)
  class_levels <- c("Shipwreck", "Fringing", "Pinnacle")
  
  fg_colors <- setNames(brewer.pal(length(fg_levels), fg_palette), fg_levels)
  class_colors <- setNames(brewer.pal(length(class_levels), classification_palette), class_levels)
  
  # Step 3: Aggregate fish_long to abundance matrix
  heat_long <- fish_long %>%
    group_by(Site, Species) %>%
    summarise(Abundance = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    left_join(functional_groups_df, by = "Species") %>%
    left_join(fish_long %>% distinct(Site, Classification), by = "Site") %>%
    mutate(Abundance = Abundance + 1)  # prevent log(0)
  
  # Step 4: Create ordered labels with colored boxes (not colored text)
  species_order_df <- heat_long %>%
    distinct(Species, Functional_Group) %>%
    arrange(factor(Functional_Group, levels = fg_levels), Species) %>%
    mutate(Species_label = paste0("<span style='color:", fg_colors[Functional_Group], ";'>&#9632;</span> ", Species))
  
  site_order_df <- heat_long %>%
    distinct(Site, Classification) %>%
    arrange(factor(Classification, levels = class_levels), Site) %>%
    mutate(Site_label = paste0("<span style='color:", class_colors[Classification], ";'>&#9632;</span> ", Site))
  
  # Merge labels into main data and convert to factors
  heat_long <- heat_long %>%
    left_join(species_order_df, by = c("Species", "Functional_Group")) %>%
    left_join(site_order_df, by = c("Site", "Classification")) %>%
    mutate(
      Species = factor(Species_label, levels = species_order_df$Species_label),
      Site = factor(Site_label, levels = rev(site_order_df$Site_label))  # Reverse for top-down
    )
  heat_prop <- heat_long %>%
    group_by(Site) %>%
    mutate(Proportion = Abundance / sum(Abundance, na.rm = TRUE)) %>%
    ungroup()
  
  # Plot heatmap using Blues (log1p scale)
  p <- ggplot(heat_prop, aes(x = Species, y = Site, fill = Proportion)) +
    geom_tile(color = "white") +
    scale_fill_distiller(palette = "Blues", direction = 1) +
    labs(
      title = "Species Proportions Heatmap",
      x = "Species",
      y = "Site",
      fill = "Abundance"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_markdown(angle = 90, hjust = 1, size = 7.5),
      axis.text.y = element_markdown(size = 8),
      axis.title.x = element_text(vjust = -0.5),
      panel.grid = element_blank(),
      plot.title = element_text(size = 12, face = "bold")
    )
  
  return(p)
}

functional_groups_df <- tibble::tribble(
  ~Species,           ~Functional_Group,
  "Parrotfish",       "Herbivore",
  "Rabbitfish",       "Herbivore",
  "Butterflyfish",    "Herbivore",
  "Angelfish",        "Invertivore",
  "Cleaner_Wrasse",   "Invertivore",
  "Batfish",          "Invertivore",
  "Thicklip",         "Invertivore",
  "Red_Breast",       "Invertivore",
  "Slingjaw",         "Invertivore",
  "Sweetlips",        "Invertivore",
  "Squirrel.Soldier", "Invertivore",
  "Triggerfish",      "Invertivore",
  "Emperorfish",      "Mesopredator",
  "Trevally",         "HTLP",
  "sml_snapper",      "Mesopredator",
  "lrg_Snapper",      "HTLP",
  "sml_Grouper",      "Mesopredator",
  "lrg_Grouper",      "HTLP"
)
plot_species_heatmap_publication(fish_long, functional_groups_df)
