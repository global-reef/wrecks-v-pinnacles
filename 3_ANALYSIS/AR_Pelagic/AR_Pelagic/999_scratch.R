
library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)
library(tidybayes)
library(grid)
library(png)
library(abind)


run_species_mv_model <- function(fish_long,
                                 species_min_obs = 10,
                                 iter = 4000,
                                 warmup = 1000,
                                 control_list = list(adapt_delta = 0.90, max_treedepth = 20)) {
  
  
  # Filter species by occurrence count
  species_occurrence <- fish_long %>%
    count(Species) %>%
    filter(n >= species_min_obs) %>%
    pull(Species)
  
  # Convert to wide format by Species
  fish_species_wide <- fish_long %>%
    filter(Species %in% species_occurrence) %>%
    group_by(survey_id, Site, Zone, Classification, Date, Researcher,Species) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Species, values_from = Count, values_fill = 0)
  # Relevel Classification to set Shipwreck as the reference
  fish_species_wide$Classification <- relevel(fish_species_wide$Classification, ref = "Shipwreck")
  
  
  # Build formula for all species
  species_vars <- setdiff(names(fish_species_wide), c("survey_id", "Site", "Zone", "Classification", "Date", "Researcher"))
  response_formula <- as.formula(paste0("mvbind(", paste(species_vars, collapse = ", "), ") ~ Classification"))
  
  # Fit model
  fit_mv <- brm(
    formula = response_formula,
    data = fish_species_wide,
    family = negbinomial(),
    chains = 4,
    cores = 4,
    iter = iter,
    warmup = warmup,
    control = control_list,
    backend = "cmdstanr"
  )
  
  # Posterior summaries using conditional_effects()
  ce <- conditional_effects(fit_mv, effects = "Classification", re_formula = NA)
  
  pred_df <- bind_rows(
    lapply(seq_along(ce), function(i) {
      ce[[i]] %>%
        mutate(Species = species_vars[i])
    })
  )
  
  # Return results
  list(
    fish_species_wide = fish_species_wide,
    fit_mv = fit_mv,
    prediction_data = pred_df
  )
}
# results_all_species <- run_species_mv_model(fish_long) # with re for site - too complex to model 16 taxa 
# summary(results_all_species$fit_mv)

spp_no_re <- run_species_mv_model(fish_long)
summary(spp_no_re$fit_mv)

fish_wide <- spp_no_re$fish_species_wide
pred_df <- spp_no_re$prediction_data

print(pred_df)



### plotting 
# Define species table and join silhouettes
shape_base_path <- "~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/2_DATA/shapes"


species_occurrence <- fish_long %>%
  count(Species) %>%
  filter(n >= species_min_obs) %>%
  pull(Species)

spp_lookup <- tibble::tribble(
  ~Species,            ~Functional_Group, ~Genus,                ~Species_epithet, ~sci_name,
  "Parrotfish",        "Grazer",          "Scarus",              "spp.",           "Scarus spp.",
  "Rabbitfish",        "Grazer",          "Siganus",             "spp.",           "Siganus spp.",
  "Butterflyfish",     "Grazer",          "Chaetodon",           "spp.",           "Chaetodon spp.",
  "Angelfish",         "Invertivore",     "Pomacanthus",         "spp.",           "Pomacanthus spp.",
  "Cleaner_Wrasse",    "Invertivore",     "Labroides",           "dimidiatus",     "Labroides dimidiatus",
  "Batfish",           "Invertivore",     "Ephippidae",          "spp.",           "Ephippidae spp.",
  "Thicklip",          "Invertivore",     "Hemigymnus",          "melapterus",     "Hemigymnus melapterus",
  "Red_Breast",        "Invertivore",     "Cheilinus",           "fasciatus",      "Cheilinus fasciatus",
  "Slingjaw",          "Invertivore",     "Epibulus",            "insidiator",     "Epibulus insidiator",
  "Sweetlips",         "Invertivore",     "Diagramma/Plectorhinchus", "spp.",     "Diagramma/ Plectorhinchus spp.",
  "Squirrel.Soldier",  "Invertivore",     "Holocentridae",       "spp.",           "Holocentridae spp.",
  "Triggerfish",       "Invertivore",     "Balistidae",          "spp.",           "Balistidae spp.",
  "Porcupine.Puffer",  "Invertivore",     "Diodon/Tetraodon",    "spp.",           "Diodon/ Tetraodon spp.",
  "Ray",               "Mesopredator",    "Taeniura/Neotrygon",  "spp.",           "Taeniura/ Neotrygon spp.",
  "sml_snapper",       "Mesopredator",    "Lutjanus",            "spp.",           "Lutjanus (<30cm) spp.",
  "lrg_Snapper",       "HTLP",            "Lutjanus",            "spp.",           "Lutjanus (>30cm) spp.",
  "Eel",               "Mesopredator",    "Gymnothorax",         "spp.",           "Gymnothorax spp.",
  "Trevally",          "HTLP",            "Caranx",              "spp.",           "Caranx spp.",
  "Emperorfish",       "Mesopredator",    "Lethrinus",           "spp.",           "Lethrinus spp.",
  "sml_Grouper",       "Mesopredator",    "Cephalopholis/Epinephelus", "spp.",     "Cephalopholis/ Epinephelus spp.",
  "lrg_Grouper",       "HTLP",            "Epinephelus",         "spp.",           "Epinephelus (>30cm)/ Plectropomus spp.",
  "Barracuda",         "HTLP",            "Sphyraena",           "spp.",           "Sphyraena spp."
) %>%
  left_join(
    tibble::tribble(
      ~sci_name,                                 ~family,           ~option,
      "Scarus spp.",                             "Labridae",        "Scarus.rivulatus",
      "Siganus spp.",                            "Siganidae",       "Siganus.virgatus",
      "Chaetodon spp.",                          "Chaetodontidae",  "Chaetodon.trifasciatus",
      "Pomacanthus spp.",                        "Pomacanthidae",   "Pomacanthus.imperator",
      "Labroides dimidiatus",                    "Labridae",        "Labroides.dimidiatus",
      "Ephippidae spp.",                         "Ephippidae",      "Platax.teira",
      "Hemigymnus melapterus",                   "Labridae",        "Chlorurus.sordidus",
      "Cheilinus fasciatus",                     "Labridae",        "Epibulus.insidiator",
      "Epibulus insidiator",                     "Labridae",        "Epibulus.insidiator",
      "Diagramma/ Plectorhinchus spp.",          "Haemulidae",      "Plectorhinchus.gibbosus",
      "Holocentridae spp.",                      "Holocentridae",   "Myripristis.violacea",
      "Balistidae spp.",                         "Balistidae",      "Balistapus.undulatus",
      "Lutjanus (<30cm) spp.",                   "Lutjanidae",      "Lutjanus.gibbus",
      "Lutjanus (>30cm) spp.",                   "Lutjanidae",      "Lutjanus.gibbus",
      "Caranx spp.",                             "Carangidae",      "Caranx.melampygus",
      "Lethrinus spp.",                          "Lethrinidae",     "Lethrinus.nebulosus",
      "Cephalopholis/ Epinephelus spp.",         "Serranidae",      "Epinephelus.merra",
      "Epinephelus (>30cm)/ Plectropomus spp.",  "Serranidae",      "Epinephelus.malabaricus"
    ),
    by = "sci_name"
  ) %>%
  filter(Species %in% species_occurrence) %>%
  mutate(
    shape_file = paste0(family, "_", option, ".png"),
    shape_path = file.path(shape_base_path, shape_file)
  )

# Set species order in plot data
pred_df <- pred_df %>%
  mutate(Species = factor(Species, levels = spp_lookup$Species))

# Create parsed scientific name labels with custom line breaks
species_labels <- spp_lookup %>%
  mutate(
    display_name = case_when(
      sci_name == "Diagramma/ Plectorhinchus spp." ~ "atop('Diagramma/','Plectorhinchus spp.')",
      sci_name == "Cephalopholis/ Epinephelus spp." ~ "atop('Cephalopholis/','Epinephelus (<30cm) spp.')",
      sci_name == "Epinephelus (>30cm)/ Plectropomus spp." ~ "atop('Epinephelus (>30cm)/','Plectropomus spp.')",
      TRUE ~ paste0("'", sci_name, "'")
    ),
    label = paste0("italic(", display_name, ")")
  ) %>%
  select(Species, label) %>%
  tibble::deframe()





# facet by species 
# Create and store the plot
p_species_prediction <- ggplot(pred_df, aes(x = Classification, y = estimate__, fill = Classification)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2) +
  facet_wrap(~ Species, scales = "free_y", ncol = 6,
             labeller = labeller(Species = as_labeller(species_labels, label_parsed))) + 
  theme_clean +
  scale_fill_brewer(palette = "BuGn") +
  labs(
    title = "Predicted Abundance per Species by Habitat Type",
    x = "Habitat Type",
    y = "Predicted Abundance"
  ) +
  theme(
    strip.placement = "outside",
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 10), lineheight = 1.1),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )


print(p_species_prediction)

# Save the plot
ggsave(
  filename = file.path(output_dir, paste0("Species_Predictions_", analysis_date, ".png")),
  plot = p_species_prediction,
  width = 13, height = 7, units = "in", dpi = 300
)

### extract posterior probabilities 
library(brms)
library(dplyr)
library(posterior)

# Extract posterior draws
post_spp <- as_draws_df(spp_no_re$fit_mv)

# Get all Classification effect names
classification_terms <- grep("^b_.*_Classification", names(post_spp), value = TRUE)

# Compute posterior probabilities and summary stats
posterior_summary_spp <- classification_terms %>%
  setNames(classification_terms) %>%
  lapply(function(term) {
    draws <- post_spp[[term]]
    tibble(
      Parameter = gsub("^b_", "", term),
      Pr_gt_0 = mean(draws > 0),
      Pr_lt_0 = mean(draws < 0),
      Median = median(draws),
      CI_lower = quantile(draws, 0.025),
      CI_upper = quantile(draws, 0.975)
    )
  }) %>%
  bind_rows()

# View or export
print(posterior_summary_spp)


# Pull all classification effects for Fringing and Pinnacle
draws_spp <- spp_no_re$fit_mv %>%
  spread_draws(`b_.*_ClassificationFringing`, `b_.*_ClassificationPinnacle`, regex = TRUE) %>%
  pivot_longer(
    cols = starts_with("b_"),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  mutate(
    Species = sub("^b_(.*?)_Classification.*", "\\1", parameter),
    Habitat = sub(".*_Classification", "", parameter)
  )%>%
  mutate(Species_clean = gsub("[_\\.]", "", Species)) %>%
  left_join(spp_lookup_clean %>% select(Species_clean, sci_name), by = "Species_clean") %>%
  left_join(spp_lookup_clean %>% select(Species_clean, Functional_Group), by = "Species_clean") %>%
  mutate(
    sci_name = factor(sci_name, levels = spp_lookup_clean$sci_name),
    Functional_Group = factor(Functional_Group, levels = c("Grazer", "Invertivore", "Mesopredator", "HTLP"))
  )


posterior_summary_contrast <- draws_spp %>%
  group_by(Species, Habitat) %>%
  summarise(
    Pr_gt_0 = mean(value > 0),
    Pr_lt_0 = mean(value < 0),
    Median = median(value),
    CI_lower = quantile(value, 0.025),
    CI_upper = quantile(value, 0.975),
    .groups = "drop"
  )

spp_lookup_clean <- spp_lookup %>%
  mutate(Species_clean = gsub("[_\\.]", "", Species))

posterior_summary_contrast <- posterior_summary_contrast %>%
  mutate(Species_clean = gsub("[_\\.]", "", Species)) %>%
  left_join(spp_lookup_clean %>% select(Species_clean, sci_name), by = "Species_clean") %>%
  relocate(sci_name, .before = Species) %>%
  select(sci_name, Species, Habitat, Pr_gt_0, Pr_lt_0, Median, CI_lower, CI_upper)


posterior_summary_contrast <- posterior_summary_contrast %>%
  mutate(Trend = case_when(
    Pr_gt_0 > 0.95 ~ "Positive",
    Pr_lt_0 > 0.95 ~ "Negative",
    TRUE ~ "Uncertain"
  ))

library(ggplot2)
# reorder 

species_diff_plot <- ggplot(draws_spp, aes(x = value, y = sci_name, fill = Habitat)) +
  stat_halfeye(
    slab_alpha = 0.8,
    point_interval = median_qi,
    .width = 0.95,
    position = position_dodge(width = 0.7)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Posterior Differences in Log-Abundance\n (vs. Shipwreck)",
    x = "Estimated Log Difference",
    y = "Taxa",
    fill = "Habitat Type"
  ) + 
  scale_y_discrete(limits = rev) + 
  scale_fill_manual(values = c(
    "Fringing" = "#188041",
    "Pinnacle" = "#a6dede"
  )) +
  theme_clean +
  theme(
    axis.text.y = element_text(size = 9)
  )

print(species_diff_plot)

heatmap_data <- posterior_summary_spp %>%
  separate(Parameter, into = c("Species_raw", "Habitat"), sep = "_Classification") %>%
  mutate(Species_clean = gsub("[_\\.]", "", Species_raw)) %>%
  left_join(spp_lookup_clean, by = "Species_clean") %>%
  mutate(Trend = case_when(
    Pr_gt_0 > 0.95 ~ "Positive",
    Pr_lt_0 > 0.95 ~ "Negative",
    TRUE ~ "Uncertain"
  ))

spp_heatmap <- 
  ggplot(heatmap_data, aes(x = Habitat, y = sci_name, fill = Median)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#ccebc5", mid = "white", high = "#0570b0", midpoint = 0) +
  geom_point(
    data = heatmap_data %>% filter(Trend != "Uncertain"),
    aes(shape = Trend),
    size = 3,
    color = "black"
  ) +
  scale_shape_manual(values = c(Positive = 3, Negative = 95)) +  # + and –
  theme_minimal(base_size = 12) +  theme_clean +
  labs(
    title = "Species-level Effects of Habitat Type",
    x = "Habitat Type vs Shipwreck",
    y = "Taxa",
    fill = "Effect size (Median)"
  )
print(spp_heatmap)

save_species_effect_plots <- function(species_diff_plot, spp_heatmap, output_dir, analysis_date) {
  # Save species-level posterior distribution plot
  ggsave(
    filename = file.path(output_dir, paste0("SpeciesLevel_Posterior_Differences_", analysis_date, ".png")),
    plot = species_diff_plot,
    width = 10,
    height = 8
  )
  
  # Save heatmap of species-level effects
  ggsave(
    filename = file.path(output_dir, paste0("SpeciesLevel_Heatmap_Effects_", analysis_date, ".png")),
    plot = spp_heatmap,
    width = 8,
    height = 10
  )
  
  message("✅ Species-level plots saved to: ", output_dir)
}


save_species_effect_plots(
  species_diff_plot = species_diff_plot,
  spp_heatmap = spp_heatmap,
  output_dir = output_dir,
  analysis_date = analysis_date
)

