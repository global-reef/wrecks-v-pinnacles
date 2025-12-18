# Load required libraries
library(tidyr)
library(brms)
library(tidybayes)
library(grid)
library(png)
library(abind)
library(posterior)
library(ggplot2)
library(dplyr)
library(tibble)

# Clean species names to be brms-safe
brms_safe_name <- function(x) {
  x %>%
    tolower() %>%
    gsub("[^a-z0-9]", "", .)  # Remove all non-alphanumeric characters
}
# Apply to species column
fish_long <- fish_long %>%
  mutate(Species = factor(Species, levels = unique(Species)),
         Species = brms_safe_name(as.character(Species)))

# Function to run species-level multivariate model
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
  
  # Convert to wide format
  fish_species_wide <- fish_long %>%
    filter(Species %in% species_occurrence) %>%
    group_by(survey_id, Site, Zone, Classification, Date, Researcher, Species) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Species, values_from = Count, values_fill = 0)
  
  # Relevel Classification to set Shipwreck as the reference
  fish_species_wide$Classification <- relevel(factor(fish_species_wide$Classification), ref = "Shipwreck")
  
  
  # Build model formula
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
  
  # Conditional effects for plotting
  ce <- conditional_effects(fit_mv, effects = "Classification", re_formula = NA)
  pred_df <- bind_rows(lapply(seq_along(ce), function(i) ce[[i]] %>% mutate(Species = species_vars[i])))
  
  list(
    fish_species_wide = fish_species_wide,
    fit_mv = fit_mv,
    prediction_data = pred_df
  )
}

# Run model
fit_path <- file.path(output_dir, "fit_mv.rds")

if (file.exists(fit_path)) {
  
  message("Loading existing species-level model from disk")
  fit_mv <- readRDS(fit_path)
  
  # Recreate prediction data from the fitted model
  ce <- conditional_effects(fit_mv, effects = "Classification", re_formula = NA)
  pred_df <- bind_rows(lapply(seq_along(ce), function(i) ce[[i]]))
  
} else {
  
  message("Fitting species-level multivariate model")
  spp_no_re <- run_species_mv_model(fish_long)
  
  fit_mv   <- spp_no_re$fit_mv
  fish_wide <- spp_no_re$fish_species_wide
  pred_df  <- spp_no_re$prediction_data
  
  saveRDS(fit_mv, fit_path)
}

summary(fit_mv)


# Filter species by occurrence count
species_occurrence <- fish_long %>%
  count(Species) %>%
  filter(n >= 10) %>%
  pull(Species)
# Species lookup and image paths
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
 #  "Porcupine.Puffer",  "Invertivore",     "Diodon/Tetraodon",    "spp.",           "Diodon/ Tetraodon spp.",
 # "Ray",               "Mesopredator",    "Taeniura/Neotrygon",  "spp.",           "Taeniura/ Neotrygon spp.",
  "sml_snapper",       "Mesopredator",    "Lutjanus",            "spp.",           "Lutjanus (<30cm) spp.",
  "lrg_Snapper",       "HTLP",            "Lutjanus",            "spp.",           "Lutjanus (>30cm) spp.",
 #  "Eel",               "Mesopredator",    "Gymnothorax",         "spp.",           "Gymnothorax spp.",
  "Trevally",          "HTLP",            "Caranx",              "spp.",           "Caranx spp.",
  "Emperorfish",       "Mesopredator",    "Lethrinus",           "spp.",           "Lethrinus spp.",
  "sml_Grouper",       "Mesopredator",    "Cephalopholis/Epinephelus", "spp.",     "Cephalopholis/ Epinephelus spp.",
  "lrg_Grouper",       "HTLP",            "Epinephelus",         "spp.",           "Epinephelus (>30cm)/ Plectropomus spp.",
 #  "Barracuda",         "HTLP",            "Sphyraena",           "spp.",           "Sphyraena spp."
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
  ) 
# spp_lookup <- spp_lookup %>%filter(Species %in% species_occurrence)


spp_lookup_clean <- spp_lookup %>%
  mutate(Species_clean = brms_safe_name(Species))

spp_lookup <- spp_lookup %>%
  mutate(Species_clean = brms_safe_name(Species))


# Posterior draws and within-model pairwise comparisons
post_spp <- as_draws_df(spp_no_re$fit_mv)

# Define all pairwise comparisons
comparison_list <- list(
  `Fringing – Shipwreck` = c("Fringing", "Shipwreck"),
  `Pinnacle – Shipwreck` = c("Pinnacle", "Shipwreck"),
  `Pinnacle – Fringing` = c("Pinnacle", "Fringing")
)

# Extract post hoc contrasts for each species and comparison using cleaned names
species_map <- spp_lookup_clean %>%
  select(Species, Species_clean)

pairwise_contrasts_all <- lapply(seq_along(species_map$Species), function(i) {
  sp_orig <- species_map$Species[i]
  sp_clean <- species_map$Species_clean[i]
  
  existing_betas <- colnames(post_spp)
  beta_fringing <- paste0("b_", sp_clean, "_ClassificationFringing")
  beta_pinnacle <- paste0("b_", sp_clean, "_ClassificationPinnacle")
  
  res <- list()
  
  if (beta_fringing %in% existing_betas) {
    res[["Fringing – Shipwreck"]] <- tibble(
      Species = sp_orig,
      Species_clean = sp_clean,
      Comparison = "Fringing – Shipwreck",
      diff = post_spp[[beta_fringing]]
    )
  }
  if (beta_pinnacle %in% existing_betas) {
    res[["Pinnacle – Shipwreck"]] <- tibble(
      Species = sp_orig,
      Species_clean = sp_clean,
      Comparison = "Pinnacle – Shipwreck",
      diff = post_spp[[beta_pinnacle]]
    )
  }
  if (all(c(beta_fringing, beta_pinnacle) %in% existing_betas)) {
    res[["Pinnacle – Fringing"]] <- tibble(
      Species = sp_orig,
      Species_clean = sp_clean,
      Comparison = "Pinnacle – Fringing",
      diff = post_spp[[beta_pinnacle]] - post_spp[[beta_fringing]]
    )
  }
  
  bind_rows(res)
}) %>% bind_rows()


# Format contrast summaries
posterior_contrasts_summary <- pairwise_contrasts_all %>%
  group_by(Species, Species_clean, Comparison) %>%
  summarise(
    Pr_gt_0 = mean(diff > 0),
    Pr_lt_0 = mean(diff < 0),
    Median = median(diff),
    CI_lower = quantile(diff, 0.025),
    CI_upper = quantile(diff, 0.975),
    .groups = "drop"
  ) %>%
  left_join(spp_lookup_clean %>% select(Species_clean, sci_name, Functional_Group), by = "Species_clean") %>%
  select(sci_name, Functional_Group, Comparison, Pr_gt_0, Pr_lt_0, Median, CI_lower, CI_upper) %>%
  mutate(
    sci_name = factor(sci_name, levels = spp_lookup_clean$sci_name),
    Functional_Group = factor(Functional_Group, levels = c("Grazer", "Invertivore", "Mesopredator", "HTLP"))
  ) %>%
  arrange(Functional_Group, sci_name, Comparison) %>%
  mutate(across(
    c(Pr_gt_0, Pr_lt_0, Median, CI_lower, CI_upper),
    ~ round(., 3)
  ))

print(posterior_contrasts_summary, n=Inf)


#### plotting ######## 
pred_df <- spp_no_re$prediction_data

# Reorder Species by Species_clean
species_order <- spp_lookup %>%
  arrange(factor(Species_clean, levels = spp_lookup$Species_clean)) %>%
  pull(Species_clean)

pred_df <- pred_df %>%
  mutate(Species = factor(Species, levels = species_order))

# predicted abundances table (tidy) 
library(dplyr)
library(tidyr)
# Reorder Species by Species_clean (for the plot)
species_order_plot <- spp_lookup %>%
  arrange(factor(Species_clean, levels = spp_lookup$Species_clean)) %>%
  pull(Species_clean)

pred_df <- pred_df %>%
  mutate(Species = factor(Species, levels = species_order_plot))
pred_df <- pred_df %>%
  filter(!is.na(Species), Species != "NA")
pred_df <- pred_df %>%
  mutate(
    Classification = factor(
      Classification,
      levels = c("Fringing", "Shipwreck", "Pinnacle")
    )
  )


# ---------- Tidy table (independent of the plot) ----------
pred_summary <- pred_df %>%
  select(Species, Classification, estimate__ = estimate__, lower__ = lower__, upper__ = upper__) %>%
  mutate(estimate_ci = sprintf("%.2f (%.2f–%.2f)", estimate__, lower__, upper__))

pred_tidy <- pred_summary %>%
  select(Species, Classification, estimate_ci) %>%
  pivot_wider(names_from = Classification, values_from = estimate_ci)

species_order_table <- c(
  "parrotfish","rabbitfish","butterflyfish","angelfish","cleanerwrasse","batfish",
  "thicklip","redbreast","slingjaw","sweetlips","squirrelsoldier","triggerfish",
  "smlsnapper","lrgsnapper","trevally","emperorfish","smlgrouper","lrggrouper"
)

pred_tidy <- pred_tidy %>%
  mutate(Species = factor(Species, levels = species_order_table)) %>%
  arrange(Species) %>%
  filter(!is.na(Species))

print(pred_tidy)


# 1) Fix facet order to your table
species_order_table <- c(
  "parrotfish","rabbitfish","butterflyfish","angelfish","cleanerwrasse","batfish",
  "thicklip","redbreast","slingjaw","sweetlips","squirrelsoldier","triggerfish",
  "smlsnapper","lrgsnapper","trevally","emperorfish","smlgrouper","lrggrouper"
)
species_in_plot <- species_order_table[species_order_table %in% unique(pred_df$Species)]
pred_df <- pred_df %>% mutate(Species = factor(Species, levels = species_in_plot))

# 2) Parsed facet strip labels (unchanged)
wrap_to_atop_italic <- function(x, width = 24) {
  w <- str_wrap(x, width = width)
  parts <- str_split(w, "\n", simplify = TRUE)
  line1 <- parts[, 1]
  line2 <- parts[, 2]
  
  ifelse(
    is.na(line2) | line2 == "",
    paste0("italic('", line1, "')"),
    paste0("atop(italic('", line1, "'), italic('", line2, "'))")
  )
}

species_labels <- spp_lookup %>%
  mutate(label = wrap_to_atop_italic(sci_name, width=24)) %>%
  select(Species_clean, label) %>%
  deframe()

force_atop <- function(x) {
  x %>%
    # Diagramma/Plectorhinchus spp.
    str_replace(
      "italic\\('Diagramma/Plectorhinchus spp\\.'\\)",
      "atop(italic('Diagramma/'), italic('Plectorhinchus spp.'))"
    ) %>%
    # Cephalopholis/Epinephelus spp.
    str_replace(
      "italic\\('Cephalopholis/Epinephelus spp\\.'\\)",
      "atop(italic('Cephalopholis/'), italic('Epinephelus spp.'))"
    )
}

species_labels <- spp_lookup %>%
  mutate(label = wrap_to_atop_italic(sci_name, width = 24)) %>%
  mutate(label = force_atop(label)) %>%
  select(Species_clean, label) %>%
  deframe()


# 3) Colors
buGn3 <- c("#B2E2E2", "#66C2A4", "#238B45")

buGn3_named <- c(
  "Fringing"   = "#238B45",
  "Shipwreck" = "#66C2A4",
  "Pinnacle"   = "#B2E2E2"
) 
# 4) Base plot (Fig 6)
p_base <- ggplot(pred_df, aes(x = Classification, y = estimate__, fill = Classification)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2) +
  facet_wrap(
    ~ Species,
    scales = "free_y",
    ncol = 4,
    labeller = labeller(Species = as_labeller(species_labels, label_parsed))
  ) +
  scale_fill_manual(values = buGn3_named) +
  labs(
    title = "Predicted Abundance per Species by Habitat Type",
    x = NULL,
    y = "Predicted Abundance"
  ) +
  theme_clean +
  theme(
    ## strip labels
    strip.text = element_text(
      size = 7.5,
      face = "bold",
      lineheight = 0.5,
      margin = margin(t = 1, b = 1)
    ),
    
    ## spacing between panels
    panel.spacing = unit(4, "pt"),
    panel.spacing.y = unit(2, "pt"),
    
    ## drop x axis entirely
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    ## legend once, underneath
    legend.position = "bottom",
    legend.title = element_blank()
  )


# 5) Letters matched to your order (top-right of each facet)
cls_levels <- if (is.factor(pred_df$Classification)) levels(pred_df$Classification) else unique(pred_df$Classification)
x_right <- length(cls_levels)

letters_df <- pred_df %>%
  group_by(Species) %>%
  summarise(y = max(upper__, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Species = factor(Species, levels = species_in_plot),
    label   = letters[seq_along(species_in_plot)][match(as.character(Species), species_in_plot)],
    x       = x_right,
    y       = y * 1.3
  )

p_species_prediction <- p_base +
  geom_text(
    data = letters_df,
    aes(x = x, y = y, label = paste0("(", label, ")")),
    inherit.aes = FALSE,
    family = "serif",
    fontface = "bold",
    size = 3,
    hjust = 0,
    vjust = 1
  )


print(p_species_prediction)  # fig 6

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


posterior_summary_contrast <- posterior_summary_contrast %>%
  mutate(Species_clean = gsub("[_\\.]", "", Species)) %>%
  left_join(spp_lookup_clean %>% select(Species_clean, sci_name), by = "Species_clean") %>%
  relocate(sci_name, .before = Species) %>%
  select(sci_name, Species, Habitat, Pr_gt_0, Pr_lt_0, Median, CI_lower, CI_upper) %>%
  mutate(Trend = case_when(
    Pr_gt_0 > 0.95 ~ "Positive",
    Pr_lt_0 > 0.95 ~ "Negative",
    TRUE ~ "Uncertain"
  ))

# Posterior Distribution Plot 
species_diff_plot <- ggplot(draws_spp, aes(x = value, y = sci_name, fill = Habitat)) +
  stat_halfeye(
    slab_alpha = 0.8,
    point_interval = median_qi,
    .width = 0.95,
    position = position_dodge(width = 0.7)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c(
    "Fringing" = "#188041",
    "Pinnacle" = "#a6dede"
  )) +
  labs(
    title = "Posterior Differences in Log-Abundance (vs. Shipwreck)",
    x = "Estimated Log Difference",
    y = "Taxa",
    fill = "Habitat Type"
  ) +
  scale_y_discrete(limits = rev) +
  theme_clean +
  theme(axis.text.y = element_text(size = 9))
print(species_diff_plot) # not using rn 

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
print(posterior_summary_spp, n=Inf)

# species-level headmap 
heatmap_data <- posterior_contrasts_summary %>%
  mutate(
    Trend = case_when(
      Pr_gt_0 > 0.95 ~ "Positive",
      Pr_lt_0 > 0.95 ~ "Negative",
      TRUE ~ "Uncertain"
    ),
    sci_name = factor(sci_name, levels = spp_lookup_clean$sci_name),
    Comparison = factor(Comparison, levels = c(
      "Fringing – Shipwreck",
      "Pinnacle – Shipwreck",
      "Pinnacle – Fringing"
    ))
  )

spp_heatmap <- ggplot(heatmap_data, aes(x = Comparison, y = sci_name, fill = Median)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#0570b0", mid = "white", high = "#188041", midpoint = 0) +
  geom_point(
    data = heatmap_data %>% filter(Trend != "Uncertain"),
    aes(shape = Trend),
    size = 3,
    color = "black"
  ) +
  scale_shape_manual(values = c(Positive = 3, Negative = 95)) +
  scale_y_discrete(limits = rev) +
  theme_clean +
  labs(
    title = " ",
    x = "Habitat Comparison",
    y = "Taxa",
    fill = "Effect size (Median)"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 25, hjust = 1),
    axis.text.y = element_text(size = 12, face = "italic"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

print(spp_heatmap) # fig 7 
library(tidyverse)
library(tidybayes)



save_species_plots <- function(pred_plot, diff_plot, heatmap_plot, output_dir, analysis_date) {
  
  save_ir_fig(pred_plot,       6, output_dir, width_mm = 165, height_mm = 165)
  # 81 for one column vertical 
  # or 169 for two col horizontal 
  
  message("✅ All species-level plots saved to: ", output_dir)
}

# Usage 
save_species_plots(
  pred_plot = p_species_prediction,
  diff_plot = species_diff_plot,
  heatmap_plot = spp_heatmap,
  output_dir = output_dir,
  analysis_date = analysis_date
) 


print(p_species_prediction) # fig 6 
print(spp_heatmap) # fig 7 
print(species_diff_plot)

print(posterior_contrasts_summary, n=Inf)



out_path <- file.path(output_dir, paste0("posterior_summary_spp_", analysis_date, ".csv"))
write.csv(posterior_summary_spp, out_path, row.names = FALSE)
