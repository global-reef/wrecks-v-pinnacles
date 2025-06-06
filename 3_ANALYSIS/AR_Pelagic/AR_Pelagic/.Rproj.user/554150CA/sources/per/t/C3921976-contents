run_species_mv_model <- function(fish_long,
                                 species_min_obs = 10,
                                 iter = 4000,
                                 warmup = 1000,
                                 control_list = list(adapt_delta = 0.90, max_treedepth = 20)) {
  library(dplyr)
  library(tidyr)
  library(brms)
  library(ggplot2)
  library(tidybayes)
  
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
  
  # Build formula for all species
  species_vars <- setdiff(names(fish_species_wide), c("survey_id", "Site", "Zone", "Classification", "Date", "Researcher"))
  response_formula <- as.formula(paste0("mvbind(", paste(species_vars, collapse = ", "), ") ~ Classification + (1 | p | Site)"))
  
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
results_all_species <- run_species_mv_model(fish_long)
summary(results_all_species$fit_mv)
fish_wide <- results_all_species$fish_species_wide
pred_df <- results_all_species$prediction_data

# facet by species 
# Create and store the plot
p_species_prediction <- ggplot(pred_df, aes(x = Classification, y = estimate__, fill = Classification)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.2) +
  facet_wrap(~ Species, scales = "free_y", ncol = 6) +
  theme_clean +
  scale_fill_brewer(palette = "BuGn") +
  labs(
    title = "Predicted Abundance per Species by Habitat Type",
    x = "Habitat Type",
    y = "Predicted Abundance"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"  # Optional: remove if you want a legend
  )
print(p_species_prediction)
# Save the plot
ggsave(
  filename = file.path(output_dir, paste0("Species_Predictions_", analysis_date, ".png")),
  plot = p_species_prediction,
  width = 12,
  height = 8
)



