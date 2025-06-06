run_diversity_models <- function(fish_long, survey_level) {
  library(dplyr)
  library(tidyr)
  library(vegan)
  library(brms)
  library(ggplot2)
  
 
  # --- Shannon Species Diversity ---
  species_diversity_df <- fish_long %>%
    group_by(survey_id, Site, Zone, Species) %>%
    summarise(Count = sum(Count), .groups = "drop") %>%
    spread(key = Species, value = Count, fill = 0) %>%
    mutate(
      shannon_species_diversity = diversity(select(., -survey_id, -Site, -Zone), index = "shannon")
    )
  
  # Merge into survey_level
  survey_level <- survey_level %>%
    left_join(species_diversity_df %>% select(survey_id, shannon_species_diversity), by = "survey_id")
  
  # Fit Shannon diversity model
  fit_species_diversity <- brm(
    formula = shannon_species_diversity ~ Zone + (1 | Site),
    data = survey_level,
    family = gaussian(),
    chains = 4,
    iter = 4000,
    warmup = 1000,
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr"
  )
  # Print summary
  print(summary(fit_species_diversity))
  
  
  # --- Simple Plots ---
  # 1. Boxplot: Shannon Diversity by Zone
  p1 <- ggplot(survey_level, aes(x = Zone, y = shannon_species_diversity)) +
    geom_boxplot(fill = "lightgreen") +
    theme_minimal() +
    labs(title = "Shannon Diversity by Zone", y = "Shannon Index", x = "Zone")
  
  print(p1)
  

  # --- Return all results ---
  return(list(
    survey_level = survey_level,
    fit_species_diversity = fit_species_diversity,
    plots = list(diversity_plot = p1)
  ))
}
results_diversity <- run_diversity_models(fish_long, survey_level)

library(tidybayes)

survey_level %>%
  add_fitted_draws(results_diversity$fit_species_diversity, re_formula = NA) %>%
  ggplot(aes(x = Zone, y = .value)) +
  stat_halfeye(.width = 0.95, fill = "skyblue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Posterior Estimates of Shannon Diversity by Zone",
       y = "Estimated Shannon Index", x = "Zone")

