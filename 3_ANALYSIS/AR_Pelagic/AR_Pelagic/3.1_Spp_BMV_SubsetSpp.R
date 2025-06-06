# if not running 3 (all species)
# fish_wide <- fish_long %>%
#   group_by(survey_id, Site, Zone, Classification, Date, Species, Researcher) %>%
#   summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
#   pivot_wider(names_from = Species, values_from = Count, values_fill = 0)



run_species_subset_analysis <- function(fish_wide,  
                                        species_subset = c("Parrotfish", "Rabbitfish", "Butterflyfish"),
                                        iter = 2000, warmup = 400,  
                                        control_list = list(adapt_delta = 0.90, max_treedepth = 15)) {
  library(dplyr)
  library(tidyr)
  library(brms)
  library(ggplot2)
  library(gridExtra)
  
  # Use globally defined output_dir
  
  # Create the multivariate response formula using only the species in species_subset
  response_str <- paste("mvbind(", paste(species_subset, collapse = ", "), ") ~ Classification")
  response_formula <- as.formula(response_str)
  print(response_formula)
  
  # Fit the model using brms
  fit_brms <- brm(
    formula = response_formula,
    data = fish_wide,
    family = negbinomial(),
    chains = 4,
    cores = 4,
    iter = iter,
    warmup = warmup,
    control = control_list,
    backend = "cmdstanr"
  )
  print(summary(fit_brms))
  
  # Extract fitted point estimates from the model using the original data
  newdata <- fit_brms$data
  fitted_summary <- as.data.frame(fitted(fit_brms, newdata = newdata))
  
  # Keep only the Estimate columns and add the Classification variable
  estimate_cols <- grep("^Estimate", names(fitted_summary), value = TRUE)
  estimates <- fitted_summary[, estimate_cols]
  estimates$Classification <- newdata$Classification
  
  # Reshape to long format
  est_long <- estimates %>%
    pivot_longer(
      cols = starts_with("Estimate"),
      names_to = "Species",
      names_prefix = "Estimate.",
      values_to = "Predicted"
    )
  
  # Summarize by species and classification
  summary_est <- est_long %>%
    group_by(Classification, Species) %>%
    summarise(mean_pred = mean(Predicted, na.rm = TRUE), .groups = "drop")
  
  # Overall grouped plot
  p_overall <- ggplot(summary_est, aes(x = Classification, y = mean_pred, fill = Species)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    labs(title = "Mean Predicted Abundance by Classification",
         x = "Classification", y = "Mean Predicted Abundance") +
    theme_clean +
    scale_fill_brewer(palette = "BuGn")
  print(p_overall)
  
  # Save overall bar plot
  ggsave(
    filename = file.path(output_dir, "Mean_Predicted_Abundance_by_Classification.png"),
    plot = p_overall,
    width = 8,
    height = 6
  )
  
  # Combined 3-panel plot function
  plot_species_predictions <- function(summary_data, species_vec, output_dir) {
    plots <- lapply(species_vec, function(sp) {
      summary_data %>%
        filter(Species == sp) %>%
        ggplot(aes(x = Classification, y = mean_pred, fill = Classification)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        scale_fill_brewer(palette = "BuGn") +
        labs(title = sp,
             x = "Classification",
             y = "Predicted Abundance") +
        theme_clean +
        theme(legend.position = "none")
    })
    
    # Combine into one panel
    combined_plot <- gridExtra::grid.arrange(grobs = plots, ncol = 3)
    
    # Save the panel plot
    ggsave(
      filename = file.path(output_dir, "Predicted_Abundance_Three_Species_Panel.png"),
      plot = combined_plot,
      width = 12,
      height = 4
    )
    
    invisible(combined_plot)
  }
  
  # Call panel plot saver
  plot_species_predictions(summary_est, species_subset, output_dir)
  
  # Save fitted model object
  save(fit_brms, file = file.path(output_dir, "SS_fit_Grazers.RData"))
  
  # Return key outputs
  list(
    fit_brms = fit_brms,
    summary_est = summary_est,
    p_overall = p_overall
  )
}



# Example usage (ensure fish_wide is available from your cleaned data)
results_species <- run_species_subset_analysis(
  fish_wide, 
  species_subset = c("Parrotfish", "Rabbitfish", "Butterflyfish")
  )


summary(results_species$fit_brms)


