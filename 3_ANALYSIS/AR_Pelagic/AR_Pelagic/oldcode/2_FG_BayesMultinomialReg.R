run_multinomial_regression <- function(fish_long, iter = 2000, warmup = 400,
                                       control_list = list(adapt_delta = 0.90, max_treedepth = 15)) {
  library(dplyr)
  library(tidyr)
  library(brms)
  library(ggplot2)
  library(gridExtra)
  
  # Ensure unique survey IDs are present
  fish_long <- fish_long %>%
    mutate(survey_id = paste(Site, Date, Time, sep = "_"))
  
  # Convert long-format data into wide-format using spread()
  survey_level <- fish_long %>%
    group_by(survey_id, Site, Classification, Functional_Group) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    spread(key = Functional_Group, value = Count, fill = 0)
  
  # Round counts to integers (needed for multinomial sampling)
  survey_level <- survey_level %>%
    mutate(across(c(Herbivore, Invertivore, Mesopredator, HTLP), ceiling))
  
  # Create a total count column (trials)
  survey_level <- survey_level %>%
    mutate(total_fish = Herbivore + Invertivore + Mesopredator + HTLP)
  
  # Set "Shipwreck" as the baseline for Classification
  survey_level$Classification <- relevel(survey_level$Classification, ref = "Shipwreck")
  
  # Fit the multinomial model (without random effects)
  fit <- brm(
    formula = cbind(Herbivore, Invertivore, Mesopredator, HTLP) | trials(total_fish) ~ Classification,
    data = survey_level,
    family = multinomial(),
    chains = 4,
    cores = 4,
    iter = iter,
    warmup = warmup,
    control = control_list
  )
  print(summary(fit))
  
  # Posterior predictive check: Compare observed vs. predicted total counts
  y_rep <- posterior_predict(fit)
  survey_totals_obs <- survey_level %>%
    mutate(total = Herbivore + Invertivore + Mesopredator + HTLP) %>%
    pull(total)
  survey_totals_pred <- apply(y_rep, 2, sum)
  
  p_ppc <- ggplot() +
    geom_histogram(aes(x = survey_totals_pred), fill = "skyblue", alpha = 0.5, bins = 30) +
    geom_vline(aes(xintercept = mean(survey_totals_obs)), color = "red", linetype = "dashed") +
    labs(x = "Total Counts per Survey", y = "Frequency",
         title = "Posterior Predictive Check: Total Survey Counts") +
    theme_minimal()
  print(p_ppc)
  
  # Plot marginal (conditional) effects
  ce <- conditional_effects(fit, categorical = TRUE)
  plot(ce)
  
  # Run hypothesis tests for the HTLP functional group
  print(hypothesis(fit, "muHTLP_ClassificationFringing = 0"))
  print(hypothesis(fit, "muHTLP_ClassificationPinnacle = 0"))
  print(hypothesis(fit, "muHTLP_ClassificationPinnacle - muHTLP_ClassificationFringing = 0"))
  
  # Fit an alternative model with a random intercept for Site
  fit_re <- brm(
    formula = cbind(Herbivore, Invertivore, Mesopredator, HTLP) | trials(total_fish) ~ Classification + (1 | Site),
    data = survey_level,
    family = multinomial(),
    chains = 4,
    cores = 4,
    iter = iter,
    warmup = warmup,
    control = control_list
  )
  print(summary(fit_re))
  
  # Optionally, save the models
  save(fit, file = "FG_fit.RData")
  save(fit_re, file = "FG_fit_re.RData")
  
  # Return key objects
  list(survey_level = survey_level, fit = fit, fit_re = fit_re, p_ppc = p_ppc)
}

# Run the function using your cleaned fish_long data
results <- run_multinomial_regression(fish_long)

print(summary(results$fit))
print(summary(results$fit_re))

# hypothesis tests 
# Define functional groups for which coefficients are estimated (excluding the baseline: Herbivore)
functional_groups <- c("Invertivore", "Mesopredator", "HTLP")
comparison_labels <- c("Fringing vs. Shipwreck", "Pinnacle vs. Shipwreck", "Pinnacle vs. Fringing")

# Initialize an empty data frame for the summary table
summary_table <- data.frame(
  Functional_Group = character(),
  Comparison = character(),
  Estimate = numeric(),
  Est.Error = numeric(),
  CI.Lower = numeric(),
  CI.Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each functional group and run three hypothesis tests:
# 1. Fringing vs. Shipwreck
# 2. Pinnacle vs. Shipwreck
# 3. Pinnacle vs. Fringing
for (g in functional_groups) {
  # Test 1: Fringing vs. Shipwreck for group g
  test1 <- hypothesis(results$fit, paste0("mu", g, "_ClassificationFringing = 0"))
  res1 <- test1$hypothesis
  
  # Test 2: Pinnacle vs. Shipwreck for group g
  test2 <- hypothesis(results$fit, paste0("mu", g, "_ClassificationPinnacle = 0"))
  res2 <- test2$hypothesis
  
  # Test 3: Pinnacle vs. Fringing for group g
  test3 <- hypothesis(results$fit, paste0("mu", g, "_ClassificationPinnacle - mu", g, "_ClassificationFringing = 0"))
  res3 <- test3$hypothesis
  
  summary_table <- rbind(summary_table,
                         data.frame(Functional_Group = g,
                                    Comparison = comparison_labels[1],
                                    Estimate = res1$Estimate[1],
                                    Est.Error = res1$Est.Error[1],
                                    CI.Lower = res1$CI.Lower[1],
                                    CI.Upper = res1$CI.Upper[1],
                                    stringsAsFactors = FALSE),
                         data.frame(Functional_Group = g,
                                    Comparison = comparison_labels[2],
                                    Estimate = res2$Estimate[1],
                                    Est.Error = res2$Est.Error[1],
                                    CI.Lower = res2$CI.Lower[1],
                                    CI.Upper = res2$CI.Upper[1],
                                    stringsAsFactors = FALSE),
                         data.frame(Functional_Group = g,
                                    Comparison = comparison_labels[3],
                                    Estimate = res3$Estimate[1],
                                    Est.Error = res3$Est.Error[1],
                                    CI.Lower = res3$CI.Lower[1],
                                    CI.Upper = res3$CI.Upper[1],
                                    stringsAsFactors = FALSE)
  )
}

# Print the summary table
print(summary_table)


## forest plot for visualization 
library(ggplot2)

# Filter summary_table to include only comparisons relative to Shipwreck
summary_table_baseline <- subset(summary_table, Comparison %in% c("Fringing vs. Shipwreck", "Pinnacle vs. Shipwreck"))
summary_table_baseline$Comparison <- factor(summary_table_baseline$Comparison,
                                            levels = c("Fringing vs. Shipwreck", "Pinnacle vs. Shipwreck"),
                                            labels = c("Fringing", "Pinnacle"))
summary_table_baseline$Functional_Group <- factor(summary_table_baseline$Functional_Group,
                                                  levels = c("Invertivore", "Mesopredator", "HTLP"))

# Add a dummy variable for y-axis positioning
summary_table_baseline$dummy <- 1

# Create the plot with facets for each functional group
ggplot(summary_table_baseline, aes(x = Estimate, y = dummy, color = Comparison, shape = Comparison)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbarh(aes(xmin = CI.Lower, xmax = CI.Upper),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~ Functional_Group, ncol = 1, scales = "free_y") +
  labs(
    x = "Difference (units)",
    y = "",
    title = "Differences in Functional Group Contributions by Site Type",
    subtitle = "Shipwreck is the baseline: Negative = lower; Positive = higher"
  ) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "#D3D3D3", color = "black"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )


