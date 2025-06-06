

## --------------------------------------------- based on fit_re ----------------------------------- # 

library(tibble)
library(tidyr)
library(stringr)


# create custom ggplot2 theme 
theme_clean <- theme_minimal(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )
# to run it use : your_plot + theme_clean


# Create a data frame with predicted values based on the model summary.
# (Baseline = Shipwreck; effects for Fringing and Pinnacle are added to the baseline intercept.)
# Build new pred_df from actual posterior means
coefs <- fixef(fit_re)
pred_df <- tibble::tribble(
  ~Functional_Group,  ~Classification, ~Intercept, ~Effect,
  "Herbivore",        "Shipwreck",     coefs["Herbivore_Intercept", "Estimate"], 0,
  "Herbivore",        "Fringing",      coefs["Herbivore_Intercept", "Estimate"], coefs["Herbivore_ClassificationFringing", "Estimate"],
  "Herbivore",        "Pinnacle",      coefs["Herbivore_Intercept", "Estimate"], coefs["Herbivore_ClassificationPinnacle", "Estimate"],
  "Invertivore",      "Shipwreck",     coefs["Invertivore_Intercept", "Estimate"], 0,
  "Invertivore",      "Fringing",      coefs["Invertivore_Intercept", "Estimate"], coefs["Invertivore_ClassificationFringing", "Estimate"],
  "Invertivore",      "Pinnacle",      coefs["Invertivore_Intercept", "Estimate"], coefs["Invertivore_ClassificationPinnacle", "Estimate"],
  "Mesopredator",     "Shipwreck",     coefs["Mesopredator_Intercept", "Estimate"], 0,
  "Mesopredator",     "Fringing",      coefs["Mesopredator_Intercept", "Estimate"], coefs["Mesopredator_ClassificationFringing", "Estimate"],
  "Mesopredator",     "Pinnacle",      coefs["Mesopredator_Intercept", "Estimate"], coefs["Mesopredator_ClassificationPinnacle", "Estimate"],
  "HTLP",             "Shipwreck",     coefs["HTLP_Intercept", "Estimate"], 0,
  "HTLP",             "Fringing",      coefs["HTLP_Intercept", "Estimate"], coefs["HTLP_ClassificationFringing", "Estimate"],
  "HTLP",             "Pinnacle",      coefs["HTLP_Intercept", "Estimate"], coefs["HTLP_ClassificationPinnacle", "Estimate"]
) %>%
  mutate(Predicted_Log = Intercept + Effect,
         Predicted = exp(Predicted_Log)) 

# Check the table (optional)
pred_df <- as.data.frame(pred_df)
print(pred_df)

# Create a grouped bar plot faceted by Functional Group.
abundance_plot <- ggplot(pred_df, aes(x = Classification, y = Predicted, fill = Classification)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  labs(title = "Predicted Abundance by Site Classification",
       x = "Site Classification",
       y = "Predicted Abundance") +
  theme_clean +
  scale_fill_brewer(palette = "BuGn")

print(abundance_plot)

## proportions
pred_df_prop <- pred_df %>%
  group_by(Classification) %>%
  mutate(Proportion = Predicted / sum(Predicted)) %>%
  ungroup()

proportion_plot <- ggplot(pred_df_prop, aes(x = Classification, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportional Composition of Functional Groups by Site Classification",
       x = "Site Classification",
       y = "Proportion of Total Predicted Abundance") +
  theme_clean +
  scale_fill_brewer(palette = "BuGn") # or "Greys" or "PuBu"

print(proportion_plot)




# forest plots 
library(dplyr)
library(tidyr)
library(ggplot2)

# Extract fixed effects from your functional group model
fixef_df <- as.data.frame(fixef(fit_re))
fixef_df$Parameter <- rownames(fixef_df)

# Rename the interval columns: Q2.5 -> Lower, Q97.5 -> Upper
fixef_df <- fixef_df %>%
  rename(Lower = `Q2.5`, Upper = `Q97.5`)

# Filter to keep only classification effects (i.e., drop intercepts)
forest_df <- fixef_df %>%
  filter(grepl("Classification", Parameter)) %>%
  separate(Parameter, into = c("Group", "Effect"), sep = "_", extra = "merge", fill = "right") %>%
  mutate(Effect = gsub("Classification", "", Effect),
         Group = gsub("^mu", "", Group),
         Label = paste0(Group, " (", Effect, ")"))

# Create the forest plot
forest_plot <- ggplot(forest_df, aes(x = Estimate, y = Label)) +
  geom_point() +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2) +
  labs(title = "Forest Plot: Classification Effects on Functional Groups",
       x = "Estimated Change (log scale)",
       y = "Functional Group (Comparison)") +
  theme_clean 
print(forest_plot)


library(tidybayes)
library(ggplot2)

# Generate posterior draws
draws <- fit_re %>%
  spread_draws(b_Herbivore_ClassificationFringing,
               b_Herbivore_ClassificationPinnacle,
               b_Invertivore_ClassificationFringing,
               b_Invertivore_ClassificationPinnacle,
               b_Mesopredator_ClassificationFringing,
               b_Mesopredator_ClassificationPinnacle,
               b_HTLP_ClassificationFringing,
               b_HTLP_ClassificationPinnacle)

# Reshape to long format
draws_long <- draws %>%
  pivot_longer(cols = starts_with("b_"),
               names_to = "Parameter",
               values_to = "Difference") %>%
  separate(Parameter, into = c("Junk", "Group", "Contrast"), sep = "_", extra = "merge") %>%
  mutate(Comparison = gsub("Classification", "", Contrast),
         Group = factor(Group, levels = c("Herbivore", "Invertivore", "Mesopredator", "HTLP")),
         Comparison = factor(Comparison, levels = c("Fringing", "Pinnacle")))


posterior_differences <-
  ggplot(draws_long, aes(x = Difference, y = Group, fill = Comparison)) +
  stat_halfeye(slab_alpha = 0.8, point_interval = median_qi, .width = 0.95,
               position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Posterior Differences in Log-Abundance (vs. Shipwrecks)",
    x = "Estimated Log Difference (relative to Shipwreck)",
    y = "Functional Group"
  ) +
  scale_fill_manual(values = c("#188041", "#a6dede")) +
  theme_minimal(base_size = 12) +
  theme_clean 

  



save_model_outputs <- function(pred_df, abundance_plot, forest_plot, posterior_differences, proportion_plot, output_dir, analysis_date) {
  # Save predicted data table
  write.csv(pred_df, file = file.path(output_dir, paste0("Predicted_Abundance_", analysis_date, ".csv")),
            row.names = FALSE)
  # Save abundance bar plot
  ggsave(filename = file.path(output_dir, paste0("Predicted_Abundance_Bars_", analysis_date, ".png")),
         plot = abundance_plot, width = 8, height = 6)
  # Save forest plot
  ggsave(filename = file.path(output_dir, paste0("Classification_ForestPlot_", analysis_date, ".png")),
         plot = forest_plot, width = 8, height = 6)
  # posterior differences
  ggsave(filename = file.path(output_dir, paste0("Posterior_Differences_", analysis_date, ".png")),
         plot = posterior_differences, width = 8, height = 6)
  # Save proportional plot
  ggsave(filename = file.path(output_dir, paste0("Proportional_Abundance_", analysis_date, ".png")),
         plot = proportion_plot, width = 8, height = 6)
  message("✅ Model predictions and all plots saved to: ", output_dir)
}


save_model_outputs(pred_df, abundance_plot, forest_plot, posterior_differences, proportion_plot, output_dir, analysis_date)





##### Function to compute posterior probabilities for all zone comparisons
library(posterior)
library(tidybayes)
library(purrr)
library(dplyr)

compute_posterior_probabilities_re <- function(fit_model, baseline_zone = "Shipwreck") {
  draws <- as_draws_df(fit_model)
  groups <- c("Herbivore", "Invertivore", "Mesopredator", "HTLP")
  
  comparisons <- list(
    c("Fringing", "Shipwreck"),
    c("Pinnacle", "Shipwreck"),
    c("Pinnacle", "Fringing")
  )
  
  probs <- map_dfr(groups, function(group) {
    map_dfr(comparisons, function(comp) {
      a <- comp[1]
      b <- comp[2]
      term_a <- paste0("b_", group, "_Classification", a)
      term_b <- if (b == baseline_zone) 0 else paste0("b_", group, "_Classification", b)
      
      diff_samples <- if (term_b == 0) {
        draws[[term_a]]
      } else {
        draws[[term_a]] - draws[[term_b]]
      }
      
      tibble(
        Functional_Group = group,
        Comparison = paste(a, "–", b),
        Pr_greater_0 = mean(diff_samples > 0),
        Pr_less_0 = mean(diff_samples < 0),
        Median = median(diff_samples),
        CI_lower = quantile(diff_samples, 0.025),
        CI_upper = quantile(diff_samples, 0.975)
      )
    })
  })
  
  return(probs)
}


# Example usage
posterior_probs_re <- compute_posterior_probabilities_re(fit_re, baseline_zone = "Shipwreck")
print(posterior_probs_re)
