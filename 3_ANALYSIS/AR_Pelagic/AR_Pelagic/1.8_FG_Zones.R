library(dplyr)
library(tidyr)
library(brms)
library(lubridate)
library(ggplot2)


pelagic_sites <- c("Chumphon", "Southwest", "No Name Pinnacle")

fish_long <- fish_long %>%
  mutate(
    Zone = case_when(
      Classification == "Shipwreck" ~ "wreck",         # override first
      Site %in% pelagic_sites       ~ "pelagic",       # then pelagic
      TRUE                          ~ "nearshore"      # default fallback
    ),
    Zone = factor(Zone, levels = c("wreck", "nearshore", "pelagic"))  # <-- set reference
  )

survey_level2 <- fish_long %>%
  group_by(survey_id, Site, Classification, Zone, Functional_Group, Researcher) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  spread(key = Functional_Group, value = Count, fill = 0) %>%
  mutate(
    Herbivore    = ifelse(is.na(Herbivore), 0, Herbivore),
    Invertivore  = ifelse(is.na(Invertivore), 0, Invertivore),
    Mesopredator = ifelse(is.na(Mesopredator), 0, Mesopredator),
    HTLP         = ifelse(is.na(HTLP), 0, HTLP),
    Zone = factor(Zone, levels = c("wreck", "nearshore", "pelagic"))  # explicitly re-level
  )




# try with zones instead of pinnacle/fringing 
fit_zone <- brm(
  formula = mvbind(Herbivore, Invertivore, Mesopredator, HTLP) ~ Zone + (1 | p | Site),
  data = survey_level2,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 500,
  control = list(adapt_delta = 0.9, max_treedepth = 20),
  backend = "cmdstanr"
)
summary(fit_zone)
loo_compare(loo(fit_re), loo(fit_zone))

#### posterior draws 
# generate new data 
library(brms)
library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(tidyverse)

# Create newdata: 1 row per zone
newdata <- data.frame(
  Zone = factor(c("wreck", "nearshore", "pelagic"), levels = c("wreck", "nearshore", "pelagic")),
  Site = NA  # Required for random effect term; set to NA to exclude them via re_formula = NA
)

# get posterior draws 
epreds <- posterior_epred(fit_zone, newdata = newdata, re_formula = NA)


# Add dimension names to array
dimnames(epreds) <- list(
  draw = 1:dim(epreds)[1],
  Zone = c("wreck", "nearshore", "pelagic"),
  Functional_Group = c("Herbivore", "Invertivore", "Mesopredator", "HTLP")
)

# Convert to tidy tibble
tidy_epreds <- as.data.frame.table(epreds) %>%
  rename(
    draw = draw,
    Zone = Zone,
    Functional_Group = Functional_Group,
    Abundance = Freq
  ) %>%
  mutate(draw = as.integer(draw))
# calculate within draw proportions 
prop_draws <- tidy_epreds %>%
  group_by(draw, Zone) %>%
  mutate(Proportion = Abundance / sum(Abundance)) %>%
  ungroup()
head(prop_draws)

prop_draws_plot <-ggplot(prop_draws, aes(x = Zone, y = Proportion, fill = Functional_Group)) +
  stat_halfeye(
    position = position_dodge(width = 0.6),
    slab_alpha = 0.6,
    point_interval = median_qi,
    .width = 0.95
  ) +
  labs(
    title = "Posterior Distribution of Functional Group Proportions by Zone",
    y = "Proportion of Total Abundance",
    x = "Zone"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_clean 
print(prop_draws_plot)

# Summarize and calculate proportions by Zone
fg_zone <- fish_long %>%
  group_by(Zone, Functional_Group) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Zone) %>%
  mutate(Proportion = Total / sum(Total))

# Plot
proportion_plot_zone <- ggplot(fg_zone, aes(x = Zone, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportional Composition of Functional Groups by Zone",
       x = "Zone",
       y = "Proportion of Total Fish Count") +
  theme_clean +
  scale_fill_brewer(palette = "BuGn") # or "Greys" or "PuBu"

print(proportion_plot_zone)


## --------------------------------------------- based on zones----------------------------------- # 

library(tidybayes)
library(ggplot2)
library(dplyr)
library(stringr)

# Extract fixed effects for Zones from the model
zone_effects <- fit_zone %>%
  gather_draws(`.*Zone.*`, regex = TRUE) %>%
  mutate(
    Functional_Group = case_when(
      str_detect(.variable, "Herbivore") ~ "Herbivore",
      str_detect(.variable, "Invertivore") ~ "Invertivore",
      str_detect(.variable, "Mesopredator") ~ "Mesopredator",
      str_detect(.variable, "HTLP") ~ "HTLP"
    ),
    Zone = case_when(
      str_detect(.variable, "Zonenearshore") ~ "nearshore",
      str_detect(.variable, "Zonepelagic") ~ "pelagic"
    )
  )

# Summarize posterior draws for plotting
zone_summary <- zone_effects %>%
  group_by(Functional_Group, Zone) %>%
  summarise(
    Estimate = median(.value),
    LowerCI = quantile(.value, 0.025),
    UpperCI = quantile(.value, 0.975),
    .groups = "drop"
  )

# Plot: Forest plot
ggplot(zone_summary, aes(x = Estimate, y = Functional_Group, color = Zone)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() + 
  scale_fill_brewer(palette = "BuGn") +
  labs(
    title = "Effect of Zone on Functional Group Abundance (log-scale)",
    x = "Posterior Median Estimate (log-scale)",
    y = "Functional Group"
  )

#  Get predicted abundance per Zone per functional group
pred_df <- conditional_effects(fit_zone, effects = "Zone", re_formula = NA)

# Extract the predicted data and bind functional group identity
pred_abund <- bind_rows(
  mutate(pred_df[[1]], Functional_Group = "Herbivore"),
  mutate(pred_df[[2]], Functional_Group = "Invertivore"),
  mutate(pred_df[[3]], Functional_Group = "Mesopredator"),
  mutate(pred_df[[4]], Functional_Group = "HTLP")
)

# Normalize within zone to get proportional predictions
pred_abund_prop <- pred_abund %>%
  group_by(Zone) %>%
  mutate(
    Total = sum(estimate__),
    Total_lower = sum(lower__),
    Total_upper = sum(upper__),
    Proportion = estimate__ / Total,
    Proportion_lower = lower__ / Total_upper,  # conservative lower bound
    Proportion_upper = upper__ / Total_lower   # conservative upper bound
  ) %>%
  ungroup()

# Plot: Proportional composition
library(ggplot2)

 ggplot(pred_abund_prop, aes(x = Zone, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  theme_clean +
  labs(
    title = " ",
    y = "Proportion of Predicted Abundance",
    x = "Zone"
  ) +
  scale_fill_brewer(palette = "BuGn")

 
library(tidybayes)
library(ggplot2)
library(dplyr)
library(tidyr)

 # Extract and reshape posterior draws for zone-level effects
 zone_diffs <- fit_zone %>%
   spread_draws(
     b_Herbivore_Zonenearshore, b_Herbivore_Zonepelagic,
     b_Invertivore_Zonenearshore, b_Invertivore_Zonepelagic,
     b_Mesopredator_Zonenearshore, b_Mesopredator_Zonepelagic,
     b_HTLP_Zonenearshore, b_HTLP_Zonepelagic
   ) %>%
   pivot_longer(
     cols = starts_with("b_"),
     names_to = "parameter",
     values_to = "value"
   ) %>%
   mutate(
     Functional_Group = case_when(
       str_detect(parameter, "Herbivore") ~ "Herbivore",
       str_detect(parameter, "Invertivore") ~ "Invertivore",
       str_detect(parameter, "Mesopredator") ~ "Mesopredator",
       str_detect(parameter, "HTLP") ~ "HTLP"
     ),
     Zone = case_when(
       str_detect(parameter, "Zonenearshore") ~ "nearshore",
       str_detect(parameter, "Zonepelagic") ~ "pelagic"
     )
   ) %>%
   mutate(Functional_Group = factor(
     Functional_Group,
     levels = c("Herbivore", "Invertivore","Mesopredator","HTLP")
     )) %>%
       mutate(Zone = factor(
         Zone,
         levels = c("nearshore", "pelagic"),
         labels = c("Nearshore", "Pelagic")
       ))
 
 # Plot posterior differences relative to wrecks
 zone_diff_plot <- ggplot(zone_diffs, aes(x = value, y = Functional_Group, fill = Zone)) +
   stat_halfeye(
     slab_alpha = 0.8,
     point_interval = median_qi,
     .width = 0.95,
     position = position_dodge(width = 0.6)
   ) +
   geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
   labs(
     title = "Posterior Differences in Log-Abundance\n (vs. Shipwrecks)",
     x = "Estimated Log Difference (relative to Shipwreck)",
     y = "Functional Group",
     fill = "Comparison"
   ) +
   scale_fill_manual(values = c(
     "Nearshore" = "#188041",
     "Pelagic" = "#a6dede"
   )) +
   theme_clean
 
 print(zone_diff_plot)
 

save_zone_proportion_outputs <- function(prop_draws_plot,  zone_diff_plot, fg_zone_plot, output_dir, analysis_date) {
  # Save posterior distribution plot
  ggsave(
    filename = file.path(output_dir, paste0("Posterior_FunctionalGroup_Proportions_", analysis_date, ".png")),
    plot = prop_draws_plot,
    width = 8,
    height = 6
  )
  
  # Save proportional composition bar plot
  ggsave(
    filename = file.path(output_dir, paste0("Zonewise_FunctionalGroup_Composition_", analysis_date, ".png")),
    plot = fg_zone_plot,
    width = 8,
    height = 6
  )
  # Save posterior differences
  ggsave(
    filename = file.path(output_dir, paste0("Zonewise_Posteror_Differences_", analysis_date, ".png")),
    plot =  zone_diff_plot,
    width = 8,
    height = 6
  )

  message("✅ Zone-level plots saved to: ", output_dir)
}

# Call the function
save_zone_proportion_outputs(prop_draws_plot = prop_draws_plot, fg_zone_plot = proportion_plot_zone, output_dir = output_dir,  zone_diff_plot =  zone_diff_plot, analysis_date = analysis_date)





##### Compute posterior probability that group abundance is higher in pelagic than wrecks

library(tidybayes)

library(tidyverse)
library(tidybayes)

# Function to compute posterior probabilities for all zone comparisons
compute_posterior_probabilities <- function(fit_model, baseline_zone = "wreck") {
  # Extract posterior draws
  draws <- as_draws_df(fit_model)
  
  # Define all functional groups
  groups <- c("Herbivore", "Invertivore", "Mesopredator", "HTLP")
  
  # Define comparisons
  comparisons <- list(
    c("nearshore", "wreck"),
    c("pelagic", "wreck"),
    c("pelagic", "nearshore")
  )
  
  # Loop through each group and comparison
  probs <- map_dfr(groups, function(group) {
    map_dfr(comparisons, function(comp) {
      a <- comp[1]
      b <- comp[2]
      term_a <- paste0("b_", group, "_Zone", a)
      term_b <- if (b == baseline_zone) 0 else paste0("b_", group, "_Zone", b)
      
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
posterior_probs <- compute_posterior_probabilities(fit_zone)
print(posterior_probs)





