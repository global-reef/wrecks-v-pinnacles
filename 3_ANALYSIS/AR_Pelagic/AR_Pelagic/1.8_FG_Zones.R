### zone factor setup and survey-level data #########################################################
library(dplyr)
library(tidyr)
library(brms)
library(lubridate)
library(ggplot2)
library(tidybayes)
library(stringr)
library(purrr)
library(posterior)

pelagic_sites <- c("Chumphon", "Southwest", "White Rock")

fish_long <- fish_long %>%
  mutate(
    Zone = case_when(
      Classification == "Shipwreck" ~ "wreck",
      Site %in% pelagic_sites       ~ "pelagic",
      TRUE                          ~ "nearshore"
    ),
    Zone = factor(Zone, levels = c("wreck", "nearshore", "pelagic"))
  )

survey_level2 <- fish_long %>%
  mutate(
    Year = factor(year(Date)),
    survey_run = paste0(survey_id, "_", as.character(Researcher))
  ) %>%
  group_by(survey_run, survey_id, Year, Site, Classification, Zone, Functional_Group, Researcher, Date) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Functional_Group, values_from = Count, values_fill = 0) %>%
  mutate(
    Grazer       = ifelse(is.na(Grazer), 0, Grazer),
    Invertivore  = ifelse(is.na(Invertivore), 0, Invertivore),
    Mesopredator = ifelse(is.na(Mesopredator), 0, Mesopredator),
    HTLP         = ifelse(is.na(HTLP), 0, HTLP),
    Zone = factor(Zone, levels = c("wreck","nearshore","pelagic"))
  )

### fit zone models (Zone replaces Classification) ##################################################
fit_zone <- brm(
  formula = mvbind(Grazer, Invertivore, Mesopredator, HTLP) ~ Zone + (1 | p | Site),
  data = survey_level2,
  family = negbinomial(),
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = list(adapt_delta = 0.9, max_treedepth = 20),
  backend = "cmdstanr"
)
summary(fit_zone)

bayes_R2(fit_zone)
bayes_R2(best_fit)

fit_zone_yr <- brm(
  formula = mvbind(Grazer, Invertivore, Mesopredator, HTLP) ~ Zone + (1 | p | Site) + (1 | Year),
  data = survey_level2,
  family = negbinomial(),
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = list(adapt_delta = 0.9, max_treedepth = 20),
  backend = "cmdstanr"
)
summary(fit_zone_yr)
bayes_R2(fit_zone_yr)
loo_compare(loo(best_fit), loo(fit_zone_yr))

fit_zone_my <- brm(
  formula = mvbind(Grazer, Invertivore, Mesopredator, HTLP) ~ Zone + (1 | p | Site) + (1 | Month_Year),
  data = survey_level,   # Month_Year lives here from the previous script
  family = negbinomial(),
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = list(adapt_delta = 0.9, max_treedepth = 20),
  backend = "cmdstanr"
)
summary(fit_zone_my)
loo_compare(loo(best_fit), loo(fit_zone_my))

fit_zone <- fit_zone_my

### posterior epreds by zone and proportional composition ##########################################
newdata <- data.frame(
  Zone = factor(c("wreck", "nearshore", "pelagic"), levels = c("wreck", "nearshore", "pelagic")),
  Site = NA
)

epreds <- posterior_epred(fit_zone, newdata = newdata, re_formula = NA)

dimnames(epreds) <- list(
  draw = 1:dim(epreds)[1],
  Zone = c("wreck", "nearshore", "pelagic"),
  Functional_Group = c("Grazer", "Invertivore", "Mesopredator", "HTLP")
)

tidy_epreds <- as.data.frame.table(epreds) %>%
  rename(draw = draw, Zone = Zone, Functional_Group = Functional_Group, Abundance = Freq) %>%
  mutate(draw = as.integer(draw))

prop_draws <- tidy_epreds %>%
  group_by(draw, Zone) %>%
  mutate(Proportion = Abundance / sum(Abundance)) %>%
  ungroup()

prop_draws_plot <- ggplot(prop_draws, aes(x = Zone, y = Proportion, fill = Functional_Group)) +
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
  theme_clean + theme(text = element_text(size = 16))
print(prop_draws_plot)

### raw totals and proportions by zone (from observed counts) ######################################
fg_zone <- fish_long %>%
  group_by(Zone, Functional_Group) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Zone) %>%
  mutate(Proportion = Total / sum(Total))

proportion_plot_zone <- ggplot(fg_zone, aes(x = Zone, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportional Composition of Functional Groups by Zone",
       x = "Zone", y = "Proportion of Total Fish Count",
       fill = "Functional \n Group") +
  theme_clean + theme(text = element_text(size = 16)) +
  scale_fill_brewer(palette = "BuGn")
print(proportion_plot_zone)

### zone fixed-effects: forest-style summary ########################################################
zone_effects <- fit_zone %>%
  gather_draws(`.*Zone.*`, regex = TRUE) %>%
  mutate(
    Functional_Group = case_when(
      str_detect(.variable, "Grazer") ~ "Grazer",
      str_detect(.variable, "Invertivore") ~ "Invertivore",
      str_detect(.variable, "Mesopredator") ~ "Mesopredator",
      str_detect(.variable, "HTLP") ~ "HTLP"
    ),
    Zone = case_when(
      str_detect(.variable, "Zonenearshore") ~ "nearshore",
      str_detect(.variable, "Zonepelagic")   ~ "pelagic"
    )
  )

zone_summary <- zone_effects %>%
  group_by(Functional_Group, Zone) %>%
  summarise(
    Estimate = median(.value),
    LowerCI  = quantile(.value, 0.025),
    UpperCI  = quantile(.value, 0.975),
    .groups  = "drop"
  )

zone_forest_plot <- ggplot(zone_summary, aes(x = Estimate, y = Functional_Group, color = Zone)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "BuGn") +
  labs(
    title = "Effect of Zone on Functional Group Abundance (log-scale)",
    x = "Posterior Median Estimate (log-scale)",
    y = "Functional Group"
  ) +
  theme_clean + theme(text = element_text(size = 16))
print(zone_forest_plot)

### proportional composition by zone from model predictions #########################################
pred_df <- conditional_effects(fit_zone, effects = "Zone", re_formula = NA)

pred_abund <- bind_rows(
  mutate(pred_df[[1]], Functional_Group = "Grazer"),
  mutate(pred_df[[2]], Functional_Group = "Invertivore"),
  mutate(pred_df[[3]], Functional_Group = "Mesopredator"),
  mutate(pred_df[[4]], Functional_Group = "HTLP")
)

pred_abund_prop <- pred_abund %>%
  group_by(Zone) %>%
  mutate(
    Total        = sum(estimate__),
    Total_lower  = sum(lower__),
    Total_upper  = sum(upper__),
    Proportion        = estimate__ / Total,
    Proportion_lower  = lower__   / Total_upper,
    Proportion_upper  = upper__   / Total_lower
  ) %>%
  ungroup()

fg_zone_plot <- ggplot(pred_abund_prop, aes(x = Zone, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack", color = "white") +
  labs(title = " ", y = "Proportion of Predicted Abundance", x = "Zone", fill = "Functional \n Group") +
  theme_clean + theme(text = element_text(size = 16)) +
  scale_fill_brewer(palette = "BuGn")
print(fg_zone_plot)  # FIGURE 4

### posterior differences in log-abundance by zone (vs wrecks) #####################################
zone_diffs <- fit_zone %>%
  spread_draws(
    b_Grazer_Zonenearshore,      b_Grazer_Zonepelagic,
    b_Invertivore_Zonenearshore, b_Invertivore_Zonepelagic,
    b_Mesopredator_Zonenearshore,b_Mesopredator_Zonepelagic,
    b_HTLP_Zonenearshore,        b_HTLP_Zonepelagic
  ) %>%
  pivot_longer(cols = starts_with("b_"), names_to = "parameter", values_to = "value") %>%
  mutate(
    Functional_Group = case_when(
      str_detect(parameter, "Grazer")       ~ "Grazer",
      str_detect(parameter, "Invertivore")  ~ "Invertivore",
      str_detect(parameter, "Mesopredator") ~ "Mesopredator",
      str_detect(parameter, "HTLP")         ~ "HTLP"
    ),
    Zone = case_when(
      str_detect(parameter, "Zonenearshore") ~ "nearshore",
      str_detect(parameter, "Zonepelagic")   ~ "pelagic"
    )
  ) %>%
  mutate(
    Functional_Group = factor(Functional_Group, levels = c("Grazer", "Invertivore","Mesopredator","HTLP")),
    Zone = factor(Zone, levels = c("nearshore", "pelagic"), labels = c("Nearshore", "Pelagic"))
  )

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
  scale_fill_manual(values = c("Nearshore" = "#188041", "Pelagic" = "#a6dede")) +
  theme_clean + theme(text = element_text(size = 16))
print(zone_diff_plot)  # FIGURE 5

### save zone-level outputs ########################################################################
save_zone_proportion_outputs <- function(prop_draws_plot, zone_diff_plot, fg_zone_plot, output_dir, analysis_date) {
  ggsave(
    filename = file.path(output_dir, paste0("Posterior_FunctionalGroup_Proportions_halfeye_", analysis_date, ".png")),
    plot = prop_draws_plot, width = 8, height = 6
  )
  ggsave(
    filename = file.path(output_dir, paste0("FIG4_Zonewise_FunctionalGroup_Composition_", analysis_date, ".png")),
    plot = fg_zone_plot, width = 8, height = 6
  )
  ggsave(
    filename = file.path(output_dir, paste0("FIG5_Zonewise_Posterior_Differences_", analysis_date, ".png")),
    plot = zone_diff_plot, width = 8, height = 6
  )
  message("✅ Zone-level plots saved to: ", output_dir)
}

save_zone_proportion_outputs(
  prop_draws_plot = prop_draws_plot,
  zone_diff_plot  = zone_diff_plot,
  fg_zone_plot    = fg_zone_plot,
  output_dir      = output_dir,
  analysis_date   = analysis_date
)

### posterior probabilities for zone contrasts ######################################################
compute_posterior_probabilities <- function(fit_model, baseline_zone = "wreck") {
  draws <- as_draws_df(fit_model)
  groups <- c("Grazer", "Invertivore", "Mesopredator", "HTLP")
  comparisons <- list(
    c("nearshore", "wreck"),
    c("pelagic",   "wreck"),
    c("pelagic",   "nearshore")
  )
  
  map_dfr(groups, function(group) {
    map_dfr(comparisons, function(comp) {
      a <- comp[1]; b <- comp[2]
      term_a <- paste0("b_", group, "_Zone", a)
      term_b <- if (b == baseline_zone) 0 else paste0("b_", group, "_Zone", b)
      
      diff_samples <- if (identical(term_b, 0)) draws[[term_a]] else draws[[term_a]] - draws[[term_b]]
      
      tibble(
        Functional_Group = group,
        Comparison = paste(a, "–", b),
        Pr_greater_0 = mean(diff_samples > 0),
        Pr_less_0    = mean(diff_samples < 0),
        Median       = median(diff_samples),
        CI_lower     = quantile(diff_samples, 0.025),
        CI_upper     = quantile(diff_samples, 0.975)
      )
    })
  })
}

posterior_probs_zone <- compute_posterior_probabilities(fit_zone)
print(posterior_probs_zone)

saveRDS(fit_zone,    file = file.path(output_dir, "fit_zone.rds"))
saveRDS(fit_zone_yr, file = file.path(output_dir, "fit_zone_yr.rds"))
