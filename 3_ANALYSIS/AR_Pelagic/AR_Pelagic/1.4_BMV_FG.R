### run functional group analysis as a multivariate instead of multinomial model so we can understand the differences between ALL functional groups
library(dplyr)
library(tidyr)
library(brms)
library(lubridate)
library(ggplot2)
 


# run the model 
run_functional_group_mv_regression <- function(fish_long, 
                                               dist = "negbinomial",  # options: "negbinomial" or "poisson"
                                               iter = 2000, warmup = 500,
                                               control_list = list(adapt_delta = 0.95, max_treedepth = 15)) {

  
  # Aggregate the long data to survey-level wide format using spread()
  survey_level <- fish_long %>%
    group_by(survey_id, Site, Classification, Zone, Functional_Group, Researcher) %>%
    summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    spread(key = Functional_Group, value = Count, fill = 0)
  
  # Ensure all four functional groups are present; if missing, set to 0
  survey_level <- survey_level %>%
    mutate(
      Herbivore    = ifelse(is.na(Grazer), 0, Grazer),
      Invertivore  = ifelse(is.na(Invertivore), 0, Invertivore),
      Mesopredator = ifelse(is.na(Mesopredator), 0, Mesopredator),
      HTLP         = ifelse(is.na(HTLP), 0, HTLP)
    )
  
  # Relevel Classification so that "Shipwreck" is the reference (baseline)
  survey_level <- survey_level %>%
    mutate(Classification = relevel(as.factor(Classification), ref = "Shipwreck"))
  
  # (Optional) Create total_fish if needed for other purposes:
  survey_level <- survey_level %>%
    mutate(total_fish = Grazer + Invertivore + Mesopredator + HTLP)
  
  # Create the multivariate response formula (without a trials() term)
  response_formula <- as.formula("mvbind(Grazer, Invertivore, Mesopredator, HTLP) ~ Classification")
  print(response_formula)
  
  # Select the family function based on the 'dist' argument
  family_obj <- if (dist == "negbinomial") {
    negbinomial()
  } else if (dist == "poisson") {
    poisson()
  } else {
    stop("Unknown distribution. Use 'negbinomial' or 'poisson'.")
  }
  
  # Fit the multivariate regression model
  fit_fg_mv <- brm(
    formula = response_formula,
    data = survey_level,
    family = family_obj,
    chains = 4,
    cores = 4,
    iter = iter,
    warmup = warmup,
    control = control_list
  )
  
  print(summary(fit_fg_mv))
  
  # Return the aggregated data and the fitted model
  list(survey_level = survey_level, fit_fg_mv = fit_fg_mv)
}

# Example usage:
results_fg_mv <- run_functional_group_mv_regression(fish_long, dist = "negbinomial")

# Now you can inspect the model:
summary(results_fg_mv$fit_fg_mv)
# evidence of overdisperson here - should try random effect for site, and also try for zero-inflated neg binom model 
survey_level <- results_fg_mv$survey_level


#####  try adding a random intercept for site 

survey_level <- fish_long %>%
  mutate(survey_run = paste0(survey_id, "_", Researcher)) %>%
  group_by(survey_run, survey_id, Site, Classification, Zone, Date, Functional_Group) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Functional_Group, values_from = Count, values_fill = 0) %>%
  mutate(Year = factor(year(Date)),
         Classification = relevel(factor(Classification), ref = "Shipwreck"))

# adding month and year 
survey_level <- fish_long %>%
  mutate(survey_run = paste0(survey_id, "_", Researcher)) %>%
  group_by(survey_run, survey_id, Site, Classification, Zone, Date, Functional_Group) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Functional_Group, values_from = Count, values_fill = 0) %>%
  mutate(
    Year = factor(year(Date)),
    Month_Year = format(floor_date(Date, "month"), "%Y-%m"),
    Month_Year = factor(
      Month_Year,
      levels = format(sort(unique(floor_date(Date, "month"))), "%Y-%m")  # keeps chrono order
    ),
    Classification = relevel(factor(Classification), ref = "Shipwreck")
  )




# Fit a multivariate regression with a random intercept for Site
fit_re <- brm(
  formula = mvbind(Grazer, Invertivore, Mesopredator, HTLP) ~ Classification + (1 | p | Site),
  data = survey_level,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 500,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)

### fit_re (negative-binomial with random effect for site) was the best model based on LOO 
summary(fit_re)

saveRDS(fit_re, file = file.path(output_dir, "fit_re.rds"))


### MODEL SELECTION 
# compare some models with loo and WAIC 
library(loo)

# LOO (Leave-One-Out cross-validation)
 # loo_result <- loo(fit_zinb_re)
# print(loo_result) # lower LOOIC = better predicted performance 
# if result is small <2, simpler model is ok to be preffered 

loo_compare(loo(fit_re),loo(results_fg_mv$fit_fg_mv))

#### fit_re (negative-binomial with random effect for site) was the best model based on LOO 
summary(fit_re)

saveRDS(fit_re, output_dir, file = "fit_re.RData")

# Extract predictions excluding random effects
ce_re <- conditional_effects(fit_re, effects = "Classification", re_formula = NA)

fg_summary <- bind_rows(lapply(seq_along(ce_re), function(i) {
  ce_re[[i]] %>%
    select(Classification, estimate__, lower__, upper__) %>%
    mutate(Functional_Group = names(ce_re)[i])
})) %>%
  mutate(
    estimate_fmt = sprintf("%.1f [%.1f–%.1f]", estimate__, lower__, upper__)
  ) %>%
  select(Functional_Group, Classification, estimate_fmt) %>%
  pivot_wider(names_from = Classification, values_from = estimate_fmt)

# View
fg_summary


# Combine functional group predictions into total per Classification
total_pred <- bind_rows(lapply(seq_along(ce_re), function(i) {
  ce_re[[i]] %>%
    select(Classification, estimate__, lower__, upper__) %>%
    mutate(FG = names(ce_re)[i])
})) %>%
  group_by(Classification) %>%
  summarise(
    estimate = sum(estimate__),
    lower = sum(lower__),
    upper = sum(upper__)
  ) %>%
  mutate(
    estimate_fmt = sprintf("%.0f [%.0f–%.0f]", estimate, lower, upper)
  )
total_pred

plot(ce_re)


### testing the addition of year as a random effect - in response to review # 1 ##########
library(dplyr); library(lubridate); library(brms)

# 1) Current best (kept for reference)
# fit_re already exists

# 2) Add random intercept for Year  (brms can't have two seperated  | p | terms ):
fit_re_year_uncorr <- brm(
  bf(Grazer       ~ Classification + (1|p|Site) + (1|Year)) +
    bf(Invertivore  ~ Classification + (1|p|Site) + (1|Year)) +
    bf(Mesopredator ~ Classification + (1|p|Site) + (1|Year)) +
    bf(HTLP         ~ Classification + (1|p|Site) + (1|Year)),
  data = survey_level,
  family = negbinomial(),
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(fit_re, file = file.path(output_dir, "fit_re_yr.rds"))


# try with month-yrs 
# assumes survey_level already has Month_Year as a plain factor in chrono order

fit_months <- brm(
  bf(Grazer        ~ Classification + (1|p|Site) + (1|Month_Year)) +
    bf(Invertivore ~ Classification + (1|p|Site) + (1|Month_Year)) +
    bf(Mesopredator~ Classification + (1|p|Site) + (1|Month_Year)) +
    bf(HTLP        ~ Classification + (1|p|Site) + (1|Month_Year)),
  data = survey_level,
  family = negbinomial(),
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)

saveRDS(fit_months, file = file.path(output_dir, "fit_months.rds"))


# LOO 
library(brms)
# Add LOO (with moment matching) to all three
fit_re             <- add_criterion(fit_re, "loo", moment_match = TRUE)
fit_re_year_uncorr <- add_criterion(fit_re_year_uncorr, "loo", moment_match = TRUE)
fit_months         <- add_criterion(fit_months, "loo", moment_match = TRUE)

# Compare models
cmp <- brms::loo_compare(fit_re, fit_re_year_uncorr, fit_months)
cmp_df <- as.data.frame(cmp)
cmp_df$model <- rownames(cmp_df)
cmp_df

# Problematic observations (k > 0.7) per model
loo::pareto_k_ids(fit_re$criteria$loo,             threshold = 0.7)
loo::pareto_k_ids(fit_re_year_uncorr$criteria$loo, threshold = 0.7)
loo::pareto_k_ids(fit_months$criteria$loo,         threshold = 0.7)

# Bayes R2
brms::bayes_R2(fit_re)
brms::bayes_R2(fit_re_year_uncorr)
brms::bayes_R2(fit_months)

best_fit <- fit_months

saveRDS(fit_months, file = file.path(output_dir, "best_fit.rds"))

# Extract predictions excluding random effects
ce_re <- conditional_effects(best_fit, effects = "Classification", re_formula = NA)

fg_summary <- bind_rows(lapply(seq_along(ce_re), function(i) {
  ce_re[[i]] %>%
    select(Classification, estimate__, lower__, upper__) %>%
    mutate(Functional_Group = names(ce_re)[i])
})) %>%
  mutate(
    estimate_fmt = sprintf("%.1f [%.1f–%.1f]", estimate__, lower__, upper__)
  ) %>%
  select(Functional_Group, Classification, estimate_fmt) %>%
  pivot_wider(names_from = Classification, values_from = estimate_fmt)

# View
fg_summary


# Combine functional group predictions into total per Classification
total_pred <- bind_rows(lapply(seq_along(ce_re), function(i) {
  ce_re[[i]] %>%
    select(Classification, estimate__, lower__, upper__) %>%
    mutate(FG = names(ce_re)[i])
})) %>%
  group_by(Classification) %>%
  summarise(
    estimate = sum(estimate__),
    lower = sum(lower__),
    upper = sum(upper__)
  ) %>%
  mutate(
    estimate_fmt = sprintf("%.0f [%.0f–%.0f]", estimate, lower, upper)
  )
total_pred

plot(ce_re)


