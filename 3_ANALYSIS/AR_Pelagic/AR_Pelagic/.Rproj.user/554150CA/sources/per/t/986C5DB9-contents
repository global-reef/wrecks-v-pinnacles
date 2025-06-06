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
      Herbivore    = ifelse(is.na(Herbivore), 0, Herbivore),
      Invertivore  = ifelse(is.na(Invertivore), 0, Invertivore),
      Mesopredator = ifelse(is.na(Mesopredator), 0, Mesopredator),
      HTLP         = ifelse(is.na(HTLP), 0, HTLP)
    )
  
  # Relevel Classification so that "Shipwreck" is the reference (baseline)
  survey_level <- survey_level %>%
    mutate(Classification = relevel(as.factor(Classification), ref = "Shipwreck"))
  
  # (Optional) Create total_fish if needed for other purposes:
  survey_level <- survey_level %>%
    mutate(total_fish = Herbivore + Invertivore + Mesopredator + HTLP)
  
  # Create the multivariate response formula (without a trials() term)
  response_formula <- as.formula("mvbind(Herbivore, Invertivore, Mesopredator, HTLP) ~ Classification")
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

#####  try adding a random intercept for site 
survey_level <- results_fg_mv$survey_level
# Fit a multivariate regression with a random intercept for Site
fit_re <- brm(
  formula = mvbind(Herbivore, Invertivore, Mesopredator, HTLP) ~ Classification + (1 | p | Site),
  data = survey_level,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 500,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend = "cmdstanr"
)

print(summary(fit_re))


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


# try with zones instead of pinnacle/fringing 
fit_zone <- brm(
  formula = mvbind(Herbivore, Invertivore, Mesopredator, HTLP) ~ Zone + (1 | p | Site),
  data = survey_level,
  family = negbinomial(),
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 500,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  backend="cmdstanr"
)
summary(fit_zone)
loo_compare(loo(fit_re), loo(fit_zone))
