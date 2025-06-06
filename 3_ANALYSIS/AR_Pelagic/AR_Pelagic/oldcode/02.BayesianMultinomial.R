#### bayesian multinomial regression (with brms) ######
library(dplyr)
library(tidyr)
library(ggplot2)

# create unique survey IDS 
fish_long <- fish_long %>%
  mutate(
    survey_id = paste(Site, Date, Time, sep = "_"))

# make fish_long into wide data again 
survey_level <- fish_long %>%
  group_by(survey_id, Site, Classification, Functional_Group) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  spread(key = Functional_Group, value = Count, fill = 0)
head(survey_level)
# check structure 
str(survey_level) # all formats looks good 


## have to round counts to integers to use multinomial sampling 
survey_level <- survey_level %>%
  mutate(
    Herbivore    = ceiling(Herbivore),
    Invertivore  = ceiling(Invertivore),
    Mesopredator = ceiling(Mesopredator),
    HTLP         = ceiling(HTLP)
  )


# create a column of total trials 
survey_level <- survey_level %>%
  mutate(
    total_fish = Herbivore + Invertivore + Mesopredator + HTLP
  )


### fit the model #####
library(brms)


fit <- brm(
  formula = cbind(Herbivore, Invertivore, Mesopredator, HTLP) | trials(total_fish) ~ Classification,
  data = survey_level,
  family = multinomial(),
  chains = 4,
  cores = 4,
  iter = 2000
)

summary(fit)       # Look at model coefficients and credible intervals

##### diagnostics and posterior checks #####
## trace posts and r-hat (check convergence)
plot(fit)
summary(fit)

## posterior predictive checks 
# this helps verify the fit of the model to the data 
y_rep <- posterior_predict(fit)


# Calculate the observed total counts per survey
survey_totals_obs <- survey_level %>%
  mutate(total = Herbivore + Invertivore + Mesopredator + HTLP) %>%
  pull(total)

# Calculate the predicted total counts per survey from the posterior predictions
# Here, y_rep is a matrix with dimensions (n_draws x n_surveys)
survey_totals_pred <- apply(y_rep, 2, sum)

# Plot a histogram of observed vs. predicted totals
ggplot() +
  geom_histogram(aes(x = survey_totals_pred), fill = "skyblue", alpha = 0.5, bins = 30) +
  geom_vline(aes(xintercept = mean(survey_totals_obs)), color = "red", linetype = "dashed") +
  labs(x = "Total Counts per Survey", y = "Frequency",
       title = "Posterior Predictive Check: Total Survey Counts")



##### post ad-hoc analysis and visualizations ##### 
## marginal effects 
# how predicted probabilites vary by classficiation 
ce <- conditional_effects(fit, categorical = TRUE)
plot(ce)

# To test whether these differences are statistically significant, you can use the hypothesis() function from brms. Here’s how you can set up the tests:
# fringing v shipwreck 
hypothesis(fit, "muHTLP_ClassificationShipwreck = 0")
# fringing v pinnacle
hypothesis(fit, "muHTLP_ClassificationPinnacle = 0")
# pinnacle v shipwreck
hypothesis(fit, "muHTLP_ClassificationShipwreck - muHTLP_ClassificationPinnacle = 0")



##### add random effects #####
# If you suspect variability among sites that isn’t explained just by classification, add a random intercept.
fit_re <- brm(
  formula = cbind(Herbivore, Invertivore, Mesopredator, HTLP) | trials(total_fish) ~ 
    Classification + (1 | Site),
  data = survey_level,
  family = multinomial(),
  chains = 4,
  cores = 4,
  iter = 2000
)
summary(fit_re)

save(fit, file="FG_fit")
save(fit_re, file="FG_fit_re")
