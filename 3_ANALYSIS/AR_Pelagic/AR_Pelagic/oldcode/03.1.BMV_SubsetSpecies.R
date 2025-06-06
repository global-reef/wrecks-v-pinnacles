# run for only a selection of sepecies 
library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

species_cols <- c("Parrotfish", "Rabbitfish", "Butterflyfish", "Angelfish", "Cleaner_Wrasse",
                  "Batfish", "Thicklip", "Red_Breast", "Slingjaw", "Sweetlips", "Squirrel.Soldier",
                  "Triggerfish", "Porcupine.Puffer", "Ray", "Brown_Stripe_Snapper", 
                  "Russels_Snapper", "lrg_Snapper", "Eel", "Trevally", "Emperorfish",
                  "sml_Grouper", "lrg_Grouper", "Barracuda")


# Define the subset of species
species_subset <- c("Parrotfish", "Rabbitfish", "Butterflyfish")

# Create the response formula using the subset
response_str <- paste("mvbind(", paste(species_subset, collapse = ", "), ") ~ Classification")
response_formula <- as.formula(response_str)

# Check the formula
print(response_formula)

# Fit the model with the subset using brms

fit_brms <- brm(
  formula = response_formula,
  data = fish_wide,  # Ensure fish_wide contains columns for these species
  family = negbinomial(),  # Use negative binomial for overdispersion
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 400,
  control = list(adapt_delta = 0.90, max_treedepth = 15)
)
summary(fit_brms)



# Step 1. Generate fitted predictions from the model.
# Use the original data from the model.
newdata <- fit_brms$data

# Get the summary predictions
fitted_summary <- as.data.frame(fitted(fit_brms, newdata = newdata))

# Extract columns whose names start with "Estimate"
estimate_cols <- grep("^Estimate", names(fitted_summary), value = TRUE)
estimates <- fitted_summary[, estimate_cols]
estimates$Classification <- newdata$Classification

##  Convert estimates (data frame) from wide to long format.
# We assume that the column names start with "Estimate." and we remove that prefix.
est_long <- estimates %>%
  pivot_longer(
    cols = starts_with("Estimate"),
    names_to = "Species",
    names_prefix = "Estimate.",
    values_to = "Predicted"
  )

# In case there are multiple rows per classification, summarize by taking the mean.
summary_est <- est_long %>%
  group_by(Classification, Species) %>%
  summarise(mean_pred = mean(Predicted, na.rm = TRUE), .groups = "drop")

# ALL TOGETHER Plot the predicted abundance by Classification for each species.
ggplot(summary_est, aes(x = Classification, y = mean_pred, fill = Species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Mean Predicted Abundance by Classification",
       x = "Classification",
       y = "Mean Predicted Abundance") +
  theme_minimal()


# Define the subset of species we're interested in (note that the "_" gets dropped)
# species_subset <- c("lrgGrouper", "Trevally", "Barracuda")

# Define a function that creates and arranges the plots
plot_species_predictions <- function(summary_data, species_vec) {
  plots <- lapply(species_vec, function(sp) {
    summary_data %>% 
      filter(Species == sp) %>%
      ggplot(aes(x = Classification, y = mean_pred, fill = Classification)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      scale_fill_grey(start = 0.8, end = 0.2) +  # Grayscale palette
      labs(title = sp,
           x = "Classification",
           y = "Predicted Abundance") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  grid.arrange(grobs = plots, ncol = length(species_vec))
}

# Now call the function with your summary_est data and species_subset
plot_species_predictions(summary_est, species_subset)


save(fit_brms, file="SS_fit_Grazers")
