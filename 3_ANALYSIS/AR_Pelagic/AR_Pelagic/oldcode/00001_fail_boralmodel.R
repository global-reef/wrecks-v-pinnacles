##### data prep ######

library(dplyr)
library(lubridate)
library(boral)
# create unique survey IDs 
AR_Pelagic <- AR_Pelagic %>%
  mutate(survey_id = paste(Site, Date, Time, sep = "_"))
# account for differences in data entry by averaging the new raw data post 2025 
AR_Pelagic <- AR_Pelagic %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    data_type = if_else(Year < 2025, "aggregated", "raw")
  )

# define species column
species_cols <- c("Parrotfish", "Rabbitfish", "Butterflyfish", "Angelfish", 
                  "Cleaner_Wrasse", "Batfish", "Thicklip", "Red_Breast", 
                  "Slingjaw", "Sweetlips", "Squirrel.Soldier", "Triggerfish", 
                  "Porcupine.Puffer", "Ray", "Brown_Stripe_Snapper", 
                  "Russels_Snapper", "lrg_Snapper", "Eel", "Trevally", 
                  "Emperorfish", "sml_Grouper", "lrg_Grouper", "Barracuda")


# For raw (post-2025) data, average the species counts by survey_id.
raw_data_avg <- AR_Pelagic %>%
  filter(data_type == "raw") %>%
  group_by(survey_id) %>%
  summarise(
    across(all_of(species_cols), ~ mean(.x, na.rm = TRUE)),
    Site = first(Site),
    Classification = first(Classification),
    Date = first(Date),
    Time = first(Time),
    Weather = first(Weather),
    Current = first(Current),
    Depth = first(Depth),
    .groups = "drop"
  )

## combine aggregated and averaged raw data 
# Pre-2025 aggregated data (assumed to have one row per survey already)
aggregated_data <- AR_Pelagic %>%
  filter(data_type == "aggregated")

# Combine the two datasets
combined_data <- bind_rows(aggregated_data, raw_data_avg)




#### create site-by-species matrix
species_matrix <- as.matrix(combined_data %>% select(all_of(species_cols)))
# round up to nearest integer 
species_matrix <- ceiling(as.matrix(combined_data %>% select(all_of(species_cols))))

# Also extract environmental data if needed
env_data <- combined_data %>% select(survey_id, Site, Classification, Date, Time, Weather, Current, Depth)

# Create an X matrix for the predictor "Classification"
# Note: model.matrix() automatically creates dummy variables for factor predictors.
X <- model.matrix(~ Classification - 1, data = combined_data)


##### fit boral model ##### 
fit_boral_cov <- boral(
  y = species_matrix,
  X = X,
  family = "negative.binomial",     # overdispersion suspected, switched from Poisson to neg Binom
  lv.control = list(num.lv = 2),   # Two latent variables for ordination
  save.model = TRUE              # Saves the underlying Stan/JAGS model
)
fit_boral <- fit_boral_cov
summary(fit_boral)

###### explore results ######

library(ggplot2)

# Create a data frame with the latent variable scores and Classification.
# Make sure that the order of the rows in combined_data matches the order of the latent scores.
df_lv <- data.frame(
  LV1 = fit_boral$lv.median[, 1],
  LV2 = fit_boral$lv.median[, 2],
  Classification = factor(combined_data$Classification)
)

# Now plot using ggplot2:
ggplot(df_lv, aes(x = LV1, y = LV2, color = Classification)) +
  geom_point(size = 3) +
  labs(title = "Ordination of Surveys by Classification",
       x = "Latent Variable 1",
       y = "Latent Variable 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # You can change the palette as desired



# Create a data frame for species loadings.
# Here we assume that each row of lv.coefs.median corresponds to one species,
# and that the first two columns contain the loadings on LV1 and LV2.
df_species <- data.frame(
  Species = rownames(fit_boral$lv.coefs.median),
  theta1 = fit_boral$lv.coefs.median[, 1],
  theta2 = fit_boral$lv.coefs.median[, 2]
)

# Inspect the data frame
head(df_species)

# Now create the ggplot for species loadings:
ggplot(df_species, aes(x = theta1, y = theta2, label = Species)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Species Loadings on Latent Variables",
       x = "Theta 1",
       y = "Theta 2") +
  theme_minimal()+ 
  scale_color_brewer(palette = "Set1")  # You can change the palette as desired




#### look at difference between diff sites for each species #####
# Convert the X.coefs.median matrix to a data frame
df_pred <- as.data.frame(fit_boral$X.coefs.median)
# Add a column with species names (which are the rownames)
df_pred$Species <- rownames(df_pred)

# Reshape from wide to long format
# (Assuming the column names are like "ClassificationFringing", etc.)
df_long <- df_pred %>%
  pivot_longer(
    cols = -Species,
    names_to = "Classification",
    values_to = "pred_log"
  )

# Clean up the classification labels if desired
df_long <- df_long %>%
  mutate(Classification = gsub("Classification", "", Classification),
         pred = exp(pred_log))  # exponentiate to get predicted abundances

# View the data frame
head(df_long)

# Plot predictions: one option is to create a faceted plot for all species
ggplot(df_long, aes(x = Classification, y = pred)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Predicted Abundance by Classification for Each Species",
       x = "Classification",
       y = "Predicted Abundance") +
  theme_minimal()


# look at difference between diff sites for one species (ex trevally) 
library(ggplot2)

# Create a data frame with Trevally's coefficients
trevally_df <- data.frame(
  Classification = c("Fringing", "Pinnacle", "Shipwreck"),
  beta0 = 0.987,  # same for all since it’s the baseline for Trevally
  X_coef = c(0.854, -0.270, 0.292)
)

# Compute predicted log-abundance and then abundance (assuming a log link)
trevally_df$pred_log <- trevally_df$beta0 + trevally_df$X_coef
trevally_df$pred <- exp(trevally_df$pred_log)

# View the data frame
print(trevally_df)

ggplot(trevally_df, aes(x = Classification, y = pred)) +
  geom_point(size = 4, color = "blue") +
  geom_line(aes(group = 1), color = "blue", linetype = "dashed") +
  labs(title = "Predicted Abundance for Trevally by Classification",
       y = "Predicted Abundance",
       x = "Classification") +
  theme_minimal()

