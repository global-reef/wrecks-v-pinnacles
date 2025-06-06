#### clean up the data and prep it for brms #####

# Load data
AR_Pelagic <- read.csv("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/2_DATA/2025.02.12_ArtificalReefs_MASTERdata.csv", stringsAsFactors=TRUE)

# Load neccessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(brms)
library(lubridate)

# DATA CLEAN UP 

# Rename columns in AR_Pelagic
AR_Pelagic <- AR_Pelagic %>%
  rename(
    sml_Grouper = Grouper.30,
    lrg_Grouper = Grouper.30.1,
    lrg_Snapper = Snapper.30
  )

# Check column names to confirm the change
colnames(AR_Pelagic)


species_cols <- c("Parrotfish", "Rabbitfish", "Butterflyfish", "Angelfish", "Cleaner_Wrasse",
                  "Batfish", "Thicklip", "Red_Breast", "Slingjaw", "Sweetlips", "Squirrel.Soldier",
                  "Triggerfish", "Porcupine.Puffer", "Ray", "Brown_Stripe_Snapper", 
                  "Russels_Snapper", "lrg_Snapper", "Eel", "Trevally", "Emperorfish",
                  "sml_Grouper", "lrg_Grouper", "Barracuda")
# Check the column names to ensure they exist in the dataset
species_cols <- species_cols[species_cols %in% colnames(AR_Pelagic)]
# Convert species columns to numeric
AR_Pelagic[species_cols] <- lapply(AR_Pelagic[species_cols], as.numeric)



### Fix Formats ###
AR_Pelagic <- AR_Pelagic[, !(names(AR_Pelagic) %in% c("total_N", "N.Observers"))]
# Convert Date column
AR_Pelagic$Date <- as.Date(AR_Pelagic$Date, format = "%d/%m/%Y")
# Convert Time column
AR_Pelagic$Time <- format(as.POSIXct(AR_Pelagic$Time, format = "%H:%M"), "%H:%M")
# couple more format changes 
AR_Pelagic$Weather <- as.factor(AR_Pelagic$Weather)

# create unique survey IDs 
AR_Pelagic <- AR_Pelagic %>%
  mutate(survey_id = paste(Site, Date, Time, sep = "_"))


# account for differences in data entry by averaging the new raw data post 2025 
AR_Pelagic<- AR_Pelagic%>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    data_type = if_else(Year < 2025, "aggregated", "raw")
  )

# For raw (post-2025) data, average the species counts by survey_id.
raw_data_avg <- AR_Pelagic %>%
  filter(data_type == "raw") %>%
  group_by(survey_id) %>%
  summarise(
    Site = first(Site),
    Type = first(Type),
    Classification = first(Classification),
    Date = first(Date),
    Time = first(Time),
    Weather = first(Weather),
    Current = first(Current),
    Depth = first(Depth),
    across(all_of(species_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )
## combine aggregated and averaged raw data 
# Pre-2025 aggregated data (assumed to have one row per survey already)
aggregated_data <- AR_Pelagic%>%
  filter(data_type == "aggregated") %>%
  select(survey_id, everything())

# make sure the names match 
common_cols <- intersect(names(aggregated_data), names(raw_data_avg))
aggregated_data <- aggregated_data %>% select(all_of(common_cols))
raw_data_avg <- raw_data_avg %>% select(all_of(common_cols))


# Combine the two datasets
combined_data <- bind_rows(aggregated_data, raw_data_avg)
# make sure they are rounded UP to the nearest integer 
combined_data <- combined_data %>%
  mutate(across(all_of(species_cols), ~ ceiling(.x)))

# get rid of all columns that we aren't using pls 
desired_cols <- c(
  "survey_id", "Site", "Type", "Classification", "Date",
  "Angelfish", "Barracuda", "Batfish", "Brown_Stripe_Snapper",
  "Butterflyfish", "Cleaner_Wrasse", "Eel", "Emperorfish",
  "lrg_Grouper", "lrg_Snapper", "Parrotfish", "Porcupine.Puffer",
  "Rabbitfish", "Ray", "Red_Breast", "Russels_Snapper", "Slingjaw",
  "sml_Grouper", "Squirrel.Soldier", "Sweetlips", "Thicklip",
  "Trevally", "Triggerfish"
)


fish_wide <- combined_data %>% select(all_of(desired_cols))
# fix the one weird NA 
colSums(is.na(fish_wide))
fish_wide <- fish_wide %>% 
  mutate(lrg_Grouper = if_else(
    survey_id == "No Name Wreck_2024-01-07_09:30" & is.na(lrg_Grouper),
    5,
    lrg_Grouper
  ))
  


'''
# Now pivot longer
fish_long <- combined_data %>%
  pivot_longer(cols = all_of(species_cols),  # Select species columns
               names_to = "Species", 
               values_to = "Count")


fish_long <- fish_long %>%
  select(survey_id, Site, Type, Classification, Date, Species, Count)

# check for any NAs 
colSums(is.na(fish_long))
# fix that one weird NA in count
fish_long <- fish_long %>%
  mutate(Count = if_else(
    survey_id == "No Name Wreck_2024-01-07_09:30" & Species == "lrg_Grouper" & is.na(Count),
    5,
    Count))
fish_long <- fish_long %>%
    mutate(Species = as.factor(Species))
    

# now make fish_long intro fish_wide 
fish_wide <- fish_long %>%
  group_by(survey_id, Site, Type, Classification, Date, Species) %>%
  summarise(Count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  spread(key = Species, value = Count, fill = 0)'''

#### multivariate species specific model in brms #########

## create multivariate response formula 
# Create a formula string by combining species names
response_str <- paste("mvbind(", paste(species_cols, collapse = ", "), ") ~ Classification")
response_formula <- as.formula(response_str)

# Check the formula
print(response_formula)

# fit the model 
library(brms)


fit_brms <- brm(
  formula = response_formula,
  data = fish_wide,
  family = negbinomial(),  # good for overdispersion - which we have 
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 400,
  control = list(adapt_delta = 0.90, max_treedepth = 15)
)
### this takes forever 


fit <- fit_brms

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
  mutate(total = Herbivore + Invertivore + Mesopredator + HTLP) %>% ###### NEEDS TO BE CHANGED BC NO LABELS OF THIS KIND 
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

