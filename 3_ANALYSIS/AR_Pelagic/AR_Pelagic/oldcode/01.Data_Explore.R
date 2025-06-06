# Load data
AR_Pelagic <- read.csv("~/Documents/1_GLOBAL REEF/0_PROJECTS/AR_Pelagic_Pinnacles/2_DATA/2025.02.12_ArtificalReefs_MASTERdata.csv", stringsAsFactors=TRUE)

# Load neccessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

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

# Step 2: Convert species columns to numeric
AR_Pelagic[species_cols] <- lapply(AR_Pelagic[species_cols], as.numeric)



### Fix Formats ###
AR_Pelagic$N.Observers[AR_Pelagic$N.Observers == "???"] <- NA
# Convert Date column
AR_Pelagic$Date <- as.Date(AR_Pelagic$Date, format = "%d/%m/%Y")
# Convert Time column
AR_Pelagic$Time <- format(as.POSIXct(AR_Pelagic$Time, format = "%H:%M"), "%H:%M")
# couple more format changes 
AR_Pelagic$Weather <- as.factor(AR_Pelagic$Weather)

# Now pivot longer
fish_long <- AR_Pelagic %>%
  pivot_longer(cols = all_of(species_cols),  # Select species columns
               names_to = "Species", 
               values_to = "Count")

# Check the summary
summary(fish_long)

#### functional groups ####

functional_groups <- data.frame(
  Species = c("Parrotfish", "Rabbitfish", "Butterflyfish", "Angelfish", "Cleaner_Wrasse",
              "Batfish", "Thicklip", "Red_Breast", "Slingjaw", "Sweetlips", "Squirrel.Soldier",
              "Triggerfish", "Porcupine.Puffer", "Ray", "Brown_Stripe_Snapper", 
              "Russels_Snapper", "lrg_Snapper", "Eel", "Trevally", "Emperorfish",
              "sml_Grouper", "lrg_Grouper", "Barracuda"),
  Functional_Group = c("Herbivore", "Herbivore", "Herbivore", "Invertivore", "Invertivore",
                       "Invertivore", "Invertivore", "Invertivore", "Invertivore", "Invertivore",
                       "Invertivore", "Invertivore", "Invertivore", "Mesopredator", "Mesopredator",
                       "Mesopredator", "HTLP", "Mesopredator", "HTLP", "Mesopredator",
                       "Mesopredator", "HTLP", "HTLP"))


fish_long <- fish_long %>%
  left_join(functional_groups, by = "Species")

fish_long <- fish_long %>%
  mutate(Functional_Group = factor(Functional_Group,
                                   levels = c("Herbivore", "Invertivore", "Mesopredator", "HTLP"),
                                   ordered = TRUE))

# Now each row in fish_long will have a new column, Functional_Group, based on your mapping.
# You can inspect the result:
head(fish_long)


##  Summarize the data by functional group and site type 
functional_summary <- fish_long %>%
  group_by(Type, Functional_Group) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  ungroup()

print(functional_summary)

# visualize 


ggplot(functional_summary, aes(x = Type, y = Total_Count, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Functional Group Abundance by Site Type",
       x = "Site Type", y = "Total Count") +
  theme_tufte()

##  Summarize the data by functional group and site classification

functional_summary <- fish_long %>%
  group_by(Classification, Functional_Group) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE)) %>%
  ungroup()

# Check the summary
print(functional_summary)


ggplot(functional_summary, aes(x = Classification, y = Total_Count, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Functional Group Abundance by Site Classification",
       x = "Site Classification",
       y = "Total Count") +
  theme_tufte()


# proportions 
# Calculate the proportion of each functional group within each classification
functional_summary <- functional_summary %>%
  group_by(Classification) %>%
  mutate(Proportion = Total_Count / sum(Total_Count)) %>%
  ungroup()

# Now create a stacked bar plot showing the proportion
ggplot(functional_summary, aes(x = Classification, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportions of Functional Groups by Site Classification",
       x = "Site Classification",
       y = "Proportion") +
  theme_tufte()


save(AR_Pelagic, file="AR_PP_cleaned")
save(fish_long, file="fish_long")
