### exploratory analysis 

# --------------------------
######### 1. Identify Outliers #######
# --------------------------
print("Number of Surveys per Site")
fish_long %>%
  distinct(Site, survey_id, Researcher) %>%  # Get one row per unique survey per site
  count(Site) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

print("Number of Unique Surveys per Site")
fish_long %>%
  distinct(Site, survey_id) %>%  # Get one row per unique survey per site
  count(Site) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

# compute zero-inflation rate for each species 
print("Zero-Inflation Rates")
zero_inflation_rates <- fish_long %>%
  group_by(Species) %>%
  summarise(
    Zero_Count = sum(Count == 0, na.rm = TRUE),
    Non_Zero_Count = sum(Count > 0, na.rm = TRUE),
    Total_Observations = n(),
    Zero_Inflation_Rate = Zero_Count / Total_Observations
  ) %>%
  arrange(desc(Zero_Inflation_Rate))

print(zero_inflation_rates)


# identify species with extremely high counts 
print("Species with extreme counts")
extreme_counts <- fish_long %>%
  group_by(Species) %>%
  summarise(
    Max_Count = max(Count, na.rm = TRUE),
    Mean_Count = mean(Count, na.rm = TRUE),
    Median_Count = median(Count, na.rm = TRUE),
    SD_Count = sd(Count, na.rm = TRUE)
  ) %>%
  arrange(desc(Max_Count))

print(extreme_counts)


# visualize 
ggplot(fish_long, aes(x = Count)) +
  geom_histogram(binwidth = 5, fill = "skyblue", alpha = 0.7) +
  facet_wrap(~ Species, scales = "free") +
  labs(title = "Distribution of Counts for Each Species", x = "Count per Survey", y = "Frequency") +
  theme_minimal()


# remove problematic species 
# fish_long <- fish_long %>% filter(!Species %in% c("Barracuda", "Eel", "Porcupine.Puffer", "Ray"))


# which functional group is the most stable? and should be used as the baseline 

# Calculate mean, standard deviation, and CV for each functional group.
stability_summary <- fish_long %>%
  group_by(Functional_Group) %>%
  summarise(
    Mean_Count = mean(Count, na.rm = TRUE),
    SD_Count = sd(Count, na.rm = TRUE),
    CV = SD_Count / Mean_Count
  ) %>%
  arrange(CV)

print(stability_summary)

## exploring dive site composition 
# Calculate total counts per site and functional group
fg_by_site <- fish_long %>%
  group_by(Site, Functional_Group) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Site) %>%
  mutate(Proportion = Total / sum(Total))

# Plot: grouped barplot of proportions per site
ggplot(fg_by_site, aes(x = Site, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Proportion of Functional Groups by Site",
    x = "Site",
    y = "Proportion",
    fill = "Functional Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



library(dplyr)

# Summarise survey counts
survey_counts <- fish_long %>%
  distinct(survey_id, Classification) %>%
  count(Classification, name = "n_surveys")

total_surveys <- sum(survey_counts$n_surveys)
total_minutes <- total_surveys * 8
total_hours <- total_minutes / 60
total_area_m2 <- total_surveys * 1200
total_area_ha <- total_area_m2 / 10000

# Total fish count
total_fish <- fish_long %>%
  summarise(total = sum(Count, na.rm = TRUE)) %>%
  pull(total)

# Fish density per survey
fish_density <- fish_long %>%
  group_by(survey_id, Classification) %>%
  summarise(Total_Fish = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(Density = Total_Fish)

# Mean ± SD density per habitat
density_summary <- fish_density %>%
  group_by(Classification) %>%
  summarise(
    mean_density = mean(Density),
    sd_density = sd(Density),
    .groups = "drop"
  )

# Print outputs
print(survey_counts)
cat("Total surveys:", total_surveys, "\n")
cat("Total survey hours:", round(total_hours, 1), "\n")
cat("Total area surveyed (ha):", round(total_area_ha, 1), "\n")
cat("Total fish recorded:", (total_fish), "\n")
print(density_summary)

# sites per classification 
fish_wide %>%
  distinct(Site, Classification) %>%
  count(Classification)
fish_wide %>%
  distinct(Site, Classification) %>%
  arrange(Classification, Site)


