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



