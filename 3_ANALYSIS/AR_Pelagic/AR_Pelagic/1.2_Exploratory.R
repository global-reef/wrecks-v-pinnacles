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


#### Checks from reviewers ##### 
library(dplyr); library(lubridate); library(ggplot2)

# One row per survey per site
surv <- fish_long %>%
  distinct(Site, Type, Zone, Date, survey_id, Depth)

# Site × date totals (all species) and by functional group
totals_site_date <- fish_long %>%
  group_by(Site, Type, Zone, Date) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups="drop")

fg_site_date <- fish_long %>%
  group_by(Site, Type, Zone, Date, Functional_Group) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups="drop")

#### Replication clarity and Type-Zone Association ####
# Sites per Type and Zone
sites_tbl <- fish_long %>% distinct(Site, Type, Zone)
table_Type   <- table(sites_tbl$Type)
table_Zone   <- table(sites_tbl$Zone)
table_TZ     <- table(sites_tbl$Type, sites_tbl$Zone)
table_Type; table_Zone; table_TZ

# Is Type correlated with Zone?
chisq.test(table_TZ)  # report p and residuals
prop.table(table_TZ, 1)  # Zone proportions within each Type

##### Repeated Measures magnitude and spacing #### 
# Surveys per site
surv %>% count(Site, name="n_surveys") %>% arrange(desc(n_surveys))

# Time span per site and median interval
span <- surv %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  summarise(
    first = min(Date), last = max(Date),
    days_span = as.integer(max(Date) - min(Date)),
    n_surveys = n(),
    med_gap_days = median(diff(sort(Date))), .groups="drop"
  )
span

# Visual: survey cadence per site
ggplot(surv, aes(x=Date, y=Site)) + 
  geom_point(alpha=0.6) + 
  labs(title="Survey dates by site", y=NULL) + theme_minimal()


### within site temporal autocorrelation (lag 1) ####
lag1_by_site <- totals_site_date %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  summarise(
    n_pts = n(),
    lag1 = if (n() >= 3) {
      x <- scale(Total, center=TRUE, scale=TRUE)[,1]
      # regularized lag-1 on ordered surveys
      stats::acf(x, lag.max=1, plot=FALSE)$acf[2]
    } else NA_real_,
    .groups="drop"
  ) %>%
  arrange(desc(lag1))

lag1_by_site
summary(lag1_by_site$lag1)

# functional groups 
lag1_fg <- fg_site_date %>%
  arrange(Site, Functional_Group, Date) %>%
  group_by(Site, Functional_Group) %>%
  summarise(
    n_pts = n(),
    lag1 = if (n() >= 3) stats::acf(scale(Total)[,1], lag.max=1, plot=FALSE)$acf[2] else NA_real_,
    .groups="drop"
  )
lag1_fg %>% group_by(Functional_Group) %>% summarise(median_lag1 = median(lag1, na.rm=TRUE))

##### trend checks within sites #### 
ggplot(totals_site_date, aes(Date, Total, group=Site)) +
  geom_point(alpha=0.3) +
  geom_smooth(se=FALSE) +
  facet_wrap(~ Site, scales="free_y") +
  labs(title="Within-site trends in total counts") + theme_minimal()

### site level contrast check (robust to psuedoreplication) #### 
# Site means across all surveys
site_means <- totals_site_date %>%
  group_by(Site, Type) %>%
  summarise(mean_total = mean(Total, na.rm=TRUE), .groups="drop")

ggplot(site_means, aes(Type, mean_total)) +
  geom_point(position=position_jitter(width=0.1, height=0)) +
  stat_summary(fun=mean, geom="point", size=3, shape=23, fill="white") +
  labs(title="Site-level means by Type") + theme_minimal()


# Simple bootstrap of Type difference at the site level
set.seed(1)
B <- 5000
types <- unique(site_means$Type)
stopifnot(length(types) == 2)  # adjust if needed

boot_diff <- replicate(B, {
  smp <- site_means %>% group_by(Type) %>% sample_frac(replace=TRUE) %>% ungroup()
  with(smp, tapply(mean_total, Type, mean))
})
boot_df <- as.data.frame(t(boot_diff))  # columns per Type
apply(boot_df, 2, quantile, c(0.025, 0.5, 0.975))
# For pairwise contrasts (example: Ship - Pinnacle)
quantile(boot_df$Ship - boot_df$Pinnacle, c(0.025, 0.5, 0.975))

#### colinearity and confounding quick looks #### 
# Depth distributions by Type
ggplot(fish_long %>% distinct(Site, Type, Depth),
       aes(Type, Depth)) + 
  geom_boxplot() + 
  labs(title="Depth by Type (site level)") + theme_minimal()

# Zone proportions within Type were shown in 1). Consider adding a small site table:
site_table <- fish_long %>%
  distinct(Site, Type, Zone) %>%
  left_join(span %>% select(Site, n_surveys, days_span), by="Site")
site_table



###### Site characteristics tables ######
library(dplyr)

# Surveys, span, gaps by Site
surv_span <- fish_long %>%
  dplyr::distinct(Site, Type, Classification, Zone, survey_id, Date) %>%
  dplyr::group_by(Site, Type, Classification, Zone) %>%
  dplyr::summarise(
    first_date    = min(Date, na.rm = TRUE),
    last_date     = max(Date, na.rm = TRUE),
    n_surveys     = dplyr::n_distinct(survey_id),
    days_span     = as.integer(last_date - first_date),
    med_gap_days  = if (dplyr::n() > 1) median(diff(sort(Date))) else NA_real_,
    .groups = "drop"
  )

# depths 
depth_stats <- fish_long %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(
    n_depth   = sum(!is.na(Depth)),
    depth_mean = if (n_depth > 0) mean(Depth, na.rm = TRUE) else NA_real_,
    depth_min  = if (n_depth > 0) min(Depth, na.rm = TRUE)  else NA_real_,
    depth_max  = if (n_depth > 0) max(Depth, na.rm = TRUE)  else NA_real_,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    depth_range = ifelse(!is.na(depth_min) & !is.na(depth_max),
                         paste0(round(depth_min,1), "–", round(depth_max,1), " m"),
                         NA_character_)
  ) %>%
  dplyr::select(-n_depth)
# Final table with Classification + dates
site_table <- surv_span %>%
  dplyr::left_join(depth_stats, by = "Site") %>%
  dplyr::transmute(
    Site, Type, Classification, Zone,
    `First date` = first_date,
    `Last date`  = last_date,
    `# surveys`  = n_surveys,
    `Span (days)` = days_span,
    `Median gap (days)` = med_gap_days,
    `Mean depth (m)` = round(depth_mean, 1),
    `Depth range` = depth_range
  ) %>%
  dplyr::arrange(Type, Classification, Zone, Site)

site_table




