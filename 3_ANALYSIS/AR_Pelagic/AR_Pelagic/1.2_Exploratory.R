### exploratory analysis ###########################################################################

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)

### 1) survey coverage and effort ##################################################################
message("Surveys per Site (distinct survey_id):")
fish_long %>%
  distinct(Site, survey_id) %>%
  count(Site, name = "n_surveys") %>%
  arrange(desc(n_surveys)) %>%
  print(n = Inf)

message("Surveys per Classification:")
survey_counts <- fish_long %>%
  distinct(survey_id, Classification) %>%
  count(Classification, name = "n_surveys")
print(survey_counts)

total_surveys   <- sum(survey_counts$n_surveys)
total_minutes   <- total_surveys * 8
total_hours     <- total_minutes / 60
total_area_m2   <- total_surveys * 1200
total_area_ha   <- total_area_m2 / 10000

total_fish <- fish_long %>% summarise(total = sum(Count, na.rm = TRUE)) %>% pull(total)
fish_density <- fish_long %>%
  group_by(survey_id, Classification) %>%
  summarise(Total_Fish = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(Density = Total_Fish)
density_summary <- fish_density %>%
  group_by(Classification) %>%
  summarise(mean_density = mean(Density), sd_density = sd(Density), .groups = "drop")

cat("Total surveys:", total_surveys, "\n")
cat("Total survey hours:", round(total_hours, 1), "\n")
cat("Total area surveyed (ha):", round(total_area_ha, 1), "\n")
cat("Total fish recorded:", total_fish, "\n")
print(density_summary)

### 2) zero inflation and extreme counts ###########################################################
message("Zero-Inflation Rates by Species:")
zero_inflation_rates <- fish_long %>%
  group_by(Species) %>%
  summarise(
    Zero_Count = sum(Count == 0, na.rm = TRUE),
    Total_Observations = n(),
    Zero_Inflation_Rate = Zero_Count / Total_Observations,
    .groups = "drop"
  ) %>%
  arrange(desc(Zero_Inflation_Rate))
print(zero_inflation_rates)

message("Extreme counts by Species (max, mean, median, sd):")
extreme_counts <- fish_long %>%
  group_by(Species) %>%
  summarise(
    Max_Count    = max(Count, na.rm = TRUE),
    Mean_Count   = mean(Count, na.rm = TRUE),
    Median_Count = median(Count, na.rm = TRUE),
    SD_Count     = sd(Count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Max_Count))
print(extreme_counts)

ggplot(fish_long, aes(x = Count)) +
  geom_histogram(binwidth = 5, fill = "skyblue", alpha = 0.7) +
  facet_wrap(~ Species, scales = "free") +
  labs(title = "Distribution of Counts per Species", x = "Count per Survey", y = "Frequency") +
  theme_minimal()

### 3) functional-group stability (motivates partial pooling) ######################################
stability_summary <- fish_long %>%
  group_by(Functional_Group) %>%
  summarise(
    Mean_Count = mean(Count, na.rm = TRUE),
    SD_Count   = sd(Count, na.rm = TRUE),
    CV         = SD_Count / Mean_Count,
    .groups = "drop"
  ) %>%
  arrange(CV)
print(stability_summary)

### 4) site composition quick look #################################################################
fg_by_site <- fish_long %>%
  group_by(Site, Functional_Group) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Site) %>%
  mutate(Proportion = Total / sum(Total)) %>%
  ungroup()

ggplot(fg_by_site, aes(x = Site, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Functional Groups by Site", x = "Site", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 5) replication, span, cadence (supports Month_Year RE) #########################################
surv <- fish_long %>%
  distinct(Site, Type, Zone, Date, survey_id, Depth)

span <- surv %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  summarise(
    first        = min(Date),
    last         = max(Date),
    days_span    = as.integer(max(Date) - min(Date)),
    n_surveys    = n(),
    med_gap_days = if (n() > 1) median(diff(sort(Date))) else NA_real_,
    .groups = "drop"
  )
print(span)

ggplot(surv, aes(x = Date, y = Site)) +
  geom_point(alpha = 0.6) +
  labs(title = "Survey dates by site", y = NULL) +
  theme_minimal()

### 6) type–zone association (structure checks) ####################################################
sites_tbl <- fish_long %>% distinct(Site, Type, Zone)
table_TZ <- table(sites_tbl$Type, sites_tbl$Zone)
print(table_TZ)
print(chisq.test(table_TZ))
print(prop.table(table_TZ, 1))  # zone proportions within each Type

### 7) temporal autocorrelation within sites #######################################################
totals_site_date <- fish_long %>%
  group_by(Site, Type, Zone, Date) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

fg_site_date <- fish_long %>%
  group_by(Site, Type, Zone, Date, Functional_Group) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

lag1_by_site <- totals_site_date %>%
  arrange(Site, Date) %>%
  group_by(Site) %>%
  summarise(
    n_pts = n(),
    lag1  = if (n() >= 3) {
      x <- scale(Total, center = TRUE, scale = TRUE)[, 1]
      stats::acf(x, lag.max = 1, plot = FALSE)$acf[2]
    } else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(desc(lag1))
print(lag1_by_site)
summary(lag1_by_site$lag1)

lag1_fg <- fg_site_date %>%
  arrange(Site, Functional_Group, Date) %>%
  group_by(Site, Functional_Group) %>%
  summarise(
    n_pts = n(),
    lag1  = if (n() >= 3) stats::acf(scale(Total)[, 1], lag.max = 1, plot = FALSE)$acf[2] else NA_real_,
    .groups = "drop"
  )
lag1_fg %>% group_by(Functional_Group) %>% summarise(median_lag1 = median(lag1, na.rm = TRUE)) %>% print()

### 8) within-site trend visuals ###################################################################
ggplot(totals_site_date, aes(Date, Total, group = Site)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Site, scales = "free_y") +
  labs(title = "Within-site trends in total counts") +
  theme_minimal()

### 9) site-level contrasts (bootstrap, avoids pseudoreplication) ##################################
site_means <- totals_site_date %>%
  group_by(Site, Type) %>%
  summarise(mean_total = mean(Total, na.rm = TRUE), .groups = "drop")

boot_diff_pair <- function(df, type_a, type_b, B = 5000, seed = 1) {
  set.seed(seed)
  sub <- df %>% filter(Type %in% c(type_a, type_b))
  if (n_distinct(sub$Type) < 2) return(tibble(type_a, type_b, l95 = NA, med = NA, u95 = NA))
  draws <- replicate(B, {
    smp <- sub %>% group_by(Type) %>% sample_frac(replace = TRUE) %>% ungroup()
    means <- tapply(smp$mean_total, smp$Type, mean)
    means[[type_a]] - means[[type_b]]
  })
  tibble(
    type_a = type_a, type_b = type_b,
    l95 = quantile(draws, 0.025),
    med = quantile(draws, 0.500),
    u95 = quantile(draws, 0.975)
  )
}

pairs_to_check <- list(
  c("Shipwreck", "Pinnacle"),
  c("Shipwreck", "Fringing"),
  c("Pinnacle",  "Fringing")
)

boot_results <- map_dfr(pairs_to_check, ~ boot_diff_pair(site_means, .x[1], .x[2]))
print(boot_results)

ggplot(site_means, aes(Type, mean_total)) +
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 23, fill = "white") +
  labs(title = "Site-level means by Type") +
  theme_minimal()

### 10) depth and site table (characterisation) ####################################################
ggplot(fish_long %>% distinct(Site, Type, Depth), aes(Type, Depth)) +
  geom_boxplot() +
  labs(title = "Depth by Type (site level)") +
  theme_minimal()

surv_span <- fish_long %>%
  distinct(Site, Type, Classification, Zone, survey_id, Date) %>%
  group_by(Site, Type, Classification, Zone) %>%
  summarise(
    first_date    = min(Date, na.rm = TRUE),
    last_date     = max(Date, na.rm = TRUE),
    n_surveys     = n_distinct(survey_id),
    days_span     = as.integer(last_date - first_date),
    med_gap_days  = if (n() > 1) median(diff(sort(Date))) else NA_real_,
    .groups = "drop"
  )

depth_stats <- fish_long %>%
  group_by(Site) %>%
  summarise(
    n_depth     = sum(!is.na(Depth)),
    depth_mean  = if (n_depth > 0) mean(Depth, na.rm = TRUE) else NA_real_,
    depth_min   = if (n_depth > 0) min(Depth, na.rm = TRUE) else NA_real_,
    depth_max   = if (n_depth > 0) max(Depth, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    depth_range = ifelse(!is.na(depth_min) & !is.na(depth_max),
                         paste0(round(depth_min, 1), "–", round(depth_max, 1), " m"),
                         NA_character_)
  ) %>%
  select(-n_depth)

site_table <- surv_span %>%
  left_join(depth_stats, by = "Site") %>%
  transmute(
    Site, Type, Classification, Zone,
    `First date`        = first_date,
    `Last date`         = last_date,
    `# surveys`         = n_surveys,
    `Span (days)`       = days_span,
    `Median gap (days)` = med_gap_days,
    `Mean depth (m)`    = round(depth_mean, 1),
    `Depth range`       = depth_range
  ) %>%
  arrange(Type, Classification, Zone, Site)

site_table
