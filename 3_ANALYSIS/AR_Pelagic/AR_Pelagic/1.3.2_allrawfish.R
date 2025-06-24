#### using all surveys - not collapsed into same survey date + site 
raw_fish <- raw_fish %>%
  mutate(
    Sweetlips = as.character(Sweetlips),
    Sweetlips = na_if(Sweetlips, "-"),      # treat "-" as NA
    Sweetlips = as.numeric(Sweetlips)
  )




# remove outliers (barracudas, rays, porcupine/puffers, and eels (zero-inflated and/or extreme counts. see exploratory analysis code 1.2))
fish_long <- fish_long %>%
  filter(!Species %in% c("Barracuda", "Eel", "Porcupine.Puffer", "Ray"))
# sorry girlie 
fish_long <- fish_long %>% 
  filter(Researcher != "Keisha")
# Filter out pre-2023-09-01 surveys for Aow Mao Wreck and No Name Wreck (pre-deployment)
fish_long <- fish_long %>%
  filter(!(Site %in% c("Aow Mao Wreck", "No Name Wreck") & Date < as.Date("2023-09-01")))



clean_raw_fish <- function(df) {
  library(dplyr)
  library(lubridate)
  library(forcats)
  
  # Remove blank rows and columns
  df[df == ""] <- NA
  df <- df[, colSums(!is.na(df)) > 0]
  df <- df[rowSums(!is.na(df)) > 0, ]
  
  # Merge Brown_Stripe_Snapper and Russels_Snapper into sml_snapper
  df <- df %>%
    mutate(
      sml_snapper = rowSums(select(., any_of(c("Brown_Stripe_Snapper", "Russels_Snapper"))), na.rm = TRUE)
    ) %>%
    select(-any_of(c("Brown_Stripe_Snapper", "Russels_Snapper")))
  
  # Rename and align remaining columns
  df <- df %>%
    rename(
      sml_Grouper = Grouper.30,
      lrg_Grouper = Grouper.30.1,
      lrg_Snapper = Snapper.30,
      Date = Date,  # already Date in raw_fish
      Duration = Duration,  # already correct
      Visibility = Visibility,
      Depth = Depth
    )
  
  # Format columns
  df <- df %>%
    mutate(
      Date = as.Date(as.character(Date), format = "%m/%d/%Y"),
      Time = format(as.POSIXct(as.character(Time), format = "%H:%M"), "%H:%M"),
      Weather = as.factor(Weather),
      SurveyID = paste(Site, Date, sep = "_")
    )
  
  return(df)
}
clean_fish <- clean_raw_fish(raw_fish)
clean_fish_timed <- clean_raw_fish(raw_fish_timed)
raw_all <- bind_rows(raw_fish, raw_fish_timed)



# Summarise survey counts
survey_counts <- raw_all %>%
  count(Classification, name = "n_surveys")

# Totals
total_surveys <- sum(survey_counts$n_surveys)
total_minutes <- total_surveys * 8
total_hours <- total_minutes / 60
total_area_m2 <- total_surveys * 1200
total_area_ha <- total_area_m2 / 10000

# Total fish count
total_fish <- raw_all %>%
  mutate(total_count = Grazer + Invertivore + Mesopredator + HTLP) %>%
  summarise(total_fish = sum(total_count))

# Mean ± SD density per habitat
density_summary <- raw_all %>%
  mutate(total_count = Grazer + Invertivore + Mesopredator + HTLP) %>%
  group_by(Classification) %>%
  summarise(
    mean_density = mean(total_count),
    sd_density = sd(total_count),
    .groups = "drop"
  )

# Area check (optional if needed independently)
total_area_ha <- nrow(raw_all) * 0.12

# Print outputs
print(survey_counts)
cat("Total surveys:", total_surveys, "\n")
cat("Total survey hours:", round(total_hours, 1), "\n")
cat("Total area surveyed (ha):", round(total_area_ha, 1), "\n")
cat("Total fish recorded:", pull(total_fish), "\n")

print(density_summary)




