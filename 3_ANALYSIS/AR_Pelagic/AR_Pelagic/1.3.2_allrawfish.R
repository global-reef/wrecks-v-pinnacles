#### using all surveys - not collapsed into same survey date + site 
raw_fish <- raw_fish %>%
  mutate(
    Sweetlips = as.character(Sweetlips),
    Sweetlips = na_if(Sweetlips, "-"),      # treat "-" as NA
    Sweetlips = as.numeric(Sweetlips)
  )

raw_fish <- raw_fish %>%
  mutate(sml_snapper = rowSums(select(., Brown_Stripe_Snapper, Russels_Snapper), na.rm = TRUE)) %>%
  select(-Brown_Stripe_Snapper, -Russels_Snapper)



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
raw_all <- bind_rows(raw_fish, raw_fish_timed)
