### using the timed fish swims to help inform the modelling 
library(dplyr)
library(ggplot2)
# df <- fish_long
# merging snapper species 
merge_snapper_species <- function(df) {
  df_clean <- df %>%
    mutate(Species = if_else(Species %in% c("Brown_Stripe_Snapper", "Russels_Snapper"),
                             "sml_snapper", Species))
  snapper_summarised <- df_clean %>%
    filter(Species == "sml_snapper") %>%
    group_by(survey_id, Species, Researcher) %>%
    reframe(
      Count = sum(Count, na.rm = TRUE),
      across(
        .cols = all_of(setdiff(names(df_clean), c("survey_id", "Species", "Researcher", "Count"))),
        ~ first(.x),
        .names = "{.col}"
      )
    ) %>%
    relocate(Count, .after = Species)
  bind_rows(
    df_clean %>% filter(Species != "sml_snapper"),
    snapper_summarised
  )
}

fish_long <- merge_snapper_species(fish_long)

# new data
raw_fish_timed <- read.csv(file_path_timed, stringsAsFactors=TRUE)


clean_timed_fish_data <- function(file_path) {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(forcats)
  # browser()
  # Load the raw data with strings as factors and strip white space
  df <- read.csv(file_path_timed, stringsAsFactors = TRUE, strip.white = TRUE)
  
  # remove blank rows and columns 
  df[df == ""] <- NA
  df <- df[, colSums(!is.na(df)) > 0]
  df <- df[rowSums(!is.na(df)) > 0, ]
  
  # Rename columns if necessary: for consistency with previous data,
  # rename the Snapper and Grouper columns if they exist.
  df <- df %>%
    rename(sml_Grouper = Grouper.30, 
           lrg_Grouper = Grouper.30.1,
           sml_snapper = Snapper.30,
           lrg_Snapper = Snapper.30.1,  
           Date = Date_mm.dd.yy,
           Time = Start_Time,
           Duration = Duration_min,
           Depth = Avg_Depth_m,
           Visibility = Visibility_m,
           Total_N = Total_abundance_N,
           )
  
  # Define species columns (using the same list as before)
  species_cols <- c("Parrotfish", "Rabbitfish", "Butterflyfish", "Angelfish", "Cleaner_Wrasse",
                    "Batfish", "Thicklip", "Red_Breast", "Slingjaw", "Sweetlips", "Squirrel.Soldier",
                    "Triggerfish", "Porcupine.Puffer", "Ray", "sml_snapper", "lrg_Snapper", 
                    "Eel", "Trevally", "Emperorfish",
                    "sml_Grouper", "lrg_Grouper", "Barracuda")
  species_cols <- species_cols[species_cols %in% colnames(df)]
  
  # Convert species columns to numeric
  df[species_cols] <- lapply(df[species_cols], as.numeric)
  
  # Fix Formats:
  # Convert Date column: assuming the new dataset's Date is in the format "01-01-2020" (adjust format if needed)
  df$Date <- as.Date(as.character(df$Date), format = "%m/%d/%Y")
  # Convert Time column (assuming it's in "%H:%M" format)
  df$Time <- format(as.POSIXct(as.character(df$Time), format = "%H:%M"), "%H:%M")
  # Convert Weather to factor if it exists
  df$Weather <- as.factor(df$Weather)
  
    # Create Survey ID column
    df <- df %>%
      mutate(survey_id = paste(Site, Date, sep = "_"))

  # Pivot longer: Convert species columns into a long format (one row per survey-species)
  fish_long_timed <- df %>%
    pivot_longer(
      cols = all_of(species_cols),
      names_to = "Species",
      values_to = "Count"
    )
  
  # Define functional groups (same mapping as before)
  functional_groups <- data.frame(
    Species = c("Parrotfish", "Rabbitfish", "Butterflyfish", "Angelfish", "Cleaner_Wrasse",
                "Batfish", "Thicklip", "Red_Breast", "Slingjaw", "Sweetlips", "Squirrel.Soldier",
                "Triggerfish", "Porcupine.Puffer", "Ray", "sml_snapper", "lrg_Snapper", "Eel", "Trevally", "Emperorfish",
                "sml_Grouper", "lrg_Grouper", "Barracuda"),
    Functional_Group = c("Herbivore", "Herbivore", "Herbivore", "Invertivore", "Invertivore",
                         "Invertivore", "Invertivore", "Invertivore", "Invertivore", "Invertivore",
                         "Invertivore", "Invertivore", "Invertivore", "Mesopredator",
                         "Mesopredator", "HTLP", "Mesopredator", "HTLP", "Mesopredator",
                         "Mesopredator", "HTLP", "HTLP")
  )
  
  fish_long_timed <- fish_long_timed %>%
    left_join(functional_groups, by = "Species") %>%
    mutate(Functional_Group = factor(Functional_Group,
                                     levels = c("Herbivore", "Invertivore", "Mesopredator", "HTLP"),
                                     ordered = TRUE))
  fish_long_timed <- fish_long_timed %>%
    # Exclude unwanted species 
    filter(!Species %in% c("Barracuda", "Eel", "Porcupine.Puffer", "Ray")) %>%
    filter(!Count %in% NA) %>%
    mutate(Site = fct_recode(Site,
                             # "Green Rock" = "Green Wall",
                             "Twins" = "Twins wall"))
  # Add Site types and classifications 
  site_info <- tibble(
    Site = factor(c("Green Rock", "Red Rock", "Twins", "White Rock")),
    Type = "Natural",  # all are natural
    Classification = c("Fringing", "Fringing", "Fringing", "Pinnacle")  # White Rock = Pinnacle
  )
  fish_long_timed <- fish_long_timed %>%
    left_join(site_info, by = "Site")
  
  # Round counts upward (using ceiling)
  fish_long_timed <- fish_long_timed <- fish_long_timed %>%
    mutate(Count = ceiling(Count))
  
  return(fish_long_timed)
}

# Example usage:
fish_long_timed <-clean_timed_fish_data(file_path_timed)
unique(fish_long_timed$Site)

# Append timed_fish_raw to fish_long
fish_long <- bind_rows(fish_long, fish_long_timed)

unique(fish_long$Site)
fish_long <- fish_long %>%
          mutate(Site = fct_recode(Site,
                         "No Name Pinnacle" = "No Name"))


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

pelagic_sites <- c("Chumphon", "Southwest", "No Name Pinnacle")
# 


fish_long <- fish_long %>%
  mutate(
    Zone = case_when(
      Classification == "Shipwreck" ~ "wreck",         # override first
      Site %in% pelagic_sites       ~ "pelagic",       # then pelagic
      TRUE                          ~ "nearshore"      # default fallback
    )
  )

# --------------------------
######### 3. Save your results #######
# --------------------------
save_results <- function(fish_long, output_dir, analysis_date) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save the cleaned dataset
  save(fish_long, file = file.path(output_dir, paste0("timed_fish_long_", analysis_date, ".RData")))
  
  cat("✅ Results saved successfully in:", output_dir, "\n")
}

save_results(fish_long, output_dir, analysis_date)

# Print completion message
cat("✅ Timed fish surveys added in! Data and plots saved in:", output_dir, "\n")

