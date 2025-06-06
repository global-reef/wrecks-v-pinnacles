# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(lubridate)





# --------------------------
######### 1. Clean up the data to be worked with #######
# --------------------------
# Function to clean and process the raw data
clean_data <- function(file_path) {
  # Load the raw data with strings as factors
  # browser() # for troubleshooting 
  df <- read.csv(file_path, stringsAsFactors = TRUE, strip.white = TRUE)
  # remove blank rows and columns 
  df[df == ""] <- NA
  df <- df[, colSums(!is.na(df)) > 0]
  df <- df[rowSums(!is.na(df)) > 0, ]
  
  # Rename columns
  df <- df %>%
    rename(sml_Grouper = Grouper.30,
           lrg_Grouper = Grouper.30.1,
           lrg_Snapper = Snapper.30)
  
  # Define the species columns of interest and keep only those that exist in the data
  species_cols <- c("Parrotfish", "Rabbitfish", "Butterflyfish", "Angelfish", "Cleaner_Wrasse",
                    "Batfish", "Thicklip", "Red_Breast", "Slingjaw", "Sweetlips", "Squirrel.Soldier",
                    "Triggerfish", "Porcupine.Puffer", "Ray", "Brown_Stripe_Snapper", 
                    "Russels_Snapper", "lrg_Snapper", "Eel", "Trevally", "Emperorfish",
                    "sml_Grouper", "lrg_Grouper", "Barracuda")
  species_cols <- species_cols[species_cols %in% colnames(df)]
  
  # Fix Formats
  df[species_cols] <- lapply(df[species_cols], as.numeric)
  df$Date <- as.Date(as.character(df$Date), format = "%m/%d/%Y")
  df$Time <- format(as.POSIXct(df$Time, format = "%H:%M"), "%H:%M")
  df$Weather <- as.factor(df$Weather)
  
  # Pivot longer: create a long-format data frame with one row per survey-species
  fish_long <- df %>%
    pivot_longer(
      cols = all_of(species_cols),
      names_to = "Species",
      values_to = "Count"
    )
  
  # Define functional groups
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
                         "Mesopredator", "HTLP", "HTLP")
  )
  
  # Join the functional groups and set order
  fish_long <- fish_long %>%
    left_join(functional_groups, by = "Species") %>%
    mutate(Functional_Group = factor(Functional_Group,
                                     levels = c("Herbivore", "Invertivore", "Mesopredator", "HTLP"),
                                     ordered = TRUE))
  # round to the nearest ineger 
  fish_long <- fish_long %>% 
    mutate(Count = ceiling(Count))
  # Create a unique survey ID if not already present
  if(!"survey_id" %in% colnames(fish_long)) {
    fish_long <- fish_long %>%
      mutate(survey_id = paste(Site, Date, sep = "_"))
  }
  return(fish_long)
}

# Use the function to clean the data 
fish_long <- clean_data(file_path)

# remove outliers (barracudas, rays, porcupine/puffers, and eels (zero-inflated and/or extreme counts. see exploratory analysis code 1.2))
fish_long <- fish_long %>%
  filter(!Species %in% c("Barracuda", "Eel", "Porcupine.Puffer", "Ray"))
# sorry girlie 
fish_long <- fish_long %>% 
  filter(Researcher != "Keisha")
# Filter out pre-2023-09-01 surveys for Aow Mao Wreck and No Name Wreck (pre-deployment)
fish_long <- fish_long %>%
  filter(!(Site %in% c("Aow Mao Wreck", "No Name Wreck") & Date < as.Date("2023-09-01")))

# --------------------------
######### 2.  Summarize Functional Groups #######
# --------------------------
plot_functional_group_summaries <- function(fish_long) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggthemes)
  library(gridExtra)
  
  # Summary by Site Type
  func_summary_type <- fish_long %>%
    group_by(Type, Functional_Group) %>%
    summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")
  
  p1 <- ggplot(func_summary_type, aes(x = Type, y = Total_Count, fill = Functional_Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    labs(title = "Functional Group Abundance by Site Type",
         x = "Site Type", y = "Total Count") +
    scale_fill_brewer(palette = "BuGn") +
    theme_tufte()
  
  # Summary by Site Classification (stacked)
  func_summary_class <- fish_long %>%
    group_by(Classification, Functional_Group) %>%
    summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")
  
  p2 <- ggplot(func_summary_class, aes(x = Classification, y = Total_Count, fill = Functional_Group)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Functional Group Abundance by Site Classification",
         x = "Site Classification", y = "Total Count") +
    scale_fill_brewer(palette = "BuGn") +
    theme_tufte()
  
  # Calculate proportions by Classification
  func_summary_class <- func_summary_class %>%
    group_by(Classification) %>%
    mutate(Proportion = Total_Count / sum(Total_Count)) %>%
    ungroup()
  
  # Proportion plot: dodge position
  p3 <- ggplot(func_summary_class, aes(x = Classification, y = Proportion, fill = Functional_Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    labs(title = "Proportions of Functional Groups by Classification",
         x = "Site Classification", y = "Proportion") +
    scale_fill_brewer(palette = "BuGn") +
    theme_tufte()
  
  # Proportion plot: stack position
  p4 <- ggplot(func_summary_class, aes(x = Classification, y = Proportion, fill = Functional_Group)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Proportions of Functional Groups by Classification",
         x = "Site Classification", y = "Proportion") +
    scale_fill_brewer(palette = "BuGn") +
    theme_tufte()
  
  # Arrange p3 and p4 side by side (2 columns)
  arranged_plots <- grid.arrange(p1, p4, ncol = 2)
  
  # Return a list of plots for further use
  list(
    p_site_type = p1,
    p_site_class = p2,
    p_proportions = list(dodge = p3, stack = p4),
    arranged = arranged_plots
  )
}

# Example usage:
plot_results <- plot_functional_group_summaries(fish_long)



# --------------------------
######### 3. Save your results #######
# --------------------------
save_results <- function(fish_long, plot_results, output_dir, analysis_date) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save the cleaned dataset
  write.csv(fish_long, file = file.path(output_dir, paste0("fish_long_", analysis_date, ".csv")), row.names = FALSE)
  
  # Define filenames and corresponding plots
  file_names <- c(
    paste0("Functional_Groups_SiteType_", analysis_date, ".png"),
    paste0("Functional_Groups_Classification_", analysis_date, ".png"),
    paste0("Functional_Group_Proportion_Dodge_", analysis_date, ".png"),
    paste0("Functional_Group_Proportion_Stack_", analysis_date, ".png")
  )
  
  plot_objects <- list(
    plot_results$p_site_type,
    plot_results$p_site_class,
    plot_results$p_proportions$dodge,
    plot_results$p_proportions$stack
  )
  
  # Loop over the lists and save each plot
  for (i in seq_along(file_names)) {
    ggsave(file.path(output_dir, file_names[i]), plot = plot_objects[[i]], width = 8, height = 6)
  }
  
  cat("✅ Results saved successfully in:", output_dir, "\n")
}

save_results(fish_long, plot_results, output_dir, analysis_date)

# Print completion message
cat("Analysis complete! Data and plots saved in:", output_dir, "\n")
