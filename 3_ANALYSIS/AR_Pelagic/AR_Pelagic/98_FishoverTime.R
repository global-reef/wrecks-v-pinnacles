library(dplyr)
library(ggplot2)

# Define the shipwreck site names
wreck_sites <- c("Aow Mao Wreck", "No Name Wreck", "Sattakut")

# Filter and summarize data
fish_wrecks <- fish_long %>%
  filter(Site %in% wreck_sites) %>%
  group_by(Species, Site, Date) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

fish_wrecks_filtered <- fish_wrecks %>%
  group_by(Species) %>%
  filter(sum(Total, na.rm = TRUE) > 0)


# Create a plot per species
ggplot(fish_wrecks, aes(x = Date, y = Total, color = Site)) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Species, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Smoothed Fish Counts Over Time at Shipwreck Sites",
    y = "Total Count",
    x = "Date",
    color = "Shipwreck Site"
  )

~~~~~~~~~~~~`

library(dplyr)
library(ggplot2)

wreck_sites <- c("Aow Mao Wreck", "No Name Wreck", "Sattakut")

# Step 1: Summarize all wrecks
fish_wrecks <- fish_long %>%
  filter(Site %in% wreck_sites) %>%
  mutate(Site = as.character(Site)) %>%
  group_by(Species, Site, Date) %>%
  summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

# Step 2: Get date range
date_range <- range(fish_wrecks$Date)

# Step 3: Create Sattakut horizontal line data
sattakut_mean <- fish_wrecks %>%
  filter(Site == "Sattakut") %>%
  group_by(Species) %>%
  summarise(Total = mean(Total, na.rm = TRUE), .groups = "drop") %>%
  mutate(Site = "Sattakut (mean)")

# Duplicate each mean across full date range
sattakut_line <- sattakut_mean %>%
  tidyr::expand(Species, Date = seq(date_range[1], date_range[2], by = "day")) %>%
  left_join(sattakut_mean, by = "Species") %>%
  mutate(Site = "Sattakut (mean)")

# Combine data
fish_wrecks_combined <- fish_wrecks %>%
  filter(Site != "Sattakut") %>%
  bind_rows(sattakut_line)

# Plot
ggplot(fish_wrecks_combined, aes(x = Date, y = Total, color = Site)) +
  geom_smooth(data = filter(fish_wrecks_combined, Site != "Sattakut (mean)"),
              method = "loess", se = FALSE) +
  geom_line(data = filter(fish_wrecks_combined, Site == "Sattakut (mean)"),
            linetype = "dashed", size = 0.8) +
  facet_wrap(~ Species, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Smoothed Fish Counts Over Time at Shipwreck Sites",
    y = "Total Count",
    x = "Date",
    color = "Shipwreck Site"
  )


