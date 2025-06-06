# plot FG BMN results 
library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)

# Extract fixed effects as a data frame
fixef_df <- as.data.frame(fixef(fit))
fixef_df$Parameter <- rownames(fixef_df)

# Clean up parameter names.
# We assume parameter names are like "lrgGrouper_Intercept", "lrgGrouper_ClassificationPinnacle", etc.
# We'll separate them into "Group" and "Effect". 
# (If a parameter doesn't have an underscore, the 'Effect' will be NA.)
fixef_df <- fixef_df %>%
  separate(Parameter, into = c("Group", "Effect"), sep = "_", extra = "merge", fill = "right")

# Filter to only classification effects (i.e., where Effect is not "Intercept")
class_effects <- fixef_df %>% 
  filter(!is.na(Effect) & Effect != "Intercept")

# For clarity, you might want to clean up the Effect names by removing a prefix if present.
# For example, if they are "ClassificationPinnacle" and "ClassificationShipwreck",
# you can remove the "Classification" part:
class_effects <- class_effects %>%
  mutate(Effect = gsub("Classification", "", Effect))

# Create a new, more informative label.
# Remove the "mu" prefix from Group and then create a label like "Mesopredator (Shipwreck)"
class_effects <- class_effects %>%
  mutate(Group = gsub("^mu", "", Group),
         Label = paste0(Group, " (", Effect, ")"))

#  Create the forest plot using the new Label
ggplot(class_effects, aes(x = Estimate, y = Label)) +
  geom_point() +
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.2) +
  labs(title = "Forest Plot of Classification Effects by Functional Group",
       x = "Estimated Effect (log scale)",
       y = "Functional Group (Habitat)") +
  theme_minimal()


### to plot in three seperate plots - not as useful #### 
plot_functional_forest <- function(model, group_vec = c("Herbivore", "Invertivore", "Mesopredator", "HTLP")) {
  # Load required libraries
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
  
  # Extract fixed effects from the model and convert to a data frame.
  fixef_df <- as.data.frame(fixef(model))
  fixef_df$Parameter <- rownames(fixef_df)
  
  # Separate the Parameter name into "Group" and "Effect".
  # For example, "Herbivore_ClassificationShipwreck" becomes Group = "Herbivore" and Effect = "ClassificationShipwreck".
  fixef_df <- fixef_df %>%
    separate(Parameter, into = c("Group", "Effect"), sep = "_", extra = "merge", fill = "right")
  
  # Remove intercept parameters and clean up Effect names.
  fixef_df <- fixef_df %>%
    filter(!is.na(Effect) & Effect != "Intercept") %>%
    mutate(Effect = gsub("Classification", "", Effect))
  
  # Create a new label in the form "Herbivore (Shipwreck)"
  fixef_df <- fixef_df %>%
    mutate(Group = gsub("^mu", "", Group),
           Label = paste0(Group, " (", Effect, ")"))
  
  # Restrict to only the functional groups of interest.
  fixef_df_subset <- fixef_df %>% filter(Group %in% group_vec)
  
  # Create a list of forest plots, one for each functional group.
  plot_list <- lapply(unique(fixef_df_subset$Group), function(g) {
    df <- fixef_df_subset %>% filter(Group == g)
    ggplot(df, aes(x = Estimate, y = Label)) +
      geom_point() +
      geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.2) +
      labs(title = paste("Forest Plot for", g),
           x = "Estimated Effect (log scale)",
           y = "Habitat") +
      theme_minimal()
  })
  
  # Arrange the plots on one sheet. Change ncol as desired.
  grid.arrange(grobs = plot_list, ncol = length(unique(fixef_df_subset$Group)))
}
# Usage:
# Assuming your functional group model is stored in the object 'fit'
plot_functional_forest(fit, group_vec = c("Invertivore", "Mesopredator", "HTLP"))




