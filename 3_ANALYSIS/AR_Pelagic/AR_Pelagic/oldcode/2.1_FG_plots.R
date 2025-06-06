library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

plot_functional_forest <- function(model, group_vec = c("Herbivore", "Invertivore", "Mesopredator", "HTLP")) {
  # Extract fixed effects from the model and convert to a data frame
  fixef_df <- as.data.frame(fixef(model))
  fixef_df$Parameter <- rownames(fixef_df)
  
  # Split the parameter names into "Group" and "Effect".
  # (e.g., "muHerbivore_ClassificationShipwreck" becomes Group = "muHerbivore" and Effect = "ClassificationShipwreck")
  fixef_df <- fixef_df %>%
    separate(Parameter, into = c("Group", "Effect"), sep = "_", extra = "merge", fill = "right")
  
  # Remove intercepts and keep only classification effects.
  fixef_df <- fixef_df %>%
    filter(!is.na(Effect) & Effect != "Intercept") %>%
    mutate(Effect = gsub("Classification", "", Effect))
  
  # Remove the "mu" prefix from Group (if present) and create a clear label.
  fixef_df <- fixef_df %>%
    mutate(Group = gsub("^mu", "", Group),
           Label = paste0(Group, " (", Effect, ")"))
  
  # Restrict to only the functional groups of interest.
  fixef_df_subset <- fixef_df %>% filter(Group %in% group_vec)
  
  # Create a list of forest plots—one for each functional group in group_vec.
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
  
  # Arrange the plots side by side (one column per functional group)
  grid.arrange(grobs = plot_list, ncol = length(unique(fixef_df_subset$Group)))
}

# Example usage:
# Assuming your functional group model is stored in the object "fit"
plot_functional_forest(results$fit, group_vec = c("Invertivore", "Mesopredator", "HTLP"))
