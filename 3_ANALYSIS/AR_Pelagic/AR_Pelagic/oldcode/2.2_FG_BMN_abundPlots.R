# Load required packages
library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)


plot_functional_group_predictions <- function(model, survey_data) {
  # Create a long-format summary of predicted abundances in one pipeline
  plot_data <- as.data.frame(fitted(model, newdata = survey_data)) %>%
    # Append Classification from the survey data
    mutate(Classification = survey_data$Classification) %>%
    # Reshape from wide to long format using pivot_longer()
    pivot_longer(
      cols = starts_with("Estimate"),
      names_to = "Functional_Group",
      names_prefix = "Estimate.",
      values_to = "Predicted"
    ) %>%
    # Group by Classification and Functional_Group and compute the mean predicted value
    group_by(Classification, Functional_Group) %>%
    summarise(mean_pred = mean(Predicted, na.rm = TRUE), .groups = "drop") %>%
    # Clean and reorder Functional_Group names:
    mutate(Functional_Group = gsub("P\\(Y = ", "", Functional_Group),
           Functional_Group = gsub("\\)", "", Functional_Group),
           Functional_Group = factor(Functional_Group,
                                     levels = c("Herbivore", "Invertivore", "Mesopredator", "HTLP")))
  
  # Create separate plots (one for each functional group) without storing extra objects
  plot_list <- lapply(levels(plot_data$Functional_Group), function(gr) {
    plot_data %>% 
      filter(Functional_Group == gr) %>%
      ggplot(aes(x = Classification, y = mean_pred, fill = Classification)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      scale_fill_grey(start = 0.8, end = 0.2) +
      labs(title = gr,
           x = "Classification",
           y = "Predicted Abundance") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Arrange and display the plots on one sheet (2 columns)
  grid.arrange(grobs = plot_list, ncol = 2)
}

# Example usage:
plot_functional_group_predictions(results$fit, results$survey_level)



