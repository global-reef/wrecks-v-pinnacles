### plotting and summaries for fixed effects — based on best_fit ####################################
library(tidyverse)
library(tibble)
library(tidyr)
library(stringr)
library(brms)
library(tidybayes)
library(posterior)
library(grDevices)
library(ggplot2)

### custom theme and colour palettes ###############################################################
theme_clean <- theme_minimal(base_family = "serif", base_size = 10) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_blank(),
    
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    strip.text   = element_text(size = 11),
    
    plot.margin = margin(6, 6, 6, 6)
  )

custom_colours1 <- c(
  "Grazer" = "#66c2a4",
  "Invertivore" = "#41b6c4",
  "Mesopredator" = "#2c7fb8",
  "HTLP" = "#253494"
)
custom_colours2 <- c(
  "Grazer" = "#F8EA8C",
  "Invertivore" = "#F49D7D",
  "Mesopredator" = "#49B3CF",
  "HTLP" = "#4783F9"
)

### predicted values from fixed effects ############################################################
coefs <- fixef(best_fit)

pred_df <- tibble::tribble(
  ~Functional_Group,  ~Classification, ~Intercept, ~Effect,
  "Grazer",           "Shipwreck",     coefs["Grazer_Intercept","Estimate"],                         0,
  "Grazer",           "Fringing",      coefs["Grazer_Intercept","Estimate"],     coefs["Grazer_ClassificationFringing","Estimate"],
  "Grazer",           "Pinnacle",      coefs["Grazer_Intercept","Estimate"],     coefs["Grazer_ClassificationPinnacle","Estimate"],
  "Invertivore",      "Shipwreck",     coefs["Invertivore_Intercept","Estimate"],                    0,
  "Invertivore",      "Fringing",      coefs["Invertivore_Intercept","Estimate"],coefs["Invertivore_ClassificationFringing","Estimate"],
  "Invertivore",      "Pinnacle",      coefs["Invertivore_Intercept","Estimate"],coefs["Invertivore_ClassificationPinnacle","Estimate"],
  "Mesopredator",     "Shipwreck",     coefs["Mesopredator_Intercept","Estimate"],                   0,
  "Mesopredator",     "Fringing",      coefs["Mesopredator_Intercept","Estimate"],coefs["Mesopredator_ClassificationFringing","Estimate"],
  "Mesopredator",     "Pinnacle",      coefs["Mesopredator_Intercept","Estimate"],coefs["Mesopredator_ClassificationPinnacle","Estimate"],
  "HTLP",             "Shipwreck",     coefs["HTLP_Intercept","Estimate"],                           0,
  "HTLP",             "Fringing",      coefs["HTLP_Intercept","Estimate"],       coefs["HTLP_ClassificationFringing","Estimate"],
  "HTLP",             "Pinnacle",      coefs["HTLP_Intercept","Estimate"],       coefs["HTLP_ClassificationPinnacle","Estimate"]
) %>%
  mutate(Predicted_Log = Intercept + Effect,
         Predicted     = exp(Predicted_Log)) %>%
  as.data.frame()

pred_df$Functional_Group <- factor(pred_df$Functional_Group,
                                   levels = c("Grazer", "Invertivore", "Mesopredator", "HTLP"))

### predicted abundance by classification ##########################################################
abundance_plot <- ggplot(pred_df, aes(x = Classification, y = Predicted, fill = Classification)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  labs(title = "Predicted Abundance by Site Classification",
       x = "Site Classification",
       y = "Predicted Abundance") +
  theme_clean + 
  scale_fill_brewer(palette = "BuGn")

print(abundance_plot)

### graphical abstract bars ########################################################################
abundance_plot2 <- ggplot(pred_df, aes(x = Functional_Group, y = Predicted, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  facet_wrap(~ Classification) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_clean +
  scale_fill_manual(values = custom_colours2) +
  theme(strip.text = element_blank(), strip.background = element_blank())

print(abundance_plot2)

### proportional composition by classification #####################################################
pred_df_prop <- pred_df %>%
  group_by(Classification) %>%
  mutate(Proportion = Predicted / sum(Predicted)) %>%
  ungroup()

proportion_plot <- ggplot(pred_df_prop, aes(x = Classification, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportional Composition of Functional Groups by Site Classification",
       x = "Site Classification",
       y = "Proportion of Predicted Abundance",
       fill = "Functional \n Group") +
  theme_clean + 
  scale_fill_brewer(palette = "BuGn")

print(proportion_plot)

pred_df_prop$Functional_Group <- factor(pred_df_prop$Functional_Group,
                                        levels = c("Grazer", "Invertivore", "Mesopredator", "HTLP"))

proportion_plot2 <- ggplot(pred_df_prop, aes(x = Functional_Group, y = Proportion, fill = Functional_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Classification) +
  labs(title = NULL, x = NULL, y = NULL, fill = NULL) +
  theme_clean +
  scale_fill_manual(values = custom_colours2) +
  theme(strip.text = element_blank(), strip.background = element_blank()) 

print(proportion_plot2)

### forest plot of classification effects from fit_re ##############################################
fixef_df <- as.data.frame(fixef(fit_re))
fixef_df$Parameter <- rownames(fixef_df)
fixef_df <- fixef_df %>% rename(Lower = `Q2.5`, Upper = `Q97.5`)

forest_df <- fixef_df %>%
  filter(grepl("Classification", Parameter)) %>%
  separate(Parameter, into = c("Group", "Effect"), sep = "_", extra = "merge", fill = "right") %>%
  mutate(
    Effect = gsub("Classification", "", Effect),
    Group  = gsub("^mu", "", Group),
    Label  = paste0(Group, " (", Effect, ")")
  )

forest_plot <- ggplot(forest_df, aes(x = Estimate, y = Label)) +
  geom_point() +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2) +
  labs(title = "Forest Plot: Classification Effects on Functional Groups",
       x = "Estimated Change (log scale)",
       y = "Functional Group (Comparison)") +
  theme_clean

print(forest_plot)

### posterior differences in log-abundance (vs. Shipwrecks) ########################################
draws <- fit_re %>%
  spread_draws(b_Grazer_ClassificationFringing,
               b_Grazer_ClassificationPinnacle,
               b_Invertivore_ClassificationFringing,
               b_Invertivore_ClassificationPinnacle,
               b_Mesopredator_ClassificationFringing,
               b_Mesopredator_ClassificationPinnacle,
               b_HTLP_ClassificationFringing,
               b_HTLP_ClassificationPinnacle)

draws_long <- draws %>%
  pivot_longer(cols = starts_with("b_"), names_to = "Parameter", values_to = "Difference") %>%
  separate(Parameter, into = c("Junk", "Group", "Contrast"), sep = "_", extra = "merge") %>%
  mutate(
    Comparison = gsub("Classification", "", Contrast),
    Group      = factor(Group, levels = c("Grazer", "Invertivore", "Mesopredator", "HTLP")),
    Comparison = factor(Comparison, levels = c("Fringing", "Pinnacle"))
  )

posterior_differences <- ggplot(draws_long, aes(x = Difference, y = Group, fill = Comparison)) +
  stat_halfeye(slab_alpha = 0.8, point_interval = median_qi, .width = 0.95,
               position = position_dodge(width = 0.6)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Posterior Differences in Log-Abundance (vs. Shipwrecks)",
       x = "Estimated Log Difference (relative to Shipwreck)",
       y = "Functional Group") +
  scale_fill_manual(values = c("#188041", "#a6dede")) +
  theme_clean 

print(posterior_differences)

### save tables and plots ##########################################################################

manuscript_id <- "M15062"

save_ir_fig <- function(plot, fig_no, output_dir,
                        width_mm, height_mm,
                        dpi = 600) {
  
  base <- paste0(manuscript_id, "_Fig", fig_no)
  
  ggsave(
    filename = file.path(output_dir, paste0(base, ".pdf")),
    plot = plot,
    width = width_mm, height = height_mm, units = "mm",
    device = grDevices::pdf
  )
  
  ggsave(
    filename = file.path(output_dir, paste0(base, ".tif")),
    plot = plot,
    width = width_mm, height = height_mm, units = "mm",
    dpi = dpi,
    compression = "lzw"
  )
}


save_model_outputs <- function(pred_df, abundance_plot, forest_plot, posterior_differences,
                               proportion_plot, output_dir, analysis_date) {
  write.csv(pred_df,
            file = file.path(output_dir, paste0("Predicted_Abundance_", analysis_date, ".csv")),
            row.names = FALSE)
  # Inter-Research submission figures
  save_ir_fig(proportion_plot,       2, output_dir, width_mm = 180, height_mm = 120)
  save_ir_fig(posterior_differences, 3, output_dir, width_mm = 180, height_mm = 110)
  
  message("✅ Saved predictions and plots to: ", output_dir)
}

save_model_outputs(pred_df, abundance_plot, forest_plot,
                   posterior_differences, proportion_plot,
                   output_dir, analysis_date)

### posterior probabilities for classification contrasts ###########################################
compute_posterior_probabilities_re <- function(fit_model, baseline_zone = "Shipwreck") {
  draws <- as_draws_df(fit_model)
  groups <- c("Grazer", "Invertivore", "Mesopredator", "HTLP")
  
  comparisons <- list(
    c("Fringing", "Shipwreck"),
    c("Pinnacle", "Shipwreck"),
    c("Pinnacle", "Fringing")
  )
  
  purrr::map_dfr(groups, function(group) {
    purrr::map_dfr(comparisons, function(comp) {
      a <- comp[1]; b <- comp[2]
      term_a <- paste0("b_", group, "_Classification", a)
      term_b <- if (b == baseline_zone) 0 else paste0("b_", group, "_Classification", b)
      
      diff_samples <- if (is.numeric(term_b) && term_b == 0) {
        draws[[term_a]]
      } else {
        draws[[term_a]] - draws[[term_b]]
      }
      
      tibble(
        Functional_Group = group,
        Comparison = paste(a, "–", b),
        Pr_greater_0 = mean(diff_samples > 0),
        Pr_less_0    = mean(diff_samples < 0),
        Median       = median(diff_samples),
        CI_lower     = quantile(diff_samples, 0.025),
        CI_upper     = quantile(diff_samples, 0.975)
      )
    })
  })
}

posterior_probs_re <- compute_posterior_probabilities_re(fit_re, baseline_zone = "Shipwreck")
print(posterior_probs_re)

posterior_probs_my <- compute_posterior_probabilities_re(best_fit, baseline_zone = "Shipwreck")
print("best fit — month-year")
print(posterior_probs_my)
