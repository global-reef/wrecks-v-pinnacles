### run functional group analysis as a multivariate model to compare all functional groups ####
library(dplyr)
library(tidyr)
library(lubridate)
library(brms)
library(loo)
library(ggplot2)



### data preparation ####
survey_level <- fish_long %>%
  mutate(survey_run = paste0(survey_id, "_", Researcher)) %>%
  group_by(survey_run, survey_id, Site, Classification, Zone, Date, Functional_Group) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Functional_Group, values_from = Count, values_fill = 0) %>%
  mutate(
    Year = factor(year(Date)),
    Month_Year = factor(
      format(floor_date(Date, "month"), "%Y-%m"),
      levels = format(sort(unique(floor_date(Date, "month"))), "%Y-%m")
    ),
    Classification = relevel(factor(Classification), ref = "Shipwreck")
  )

for (nm in c("Grazer","Invertivore","Mesopredator","HTLP")) {
  if (!nm %in% names(survey_level)) survey_level[[nm]] <- 0L
}

ctrl <- list(adapt_delta = 0.95, max_treedepth = 15)
fam  <- negbinomial()
mv_formula <- mvbind(Grazer, Invertivore, Mesopredator, HTLP) ~ Classification

### initial model (no random effects) ####
fit_none <- brm(
  formula = mv_formula,
  data = survey_level,
  family = fam,
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = ctrl, backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(fit_none, file = file.path(output_dir, "fit_none.rds"))

### add random intercept for site ####
fit_site <- brm(
  formula = update(mv_formula, . ~ . + (1 | p | Site)),
  data = survey_level,
  family = fam,
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = ctrl, backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(fit_site, file = file.path(output_dir, "fit_site.rds"))

### testing the addition of year as a random effect - in response to review #1 ##########
fit_year <- brm(
  bf(Grazer        ~ Classification + (1|p|Site) + (1|Year)) +
    bf(Invertivore   ~ Classification + (1|p|Site) + (1|Year)) +
    bf(Mesopredator  ~ Classification + (1|p|Site) + (1|Year)) +
    bf(HTLP          ~ Classification + (1|p|Site) + (1|Year)),
  data = survey_level,
  family = fam,
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = ctrl, backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(fit_year, file = file.path(output_dir, "fit_year.rds"))

### testing the addition of month-year as a random effect - final model ##########
fit_months <- brm(
  bf(Grazer        ~ Classification + (1|p|Site) + (1|Month_Year)) +
    bf(Invertivore   ~ Classification + (1|p|Site) + (1|Month_Year)) +
    bf(Mesopredator  ~ Classification + (1|p|Site) + (1|Month_Year)) +
    bf(HTLP          ~ Classification + (1|p|Site) + (1|Month_Year)),
  data = survey_level,
  family = fam,
  chains = 4, cores = 4, iter = 2000, warmup = 500,
  control = ctrl, backend = "cmdstanr", save_pars = save_pars(all = TRUE)
)
saveRDS(fit_months, file = file.path(output_dir, "fit_months.rds"))

### model selection and diagnostics ####
fit_none   <- add_criterion(fit_none,   "loo", moment_match = TRUE)
fit_site   <- add_criterion(fit_site,   "loo", moment_match = TRUE)
fit_year   <- add_criterion(fit_year,   "loo", moment_match = TRUE)
fit_months <- add_criterion(fit_months, "loo", moment_match = TRUE)

cmp <- brms::loo_compare(fit_none, fit_site, fit_year, fit_months)
cmp_df <- as.data.frame(cmp)
cmp_df$model <- rownames(cmp_df)
write.csv(cmp_df, file = file.path(output_dir, "model_comparison_loo.csv"), row.names = FALSE)

pk_ids <- loo::pareto_k_ids(fit_months$criteria$loo, threshold = 0.7)
saveRDS(pk_ids, file = file.path(output_dir, "pareto_k_ids_fit_months.rds"))

r2_months <- brms::bayes_R2(fit_months)
saveRDS(r2_months, file = file.path(output_dir, "bayesR2_fit_months.rds"))

best_fit <- fit_months
saveRDS(best_fit, file = file.path(output_dir, "best_fit.rds"))

### conditional effects by classification for each functional group ####
fgs <- c("Grazer","Invertivore","Mesopredator","HTLP")

# conditional_effects object for all responses #
ce_re <- conditional_effects(best_fit, effects = "Classification", re_formula = NA)

# tabular summary (per response to avoid indexing issues) # 
ce_list <- lapply(fgs, function(resp) {
  ce <- conditional_effects(best_fit, effects = "Classification", re_formula = NA, resp = resp)[[1]]
  ce$Functional_Group <- resp
  ce %>% select(Functional_Group, Classification, estimate__, lower__, upper__)
})
fg_summary <- bind_rows(ce_list) %>%
  mutate(estimate_fmt = sprintf("%.1f [%.1f–%.1f]", estimate__, lower__, upper__)) %>%
  select(Functional_Group, Classification, estimate_fmt) %>%
  pivot_wider(names_from = Classification, values_from = estimate_fmt)
write.csv(fg_summary, file = file.path(output_dir, "fg_summary_by_classification.csv"), row.names = FALSE)

### combine functional group predictions into total per classification ####
total_pred <- bind_rows(lapply(seq_along(ce_re), function(i) {
  ce_re[[i]] %>%
    select(Classification, estimate__, lower__, upper__) %>%
    mutate(FG = names(ce_re)[i])
})) %>%
  group_by(Classification) %>%
  summarise(
    estimate = sum(estimate__),
    lower = sum(lower__),
    upper = sum(upper__)
  ) %>%
  mutate(estimate_fmt = sprintf("%.0f [%.0f–%.0f]", estimate, lower, upper))
write.csv(total_pred, file = file.path(output_dir, "total_pred_by_classification.csv"), row.names = FALSE)

### plots ####
# 1) brms list plot ####
plot(ce_re)

# 2) ggplot per response and saved to disk ####
plots <- lapply(fgs, function(resp) {
  ce <- conditional_effects(best_fit, effects = "Classification", re_formula = NA, resp = resp)[[1]]
  p <- ggplot(ce, aes(x = Classification, y = estimate__)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower__, ymax = upper__), width = 0.15) +
    labs(y = paste0(resp, " (expected count)"), x = "Classification",
         title = paste0("Conditional means for ", resp)) +
    theme_minimal()
  ggsave(filename = file.path(output_dir, paste0("CE_", resp, ".png")),
         plot = p, width = 6.5, height = 4.5, dpi = 300)
  p
})

# also save a combined PDF of the four ggplots ####
pdf(file = file.path(output_dir, "CE_all_groups.pdf"), width = 7.5, height = 5.5)
for (p in plots) print(p)
dev.off()
