# 11_regressions.r
library(broom)

# add its variables to full design 
# (prevalence model needs all respondents)
design_its <- update(
  preanalytic_design,
  year_delta = year - min(year, na.rm = TRUE),
  post2020   = as.numeric(year >= 2020),
  time_post  = (year - 2020) * as.numeric(year >= 2020)
)

# labels
its_labels <- list(
  year_delta = "Year (centered at 2017)",
  post2020   = "Post-2020",
  time_post  = "Time since 2020 (years)",
  age_group = "Age group",
  sex = "Sex",
  race = "Race",
  marital = "Marital status",
  education = "Education",
  has_insurance = "Has insurance",
  povcat = "Poverty category"
)

# its prevalence --------------------------------------------------------------
its_prevalence_full <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + ethnicity + race + marital + education +
                    has_insurance + povcat + comorbid_dx,
  design    = design_its,
  family    = gaussian(),
  na.action = na.omit
)

# -comorbid_dx
its_prevalence_1 <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + ethnicity + race + marital + education +
                    has_insurance + povcat,
  design    = design_its,
  family    = gaussian(),
  na.action = na.omit
)

# -comorbid_dx
# -ethnicity
its_prevalence_2 <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + marital + education +
                    has_insurance + povcat,
  design    = design_its,
  family    = gaussian(),
  na.action = na.omit
)

# -comorbid_dx
# -ethnicity
# -marital status: not sensible
# -education: not sensible
its_prevalence_3 <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + 
                    has_insurance + povcat,
  design    = design_its,
  family    = gaussian(),
  na.action = na.omit
)

AIC(its_prevalence_full, its_prevalence_1, its_prevalence_2, its_prevalence_3)

its_prevalence <- its_prevalence_3

its_prevalence_results <- its_prevalence %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)

# export to gt table format
its_prevalence_results_gt <- its_prevalence_results %>%
  mutate(
    term = sapply(term, function(t) {
      matched <- Filter(function(k) startsWith(t, k), names(its_labels))
      if (length(matched) == 0) return(t)
      key    <- matched[[which.max(nchar(matched))]]  # longest matching key
      suffix <- substr(t, nchar(key) + 1, nchar(t))
      if (nchar(suffix) == 0) its_labels[[key]] else paste0(its_labels[[key]], ": ", suffix)
    }),
    estimate_ci = paste0(
      formatC(estimate, format = "f", digits = 4),
      "\n(",
      formatC(conf.low, format = "f", digits = 4),
      ", ",
      formatC(conf.high, format = "f", digits = 4),
      ")"
    ),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  ) %>%
  select(term, estimate_ci, p.value) %>%
  gt() %>%
  cols_label(
    term = md("**Term**"),
    estimate_ci = md("**Estimate (95% CI)**"),
    p.value = md("**p-value**")
  )

# table3
gtsave(its_prevalence_results_gt, "exports/table3.html")
gtsave(its_prevalence_results_gt, "exports/table3.docx")

# its prevalence by age group
its_prevalence_full_adults <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    sex + ethnicity + race + marital + education +
                    has_insurance + povcat + comorbid_dx,
  design    = subset(design_its, subset = age_group == "18-64"),
  family    = gaussian(),
  na.action = na.omit
)

summary(its_medspend_full_adults)

# -povcat
its_prevalence_1_adults <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates 
                    sex + ethnicity + race + marital + education +
                    has_insurance + comorbid_dx,
  design    = subset(design_its, subset = age_group == "18-64"),
  family    = gaussian(),
  na.action = na.omit
)

# -povcat
# -sex
its_prevalence_2_adults <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates 
                    ethnicity + race + marital + education +
                    has_insurance,
  design    = subset(design_its, subset = age_group == "18-64"),
  family    = gaussian(),
  na.action = na.omit
)

# -povcat
# -sex
# -ethnicity
its_prevalence_3_adults <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates 
                    race + marital + education +
                    has_insurance,
  design    = subset(design_its, subset = age_group == "18-64"),
  family    = gaussian(),
  na.action = na.omit
)

its_prevalence_full_nonadults <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    sex + ethnicity + race + marital + education +
                    has_insurance + povcat + comorbid_dx,
  design    = subset(design_its, subset = age_group == "<18"),
  family    = gaussian(),
  na.action = na.omit
)

# remove covariates that are not likely to be relevant for children
# -marital
# -education
its_prevalence_1_nonadults <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    sex + ethnicity + race +
                    has_insurance + povcat,
  design    = subset(design_its, subset = age_group == "<18"),
  family    = gaussian(),
  na.action = na.omit
)

# -marital
# -education
# -ethnicity
its_prevalence_2_nonadults <- svyglm(
  adhd_dx ~ year_delta + post2020 + time_post + #covariates
                    sex + race +
                    has_insurance + povcat,
  design    = subset(design_its, subset = age_group == "<18"),
  family    = gaussian(),
  na.action = na.omit
)

its_prevalence_adults <- its_prevalence_3_adults
its_prevalence_nonadults <- its_prevalence_2_nonadults

its_prevalence_adults_results <- its_prevalence_adults %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)

its_prevalence_nonadults_results <- its_prevalence_nonadults %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)

its_prevalence_adults_results_gt <- its_prevalence_adults_results %>%
  mutate(
    term = sapply(term, function(t) {
      matched <- Filter(function(k) startsWith(t, k), names(its_labels))
      if (length(matched) == 0) return(t)
      key    <- matched[[which.max(nchar(matched))]]  # longest matching key
      suffix <- substr(t, nchar(key) + 1, nchar(t))
      if (nchar(suffix) == 0) its_labels[[key]] else paste0(its_labels[[key]], ": ", suffix)
    }),
    estimate_ci = paste0(
      formatC(estimate, format = "f", digits = 4),
      "\n(",
      formatC(conf.low, format = "f", digits = 4),
      ", ",
      formatC(conf.high, format = "f", digits = 4),
      ")"
    ),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  ) %>%
  select(term, estimate_ci, p.value) %>%
  gt() %>%
  cols_label(
    term = md("**Term**"),
    estimate_ci = md("**Estimate (95% CI)**"),
    p.value = md("**p-value**")
  )

its_prevalence_nonadults_results_gt <- its_prevalence_nonadults_results %>%
  mutate(
    term = sapply(term, function(t) {
      matched <- Filter(function(k) startsWith(t, k), names(its_labels))
      if (length(matched) == 0) return(t)
      key    <- matched[[which.max(nchar(matched))]]  # longest matching key
      suffix <- substr(t, nchar(key) + 1, nchar(t))
      if (nchar(suffix) == 0) its_labels[[key]] else paste0(its_labels[[key]], ": ", suffix)
    }),
    estimate_ci = paste0(
      formatC(estimate, format = "f", digits = 4),
      "\n(",
      formatC(conf.low, format = "f", digits = 4),
      ", ",
      formatC(conf.high, format = "f", digits = 4),
      ")"
    ),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  ) %>%
  select(term, estimate_ci, p.value) %>%
  gt() %>%
  cols_label(
    term = md("**Term**"),
    estimate_ci = md("**Estimate (95% CI)**"),
    p.value = md("**p-value**")
  )

forestplot_labels <- list(
  "(Intercept)" = "(Intercept)",
  "year_delta" = "Year (centered at 2017)",
  "post2020"   = "Post-2020",
  "time_post"  = "Time since 2020 (years)",
  "sexMale" = "Sex - Male (ref: Female)",
  "raceBlack" = "Race - Black (ref: White)",
  "raceOther/Unknown" = "Race - Other/Unknown (ref: White)",
  "maritalNot married" = "Marital status - Not married (ref: Married)",
  "educationHigh school graduation or less" = "Education - ≤HS (ref: >HS)",
  "has_insuranceNo insurance" = "Has insurance - No (ref: Yes)",
  "povcatLow income" = "Poverty category - Low (ref: Very low income)",
  "povcatMiddle income" = "Poverty category - Middle (ref: Very low income)",
  "povcatHigh income" = "Poverty category - High (ref: Very low income)"
)

# its_prevalence_adults_results to forest plot
its_prevalence_adults_results_plot <- ggplot(its_prevalence_adults_results, 
  aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(x = "Term", y = "Estimate (95% CI)") +
  # change labels
  scale_x_discrete(limits = rev(names(forestplot_labels)), labels = function(x) {
    sapply(x, function(t) {
      matched <- Filter(function(k) startsWith(t, k), names(forestplot_labels))
      if (length(matched) == 0) return(t)
      key    <- matched[[which.max(nchar(matched))]]  # longest matching key
      suffix <- substr(t, nchar(key) + 1, nchar(t))
      if (nchar(suffix) == 0) forestplot_labels[[key]] else paste0(forestplot_labels[[key]], ": ", suffix)
    })
  }) +
  # add a line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal(base_size = 12)

its_prevalence_nonadults_results_plot <- ggplot(its_prevalence_nonadults_results, 
  aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(x = "Term", y = "Estimate (95% CI)") +
  # change labels
  scale_x_discrete(limits = rev(names(forestplot_labels)), labels = function(x) {
    sapply(x, function(t) {
      matched <- Filter(function(k) startsWith(t, k), names(forestplot_labels))
      if (length(matched) == 0) return(t)
      key    <- matched[[which.max(nchar(matched))]]  # longest matching key
      suffix <- substr(t, nchar(key) + 1, nchar(t))
      if (nchar(suffix) == 0) forestplot_labels[[key]] else paste0(forestplot_labels[[key]], ": ", suffix)
    })
  }) +
  # add a line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal(base_size = 12)

gtsave(its_prevalence_adults_results_gt, "exports/table3_adults.html")
gtsave(its_prevalence_adults_results_gt, "exports/table3_adults.docx")
gtsave(its_prevalence_nonadults_results_gt, "exports/table3_nonadults.html")
gtsave(its_prevalence_nonadults_results_gt, "exports/table3_nonadults.docx")

ggsave("exports/its_prevalence_adults_forestplot.png", 
       plot = its_prevalence_adults_results_plot, 
       width = 9, height = 5)
ggsave("exports/its_prevalence_adults_forestplot.svg", 
       plot = its_prevalence_adults_results_plot, 
       width = 9, height = 5)
ggsave("exports/its_prevalence_nonadults_forestplot.png", 
       plot = its_prevalence_nonadults_results_plot, 
       width = 9, height = 5)
ggsave("exports/its_prevalence_nonadults_forestplot.svg", 
       plot = its_prevalence_nonadults_results_plot, 
       width = 9, height = 5)

# its rx total expenditure ----------------------------------------------------

its_rx_expenditure_full <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + ethnicity + race + marital + education +
                    has_insurance + povcat + comorbid_dx,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_1 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + marital + education +
                    has_insurance + povcat + comorbid_dx,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_2 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + marital + education +
                    has_insurance + povcat,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_3 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + education +
                    has_insurance + povcat,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_4 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race +
                    has_insurance + povcat,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_5 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race +
                    has_insurance,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_6 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + race +
                    has_insurance,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

AIC(its_rx_expenditure_full, its_rx_expenditure_1, 
    its_rx_expenditure_2, its_rx_expenditure_3,
    its_rx_expenditure_4, its_rx_expenditure_5,
    its_rx_expenditure_6)

its_rx_expenditure <- its_rx_expenditure_6

its_rx_expenditure_results <- its_rx_expenditure %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)

its_rx_expenditure_gt <- its_rx_expenditure_results %>%
  mutate(
    term = sapply(term, function(t) {
      matched <- Filter(function(k) startsWith(t, k), names(its_labels))
      if (length(matched) == 0) return(t)
      key    <- matched[[which.max(nchar(matched))]]  # longest matching key
      suffix <- substr(t, nchar(key) + 1, nchar(t))
      if (nchar(suffix) == 0) its_labels[[key]] else paste0(its_labels[[key]], ": ", suffix)
    }),
    estimate_ci = paste0(
      formatC(estimate, format = "f", digits = 2),
      "\n(",
      formatC(conf.low, format = "f", digits = 2),
      ", ",
      formatC(conf.high, format = "f", digits = 2),
      ")"
    ),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  ) %>%
  select(term, estimate_ci, p.value) %>%
  gt() %>%
  cols_label(
    term = md("**Term**"),
    estimate_ci = md("**Estimate (95% CI)**"),
    p.value = md("**p-value**")
  )

# table4
gtsave(its_rx_expenditure_gt, "exports/table4.html")
gtsave(its_rx_expenditure_gt, "exports/table4.docx")

# its ob total expenditure ----------------------------------------------------

its_ob_expenditure_full <- svyglm(
  ob_totalexp ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + ethnicity + race + marital + education +
                    has_insurance + povcat + comorbid_dx,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_1 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + marital + education +
                    has_insurance + povcat + comorbid_dx,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_2 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + marital + education +
                    has_insurance + povcat,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_3 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race + education +
                    has_insurance + povcat,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_4 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race +
                    has_insurance + povcat,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_5 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + sex + race +
                    has_insurance,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

its_rx_expenditure_6 <- svyglm(
  med_total_spend ~ year_delta + post2020 + time_post + #covariates
                    age_group + race +
                    has_insurance,
  design    = analytic_design,
  family    = gaussian(),
  na.action = na.omit
)

AIC(its_rx_expenditure_full, its_rx_expenditure_1, 
    its_rx_expenditure_2, its_rx_expenditure_3,
    its_rx_expenditure_4, its_rx_expenditure_5,
    its_rx_expenditure_6)

its_rx_expenditure <- its_rx_expenditure_6

its_rx_expenditure_results <- its_rx_expenditure %>%
  tidy(conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)

its_rx_expenditure_gt <- its_rx_expenditure_results %>%
  mutate(
    term = sapply(term, function(t) {
      matched <- Filter(function(k) startsWith(t, k), names(its_labels))
      if (length(matched) == 0) return(t)
      key    <- matched[[which.max(nchar(matched))]]  # longest matching key
      suffix <- substr(t, nchar(key) + 1, nchar(t))
      if (nchar(suffix) == 0) its_labels[[key]] else paste0(its_labels[[key]], ": ", suffix)
    }),
    estimate_ci = paste0(
      formatC(estimate, format = "f", digits = 2),
      "\n(",
      formatC(conf.low, format = "f", digits = 2),
      ", ",
      formatC(conf.high, format = "f", digits = 2),
      ")"
    ),
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ formatC(p.value, format = "f", digits = 3)
    )
  ) %>%
  select(term, estimate_ci, p.value) %>%
  gt() %>%
  cols_label(
    term = md("**Term**"),
    estimate_ci = md("**Estimate (95% CI)**"),
    p.value = md("**p-value**")
  )

# table4
gtsave(its_rx_expenditure_gt, "exports/table4.html")
gtsave(its_rx_expenditure_gt, "exports/table4.docx")

