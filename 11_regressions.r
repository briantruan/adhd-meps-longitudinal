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

AIC(its_prevalence_full, its_prevalence_1, its_prevalence_2)

its_prevalence <- its_prevalence_2

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

