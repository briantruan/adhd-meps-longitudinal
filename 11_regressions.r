# 11_regressions.r
library(survey)
library(dplyr)
library(broom)


# # do an ITS to see if there's a change in prevalence using 2020 as my intervention point
# analytic_design <- update(
#   design,
#   year_c = year - min(year, na.rm = TRUE),
#   post2020 = as.numeric(year >= 2020),
#   time_post = (year - 2020) * as.numeric(year >= 2020)
# )

# its_model_prev <- svyglm(
#   adhd_dx ~ year_c + post2020 + time_post,
#   design = analytic_design,
#   family = gaussian(),
#   na.action = na.omit
# )

# results_prev <- its_model_prev %>%
#   tidy(conf.int = TRUE) %>%
#   filter(term %in% c("post2020", "time_post")) %>%
#   select(term, estimate, conf.low, conf.high, p.value)

# results_prev

# # graph the its_model_prev results
# pred_grid <- tibble(
#   year = 2017:2023,
#   year_c = year - min(year, na.rm = TRUE),
#   post2020 = as.numeric(year >= 2020),
#   time_post = (year - 2020) * as.numeric(year >= 2020)
# )

# pred_vals <- predict(its_model_prev, newdata = pred_grid)

# its_prev_plot_data <- pred_grid %>%
#   mutate(
#     predicted_prev = as.numeric(pred_vals),
#     predicted_prev_se = as.numeric(SE(pred_vals)),
#     ci_low = predicted_prev - 1.96 * predicted_prev_se,
#     ci_high = predicted_prev + 1.96 * predicted_prev_se
#   ) %>%
#   ggplot(aes(x = year, y = predicted_prev)) +
#   geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "#2C7FB8") +
#   geom_line(color = "#2C7FB8", linewidth = 1) +
#   geom_point(color = "#2C7FB8", size = 2) +
#   labs(
#     title = "Predicted Prevalence Over Time",
#     x = "Year",
#     y = "Predicted prevalence"
#   ) +
#   theme_minimal()

# its_prev_plot_data

# Create ITS variables on the survey design: centered time, post indicator (2020), and post slope
analytic_design <- update(
  analytic_design,
  year_c = year - min(year, na.rm = TRUE),
  post2020 = as.numeric(year >= 2020),
  time_post = (year - 2020) * as.numeric(year >= 2020)
)

# Fit interrupted time series (level change + slope change) using survey-weighted linear regression
its_model_med_spend <- svyglm(
  adhd_dx ~ year_c + post2020 + time_post,
  design = analytic_design,
  family = gaussian(),
  na.action = na.omit
)

results <- its_model_med_spend %>%
  tidy(conf.int = TRUE) %>%
  filter(term %in% c("post2020", "time_post")) %>%
  select(term, estimate, conf.low, conf.high, p.value)
