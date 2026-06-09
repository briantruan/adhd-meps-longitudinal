# 10_graphs.r
# goal: make graphs that seem interesting
# already made prevalence graphs; see 06_prevalence.r
# also includes % ADHD-diagnosed with medication fills

total_rx_exp_by_year <- svyby(
  ~med_total_spend,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

total_ob_exp_by_year <- svyby(
  ~ob_totalexp,
  ~year,
  design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

total_rx_ob_exp <- as_tibble(total_rx_exp_by_year) %>%
  rename(
    var1 = med_total_spend,
    var1_se = se
  ) %>%
  left_join(
    as_tibble(total_ob_exp_by_year) %>%
      select(year, ob_totalexp, se) %>%
      rename(
        var2 = ob_totalexp,
        var2_se = se
      ),
    by = "year"
  ) %>%
  select(year, var1, var1_se, var2, var2_se)

total_rx_ob_exp_long <- bind_rows(
  total_rx_ob_exp %>%
    transmute(
      year,
      series = "Rx spend",
      mean_exp = var1,
      se = var1_se
    ),
  total_rx_ob_exp %>%
    transmute(
      year,
      series = "Out-of-pocket spend",
      mean_exp = var2,
      se = var2_se
    )
) %>%
  mutate(
    lower = mean_exp - 1.96 * se,
    upper = mean_exp + 1.96 * se
  )

total_rx_ob_exp_long_plot <- ggplot(total_rx_ob_exp_long, 
  aes(x = year, y = mean_exp, color = series, group = series)) +
  geom_line(linewidth = 1) +
  geom_point() +
  # they look ugly
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Year", y = "Mean expenditure (2023 USD)", color = "Spend type") +
  theme_minimal()

# skip if not needed
ggsave("exports/total_rx_ob_exp_long_plot.png", plot = total_rx_ob_exp_long_plot, width = 9, height = 5)
ggsave("exports/total_rx_ob_exp_long_plot.svg", plot = total_rx_ob_exp_long_plot, width = 9, height = 5)
