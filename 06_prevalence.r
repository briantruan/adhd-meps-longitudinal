# 06_prevalence.r
# goal: tibble and graph treated prevalence prevalence of ADHD diagnosis

prev_by_year <- svyby(
  ~adhd_dx,
  ~year,
  design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

n_by_year <- svyby(
  ~adhd_dx,
  ~year,
  design,
  FUN = svytotal,
  na.rm = TRUE,
  keep.var = TRUE
)

prevalence <- as_tibble(prev_by_year) %>%
  rename(
    prev = adhd_dx,
    prev_se = se
  ) %>%
  left_join(
    as_tibble(n_by_year) %>%
      select(year, adhd_dx, se) %>%
      rename(
        weighted_n = adhd_dx,
        weighted_n_se = se
      ),
    by = "year"
  ) %>%
  mutate(
    prev_pct = 100 * prev,
    prev_pct_se = 100 * prev_se
  ) %>%
  select(year, weighted_n, weighted_n_se, prev_pct, prev_pct_se)

# # tibble out as .csv file
# # skip if not needed
# write.csv(prevalence, "exports/prevalence.csv")

# ggplot of prevalence
prevalence_plot <- ggplot(prevalence, aes(x = year, y = prev_pct)) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 2) +
  geom_errorbar(
    aes(
      ymin = prev_pct - prev_pct_se,
      ymax = prev_pct + prev_pct_se
    ),
    width = 0.15,
    color = "#2C7FB8"
  ) +
  scale_x_continuous(breaks = prevalence$year) +
  labs(
    x = "Year",
    y = "Treated prevalence (%)"
  ) +
  theme_minimal()

# # save as png and svg
# # can skip if not needed
# ggsave("exports/prevalence_plot.png", plot = prevalence_plot, width = 9, height = 5)
# ggsave("exports/prevalence_plot.svg", plot = prevalence_plot, width = 9, height = 5)

# figure out N and % of people who have pmed_flag == 1 among those with adhd_dx == 1
adhd_with_pmed <- svyby(
  ~pmed_flag,
  ~year,
  subset(design, adhd_dx == 1),
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

n_adhd_with_pmed <- svyby(
  ~pmed_flag,
  ~year,
  subset(design, adhd_dx == 1),
  FUN = svytotal,
  na.rm = TRUE,
  keep.var = TRUE
)

adhd_with_pmed_csv <- tibble(
  year = adhd_with_pmed$year,
  pmed_pct = 100 * adhd_with_pmed$pmed_flag,
  pmed_pct_se = 100 * adhd_with_pmed$se,
  pmed_n = n_adhd_with_pmed$pmed_flag,
  pmed_n_se = n_adhd_with_pmed$se
)

# # skip if not needed
# write_csv(adhd_with_pmed_csv, "exports/adhd_with_pmed.csv")

adhd_with_pmed_plot <- ggplot(adhd_with_pmed_csv, aes(x = year, y = pmed_pct)) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 2) +
  geom_errorbar(
    aes(
      ymin = pmed_pct - pmed_pct_se,
      ymax = pmed_pct + pmed_pct_se
    ),
    width = 0.15,
    color = "#2C7FB8"
  ) +
  scale_x_continuous(breaks = adhd_with_pmed_csv$year) +
  labs(
    x = "Year",
    y = "ADHD-diagnosed with medication fills (%)"
  ) +
  theme_minimal()

# # skip if not needed
# ggsave("exports/adhd_with_pmed_plot.png", plot = adhd_with_pmed_plot, width = 9, height = 5)
# ggsave("exports/adhd_with_pmed_plot.svg", plot = adhd_with_pmed_plot, width = 9, height = 5)
