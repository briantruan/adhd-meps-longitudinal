# 10_graphs.r
# goal: make graphs that seem interesting
# already made prevalence graphs; see 06_prevalence.r
# also includes % ADHD-diagnosed with medication fills


# prevalence -------------------------------------------------------------

# prevalence of all, age_group == "<18", and age_group == "18-64" on the same plot

prev_age_subset_by_year <- svyby(
  ~adhd_dx,
  ~year + age_group,
  preanalytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

ggplot(prev_age_subset_by_year, aes(x = year, y = adhd_dx, color = age_group, group = age_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = adhd_dx - se, ymax = adhd_dx + se), width = 0.15) +
  scale_x_continuous(breaks = prev_age_subset_by_year$year) +
  labs(x = "Year", y = "Prevalence of ADHD diagnosis", color = "Age group") +
  theme_minimal()


# total_rx_ob_exp --------------------------------------------------------

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
      series = "Medication",
      mean_exp = var1,
      se = var1_se
    ),
  total_rx_ob_exp %>%
    transmute(
      year,
      series = "Office-based visit",
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
  scale_x_continuous(breaks = 2017:2023) +
  theme_minimal()

# # skip if not needed
# ggsave("exports/total_rx_ob_exp_long_plot.png", plot = total_rx_ob_exp_long_plot, width = 9, height = 5)
# ggsave("exports/total_rx_ob_exp_long_plot.svg", plot = total_rx_ob_exp_long_plot, width = 9, height = 5)


# telehealth visit trends ------------------------------------------------

total_ob_exp_by_year <- svyby(
  ~ob_totalexp,
  ~year,
  design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

teleheatlh_exp_by_year <- svyby(
  ~ob_totalexp_tele,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

mental_visit_exp_by_year <- svyby(
  ~ob_totalexp_mental,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

telehealth_mental_visit_exp_by_year <- svyby(
  ~ob_totalexp_tele_mental,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

total_ob_breakdown_exp <- as_tibble(total_ob_exp_by_year) %>%
  rename(
    var1 = ob_totalexp,
    var1_se = se
  ) %>%
  left_join(
    as_tibble(teleheatlh_exp_by_year) %>%
      select(year, ob_totalexp_tele, se) %>%
      rename(
        var2 = ob_totalexp_tele,
        var2_se = se
      ),
    by = "year"
  ) %>%
  left_join(
    as_tibble(mental_visit_exp_by_year) %>%
      select(year, ob_totalexp_mental, se) %>%
      rename(
        var3 = ob_totalexp_mental,
        var3_se = se
      ),
    by = "year"
  ) %>%
  left_join(
    as_tibble(telehealth_mental_visit_exp_by_year) %>%
      select(year, ob_totalexp_tele_mental, se) %>%
      rename(
        var4 = ob_totalexp_tele_mental,
        var4_se = se
      ),
    by = "year"
  ) %>%
  select(year, var1, var1_se, 
               var2, var2_se,
               var3, var3_se,
               var4, var4_se)

total_ob_breakdown_exp_long <- bind_rows(
  total_ob_breakdown_exp %>%
    transmute(
      year,
      series = "Total",
      mean_exp = var1,
      se = var1_se
    ),
  total_ob_breakdown_exp %>%
    transmute(
      year,
      series = "Telehealth",
      mean_exp = var2,
      se = var2_se
    ),
  total_ob_breakdown_exp %>%
    transmute(
      year,
      series = "Mental health",
      mean_exp = var3,
      se = var3_se
    ),
  total_ob_breakdown_exp %>%
    transmute(
      year,
      series = "Mental telehealth",
      mean_exp = var4,
      se = var4_se
    )
) %>%
  mutate(
    lower = mean_exp - 1.96 * se,
    upper = mean_exp + 1.96 * se
  )

total_ob_breakdown_exp_long_plot <- ggplot(
  total_ob_breakdown_exp_long %>% 
    mutate(mean_exp = na_if(mean_exp, 0)),
  aes(x = year, y = mean_exp, color = series, group = series)
) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  # they look ugly
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Year", y = "Mean expenditure (2023 USD)", color = "Office-based visit type") +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = 2017:2023) +
  theme_minimal()

# # skip if not needed
# ggsave("exports/total_ob_breakdown_exp_long_plot.png", plot = total_ob_breakdown_exp_long_plot, width = 9, height = 5)
# ggsave("exports/total_ob_breakdown_exp_long_plot.svg", plot = total_ob_breakdown_exp_long_plot, width = 9, height = 5)

# rm objects
rm(mental_visit_exp_by_year, telehealth_mental_visit_exp_by_year, teleheatlh_exp_by_year, 
  total_ob_breakdown_exp, total_ob_breakdown_exp_long, total_ob_breakdown_exp_long_plot, total_ob_exp_by_year)

# visit count trends -----------------------------------------------------

total_ob_visits_by_year <- svyby(
  ~ob_visits,
  ~year,
  design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

total_telehealth_by_year <- svyby(
  ~ob_telehealth_visits,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

total_mental_visits_by_year <- svyby(
  ~ob_mental_health_visits,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

total_telemental_visits_by_year <- svyby(
  ~ob_telehealth_mental_visits,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

total_ob_breakdown_visits <- as_tibble(total_ob_visits_by_year) %>%
  rename(
    var1 = ob_visits,
    var1_se = se
  ) %>%
  left_join(
    as_tibble(total_telehealth_by_year) %>%
      select(year, ob_telehealth_visits, se) %>%
      rename(
        var2 = ob_telehealth_visits,
        var2_se = se
      ),
    by = "year"
  ) %>%
  left_join(
    as_tibble(total_mental_visits_by_year) %>%
      select(year, ob_mental_health_visits, se) %>%
      rename(
        var3 = ob_mental_health_visits,
        var3_se = se
      ),
    by = "year"
  ) %>%
  left_join(
    as_tibble(total_telemental_visits_by_year) %>%
      select(year, ob_telehealth_mental_visits, se) %>%
      rename(
        var4 = ob_telehealth_mental_visits,
        var4_se = se
      ),
    by = "year"
  ) %>%
  select(year, var1, var1_se, 
               var2, var2_se,
               var3, var3_se,
               var4, var4_se)

total_ob_breakdown_visits_long <- bind_rows(
  total_ob_breakdown_visits %>%
    transmute(
      year,
      series = "Total",
      ob_visits = var1,
      se = var1_se
    ),
  total_ob_breakdown_visits %>%
    transmute(
      year,
      series = "Telehealth",
      ob_visits = var2,
      se = var2_se
    ),
  total_ob_breakdown_visits %>%
    transmute(
      year,
      series = "Mental health",
      ob_visits = var3,
      se = var3_se
    ),
  total_ob_breakdown_visits %>%
    transmute(
      year,
      series = "Mental telehealth",
      ob_visits = var4,
      se = var4_se
    )
) %>%
  mutate(
    lower = ob_visits - 1.96 * se,
    upper = ob_visits + 1.96 * se
  )

total_ob_breakdown_visits_long_plot <- ggplot(
  total_ob_breakdown_visits_long %>% 
    mutate(ob_visits = na_if(ob_visits, 0)),
  aes(x = year, y = ob_visits, color = series, group = series)
) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  # they look ugly
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(x = "Year", y = "Mean visits", color = "Office-based visit type") +
  scale_x_continuous(breaks = 2017:2023) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal()

# # skip if not needed
# ggsave("exports/total_ob_breakdown_visits_long_plot.png", plot = total_ob_breakdown_visits_long_plot, width = 9, height = 5)
# ggsave("exports/total_ob_breakdown_visits_long_plot.svg", plot = total_ob_breakdown_visits_long_plot, width = 9, height = 5)

# stimulant class percentage share - see 08_table2_rx.r ------------------
# rx expenditures: OOP share ---------------------------------------------

oop_rx_exp_by_year <- svyby(
  ~oop_share,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

oop_rx_exp <- as_tibble(oop_rx_exp_by_year) %>%
  rename(
    var1 = oop_share,
    var1_se = se
  ) %>%
  select(year, var1, var1_se)

oop_rx_exp_long <- bind_rows(
  oop_rx_exp %>%
    transmute(
      year,
      series = "Total",
      oop_share = var1,
      se = var1_se
    )) %>%
  mutate(
    lower = oop_share - 1.96 * se,
    upper = oop_share + 1.96 * se
  )

oop_rx_exp_long_plot <- ggplot(oop_rx_exp_long, 
  aes(x = year, y = oop_share)) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 2) +
  labs(x = "Year", y = "Out-of-pocket share") +
  scale_x_continuous(breaks = 2017:2023) +
  theme_minimal()

# skip if not needed
ggsave("exports/oop_rx_exp_long_plot.png", plot = oop_rx_exp_long_plot, width = 9, height = 5)
ggsave("exports/oop_rx_exp_long_plot.svg", plot = oop_rx_exp_long_plot, width = 9, height = 5)

# rx expenditures: mean days supplied ------------------------------------

mean_days_supp_by_year <- svyby(
  ~med_days_supp,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

mean_days_supp <- as_tibble(mean_days_supp_by_year) %>%
  rename(
    var1 = med_days_supp,
    var1_se = se
  ) %>%
  select(year, var1, var1_se)

mean_days_supp_long <- bind_rows(
  mean_days_supp %>%
    transmute(
      year,
      series = "Total",
      mean_days_supp = var1,
      se = var1_se
    )) %>%
  mutate(
    lower = mean_days_supp - 1.96 * se,
    upper = mean_days_supp + 1.96 * se
  )

mean_days_supp_long_plot <- ggplot(mean_days_supp_long, 
  aes(x = year, y = mean_days_supp)) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 2) +
  labs(x = "Year", y = "Mean days supplied") +
  scale_x_continuous(breaks = 2017:2023) +
  theme_minimal()

ggsave("exports/mean_days_supp_long_plot.png", plot = mean_days_supp_long_plot, width = 9, height = 5)
ggsave("exports/mean_days_supp_long_plot.svg", plot = mean_days_supp_long_plot, width = 9, height = 5)

# rx expenditures: expenditures in general -------------------------------

expenditures_by_year <- svyby(
  ~med_total_spend,
  ~year,
  analytic_design,
  FUN = svymean,
  na.rm = TRUE,
  keep.var = TRUE
)

expenditures <- as_tibble(expenditures_by_year) %>%
  rename(
    var1 = med_total_spend,
    var1_se = se
  ) %>%
  select(year, var1, var1_se)

expenditures_long <- bind_rows(
  expenditures %>%
    transmute(
      year,
      series = "Total",
      med_total_spend = var1,
      se = var1_se
    )) %>%
  mutate(
    lower = med_total_spend - 1.96 * se,
    upper = med_total_spend + 1.96 * se
  )

expenditures_long_plot <- ggplot(expenditures_long, 
  aes(x = year, y = med_total_spend)) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 2) +
  labs(x = "Year", y = "Mean expenditures (2023 USD)") +
  scale_x_continuous(breaks = 2017:2023) +
  theme_minimal()

ggsave("exports/expenditures_long_plot.png", plot = expenditures_long_plot, width = 9, height = 5)
ggsave("exports/expenditures_long_plot.svg", plot = expenditures_long_plot, width = 9, height = 5)
