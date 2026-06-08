# get prevalence

# this represents the treated prevalence
adhd_results <- purrr::map_dfr(years, function(yr) {
  cond_adhd <- get(paste0("cond_", yr)) %>%
    filter(ICD10CDX == "F90") %>%
    mutate(adhd = 1) %>%
    distinct(DUPERSID, .keep_all = TRUE)

  merged <- get(paste0("fyc_", yr)) %>%
    left_join(cond_adhd, by = "DUPERSID", suffix = c("", "_cond")) %>%
    select(-ends_with("_cond")) %>%
    mutate(adhd = ifelse(is.na(adhd), 0, 1)) %>% 
    mutate(one = 1)

  design <- svydesign(
    id = ~VARPSU,
    strata = ~VARSTR,
    weights = ~PERWT,
    data = merged,
    nest = TRUE
  )

  total_obj <- svytotal(~adhd, design, na.rm = TRUE)
  mean_obj  <- svymean(~adhd, design, na.rm = TRUE)
  all_total <- svytotal(~one, design, na.rm = TRUE)  

  tibble(
    year = yr,
    adhd_total = as.numeric(coef(total_obj)),
    adhd_total_se = as.numeric(SE(total_obj)),
    adhd_prevalence = as.numeric(coef(mean_obj)),
    adhd_prevalence_se = as.numeric(SE(mean_obj)),
    total_population = as.numeric(coef(all_total)),
    total_population_se = as.numeric(SE(all_total))
  )
})

adhd_results

# plot

ggplot(adhd_results, aes(x = year, y = adhd_prevalence)) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 2) +
  geom_errorbar(
    aes(
      ymin = adhd_prevalence - adhd_prevalence_se,
      ymax = adhd_prevalence + adhd_prevalence_se
    ),
    width = 0.15,
    color = "#2C7FB8"
  ) +
  scale_x_continuous(breaks = adhd_results$year) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Estimated ADHD treated prevalence by year",
    x = "Year",
    y = "Treated prevalence (with SE bars)"
  ) +
  theme_minimal()