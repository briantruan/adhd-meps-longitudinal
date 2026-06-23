# 05_flowchart.r
# goal: make a flowchart showing how we select our cohort

# drop problematic rows for convenience
for (yr in years) {
  merged_name <- paste0("merged_", yr)
  merged <- get(merged_name) %>%
    select(-any_of(c("INSURC")))
  assign(merged_name, merged, envir = .GlobalEnv)
}

merged_df <- map_dfr(years, \(yr) {
  get(paste0("merged_", yr)) %>% 
    mutate(year = yr)
})

merged_df <- merged_df %>% 
  mutate(
    age_group = case_when(
      AGE53X < 18 ~ "<18",
      AGE53X >= 18 & AGE53X < 65 ~ "18-64",
      AGE53X >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    age_sex_group = case_when(
      SEX == "MALE" & age_group == "<18" ~ "Male (<18)",
      SEX == "MALE" & age_group == "18-64" ~ "Male (18-64)",
      SEX == "MALE" & age_group == "65+" ~ "Male (65+)",
      SEX == "FEMALE" & age_group == "<18" ~ "Female (<18)",
      SEX == "FEMALE" & age_group == "18-64" ~ "Female (18-64)",
      SEX == "FEMALE" & age_group == "65+" ~ "Female (65+)",
      TRUE ~ NA_character_
    ),
    adhd_dx = if_else(!is.na(ICD10CDX) & str_starts(ICD10CDX, "F90"), 1, 0),
    comorbid_dx = if_else(!is.na(sum_comorbidities) & sum_comorbidities > 0, 1, 0)
  )

# set up survey design
options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = merged_df,
  nest = TRUE
)

# clean up the environment too many vars
# keep only if it's merged_df, design, or years
rm(list = setdiff(ls(), c("merged_df", "design", "years")))

# EXCLUSION PATHWAY
# 1. total unweighted n
step1 <- merged_df

# 2. exclude AGE53X >= 65
step2 <- step1 %>% 
  filter(AGE53X < 65)

# 3. exclude Medicare
step3 <- step2 %>% 
  filter(medicare == "No Medicare")

# 4. exclude those without ADHD diagnosis (ICD10CDX not starting with F90)
step4 <- step3 %>% 
  filter(!is.na(ICD10CDX) & str_starts(ICD10CDX, "F90"))

# 5. those with RX fills
rx <- step4 %>% 
  filter(pmed_flag == 1)

# 6. those with OB visits
ob <- step4 %>% 
  filter(ob_flag == 1)

n_total <- nrow(step1)
n_age <- nrow(step2)
n_medicare <- nrow(step3)
n_adhd <- nrow(step4)
n_rx <- nrow(rx)
n_ob <- nrow(ob)

# # if the flow chart ever needs an update, can uncomment

# flow_chart <- DiagrammeR::grViz(sprintf(
#   "
#   digraph flowchart {
#     graph [
#       layout = dot,
#       rankdir = TB,
#       bgcolor = 'white',
#       splines = ortho,
#       nodesep = 0.45,
#       ranksep = 0.6
#     ]

#     node [
#       shape = box,
#       style = 'rounded,filled',
#       fontname = Helvetica,
#       fontsize = 12,
#       margin = '0.18,0.10',
#       width = 3.5,
#       height = 0.8,
#       color = '#3B5B92',
#       fillcolor = '#F7FAFF',
#       align = center
#     ]

#     edge [
#       color = '#6B7280',
#       penwidth = 1.2,
#       arrowsize = 0.8
#     ]

#     total [label = 'Total records\\nN = %s\\n']
#     age   [label = 'Age-eligible cohort (<65 years)\\nN = %s\\n']
#     med   [label = 'Non-Medicare, age-eligible cohort\\nN = %s\\n']
#     adhd  [label = 'Has ADHD diagnosis (F90.x)\\nN = %s\\n']

#     rx_node [label = 'ADHD stimulant medication fills\\nN = %s\\n', fillcolor = '#F0FDF4', color = '#15803D']
#     ob_node [label = 'ADHD-related office-based visits\\nN = %s\\n', fillcolor = '#F0FDF4', color = '#15803D']

#     exc_age  [label = 'Excluded: Age ≥65 years\\nN = %s\\n', fillcolor = '#FEF2F2', color = '#B91C1C']
#     exc_med  [label = 'Excluded: Medicare beneficiaries\\nN = %s\\n', fillcolor = '#FEF2F2', color = '#B91C1C']
#     exc_adhd [label = 'Excluded: No ADHD diagnosis\\nN = %s\\n', fillcolor = '#FEF2F2', color = '#B91C1C']
#     exc_rx   [label = 'No stimulant medication fills\\nN = %s\\n', fillcolor = '#FEF2F2', color = '#B91C1C']
#     exc_ob   [label = 'No ADHD-related office visits\\nN = %s\\n', fillcolor = '#FEF2F2', color = '#B91C1C']

#     total -> age -> med -> adhd -> rx_node
#     adhd -> ob_node

#     rx_node -> exc_rx [style = dashed, color = '#B91C1C']
#     ob_node -> exc_ob [style = dashed, color = '#B91C1C']

#     age  -> exc_age  [style = dashed, color = '#B91C1C', constraint = false]
#     med  -> exc_med  [style = dashed, color = '#B91C1C', constraint = false]
#     adhd -> exc_adhd [style = dashed, color = '#B91C1C', constraint = false]

#     { rank = same; total; }
#     { rank = same; age;   exc_age; }
#     { rank = same; med;   exc_med; }
#     { rank = same; adhd;  exc_adhd; }
#     { rank = same; rx_node; ob_node; }
#     { rank = same; exc_rx; exc_ob; }
#   }
#   ",
#   format(n_total, big.mark = ","),
#   format(n_age, big.mark = ","),
#   format(n_medicare, big.mark = ","),
#   format(n_adhd, big.mark = ","),
#   format(n_rx, big.mark = ","),
#   format(n_ob, big.mark = ","),
#   format(n_total - n_age, big.mark = ","),
#   format(n_age - n_medicare, big.mark = ","),
#   format(n_medicare - n_adhd, big.mark = ","),
#   format(n_adhd - n_rx, big.mark = ","),
#   format(n_adhd - n_ob, big.mark = ",")
# ))

# # export as png/svg
# svg <- DiagrammeRsvg::export_svg(flow_chart)
# rsvg::rsvg_svg(charToRaw(svg), "exports/inclusion_flowchart.svg")
# rsvg::rsvg_png(charToRaw(svg), "exports/inclusion_flowchart.png")

rm(list = c("ob", "rx", "step1", "step2", "step3", "step4", "flow_chart",
            "n_adhd", "n_age", "n_medicare", "n_ob", "n_rx", "n_total", "svg"))
