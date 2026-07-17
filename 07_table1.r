# 07_table1.r
# goal: produce a table 1

# table 1: collapse some categories
merged_df <- merged_df %>% 
  mutate(
    race = case_when(
      race %in% c("Asian", "Other") ~ "Other/Unknown",
      is.na(race) ~ "Other/Unknown",
      TRUE ~ race
    ),
    race = factor(race, levels = c("White", "Black", "Other/Unknown")),
    insurance = case_when(
      insurance %in% c("Medicaid only",
                       "Medicaid, with private") ~ "Medicaid",
      str_starts(insurance, "Medicare") ~ NA_character_,
      TRUE ~ insurance
    ),
    insurance = factor(insurance, levels = c("Private only", "Medicaid", "Other public", "Uninsured")),
    AGEDIAG = if_else(AGE53X >= 0, AGE53X, NA_real_),
    drug_classes = case_when(
      tolower(drug_names) == "amphetamines" ~ "Amphetamines",
      tolower(drug_names) == "methylphenidates" ~ "Methylphenidate",
      tolower(drug_names) == "lisdexamfetamine" ~ "Lisdexamfetamine",
      tolower(drug_names) == "amphetamines; lisdexamfetamine" ~ "Multiple stimulants",
      tolower(drug_names) == "amphetamines; lisdexamfetamine; methylphenidates" ~ "Multiple stimulants",
      tolower(drug_names) == "amphetamines; methylphenidates" ~ "Multiple stimulants",
      tolower(drug_names) == "lisdexamfetamine; methylphenidates" ~ "Multiple stimulants",
      TRUE ~ NA_character_
    )
  )

# remake design vars
design <- svydesign(
  id = ~VARPSU,
  strata = ~VARSTR,
  weights = ~PERWT,
  data = merged_df,
  nest = TRUE
)

preanalytic_design <- subset(design, subset = AGE53X < 65 &
                                           medicare == "No Medicare")

analytic_design <- subset(design, subset = AGE53X < 65 &
                                           medicare == "No Medicare" &
                                           adhd_dx == 1)

# # don't need to constantly rerun table 1 if not needed
# # table 1 labels
# table1_labels <- list(
#   AGE53X = "Age (years)",
#   age_group = "Age group",
#   sex = "Sex",
#   ethnicity = "Ethnicity",
#   race = "Race",
#   education = "Education",
#   has_insurance = "Has insurance",
#   insurance = "Insurance type"
# )

# # table1
# table1 <- tbl_svysummary(
#   analytic_design,
#   by = year,
#   include = c("AGE53X", "age_group", "sex", "ethnicity", "race", "education", "insurance"),
#   label = table1_labels,
#   statistic = list(
#     all_continuous() ~ "{mean} ({sd})", 
#     all_categorical() ~ "{n} ({p}%)"
#   ),
#   digits = all_continuous() ~ 2
# ) %>% 
#   bold_labels() %>% 
#   add_p() %>% 
#   as_gt()

# gtsave(table1, "exports/table1.html")
# gtsave(table1, "exports/table1.docx")