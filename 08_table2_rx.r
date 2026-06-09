# 08_table2_rx.r
# goal: produce a table 2, a summary of rx findings

# design_var: analytic_design
# can skip if not needed

# # table 2 labels
# table2_labels <- list(
#   AGEDIAG = "Age at diagnosis (years)",
#   n_drug_names = "Number of unique stimulant classes",
#   med_fills = "Number of stimulant medication fills",
#   med_days_supp = "Number of stimulant medication days supplied",
#   pill_qty_mean = "Mean quantity per stimulant medication fill",
#   med_total_spend = "Total expenditure on stimulant medications (2023 USD)",
#   med_private = "Expenditure by private insurance (2023 USD)",
#   med_medicaid = "Expenditure by Medicaid (2023 USD)",
#   med_oop = "Out-of-pocket expenditure (2023 USD)",
#   oop_share = "Out-of-pocket share of total expenditure (%)"
# )

# # table2
# table2 <- tbl_svysummary(
#   analytic_design,
#   by = year,
#   include = c("AGEDIAG", "n_drug_names", "med_fills", "med_days_supp", "pill_qty_mean",
#               "med_total_spend", "med_private", "med_medicaid", "med_oop", "oop_share"),
#   label = table2_labels,
#   statistic = list(
#     all_continuous() ~ "{mean} ({sd})", 
#     all_categorical() ~ "{n} ({p}%)"
#   ),
#   digits = all_continuous() ~ 2,
#   missing = "no"
# ) %>% 
#   bold_labels() %>% 
#   add_p() %>% 
#   as_gt()

# gtsave(table2, "exports/table2.html")
# gtsave(table2, "exports/table2.docx")
