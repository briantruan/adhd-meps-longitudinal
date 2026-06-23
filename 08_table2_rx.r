# 08_table2_rx.r
# goal: produce a table 2, a summary of rx findings

# design_var: analytic_design
# can skip if not needed

# table 2 labels
table2_labels <- list(
  AGEDIAG = "Age at diagnosis (years)",
  comorbid_dx = "Has any comorbid diagnosis",
  n_drug_names = "Number of unique stimulant classes",
  med_days_supp = "Number of stimulant medication days supplied",
  med_total_spend = "Total expenditure on stimulant medications (2023 USD)",
  med_oop = "Out-of-pocket expenditure (2023 USD)",
  oop_share = "Out-of-pocket share of total expenditure (%)"
)

# table2
table2 <- tbl_svysummary(
  analytic_design,
  by = year,
  include = c("AGEDIAG", "comorbid_dx", "n_drug_names", "med_days_supp", 
              "med_total_spend", "med_oop", "oop_share"),
  label = table2_labels,
  statistic = list(
    all_continuous() ~ "{mean} ({sd})", 
    all_categorical() ~ "{n} ({p}%)"
  ),
  digits = all_continuous() ~ 2,
  missing = "no"
) %>% 
  bold_labels() %>% 
  add_p() %>% 
  as_gt()

gtsave(table2, "exports/table2.html")
gtsave(table2, "exports/table2.docx")

# table 2a will list counts of specific stimulant classes

table2a <- tbl_svysummary(
  analytic_design,
  by = year,
  include = "drug_classes",
  label = list(drug_classes = "Stimulant medication class"),
  statistic = list(all_categorical() ~ "{n} ({p}%)"),
  missing = "no"
) %>% 
  bold_labels() %>% 
  add_p() %>% 
  as_gt()

gtsave(table2a, "exports/table2a.html")
gtsave(table2a, "exports/table2a.docx")

# graph table2a
drug_class_plot_data <- svytable(~drug_classes + year, analytic_design) %>% 
  prop.table(margin = 2) %>% 
  as.data.frame() %>% 
  rename(proportion = Freq) %>% 
  mutate(proportion = proportion * 100)

stimulant_classes_plot <- ggplot(drug_class_plot_data, aes(x = year, 
  y = proportion, color = drug_classes, group = drug_classes)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Percentage of ADHD-diagnosed individuals", 
       color = "Stimulant class") +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal()

ggsave("exports/stimulant_classes_plot.png", plot = stimulant_classes_plot, 
                                             width = 9, height = 5)
ggsave("exports/stimulant_classes_plot.svg", plot = stimulant_classes_plot, 
                                             width = 9, height = 5)
