# 02_clean.r
# goal: clean data

# HAVEN LABEL CORRECTION
cond_2018 <- copy_labels(from = cond_2020, to = cond_2018)
cond_2019 <- copy_labels(from = cond_2020, to = cond_2019)
fyc_2017 <- copy_labels(from = fyc_2018, to = fyc_2017)
link_2017 <- copy_labels(from = link_2019, to = link_2017)
link_2018 <- copy_labels(from = link_2019, to = link_2018)
ob_2017 <- copy_labels(from = ob_2019, to = ob_2017)
ob_2018 <- copy_labels(from = ob_2019, to = ob_2018)
rx_2017 <- copy_labels(from = rx_2019, to = rx_2017)
rx_2018 <- copy_labels(from = rx_2019, to = rx_2018)

# in some years, some things are inappropriately haven labelled
# convert haven labels to numeric
inappropriate_labels <- c("VPCS42", "VMCS42", "OBTOTV", "OPTOTV", 
                          "IPDIS", "IPNGTD", "ERTOT", "AGEDIAG", 
                          "RXBEGMM", "RXBEGYRX", "RXDAYSUP",
                          "OBDATEYR", "OBDATEMM", 
                          "PANEL", "EDUCYR")

# convert haven labels to factors in all dfs
# remove preceding numbers from factor levels
clean_labels <- function(x) {
  if (haven::is.labelled(x)) x <- haven::as_factor(x)
  if (is.factor(x)) levels(x) <- gsub("^[0-9]+\\s*", "", levels(x))
  x
}

for (year in years) {
  cond <- get(paste0("cond_", year), envir = .GlobalEnv)
  fyc  <- get(paste0("fyc_", year), envir = .GlobalEnv)
  link <- get(paste0("link_", year), envir = .GlobalEnv)
  ob   <- get(paste0("ob_", year), envir = .GlobalEnv)
  rx   <- get(paste0("rx_", year), envir = .GlobalEnv)

  res_cond <- cond %>%
    mutate(across(any_of(inappropriate_labels), ~ as.numeric(as.character(.x)))) %>% 
    mutate(across(everything(), clean_labels))
  res_fyc <- fyc %>% 
    mutate(across(any_of(inappropriate_labels), ~ as.numeric(as.character(.x)))) %>% 
    mutate(across(everything(), clean_labels))  
  res_link <- link %>%
    mutate(across(any_of(inappropriate_labels), ~ as.numeric(as.character(.x)))) %>% 
    mutate(across(everything(), clean_labels))  
  res_ob <- ob %>%
    mutate(across(any_of(inappropriate_labels), ~ as.numeric(as.character(.x)))) %>% 
    mutate(across(everything(), clean_labels)) %>% 
    # there was a major switch from 2017 to 2018 so haven labels are not correct
    # manual recoding required
    {
      if (year == 2017) {
        mutate(
          .,
          DRSPLTY = case_when(
            DRSPLTY == 6 ~ "FAMILY PRACTICE",
            DRSPLTY == 8 ~ "GENERAL PRACTICE",
            DRSPLTY == 14 ~ "INTERNAL MEDICINE",
            DRSPLTY == 21 ~ "OSTEOPATHY",
            DRSPLTY == 24 ~ "PEDIATRICIAN",
            DRSPLTY == 28 ~ "PSYCHIATRY",
            DRSPLTY == 91 ~ "OTHER DR SPECIALTY",
            TRUE ~ NA_character_
          ),
          VSTRELCN = case_when(
            VSTRELCN == 1 ~ "YES",
            VSTRELCN == 2 ~ "NO",
            TRUE ~ NA_character_
          )
        )
      } else {
        .
      }
    }
  res_rx <- rx %>%
    mutate(across(any_of(inappropriate_labels), ~ as.numeric(as.character(.x)))) %>% 
    mutate(across(everything(), clean_labels))  

  assign(paste0("cond_", year), res_cond, envir = .GlobalEnv)
  assign(paste0("fyc_", year),  res_fyc, envir = .GlobalEnv)
  assign(paste0("link_", year), res_link, envir = .GlobalEnv)
  assign(paste0("ob_", year),   res_ob, envir = .GlobalEnv)
  assign(paste0("rx_", year),   res_rx, envir = .GlobalEnv)
}

### remove YYX suffix ###

rx_year_specific_vars <- c("RXSF", "RXMR", "RXMD", "RXPV", "RXVA", "RXTR", 
                           "RXOF", "RXSL", "RXWC", "RXOT", "RXOR", "RXOU", "RXXP")

ob_year_specific_vars <- c("OBMD", "OBMR", "OBOF", "OBOR", "OBOT", "OBOU", "OBPV", 
                           "OBSF", "OBSL", "OBTC", "OBTR", "OBVA", "OBWC", "OBXP")

# remove the YYX suffix from each of the year-specific variables in the rx and ob 
# datasets
for (yr in 2017:2023) {
  rx_vars <- paste0(rx_year_specific_vars, substr(yr, 3, 4), "X")
  ob_vars <- paste0(ob_year_specific_vars, substr(yr, 3, 4), "X")
  rx_df <- get(paste0("rx_", yr))
  ob_df <- get(paste0("ob_", yr))
  
  # Rename only variables that exist in rx_df
  rx_vars_exist <- rx_vars[rx_vars %in% names(rx_df)]
  if (length(rx_vars_exist) > 0) {
    rx_df <- rx_df %>%
      rename_with(~ gsub(paste0(substr(yr, 3, 4), "X$"), "", .x), 
                                all_of(rx_vars_exist))
  }
  
  # Rename only variables that exist in ob_df
  ob_vars_exist <- ob_vars[ob_vars %in% names(ob_df)]
  if (length(ob_vars_exist) > 0) {
    ob_df <- ob_df %>%
      rename_with(~ gsub(paste0(substr(yr, 3, 4), "X$"), "", .x), 
                                all_of(ob_vars_exist))
  }
  
  assign(paste0("rx_", yr), rx_df)
  assign(paste0("ob_", yr), ob_df)
}

### CPI conversions ###

# dollars: convert to all dollars to 2023 using CPI
# December values
cpi_2017 <- 246.524
cpi_2018 <- 251.233
cpi_2019 <- 256.974
cpi_2020 <- 260.474
cpi_2021 <- 278.802
cpi_2022 <- 296.797
cpi_2023 <- 306.746

cpi_ratios <- c(
  cpi_2023 / cpi_2017,
  cpi_2023 / cpi_2018,
  cpi_2023 / cpi_2019,
  cpi_2023 / cpi_2020,
  cpi_2023 / cpi_2021,
  cpi_2023 / cpi_2022,
  1
)

monetary_vars_fyc <- c("TTLP", "TOTSLF")

for (i in seq_along(years)) {
  yr <- years[i]
  df <- get(paste0("fyc_", yr))
  
  df <- df %>%
    mutate(across(any_of(monetary_vars_fyc), ~ .x * cpi_ratios[i]))
  
  assign(paste0("fyc_", yr), df)
}

for (i in seq_along(years)) {
  yr <- years[i]
  df <- get(paste0("rx_", yr))

  df <- df %>%
    mutate(across(any_of(rx_year_specific_vars), ~ .x * cpi_ratios[i]))

  assign(paste0("rx_", yr), df)
}

for (i in seq_along(years)) {
  yr <- years[i]
  df <- get(paste0("ob_", yr))

  df <- df %>%
    mutate(across(any_of(ob_year_specific_vars), ~ .x * cpi_ratios[i]))

  assign(paste0("ob_", yr), df)
}

# INSURC is also year specific in the format INSURCYY, so remove YY suffix
for (year in 2017:2023) {
  obj <- paste0("fyc_", year)

  if (exists(obj)) {
    df <- get(obj)
    insurc_var <- paste0("INSURC", substr(year, 3, 4))

    if (insurc_var %in% names(df)) {
      df <- df |>
        rename(INSURC = !!rlang::sym(insurc_var))
    }

    assign(obj, df)
  }
}

recode_fyc <- function(df) {
  df %>% 
    mutate(
      sex = case_when(
        SEX == "MALE" ~ "Male",
        SEX == "FEMALE" ~ "Female",
        TRUE ~ NA_character_
      ),
      ethnicity = case_when(
        RACETHX == "HISPANIC" ~ "Hispanic",
        TRUE ~ "Non-Hispanic"
      ),
      race = case_when(
        RACETHX == "NON-HISPANIC ASIAN ONLY" ~ "Asian",
        RACETHX == "NON-HISPANIC BLACK ONLY" ~ "Black",
        RACETHX == "NON-HISPANIC OTHER RACE OR MULTIPLE RACE" ~ "Other",
        RACETHX == "NON-HISPANIC WHITE ONLY" ~ "White",
        TRUE ~ NA_character_
      ),
      race = factor(race, levels = c("White", "Black", "Asian", "Other")),
      marital = case_when(
        MARRY53X %in% c("-1 INAPPLICABLE", "-7 REFUSED", "-8 DK") ~ NA_character_,
        MARRY53X %in% c("MARRIED", "MARRIED IN ROUND") ~ "Married",
        TRUE ~ "Not married"
      ),
      REGION53 = na_if(as.character(REGION53), "-1 INAPPLICABLE"),
      education = case_when(
        EDUCYR < 0 ~ NA_character_,
        EDUCYR >= 0 & EDUCYR <= 12 ~ "High school graduation or less",
        EDUCYR >= 13 ~ "College education or greater",
        TRUE ~ NA_character_
      ),
      insurance = case_when(
        INSURC %in% c("<65 UNINSURED", "65+ UNINSURED") ~ "Uninsured",
        INSURC == "<65 ANY PRIVATE" & MCAID53X != "YES" ~ "Private only",
        INSURC == "<65 ANY PRIVATE" & MCAID53X == "YES" ~ "Medicaid, with private",
        INSURC == "<65 PUBLIC ONLY" & MCAID53X == "YES" ~ "Medicaid only",
        INSURC == "<65 PUBLIC ONLY" & MCAID53X != "YES" ~ "Other public",
        INSURC == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X != "YES" ~ "Medicare, with private",
        INSURC == "65+ EDITED MEDICARE AND PRIVATE" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE ONLY" & MCAID53X != "YES" ~ "Medicare only",
        INSURC == "65+ EDITED MEDICARE ONLY" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X == "YES" ~ "Medicare, dual-eligible",
        INSURC == "65+ EDITED MEDICARE AND OTH PUB ONLY" & MCAID53X != "YES" ~ "Medicare, with other public",
        INSURC == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X == "YES" ~ "Medicaid, with private",
        INSURC == "65+ NO MEDICARE AND ANY PUBLIC/PRIVATE" & MCAID53X != "YES" ~ "Private only",
        INSCOV == "UNINSURED" ~ "Uninsured",
        INSCOV == "ANY PRIVATE" ~ "Private only",
        INSCOV == "PUBLIC ONLY" & MCAID53X == "YES" ~ "Medicaid only",
        INSCOV == "PUBLIC ONLY" & MCAID53X != "YES" ~ "Other public",
        TRUE ~ NA_character_
      ),
      insurance = factor(insurance, levels = c(
        "Private only",
        "Medicaid only",
        "Medicaid, with private",
        "Medicare only",
        "Medicare, with private",
        "Medicare, with other public",
        "Medicare, dual-eligible",
        "Other public",
        "Uninsured"
      )),
      medicaid = case_when(
        insurance == "Medicaid only" ~ "Medicaid only",
        insurance == "Medicaid, with private" ~ "Medicaid, with private, non-Medicare",
        TRUE ~ "No Medicaid"
      ),
      medicare = case_when(
        insurance %in% c("Medicare, with private", "Medicare, dual-eligible", "Medicare, with other public", "Medicare only") ~ "Medicare, any",
        TRUE ~ "No Medicare"
      ),
      private_ins = case_when(
        insurance == "Private only" ~ "Private only",
        TRUE ~ "Other/uninsured"
      ),
      has_insurance = case_when(
        INSCOV == "UNINSURED" ~ "No insurance",
        INSCOV %in% c("ANY PRIVATE", "PUBLIC ONLY") ~ "Has insurance",
        TRUE ~ NA_character_
      ),
      povcat = case_when(
        POVCAT %in% c("POOR/NEGATIVE", "NEAR POOR") ~ "Very low income",
        POVCAT == "LOW INCOME" ~ "Low income",
        POVCAT == "MIDDLE INCOME" ~ "Middle income",
        POVCAT == "HIGH INCOME" ~ "High income",
        TRUE ~ NA_character_
      ),
      povcat = factor(povcat, levels = c(
        "Very low income", 
        "Low income", 
        "Middle income", 
        "High income"
      )),
      region = case_when(
        REGION53 == "NORTHEAST" ~ "Northeast",
        REGION53 == "MIDWEST" ~ "Midwest",
        REGION53 == "SOUTH" ~ "South",
        REGION53 == "WEST" ~ "West",
        TRUE ~ NA_character_
      )
    )
}

for (year in 2017:2023) {
  obj <- paste0("fyc_", year)

  if (exists(obj, envir = .GlobalEnv)) {
    assign(obj, recode_fyc(get(obj, envir = .GlobalEnv)), envir = .GlobalEnv)
  }
}

cond_keep <- c("DUID", "PID", "DUPERSID", "CONDIDX", "PANEL",
               "AGEDIAG", "ICD10CDX",
               "HHNUM", "IPNUM", "OPNUM", "OBNUM", "ERNUM", "RXNUM",
               "HHCOND", "IPCOND", "OPCOND", "OBCOND", "ERNUM", "RXCOND")

for (year in 2017:2023) {
  obj <- paste0("cond_", year)
  if (exists(obj, envir = .GlobalEnv)) {
    df <- get(obj, envir = .GlobalEnv)
    df <- df %>% select(any_of(cond_keep))
    assign(obj, df, envir = .GlobalEnv)
  }
}

rx_drop <- "PHARTP" # drop if starts with

for (year in 2017:2023) {
  obj <- paste0("rx_", year)
  if (exists(obj, envir = .GlobalEnv)) {
    df <- get(obj, envir = .GlobalEnv)
    df <- df %>% select(-starts_with(rx_drop))
    assign(obj, df, envir = .GlobalEnv)
  }
}

# clean up mess
rm(df, ob_df, rx_df, i, inappropriate_labels, insurc_var, monetary_vars_fyc, ob_year_specific_vars, 
   rx_year_specific_vars, yr, cpi_2017, cpi_2018, cpi_2019, cpi_2020, cpi_2021, 
   cpi_2022, cpi_2023, ob_vars, ob_vars_exist, rx_vars, rx_vars_exist, cond, fyc, link, ob, rx,
   res_cond, res_fyc, res_link, res_ob, res_rx, obj, year, rx_drop, clean_labels, load_meps_year,
   recode_fyc, save_all_years, cond_keep, cpi_ratios)
