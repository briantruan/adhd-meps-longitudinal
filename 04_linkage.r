# 04_linkage.r
# goal: link the condfile to the ob and rx files using the link file

# -----------------------------------------------------------------------------
# Code credit: 
# https://github.com/HHS-AHRQ/MEPS/blob/master/R/workshop_exercises/cond_pmed_2020.R
# -----------------------------------------------------------------------------

options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

link_year <- function(link, cond, rx, ob, fyc, year) {
  rx <- rx %>% 
    rename(EVNTIDX = LINKIDX)

  adhd <- cond %>% 
    filter(str_starts(ICD10CDX, "F90"))

  adhd_clnk_distinct <- adhd %>%
    inner_join(link, by = c("DUPERSID", "CONDIDX", "PANEL")) %>%
    distinct(PANEL, DUPERSID, EVNTIDX, ICD10CDX, EVENTYPE)

  # rx-specific things
  rx_adhd_merged <- adhd_clnk_distinct %>%
    inner_join(rx, by = c("DUPERSID", "EVNTIDX", "PANEL"))

  rx_adhd_merged <- rx_adhd_merged %>% 
    filter(
      RXDRGNAM %in% c(
        "AMPHETAMINE-DEXTROAMPHETAMINE",
        "DEXMETHYLPHENIDATE",
        "LISDEXAMFETAMINE", "METHYLPHENIDATE") 
    ) %>% 
    mutate(
      stimulant_class = case_when(
        RXDRGNAM == "AMPHETAMINE-DEXTROAMPHETAMINE" ~ "AMPHETAMINES",
        RXDRGNAM == "DEXMETHYLPHENIDATE" ~ "METHYLPHENIDATES",
        RXDRGNAM == "LISDEXAMFETAMINE" ~ "LISDEXAMFETAMINE",
        RXDRGNAM == "METHYLPHENIDATE" ~ "METHYLPHENIDATES",
        TRUE ~ NA_character_
      )
    ) %>% 
    mutate(
      RXBEGMM = if_else(RXBEGMM < 1, NA_integer_, RXBEGMM),
      RXBEGYRX = if_else(RXBEGYRX < 1, NA_integer_, RXBEGYRX),
      RXDAYSUP = if_else(RXDAYSUP < 1 | RXDAYSUP == 999, NA_integer_, RXDAYSUP)
    )

  rx_person_lvl <- rx_adhd_merged %>%
    summarize(
      med_fills = n_distinct(RXRECIDX),
      med_days_supp = sum(RXDAYSUP, na.rm = TRUE),
      med_total_spend = sum(RXXP, na.rm = TRUE),
      med_oop = sum(RXSF, na.rm = TRUE),
      med_private = sum(RXPV, na.rm = TRUE),
      med_medicaid = sum(RXMD, na.rm = TRUE),
      n_drug_names = n_distinct(stimulant_class),
      drug_names = paste(sort(unique(na.omit(stimulant_class))), collapse = "; "),
      pill_qty_total = sum(RXQUANTY, na.rm = TRUE),
      pill_qty_mean = mean(RXQUANTY, na.rm = TRUE),
      .by = DUPERSID
    ) %>%
    mutate(
      med_days_supp = if_else(med_days_supp == 0, NA_real_, med_days_supp),
      pmed_flag = 1,
      oop_share = if_else(med_total_spend > 0, med_oop / med_total_spend * 100, NA_real_)
    )

  # ob-specific things
  ob_adhd_merged <- adhd_clnk_distinct %>%
    inner_join(ob, by = c("DUPERSID", "EVNTIDX", "PANEL"))

  ob_adhd_merged <- ob_adhd_merged %>% 
    mutate(
      OBMD = if_else(OBMD < 0, NA_real_, OBMD),
      OBMR = if_else(OBMR < 0, NA_real_, OBMR),
      OBPV = if_else(OBPV < 0, NA_real_, OBPV),
      OBSF = if_else(OBSF < 0, NA_real_, OBSF),
      OBOF = if_else(OBOF < 0, NA_real_, OBOF),
      OBSL = if_else(OBSL < 0, NA_real_, OBSL),
      OBWC = if_else(OBWC < 0, NA_real_, OBWC),
      OBOT = if_else(OBOT < 0, NA_real_, OBOT),
      OBXP = if_else(OBXP < 0, NA_real_, OBXP),
      OBTC = if_else(OBTC < 0, NA_real_, OBTC)
    ) %>% 
    # need to do quite a bit of filtering because obvisits are pretty braod
    # even if condition linked, many visits are not really for ADHD
    # can checks: VSTRELCN_M18 (2017: VSTRELCN) is "YES"
    # first rename 2017 to match 2018-2023 convention
    { if (year == 2017) rename(., VSTRELCN_M18 = VSTRELCN) else . } %>%
    filter(VSTRELCN_M18 == "YES")
  
  ob_person_lvl <- ob_adhd_merged %>%
    summarize(
      ob_visits = n_distinct(EVNTIDX),
      # TELEHEALTHFLAG only become available with datasets 2020 and later
      # check the year first, then count number of telehealth visits
      # i think we can just count the number of flags
      ob_telehealth_visits = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                                  sum(TELEHEALTHFLAG == "YES", na.rm = TRUE) 
                              else NA_integer_,
      # let's also count number of visits that are
      # VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING"
      ob_mental_health_visits = sum(VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", na.rm = TRUE),

      # see if there wre any telehealth+mental health visits
      ob_telehealth_mental_visits = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                                      sum(TELEHEALTHFLAG == "YES" & VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", na.rm = TRUE) 
                                    else NA_integer_,

      # these are global sums
      ob_medicaid = sum(OBMD, na.rm = TRUE),
      ob_medicare = sum(OBMR, na.rm = TRUE),
      ob_private = sum(OBPV, na.rm = TRUE),
      ob_selfpay = sum(OBSF, na.rm = TRUE),
      ob_totalexp = sum(OBXP, na.rm = TRUE),

      # let's get telehealth-specific sums for medicaid, medicare, private, selfpay, totalexp
      ob_medicaid_tele = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                              sum(if_else(TELEHEALTHFLAG == "YES", OBMD, 0), na.rm = TRUE) 
                          else NA_real_,
      ob_medicare_tele = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                              sum(if_else(TELEHEALTHFLAG == "YES", OBMR, 0), na.rm = TRUE) 
                          else NA_real_,
      ob_private_tele = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                              sum(if_else(TELEHEALTHFLAG == "YES", OBPV, 0), na.rm = TRUE) 
                          else NA_real_,
      ob_selfpay_tele = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                              sum(if_else(TELEHEALTHFLAG == "YES", OBSF, 0), na.rm = TRUE) 
                          else NA_real_,
      ob_totalexp_tele = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                              sum(if_else(TELEHEALTHFLAG == "YES", OBXP, 0), na.rm = TRUE) 
                          else NA_real_,

      # let's get mental health-specific sums for medicaid, medicare, private, selfpay, totalexp
      ob_medicaid_mental = sum(if_else(VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBMD, 0), na.rm = TRUE),
      ob_medicare_mental = sum(if_else(VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBMR, 0), na.rm = TRUE),
      ob_private_mental = sum(if_else(VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBPV, 0), na.rm = TRUE),
      ob_selfpay_mental = sum(if_else(VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBSF, 0), na.rm = TRUE),
      ob_totalexp_mental = sum(if_else(VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBXP, 0), na.rm = TRUE),

      # get telehealth+mental health specific sums for medicaid, medicare, private, selfpay, totalexp
      ob_medicaid_tele_mental = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                                  sum(if_else(TELEHEALTHFLAG == "YES" & VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBMD, 0), na.rm = TRUE) 
                                else NA_real_,
      ob_medicare_tele_mental = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                                  sum(if_else(TELEHEALTHFLAG == "YES" & VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBMR, 0), na.rm = TRUE) 
                                else NA_real_,
      ob_private_tele_mental = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                                  sum(if_else(TELEHEALTHFLAG == "YES" & VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBPV, 0), na.rm = TRUE) 
                                else NA_real_,
      ob_selfpay_tele_mental = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                                  sum(if_else(TELEHEALTHFLAG == "YES" & VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBSF, 0), na.rm = TRUE) 
                                else NA_real_,
      ob_totalexp_tele_mental = if ("TELEHEALTHFLAG" %in% names(ob_adhd_merged)) 
                                  sum(if_else(TELEHEALTHFLAG == "YES" & VSTCTGRY == "PSYCHOTHERAPY/MENTAL HEALTH COUNSELING", OBXP, 0), na.rm = TRUE) 
                                else NA_real_,

      .by = DUPERSID
    ) %>% 
    mutate(
      ob_flag = if_else(ob_visits > 0, 1, 0),
      ob_total_oop_share = if_else(ob_totalexp > 0, ob_selfpay / ob_totalexp * 100, NA_real_),
      ob_tele_oop_share = if_else(ob_totalexp_tele > 0, ob_selfpay_tele / ob_totalexp_tele * 100, NA_real_),
      ob_mental_oop_share = if_else(ob_totalexp_mental > 0, ob_selfpay_mental / ob_totalexp_mental * 100, NA_real_),
      ob_tele_mental_oop_share = if_else(ob_totalexp_tele_mental > 0, ob_selfpay_tele_mental / ob_totalexp_tele_mental * 100, NA_real_)
    )
  
  fyc_merged <- fyc %>%
    full_join(rx_person_lvl, by = "DUPERSID") %>%
    replace_na(list(pmed_flag = 0)) %>% 
    full_join(ob_person_lvl, by = "DUPERSID") %>%
    replace_na(list(ob_flag = 0))
  
  assign(paste0("merged_", year), fyc_merged, envir = .GlobalEnv)
}

for (yr in years) {
  link_year(get(paste0("link_", yr)), get(paste0("cond_", yr)), get(paste0("rx_", yr)), get(paste0("ob_", yr)), get(paste0("fyc_", yr)), yr)
}

# This code builds year-specific ADHD linkage files and attaches them to each
# corresponding merged dataset without collapsing records to one row per person.
#
# For each year in `years`:
# - Construct the matching condition dataset name (`cond_<year>`) and merged
#   dataset name (`merged_<year>`).
# - Extract only records with ICD-10 diagnosis codes beginning with "F90"
#   (ADHD-related diagnoses).
# - Keep only `DUPERSID`, `AGEDIAG`, and `ICD10CDX`, then save that subset as
#   `adhd_<year>` in the global environment.
# - Left-join the ADHD subset onto `merged_<year>` by `DUPERSID`, preserving
#   all rows in the merged data and adding diagnosis information where present.
#
# The second loop appears intended for a later filtering step to count
# exclusions, but as written it simply reassigns `merged_df` back into each
# `merged_<year>` object without making any changes. If the commented-out
# filter is enabled, it would restrict the data to rows with non-missing ADHD
# ICD-10 codes starting with "F90".
# create per-condition adhd dataframes and join onto merged_* without collapsing by person
for (yr in years) {
  cond_name <- paste0("cond_", yr)
  adhd_name <- paste0("adhd_", yr)
  adhd_df <- get(cond_name) %>% 
    filter(str_starts(ICD10CDX, "F90")) %>% 
    select(DUPERSID, AGEDIAG, ICD10CDX)

  # adhd_df has duplicated DUPERSID. we only want to keep one instance
  # some also have AGEDIAG == -1
  #
  # to be as robust as possible, keep only the DUPERSID where AGEDIAG >= 0, 
  # and if there are multiple with AGEDIAG < 0, then just keep the first one (they don't have a meaningful order anyway)
  adhd_df <- adhd_df %>% 
    arrange(DUPERSID, desc(AGEDIAG)) %>% 
    distinct(DUPERSID, .keep_all = TRUE)

  assign(adhd_name, adhd_df, envir = .GlobalEnv)

  merged_name <- paste0("merged_", yr)
  merged_df <- get(merged_name) %>% 
    left_join(adhd_df, by = "DUPERSID")

  assign(merged_name, merged_df, envir = .GlobalEnv)
}