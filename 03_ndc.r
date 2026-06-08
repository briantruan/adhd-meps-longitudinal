# 03_ndc.r
# goal: process NDC data and link to RX data

# we will do the linkage first and see if that is a better way of sorting out drugs that are specifically for ADHD diagnosis.
# this works! don't need this file anymore





# DO NOT USE
# DO NOT USE
# DO NOT USE







# so it turns out that using NDC files are a lot trickier than originally planned because they're 
# inconsistently coded. some have a 5-4-2 format, some have a 5-3-2 format, and some have a 4-4-2 
# format. we will revisit this later. let's just do the best we can with the RXDRGNAM variable

# for (yr in years) {
#   rx_name <- paste0("rx_", yr)
#   rx_df <- get(rx_name, envir = .GlobalEnv)
#   res <- rx_df %>%
#     mutate(
#       RXBEGMM = as.numeric(as.character(RXBEGMM)),
#       RXBEGYRX = as.numeric(as.character(RXBEGYRX)),
#       RXDAYSUP = as.numeric(as.character(RXDAYSUP))
#     ) %>% 
#     mutate(
#       RXBEGMM = if_else(RXBEGMM < 0, NA_real_, RXBEGMM),
#       RXBEGYRX = if_else(RXBEGYRX < 0, NA_real_, RXBEGYRX),
#       RXDAYSUP = if_else(RXDAYSUP < 1 | RXDAYSUP == 999, NA_real_, RXDAYSUP)
#     ) %>% 
#     mutate(across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))
#   assign(rx_name, res, envir = .GlobalEnv)
# }

# # for (yr in years) {
# #   rx_name <- paste0("rx_", yr)
# #   rx_df <- get(rx_name, envir = .GlobalEnv)
# #   res <- rx_df %>%
# #     # this also isn't as effective of a method
# #     filter(TC1 == "CENTRAL NERVOUS SYSTEM AGENTS")
# #   assign(rx_name, res, envir = .GlobalEnv)
# # }

# # --- ignore ---

# # # load NDC files once and build stimulant lookup
# # files <- list.files("data/ndc", pattern = "\\.csv$", full.names = TRUE)

# # ndc_all <- files %>%
# #   set_names(basename(.)) %>%
# #   map_dfr(
# #     ~ read_csv(.x, col_types = cols(.default = col_character())),
# #     .id = "source_file"
# #   ) %>%
# #   distinct() %>%
# #   mutate(
# #     formulation = if_else(str_detect(toupper(`Dosage Form`), "EXTENDED"), "Extended", "Normal")
# #   )

# # ndc_all <- ndc_all %>%
# #   select(source_file, `Proprietary Name`, `NDC Package Code`, `Substance Name`, `Pharm Class`, formulation) %>%
# #   distinct() %>%
# #   mutate(
# #     ndc_compatible = str_remove_all(`NDC Package Code`, "-"),
# #     ndc_compatible = str_pad(ndc_compatible, width = 11, side = "left", pad = "0")
# #   )

# # # like the fyc, 2017-1028 are missing haven labels, 
# # # so we can use copy_labels()
# # rx_2017 <- copy_labels(from = rx_2019, to = rx_2017)
# # rx_2018 <- copy_labels(from = rx_2019, to = rx_2018)

# # for (yr in years) {
# #   rx_name <- paste0("rx_", yr)
# #   rx_df <- get(rx_name, envir = .GlobalEnv)
# #   res <- rx_df %>%
# #     mutate(RXNDC = as.character(RXNDC)) %>%
# #     left_join(
# #       ndc_all %>% select(source_file, `Proprietary Name`, `NDC Package Code`, 
# #                         `Substance Name`, `Pharm Class`, formulation, ndc_compatible),
# #       by = c("RXNDC" = "ndc_compatible"),
# #       relationship = "many-to-many"
# #     ) %>% 
# #     mutate(
# #       RXBEGMM = as.numeric(as.character(RXBEGMM)),
# #       RXBEGYRX = as.numeric(as.character(RXBEGYRX)),
# #       RXDAYSUP = as.numeric(as.character(RXDAYSUP))
# #     ) %>% 
# #     mutate(
# #       RXBEGMM = if_else(RXBEGMM < 0, NA_real_, RXBEGMM),
# #       RXBEGYRX = if_else(RXBEGYRX < 0, NA_real_, RXBEGYRX),
# #       RXDAYSUP = if_else(RXDAYSUP < 1 | RXDAYSUP == 999, NA_real_, RXDAYSUP)
# #     ) %>% 
# #     mutate(across(where(~ haven::is.labelled(.x) || is.factor(.x)), clean_labels))
# #   assign(rx_name, res, envir = .GlobalEnv)
# # }

# rm(ndc_all, res, rx_df, files, rx_name, yr)

# # check to see if LISDEXAMFETAMINE has any records in RXDRGNAM for rx_2017
# rx_2017 %>% 
#   filter(str_detect(toupper(RXDRGNAM), "LISDEXAMFETAMINE")) %>% 
#   select(source_file, RXDRGNAM, RXNDC, `NDC Package Code`)


# for (yr in years) {
#   rx_name <- paste0("rx_", yr)
#   rx_df <- get(rx_name, envir = .GlobalEnv)
#   res <- rx_df %>%
#     # we only care about the ADHD rx, so let's drop NAs
#     filter(!is.na(`source_file`)) # %>% 
#     # Combine DEXTROAMPHETAMINE and AMPHETAMINE-DEXTROAMPHETAMINE into "AMPHETAMINES"
#     # for col RXDRGNAM
#     # mutate(
#     #   RXDRGNAM = case_when(
#     #     str_detect(toupper(RXDRGNAM), "DEXTROAMPHETAMINE") ~ "AMPHETAMINES",
#     #     str_detect(toupper(RXDRGNAM), "AMPHETAMINE-DEXTROAMPHETAMINE") ~ "AMPHETAMINES",
#     #     str_detect(toupper(RXDRGNAM), "DEXMETHYLPHENIDATE") ~ "METHYLPHENIDATES",
#     #     str_detect(toupper(RXDRGNAM), "METHYLPHENIDATE") ~ "METHYLPHENIDATES",
#     #     str_detect(toupper(RXDRGNAM), "ATOMOXETINE") ~ "NON-STIMULANTS",
#     #     str_detect(toupper(RXDRGNAM), "BUPROPION") ~ "NON-STIMULANTS",
#     #     str_detect(toupper(RXDRGNAM), "CLONIDINE") ~ "NON-STIMULANTS",
#     #     str_detect(toupper(RXDRGNAM), "GUANFACINE") ~ "NON-STIMULANTS",
#     #     TRUE ~ as.character(RXDRGNAM)
#     #   )
#     # )
#   assign(rx_name, res, envir = .GlobalEnv)
# }

# rm(res, rx_df, rx_name, yr)

# # eda: weighted counts of each rxdrgnam by year
# rx_weighted_counts <- map_dfr(years, function(yr) {
#   rx_name <- paste0("rx_", yr)
#   rx_df <- get(rx_name, envir = .GlobalEnv)

#   wt_var <- paste0("PERWT", substr(as.character(yr), 3, 4), "F")

#   rx_df <- rx_df %>% 
#     mutate(
#       RXDRGNAM = as.factor(RXDRGNAM),
#       wt = as.numeric(.data[[wt_var]])
#     ) %>% 
#     filter(!is.na(RXDRGNAM), !is.na(wt))

#   des <- svydesign(
#     ids = ~1,
#     weights = ~wt,
#     data = rx_df
#   )

#   as.data.frame(svytable(~RXDRGNAM, des)) %>% 
#     rename(weighted_n = Freq) %>% 
#     mutate(year = yr, .before = 1)
# })

# rx_weighted_counts

# # graph
# ggplot(rx_weighted_counts, aes(x = year, y = weighted_n, color = RXDRGNAM)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "Weighted counts of ADHD medications by year",
#     x = "Year",
#     y = "Weighted count",
#     color = "Medication Name"
#   ) +
#   theme_minimal()

# # TODO
# # need to figure out what lisdexamfetamine has no record