# # Only need to run these once (uncomment if needed)
# install.packages("foreign")
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("readxl")
# install.packages("haven")
# install.packages("labelled")
# install.packages("dplyr")
# install.packages("gtsummary")
# isntall.packages("gt")
# install.packages("stringr")
# install.packages("survey")

# # Run these every time you restart R:
library(foreign)
library(devtools)
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(labelled)
library(dplyr)
library(gtsummary)
library(gt)
library(stringr)
library(survey)

# only run once if needed
# install_github("sshrestha274/meps_r_pkg/MEPS")

library(MEPS)

# Load MEPS data for a single year and return a named list: fyc, cond, rx

load_meps_year <- function(year) {
  short_year <- substr(year, 3, 4)
  
  perwt  <- paste0("PERWT",  short_year, "F")
  ttlp   <- paste0("TTLP",   short_year, "X")
  povcat <- paste0("POVCAT", short_year)
  inscov <- paste0("INSCOV", short_year)
  insurc <- paste0("INSURC", short_year)
  totslf <- paste0("TOTSLF", short_year)
  obtotv <- paste0("OBTOTV", short_year)
  optotv <- paste0("OPTOTV", short_year)
  ipdis  <- paste0("IPDIS",  short_year)
  ipngtd <- paste0("IPNGTD", short_year)
  rxtot  <- paste0("RXTOT",  short_year)
  ertot  <- paste0("ERTOT",  short_year)
  
  vars_to_keep <- c(
    "DUPERSID", "PANEL", "VARSTR", "VARPSU", perwt, ttlp,
    "SEX", "AGE53X", "RACETHX", "MARRY53X", "REGION53",
    "EDUYRDG", "EDUCYR", povcat,
    "PCS42", "MCS42", "VPCS42", "VMCS42",
    "HAVEUS42", inscov, insurc, "MCAID53X",
    totslf, obtotv, optotv, ipdis, ipngtd, rxtot, ertot,
    "ADHDADDX"
  )
  
  fyc <- read_MEPS(year = year, type = "FYC") %>%
    select(any_of(vars_to_keep)) %>%
    rename(
      PERWT  = all_of(perwt),
      TTLP   = all_of(ttlp),
      POVCAT = all_of(povcat),
      INSCOV = all_of(inscov),
      TOTSLF = all_of(totslf),
      OBTOTV = all_of(obtotv),
      OPTOTV = all_of(optotv),
      IPDIS  = all_of(ipdis),
      IPNGTD = all_of(ipngtd),
      RXTOT  = all_of(rxtot),
      ERTOT  = all_of(ertot)
    )
  
  cond <- read_MEPS(year = year, type = "COND")
  
  rx <- read_MEPS(year = year, type = "RX")

  ob <- read_MEPS(year = year, type = "OB")
  
  link <- read_MEPS(year = year, type = "CLNK")
  
  list(fyc = fyc, cond = cond, rx = rx, ob = ob, link = link)

}

save_all_years <- function(years = 2017:2023, out_file = file.path("data", "meps_all_years.rds")) {
  if (!dir.exists("data")) dir.create("data", recursive = TRUE)
  
  all_data <- list()
  for (yr in years) {
    d <- load_meps_year(yr)
    suffix <- paste0("_", yr)
    all_data[[paste0("fyc", suffix)]]  <- d$fyc
    all_data[[paste0("cond", suffix)]] <- d$cond
    all_data[[paste0("rx", suffix)]]   <- d$rx
    all_data[[paste0("ob", suffix)]]   <- d$ob
    all_data[[paste0("link", suffix)]] <- d$link
  }
  
  saveRDS(all_data, file = out_file)
  invisible(out_file)
}

# Save the MEPS data into an accessible format

# options(timeout = 300)
# save_all_years(years = 2017:2023)

# load data

all_data <- readRDS(file.path("data", "meps_all_years.rds"))

for (i in 1:7) {
  assign(paste0("fyc_", 2016 + i), all_data[[paste0("fyc_", 2016 + i)]])
  assign(paste0("cond_", 2016 + i), all_data[[paste0("cond_", 2016 + i)]])
  assign(paste0("rx_", 2016 + i), all_data[[paste0("rx_", 2016 + i)]])
  assign(paste0("ob_", 2016 + i), all_data[[paste0("ob_", 2016 + i)]])
  assign(paste0("link_", 2016 + i), all_data[[paste0("link_", 2016 + i)]])
}

rm(all_data)
gc()

years <- 2017:2023

# simple setup: will run the next files in order
source("02_clean.r")
source("04_linkage.r")
source("05_flowchart.r")
source("07_table1.r")