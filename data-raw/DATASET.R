## code to prepare `DATASET` dataset goes here
library(readr)
library(dplyr)
library(lubridate)
inits_data <- read_csv(here::here("data-raw","initiations.csv"),col_types = cols(.default = col_character()))
inits_data <- inits_data %>%
  rename_with(~stringr::str_to_lower(.)) %>%
  # handle all of the parsing for the data
  mutate(across(ends_with("date")|starts_with("bond_date"),~mdy_hms(.)),
        across(contains("flag"),~case_when(. == "true"~TRUE,
                                            . == "false"~FALSE)),
        across(contains("bond_amount")|starts_with("age_at")|contains("charge_count"),~parse_number(.))) %>%
  # correct a misspelling on the bond flag
  rename("bond_electronic_monitor_flag_current" = bond_electroinic_monitor_flag_current )

inits_data<- inits_data %>%
  #fix up the race information
  mutate(race = race %>%
           stringr::str_to_title() %>%
           stringr::str_replace_all("^Hispanic","White/Black [Hispanic Or Latino]"))

initiations <- inits_data
usethis::use_data(initiations, overwrite = TRUE)
