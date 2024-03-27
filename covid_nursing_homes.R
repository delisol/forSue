# covid_nursing_homes.R
library(tidyverse)
library(janitor)
library(lubridate)
library(gt)
library(here)

nurs_home <- read_csv(file = paste0(
  "G:\\My Drive\\ForSue\\covid\\",
  "COVID-19 Nursing Home Data\\2024-03-10\\",
  "COVID-19 Nursing Home Data 03.10.2024.csv"
)) %>%
  clean_names() %>%
  mutate(week_ending = as.Date(week_ending, "%m/%d/%y"))
glimpse(nurs_home)

ma_nurs_home <-
  nurs_home %>% filter(provider_state == "MA")

ma_nurs_home %>%
  filter(str_detect(provider_name, str_to_upper("Neville")))

nurs_home_sel <-
  ma_nurs_home %>%
  filter(provider_name == str_to_upper("Cambridge Rehabilitation & Nursing Center") |
    provider_name == str_to_upper("Sancta Maria Nursing Facility") |
    str_detect(provider_name, str_to_upper("Neville Center"))) %>%
  filter(week_ending >= "2024-01-01") %>%
  select(
    week_ending, provider_name, residents_weekly_confirmed_covid_19,
    percentage_of_current_residents_up_to_date_with_covid_19_vaccines,
    percentage_of_current_healthcare_personnel_up_to_date_with_covid_19_vaccines,
    staff_weekly_confirmed_covid_19, submitted_data,
    passed_quality_assurance_check
  )

nurs_home_sel %>%
  select(-submitted_data, -passed_quality_assurance_check) %>%
  gt(.) %>%
  tab_header(
    title = html("COVID cases and vaccination levels at <br>selected Cambridge nursing facilities"),
    subtitle = "Weeks ending January 7, 2024 through March 10, 2024"
  ) %>%
  cols_label(
    week_ending = "Week ending",
    residents_weekly_confirmed_covid_19 = html("Resident <br>COVID cases"),
    percentage_of_current_residents_up_to_date_with_covid_19_vaccines = html("Vaccinated <br>residents (%)"),
    percentage_of_current_healthcare_personnel_up_to_date_with_covid_19_vaccines =
      html("Vaccinated <br>health care <br>personnel (%)"),
    staff_weekly_confirmed_covid_19 = html("Staff <br>COVID cases")
  ) %>%
  cols_align(align = "center", columns = c(2:6)) %>%
  fmt_number(columns = c(4:6), decimals = 0) %>%
  tab_row_group(label = "Cambridge Rehab & Nursing", str_detect(provider_name, "CAMBRIDGE")) %>%
  tab_row_group(label = "Sancta Maria", str_detect(provider_name, "SANCTA")) %>%
  tab_row_group(
    label = "Neville Center at Fresh Pond",
    str_detect(provider_name, str_to_upper("Neville"))
  ) %>%
  cols_hide(provider_name) %>%
  tab_footnote(
    footnote = "Resident COVID cases is the number of laboratory-confirmed
  new COVID cases for the week in residents.",
    locations = cells_column_labels(columns = 3)
  ) %>%
  tab_footnote(
    footnote = "Vaccinated residents is the percentage of residents who are up to date
               with COVID vaccines.",
    locations = cells_column_labels(columns = 4)
  ) %>%
  tab_footnote(
    footnote = "Vaccinated health care personnel is the percentage of
  health care personnel  who are up to date with COVID vaccines.",
    locations = cells_column_labels(columns = 5)
  ) %>%
  tab_footnote(
    footnote = "Staff COVID cases is the number of laboratory-confirmed
               new COVID cases for the week in staff and facility personnel.",
    locations = cells_column_labels((columns <- 6))
  ) %>%
  tab_source_note(source_note = "Source: COVID-19 Nursing Home Data , Centers for Disease Control
                  and Prevention, March 10, 2024") %>%
  gtsave(filename = "COVID_round_1a.pdf")
