# covid_nursing_homes.R
# creates a dataset of selected providers from
# the CMS COVID-19 Nursing Home Data

# packages needed
library(here)
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(lubridate)
library(gt)
library(googledrive)
library(googlesheets4)
# end packages needed

# check that the CMS API is working
res <- GET(url = "https://data.cms.gov/data.json")
res # code 200 means it's ok

# CMS table of contents for datasets
toc <- fromJSON(rawToChar(res$content))
names(toc)
# select the one we want
df <- as.data.frame(toc$dataset) %>%
  filter(title == "COVID-19 Nursing Home Data")
glimpse(df)

# look at file structures
colnames(as.data.frame(df$distribution))
glimpse(as.data.frame(df$distribution)$description)
tabyl(as.data.frame(df$distribution)$mediaType)

# identify download url
filter(as.data.frame(df$distribution), mediaType == "text/csv") %>%
  select(downloadURL)

# create updated file that is uploaded to Drive
#   (and then is modified to create PDF table)

# read COVID Nursing Home csv file using API, filter to desired dates and providers
forDrive <- read_csv(
  file =
    as.character(filter(as.data.frame(df$distribution), mediaType == "text/csv") %>%
      select(downloadURL))
) %>%
  mutate(`Week Ending` = as.Date(`Week Ending`, "%m/%d/%y")) %>%
  filter(`Provider Name` == str_to_upper("Cambridge Rehabilitation & Nursing Center") |
    `Provider Name` == str_to_upper("Sancta Maria Nursing Facility") |
    str_detect(`Provider Name`, str_to_upper("Neville Center"))) %>%
  filter(`Week Ending` >= "2024-01-01")

# Interact with Google Drive
# start with clean authorization plate
gs4_deauth()
gs4_user()

# set up authorization
drive_auth(email = "dsolet@gmail.com")
gs4_auth(token = drive_token())

# upload forDrive dataset to Drive and move it to desired location
gs4_create("COVID Nursing Home Data", sheets = list(forDrive))
drive_mv(
  file = as_id(drive_find(pattern = "COVID Nursing Home Data", n_max = 1)),
  path = as_id("https://drive.google.com/drive/u/0/folders/1mN2Fl-WbB1nGZbvF-ccoVbBrR8WgPu1C"),
  overwrite = TRUE
)
# dataset is now uploaded

# define start and end dates for use in PDF table title
start_date <-
  format(min(forDrive$`Week Ending`), "%B %d, %Y")
end_date <-
  format(max(forDrive$`Week Ending`), "%B %d, %Y")

# Make the updated PDF table
forDrive %>%
  clean_names() %>%
  select(
    week_ending, provider_name, residents_weekly_confirmed_covid_19,
    percentage_of_current_residents_up_to_date_with_covid_19_vaccines,
    percentage_of_current_healthcare_personnel_up_to_date_with_covid_19_vaccines,
    staff_weekly_confirmed_covid_19
  ) %>%
  gt(.) %>%
  tab_header(
    title = html("COVID cases and vaccination levels at <br>selected Cambridge nursing facilities"),
    subtitle =
      sprintf("Weeks ending %s through %s", start_date, end_date)
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
  tab_source_note(source_note = "Source: COVID-19 Nursing Home Data , Centers for Disease Control
                  and Prevention") %>%
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
  gtsave(filename = "COVID_Nursing_Home.pdf")

# upload the PDF tale to Google Drive
drive_upload(
  media = "COVID_Nursing_Home.pdf",
  path = as_id("https://drive.google.com/drive/u/0/folders/1mN2Fl-WbB1nGZbvF-ccoVbBrR8WgPu1C"),
  overwrite = TRUE
)
