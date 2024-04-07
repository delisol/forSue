# access_hosp.R
# retrieves COVID-19 Reported Patient Impact and Hospital Capacity by Facility -- RAW file
# Source is HHS
# Documentation
#   https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/uqq2-txqb/about_data
#   https://support.socrata.com/hc/en-us/articles/202949268-How-to-query-more-than-1000-rows-of-a-dataset
#   https://dev.socrata.com/docs/queries/offset.html
#   https://dev.socrata.com/docs/paging.html
#   https://dev.socrata.com/docs/filtering
#   aided by chatgbt 3.5, on David's account , https://chat.openai.com/c/4a990fc3-0967-4257-a567-9ec748f68f41

# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(googledrive)
library(googlesheets4)

# Function to retrieve all data
retrieve_all_data <- function(url) {
  all_data <- list()
  page <- 1
  
  while (TRUE) {
    # Append page parameter to the URL
    page_url <- modify_url(url, query = list("$limit" = 50000, "$offset" = (page - 1) * 50000))
    
    # Send GET request to the API endpoint
    response <- GET(page_url)
    
    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse JSON content
      data <- fromJSON(content(response, "text"))
      
      # Check if there is no more data
      if (length(data) == 0) {
        break
      }
      
      # Append data to the list
      all_data[[page]] <- data
      
      # Increment page counter
      page <- page + 1
    } else {
      print("Failed to retrieve data.")
      break
    }
  }
  
  # Combine all data frames into a single data frame
  combined_data <- bind_rows(all_data)
  return(combined_data)
}

# API endpoint (SODA 2.1)
# note filter to Massachusetts in url
url <- "https://healthdata.gov/resource/anag-cw7u.json?state=MA"

# Retrieve all data
patientImpact_MA <- retrieve_all_data(url)
colnames(patientImpact_MA)
CamHlthAll <- filter(patientImpact_MA , hospital_name == 'CAMBRIDGE HEALTH ALLIANCE')
MtAub <- filter(patientImpact_MA , hospital_name == 'MOUNT AUBURN HOSPITAL')


############### UPLOAD DATA FILE TO GOOGLE DRIVE ###############
# Interact with Google Drive
# Authorize connection: start with clean authorization slate
gs4_deauth()
gs4_user()

# set up authorization
drive_auth(email = "dsolet@gmail.com")
gs4_auth(token = drive_token())

# upload hospital datasets to Drive and move it to desired location
# Cambridge Health Alliance
gs4_create("Patient impact Cambridge Health All", sheets = list(CamHlthAll %>% select(-geocoded_hospital_address)))
# for path = as_id, insert url for the destination folder
drive_mv(
  file = as_id(drive_find(pattern = "Patient impact Cambridge Health All", n_max = 1)),
  path = as_id("https://drive.google.com/drive/u/0/folders/1mN2Fl-WbB1nGZbvF-ccoVbBrR8WgPu1C"),
  overwrite = TRUE
)

gs4_create("Patient impact Mount Auburn Hospital", sheets = list(MtAub %>% select(-geocoded_hospital_address)))
# for path = as_id, insert url for the destination folder
drive_mv(
  file = as_id(drive_find(pattern = "Patient impact Mount Auburn Hospital", n_max = 1)),
  path = as_id("https://drive.google.com/drive/u/0/folders/1mN2Fl-WbB1nGZbvF-ccoVbBrR8WgPu1C"),
  overwrite = TRUE
)
