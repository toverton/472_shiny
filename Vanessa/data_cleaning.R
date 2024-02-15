library(tidyverse)
library(dplyr)
library(latlong2)
library(stringr)

orig_df <- read_csv("~/Desktop/Statistics/STAT 472 - Project/gun_violence_data.csv")

#removing variables 
orig_df |> 
  select(-notes,
         -incident_id, 
         -address, 
         -incident_url, 
         -source_url, 
         -incident_url_fields_missing, 
         -congressional_district, 
         -gun_stolen, 
         -participant_name, 
         -participant_relationship, 
         -participant_status, 
         -participant_type,
         -sources, 
         -state_house_district, 
         -state_senate_district) -> lean_df

lat_long_df <- tibble(longitude = as.numeric(lean_df$longitude), 
                   latitude = as.numeric(lean_df$latitude), 
                   state = lean_df$state)

test_string = "0::51||1::40||2::9||3::5||4::2||5::15"

new_string <- str_subset(test_string, "::") 

age_list <- strsplit(new_string, ",")



