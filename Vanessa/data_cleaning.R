library(tidyverse)
#library(latlong2)

#reading in original file
orig_df <- read_csv("~/Desktop/Statistics/STAT 472 - Project/gun_violence_data.csv")

#removing variables 
orig_df |> 
  select(-notes, 
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

#manually adding the las vegas shooting on oct 1, 2017
las_vegas <- tibble(incident_id = "0", 
               date = date("2017-10-01"), 
               state = "Nevada", 
               city_or_county = "Clark (county)", 
               n_killed = 59, 
               n_injured = 489, 
               gun_type = NA, 
               incident_characteristics = NA, 
               latitude = 36.095, 
               location_description = "Hotel", 
               longitude = -115.171667, 
               n_guns_involved = 24, 
               participant_age = 64, 
               participant_age_group = NA, 
               participant_gender = "0::Male")
lean_df <- rbind(lean_df, las_vegas)

#adding more columns for date information
date_decomp <- tibble(incident_year = year(lean_df$date),
                      incident_month = month(lean_df$date, label = TRUE),
                      incident_day = day(lean_df$date),
                      incident_wday = wday(lean_df$date, label = TRUE))
lean_df |>
  add_column(date_decomp, .after = "date") -> final_df

#making a df to convert long/lat to counties
#can't get it to work, also lots of NAs

# lat_long_df <- tibble(longitude = as.numeric(lean_df$longitude), 
#                       latitude = as.numeric(lean_df$latitude), 
#                       incident_id = lean_df$incident_id) |>
#   drop_na()
# 
# #taking the age column, parsing through the text, and turning it into mean age
# ages_to_mean_age <- function(a_str){
#   if(is.na(a_str)){
#     return(NA)
#   }
#   a_str |>
#     str_extract_all("(?<=::)\\d+") |> 
#     unlist() |> 
#     as.numeric( ) |> 
#     mean() -> mean_age
#   mean_age <- format(round(mean_age, 2), nsmall = 2)
#   return(mean_age)
# }
# 
# final_df$participant_age <- sapply(final_df$participant_age, ages_to_mean_age)
# final_df |> 
#   rename(participant_mean_age = participant_age) -> final_df #renaming age column