library(tidyverse)
library(readxl)
library(reshape2)
library(ggplot2)

#reading in original file
orig_df <- read_csv("./gun_violence_data.csv")

#-- For Nathan - Disregard Otherwise --#
str_to_date <- function(str_date){
  return(str_date |>
           as.Date("%m/%d/%Y"))
}

if(is.Date(orig_df$date) == F){
  orig_df$date <- sapply(orig_df$date, str_to_date)
}
#-----                            -----#

Pos_to_Neg_Long <- function(long_val){
  if(isTRUE(long_val > 0) == TRUE) {
    return(-long_val)
  } else {
    return(long_val)
  }
}

orig_df$longitude <- sapply(orig_df$longitude, Pos_to_Neg_Long)

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
date_decomp <- tibble(year = year(lean_df$date),
                      incident_month = month(lean_df$date, label = TRUE),
                      incident_day = day(lean_df$date),
                      incident_wday = wday(lean_df$date, label = TRUE))
lean_df |>
  add_column(date_decomp, .after = "date") -> final_df

#adding yearly population data
yearly_pop <- read_excel("./yearly_pop.xlsx")

yearly_pop |> 
  filter(!row_number() %in% seq(1:8)) |> 
  select(-c(2, 3, 4, 5, 6, 13)) |> 
  rename(state = 1, 
        "2013" = 2, 
        "2014" = 3,
        "2015" = 4,
        "2016" = 5,
        "2017" = 6,
        "2018" = 7) |> 
  na.omit() -> yearly_pop_final

remove_state_periods <- function(a_state_str) {
  a_state_str |> 
    str_replace("\\.", "") -> period_rm_state
  return(period_rm_state)
}

yearly_pop_final$state <- sapply(yearly_pop_final$state, remove_state_periods)

yearly_pop_final |>
  melt(na.rm = FALSE, value.name = "state_population", id = "state") |>
  rename(year = variable) -> yearly_pop_melt

yearly_pop_melt$year <- as.numeric(as.character(yearly_pop_melt$year))

final_df <- final_df |>
  inner_join(yearly_pop_melt, by=c("state","year"))

#adding number killed per 100,000
final_df |>
  mutate(per_hthous_killed = ((n_killed / state_population)*100000)) -> final_df

#making a df to convert long/lat to counties
#can't get it to work, also lots of NAs

# lat_long_df <- tibble(longitude = as.numeric(lean_df$longitude), 
#                       latitude = as.numeric(lean_df$latitude), 
#                       incident_id = lean_df$incident_id) |>
#   drop_na()