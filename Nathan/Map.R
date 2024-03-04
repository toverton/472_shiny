# Packages:
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(forcats)
library(leaflet)

# import original data 

# laptop path
gunViolence_orig = read_csv("./gun_violence_data.csv")

# remove NA entries & variables that are not needed to make the map
# gunViolence = na.omit(gunViolence)
gunViolence_cleaned = subset(gunViolence, select = -c(notes, 
                                              address, 
                                              incident_url, 
                                              source_url, 
                                              incident_url_fields_missing, 
                                              congressional_district, 
                                              gun_stolen, 
                                              participant_name, 
                                              participant_relationship, 
                                              participant_status, 
                                              participant_type,
                                              sources, 
                                              state_house_district, 
                                              state_senate_district, 
                                              incident_characteristics, 
                                              location_description, 
                                              incident_id))

#-----                        -----#
#reading in original file
#orig_df <- read_csv("C:/Users/NSETO/Documents/RStudio Documents/gun-violence-data_01-2013_03-2018.csv")
orig_df = gunViolence_cleaned


str_to_date <- function(str_date){
  return(str_date |>
           as.Date("%m/%d/%Y"))
}

if(is.Date(orig_df$date) == F){
  orig$df <- sapply(orig_df$date, str_to_date)
}


date_decomp <- tibble(year = year(orig_df$date),
                      incident_month = month(orig_df$date, label = TRUE),
                      incident_day = day(orig_df$date),
                      incident_wday = wday(orig_df$date, label = TRUE))
orig_df |>
  add_column(date_decomp, .after = "date") -> final_df



#-----                        -----#

# create map using Leaflet package
originalMap = leaflet() %>% 
  addTiles() %>% 
  # Zoom in on (mainland) USA
  setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addCircleMarkers(lng = gunViolence$longitude, 
                   lat = gunViolence$latitude, 
                   radius = 0.5, color = "red")
# original map output
originalMap


# starting to filter

# 2013 map data
gunViolence_2013 = filter(gunViolence, year == "2013")
map_2013 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2013$longitude, lat = gunViolence_2013$latitude, 
                   radius = 1, color = "maroon")
map_2013

# 2014 map data
gunViolence_2014 = filter(gunViolence, year == "2014")
map_2014 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2014$longitude, lat = gunViolence_2014$latitude, 
                   radius = 1, color = "orange")
map_2014

# 2015 map data
gunViolence_2015 = filter(gunViolence, year == "2015")
map_2015 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2015$longitude, lat = gunViolence_2015$latitude, 
                   radius = 1, color = "#FFDB58")
map_2015

