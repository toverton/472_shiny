# Packages:
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)

# import data 
gunViolence = read.csv("C:/Users/natha/OneDrive/Desktop/School/gun-violence-data_01-2013_03-2018.csv")

# removing NA entries & variables that are not needed to make the map
gunViolence = na.omit(gunViolence)
gunViolence = subset(gunViolence, select = -c(notes, 
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
                                              state_senate_district))

# create map using Leaflet package
map = leaflet() %>% 
  addTiles() %>% 
  # Zoom in on USA
  setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addCircleMarkers(lng = gunViolence$longitude, 
                   lat = gunViolence$latitude, 
                   radius = 1, color = "red")
# output
map

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
gunViolence <- rbind(gunViolence, las_vegas)


date_decomp <- tibble(year = year(gunViolence$date),
                      incident_month = month(gunViolence$date, label = TRUE),
                      incident_day = day(gunViolence$date), label = TRUE)


gunViolence$date = as_date(as.POSIXct(gunViolence$date))
gunViolence %>% dplyr::mutate(year = lubridate::year(date),
                              month = lubridate::month(date),
                              day = lubridate::day(date))