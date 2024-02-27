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
gunViolence = read.csv("C:/Users/natha/OneDrive/Desktop/School/gun-violence-data_01-2013_03-2018.csv")

# desktop path(s)
#gunViolence = read.csv("C:/Users/NSETO/Documents/RStudio Documents/gun-violence-data_01-2013_03-2018.csv")
gunViolence = read.csv("C:/Users/NSETO/Documents/RStudio Documents/gun-violence-data_01-2013_03-2018_COPY.csv")

# remove NA entries & variables that are not needed to make the map
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
                                              state_senate_district, 
                                              incident_characteristics, 
                                              location_description, 
                                              incident_id))





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
# filter() year = 2013
gunViolence_2013 = filter(gunViolence, year == "2013")
map_2013 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2013$longitude, lat = gunViolence_2013$latitude, 
                   radius = 1, color = "blue")
map_2013


