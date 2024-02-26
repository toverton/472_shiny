# Packages:
library(leaflet)
library(readr)
library(dplyr)

# import data 
gunViolence = read.csv("C:/Users/natha/OneDrive/Desktop/School/gun-violence-data_01-2013_03-2018.csv")

# removing NA entries & variables that are not needed to make the map
gunViolence = na.omit(gunViolence)
gunViolence = subset(gunViolence, select = -c(incident_id, notes, 
                                              incident_url_fields_missing, 
                                              location_description, 
                                              participant_relationship,
                                              sources,
                                              participant_type,
                                              participant_status, 
                                              incident_url, 
                                              source_url, gun_stolen, gun_type, 
                                              incident_characteristics, 
                                              n_guns_involved))

# create map using Leaflet package
map = leaflet() %>% 
  addTiles() %>% 
  setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addCircleMarkers(lng = gunViolence$longitude, 
                   lat = gunViolence$latitude, 
                   radius = 1, color = "red")
map


