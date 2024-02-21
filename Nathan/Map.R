library(leaflet)
library(readr)
library(dplyr)

gunViolence = read.csv("C:/Users/natha/OneDrive/Desktop/School/gun-violence-data_01-2013_03-2018.csv")
gunViolence = na.omit(gunViolence)

map = leaflet() %>% 
  addTiles() %>% 
  setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addCircleMarkers(lng = gunViolence$longitude, lat = gunViolence$latitude, radius = 1)
map


