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
library(earth)

# Read in dataset

gunViolence = read.csv("final_df.csv")



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


# Maps with Filtered Data

# 2013 map data
gunViolence_2013 = filter(gunViolence, year == "2013")
map_2013 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2013$longitude, lat = gunViolence_2013$latitude, 
                   radius = 3, color = "black", stroke = TRUE, weight = .5, 
                   fill = TRUE, fillColor = "maroon", fillOpacity = 1)
map_2013


# 2014 map data
gunViolence_2014 = filter(gunViolence, year == "2014")
map_2014 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2014$longitude, lat = gunViolence_2014$latitude, 
                   radius = 3, color = "black", stroke = TRUE, weight = .5, 
                   fill = TRUE, fillColor = "orange", fillOpacity = 1)
map_2014


# 2015 map data
gunViolence_2015 = filter(gunViolence, year == "2015")
map_2015 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2015$longitude, lat = gunViolence_2015$latitude, 
                   radius = 3, color = "black", stroke = TRUE, weight = .5, 
                   fill = TRUE, fillColor = "#FFDB58", fillOpacity = 1)
map_2015


# 2016 map data
gunViolence_2016 = filter(gunViolence, year == "2016")
map_2016 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2016$longitude, lat = gunViolence_2016$latitude, 
                   radius = 3, color = "black", stroke = TRUE, weight = .5, 
                   fill = TRUE, fillColor = "green", fillOpacity = 1)
map_2016


# 2017 map data
gunViolence_2017 = filter(gunViolence, year == "2017")
map_2017 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2017$longitude, lat = gunViolence_2017$latitude, 
                   radius = 3, color = "black", stroke = TRUE, weight = .5, 
                   fill = TRUE, fillColor = "blue", fillOpacity = 1)
map_2017
 

# 2018 map data
gunViolence_2018 = filter(gunViolence, year == "2018" & -175 <= longitude & longitude <= -45)
map_2018 = leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addCircleMarkers(lng = gunViolence_2018$longitude, lat = gunViolence_2018$latitude, 
                   radius = 3, color = "black", stroke = TRUE, weight = .5, 
                   fill = TRUE, fillColor = "purple", fillOpacity = gunViolence_2018$n_killed)
map_2018





#----                                                                       ----#
# From internet:  
table(gunViolence$state)

states <- geojsonio::geojson_read("final_df", what = "sp")
class(states)
names(states)

m <- leaflet(states) %>% 
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
m = m %>% addPolygons()
m

bins <- c(200, 300, 400, 500, 600, 700, 800, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
m

#----                                                                       ----#
