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

# for laptop computer path
#gunViolence = read.csv("C:/Users/natha/OneDrive/Desktop/School/gun-violence-data_01-2013_03-2018.csv")
# write.csv(gunViolence_orig, "final_df.csv", row.names = FALSE)

# for desktop computer path
gunViolence = read.csv("C:/Users/NSETO/Documents/472_shiny/final_df.csv")


#-----                        -----#
#reading in original file
#orig_df <- read_csv("C:/Users/NSETO/Documents/RStudio Documents/gun-violence-data_01-2013_03-2018.csv")
orig_df = gunViolence


# str_to_date <- function(str_date){
#  return(str_date |>
#          as.Date("%m/%d/%Y"))
#}

#if(is.Date(orig_df$date) == F){
#  orig$df <- sapply(orig_df$date, str_to_date)
#}


#date_decomp <- tibble(year = year(orig_df$date),
#                      incident_month = month(orig_df$date, label = TRUE),
#                      incident_day = day(orig_df$date),
#                      incident_wday = wday(orig_df$date, label = TRUE))
#orig_df |>
#  add_column(date_decomp, .after = "date") -> final_df
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

table(gunViolence$state)

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
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