# Packages:
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(leaflet)
library(earth)
library(viridis)
library(leaflet.extras)

# Read in dataset
gunViolence = read.csv("final_df.csv")

# Mapping all incidents of mass shootings, defined by U.S. statute 
# (the Investigative Assistance for Violent Crimes Act of 2012) 
# in the dataset
massShooting = filter(gunViolence, n_killed >= 3)
map_ShootingMass = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addAwesomeMarkers(lng = massShooting$longitude, lat = massShooting$latitude, label = massShooting$n_killed)
map_ShootingMass


# Mapping all incidents where deaths were greater than or equal to ten
highCasuality = filter(gunViolence, n_killed >= 10)
map_highCasuality = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addAwesomeMarkers(lng = massShooting$longitude, lat = massShooting$latitude, label = massShooting$n_killed)
map_highCasuality



# Mapping all incidents where there were no reported deaths
noDeaths = filter(gunViolence, gunViolence$n_killed == 0)
map_noDeaths = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addCircleMarkers(lng = noDeaths$longitude, lat = noDeaths$latitude, radius = 1)
map_noDeaths



#---                                                                                    ---#


# List of state capitals
state_capitals <- c("Albany", "Annapolis", "Atlanta", "Augusta", "Austin", "Baton Rouge", "Bismarck", "Boise",
                    "Boston", "Carson City", "Charleston", "Cheyenne", "Columbia", "Columbus", "Concord",
                    "Denver", "Des Moines", "Dover", "Frankfort", "Harrisburg", "Hartford", "Helena",
                    "Honolulu", "Indianapolis", "Jackson", "Jefferson City", "Juneau", "Lansing", "Lincoln",
                    "Little Rock", "Madison", "Montgomery", "Montpelier", "Nashville", "Oklahoma City",
                    "Olympia", "Phoenix", "Pierre", "Providence", "Raleigh", "Richmond", "Sacramento",
                    "Saint Paul", "Salem", "Salt Lake City", "Santa Fe", "Springfield", "St. Paul", "Tallahassee",
                    "Topeka", "Trenton")

# Filter out state capitals
gunViolence_capitalCities = gunViolence[gunViolence$city_or_county %in% state_capitals,]




# Filter out NA values in longitude and latitude
gunViolence = gunViolence %>% 
  filter(!is.na(longitude) & !is.na(latitude))


# Create Leaflet map
heatmap = leaflet(gunViolence) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addHeatmap(
    lng = ~longitude,   # Longitude column
    lat = ~latitude,   # Latitude column
    intensity = ~n_killed,  # Use n_killed column as intensity
    blur = 20,     # Blur radius
    max = max(gunViolence$n_killed, na.rm = TRUE),       # Maximum intensity
    radius = 15,   # Radius of each point
    gradient = c("blue", "yellow", "red")  # Color gradient
  ) %>%
  addScaleBar(position = "bottomright") # Add scale bar

# Display the map
heatmap

heatmap = heatmap %>% addProviderTiles("Esri.WorldStreetMap")

#heatmap_capitals = heatmap %>% addMarkers(data = gunViolence_capitalCities, 
#                       lng = gunViolence_capitalCities$longitude, 
#                       lat = gunViolence_capitalCities$latitude, 
#                       label = gunViolence_capitalCities$city_or_county)
#heatmap_capitals
