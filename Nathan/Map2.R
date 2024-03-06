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
  addAwesomeMarkers(lng = highCasuality$longitude, lat = highCasuality$latitude, label = highCasuality$n_killed)
map_highCasuality


# Mapping all incidents where there were no reported deaths
noDeaths = filter(gunViolence, gunViolence$n_killed == 0)
map_noDeaths = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addCircleMarkers(lng = noDeaths$longitude, lat = noDeaths$latitude, radius = 1)
map_noDeaths



#---                                                                                    ---#

# Filter out NA values in longitude and latitude
gunViolence = gunViolence %>% 
  filter(!is.na(longitude) & !is.na(latitude))


# Create Leaflet map
heatmap = leaflet(gunViolence) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,   
    lat = ~latitude,   
    intensity = ~n_killed,  
    blur = 20,     
    max = max(gunViolence$n_killed, na.rm = TRUE),       
    radius = 20,   
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "bottomright") # Add scale bar


# Define legend HTML content with gradient including green
legend_html <- '<div style="background-color: rgba(255, 255, 255, 0.7); padding: 5px; border-radius: 5px; border: 1px solid black; width: 120px; text-align: center;">
                  <h4 style="color: black;">Amount of People Killed</h4>
                  <div style="background: linear-gradient(to right, blue, green, yellow, red); height: 20px; border-radius: 5px;"></div>
                  <div>Low  Medium  High</div>
              </div>'



# Add legend to the map
heatmap <- heatmap %>%
  addControl(html = legend_html, position = "bottomleft")

# Display the map
heatmap

# Attempt to make markers for state capitals
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
#
#heatmap_capitals = heatmap %>% addMarkers(data = gunViolence_capitalCities, 
 #                      lng = gunViolence_capitalCities$longitude, 
 #                      lat = gunViolence_capitalCities$latitude, 
 #                      label = gunViolence_capitalCities$city_or_county)
#heatmap_capitals


heatmapTest = heatmap %>% addAwesomeMarkers(lng = highCasuality$longitude, lat = highCasuality$latitude, label = highCasuality$n_killed)
heatmapTest





