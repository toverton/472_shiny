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


# Load necessary libraries
library(leaflet)
library(dplyr)
library(viridis)
library(leaflet.extras)

# Assuming 'gunViolence' is a dataset with latitude, longitude, and n_killed columns

# Filter out NA values in longitude and latitude
gunViolence <- gunViolence %>% 
  filter(!is.na(longitude) & !is.na(latitude))

# Create Leaflet map
m <- leaflet(gunViolence) %>%
  addTiles() %>% 
  setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addHeatmap(
    lng = ~longitude,   # Longitude column
    lat = ~latitude,   # Latitude column
    intensity = ~n_killed,  # Use n_killed column as intensity
    blur = 20,     # Blur radius
    max = max(gunViolence$n_killed, na.rm = TRUE),       # Maximum intensity
    radius = 15,   # Radius of each point
    gradient = viridisLite::viridis(20)  # Color gradient
  ) %>%
  addScaleBar(position = "bottomright") # Add scale bar

# Display the map
m








