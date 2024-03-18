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
library(reshape2)


# Read in datasets
gunViolence = read.csv("final_df.csv")
yearly_pop = read_excel("./yearly_pop.xlsx")

#----                                                             ----#
yearly_pop |> 
  filter(!row_number() %in% seq(1:8)) |> 
  select(-c(2, 3, 4, 5, 6, 13)) |> 
  rename(state = 1, 
         "2013" = 2, 
         "2014" = 3,
         "2015" = 4,
         "2016" = 5,
         "2017" = 6,
         "2018" = 7) |> 
  na.omit() -> yearly_pop_final

remove_state_periods <- function(a_state_str) {
  a_state_str |> 
    str_replace("\\.", "") -> period_rm_state
  return(period_rm_state)
}

yearly_pop_final$state <- sapply(yearly_pop_final$state, remove_state_periods)

yearly_pop_final |>
  melt(na.rm = FALSE, value.name = "state_population", id = "state") |>
  rename(year = variable) -> yearly_pop_melt

yearly_pop_melt$year <- as.numeric(as.character(yearly_pop_melt$year))

gunViolence = gunViolence |>
  inner_join(yearly_pop_melt, by=c("state","year"))

#adding number killed per 100,000
gunViolence |>
  mutate(per_hthous_killed = ((n_killed / state_population)*100000)) -> gunViolence

#write.csv(final_df, "~/Desktop/final_df.csv", row.names = FALSE)
#----                                                             ----#
 

highCasuality = filter(gunViolence, n_killed >= 10)
map_highCasuality = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addAwesomeMarkers(lng = highCasuality$longitude, lat = highCasuality$latitude, label = highCasuality$n_killed)
map_highCasuality                                                                            ---#

  
# Filter out NA values in longitude and latitude
gunViolence = gunViolence %>% 
  filter(!is.na(longitude) & !is.na(latitude))

# Define legend HTML content with gradient
legend_html = '<div style="background-color: rgba(255, 255, 255, 0.4); padding: 5px; border-radius: 5px; border: 1px solid black; width: 120px; text-align: center;">
                  <h4 style="color: black;">Amount of People Killed</h4>
                  <div style="background: linear-gradient(to right, blue, green, yellow, red); height: 20px; border-radius: 5px;"></div>
                  <div>Low  Medium  High</div>
              </div>'

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
  addScaleBar(position = "topright") %>% 
  addControl(html = legend_html, position = "bottomleft") %>%
  addAwesomeMarkers(lng = highCasuality$longitude, 
                    lat = highCasuality$latitude, 
                    label = highCasuality$n_killed)

# Display the map
heatmap


legend_html_perCapita = '<div style="background-color: rgba(255, 255, 255, 0.4); padding: 5px; border-radius: 5px; border: 1px solid black; width: 150px; text-align: center;">
                  <h4 style="color: black;">People Killed per 100,000</h4>
                  <div style="background: linear-gradient(to right, blue, green, yellow, red); height: 20px; border-radius: 5px;"></div>
                  <div> Low   Medium   High </div>
              </div>'

heatmap_perCapita = leaflet(gunViolence) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,   
    lat = ~latitude,   
    intensity = ~per_hthous_killed,  
    blur = 20,     
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),       
    radius = 20,   
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>% 
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita


# Create pop-ups for incidents where n_killed ≥ 10
LVshooting = paste(sep = "<br/>",
                   "Las Vegas Shooting",
                   "1 October 2017",
                   "Las Vegas, NV 89119")
Roseburg = paste(sep = "<br/>",
                   "Umpqua Community College Shooting",
                   "1 October 2015",
                   "Roseburg, OR 97470")
NavyYard = paste(sep = "<br/>",
                 "Washington Navy Yard Shooting",
                 "16 September 2013",
                 "Washington, DC 20376")
SanBernadinoAttack = paste(sep = "<br/>",
                 "San Bernardino Attack",
                 "2 December 2015",
                 "San Bernardino, CA 92408")
Sutherland = paste(sep = "<br/>",
                  "Sutherland Springs Church Shooting",
                  "5 November 2017",
                  "Sutherland Springs, TX 78161")
Pulse = paste(sep = "<br/>",
                   "Pulse Nightclub Shooting",
                   "12 June 2016",
                   "Orlando, FL 32806")
Parkland = paste(sep = "<br/>",
              "Marjory Stoneman Douglas High School Shooting",
              "14 February 2018",
              "Parkland, FL 33076")

heatmap_markers = heatmap %>% addPopups(lng = -115.1717, lat = 36.0950, 
                                    LVshooting, options = popupOptions(closeButton = TRUE)) %>% 
                                    addPopups(lng = -123.2800, lat = 43.2628, 
                                              Roseburg, options = popupOptions(closeButton = TRUE)) %>% 
                                    addPopups(lng = -76.9977, lat = 38.8730, 
                                              NavyYard, options = popupOptions(closeButton = TRUE)) %>% 
                                    addPopups(lng = -117.2770, lat = 34.0758, 
                                              SanBernadinoAttack, options = popupOptions(closeButton = TRUE)) %>% 
                                    addPopups(lng = -98.0564, lat = 29.2733, 
                                              Sutherland, options = popupOptions(closeButton = TRUE)) %>% 
                                    addPopups(lng = -81.3767, lat = 28.5195, 
                                              Pulse, options = popupOptions(closeButton = TRUE)) %>% 
                                    addPopups(lng = -80.2694, lat = 26.3045, 
                                              Parkland, options = popupOptions(closeButton = TRUE))
heatmap_markers



# Attempt to make markers for state capitals
# List of state capitals
state_capitals = c("Albany", "Annapolis", "Atlanta", "Augusta", "Austin", "Baton Rouge", "Bismarck", "Boise",
                    "Boston", "Carson City", "Charleston", "Cheyenne", "Columbia", "Columbus", "Concord",
                    "Denver", "Des Moines", "Dover", "Frankfort", "Harrisburg", "Hartford", "Helena",
                    "Honolulu", "Indianapolis", "Jackson", "Jefferson City", "Juneau", "Lansing", "Lincoln",
                    "Little Rock", "Madison", "Montgomery", "Montpelier", "Nashville", "Oklahoma City",
                   "Olympia", "Phoenix", "Pierre", "Providence", "Raleigh", "Richmond", "Sacramento",
                    "Saint Paul", "Salem", "Salt Lake City", "Santa Fe", "Springfield", "St. Paul", "Tallahassee",
                    "Topeka", "Trenton")

















