# Packages:
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(leaflet)
library(viridis)
library(leaflet.extras)
library(reshape2)
library(htmltools)
library(htmlwidgets)

#---------------------------------------Datasets-------------------------------------------------
gunViolence = read.csv("final_df.csv")
yearly_pop = read_excel("./yearly_pop.xlsx")
gun_violence_restrictions = read_csv("gun_violence_restrictions.csv")

#-------------------------------------Data Cleaning-------------------------------------------------
# Filter out NA values in longitude and latitude
gunViolence = gunViolence %>% filter(!is.na(longitude) & !is.na(latitude))

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

#Dataset Ordered by gunViolence$per_hthous_killed
gunViolence_ordered = gunViolence[order(gunViolence$per_hthous_killed, decreasing = TRUE), ]
head(gunViolence_ordered)                                                                           
  
#-----------------------------------Heatmap Raw Deaths-------------------------------------------------
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

#----------------------------------Heatmap Per Capita Deaths-------------------------------------------------
legend_html_perCapita = '<div style="background-color: rgba(255, 255, 255, 0.4); padding: 3px; border-radius: 5px; border: 1px solid black; width: 150px; text-align: center;">
                  <h4 style="color: black;">Fatalities per 100,000</h4>
                  <div style="background: linear-gradient(to right, blue, green, yellow, red); height: 25px; border-radius: 5px;"></div>
                  <div> Low ------> High </div>
              </div>'

heatmap_perCapita = leaflet(gunViolence) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),      
    radius = 18,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft") %>% 
  addCircleMarkers(lng = -123.2800, lat = 43.2628, radius = 4.5, stroke = TRUE, fill = TRUE,
                   fillColor = "lightblue", color = "black", weight = .8, fillOpacity = 1, opacity = 0.5) %>% 
  addCircleMarkers(lng = -76.9977, lat = 38.8730, radius = 4.5, stroke = TRUE, fill = TRUE,
                   fillColor = "lightblue", color = "black", weight = .8, fillOpacity = 1, opacity = 0.5) %>% 
  addCircleMarkers(lng = -117.2770, lat = 34.0758, radius = 4.5, stroke = TRUE, fill = TRUE,
                   fillColor = "lightblue", color = "black", weight = .8, fillOpacity = 1, opacity = 0.5) %>%
  addCircleMarkers(lng = -98.0564, lat = 29.2733, radius = 4.5, stroke = TRUE, fill = TRUE,
                   fillColor = "lightblue", color = "black", weight = .8, fillOpacity = 1, opacity = 0.5) %>% 
  addCircleMarkers(lng = -81.3767, lat = 28.5195, radius = 4.5, stroke = TRUE, fill = TRUE,
                   fillColor = "lightblue", color = "black", weight = .8, fillOpacity = 1, opacity = 0.5) %>% 
  addCircleMarkers(lng = -80.2694, lat = 26.3045, radius = 4.5, stroke = TRUE, fill = TRUE,
                   fillColor = "lightblue", color = "black", weight = .8, fillOpacity = 1, opacity = 0.5) %>% 
  addCircleMarkers(lng = -115.1717, lat = 36.0950, radius = 4.5, stroke = TRUE, fill = TRUE,
                   fillColor = "lightblue", color = "black", weight = .8, fillOpacity = 1, opacity = 0.5) 
heatmap_perCapita


#----------------------------------Heatmap Per Capita Deaths (2013)-------------------------------------------------
gunViolence_2013 = filter(gunViolence, year == "2013")

heatmap_perCapita_2013 = leaflet(gunViolence_2013) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence_2013$per_hthous_killed, na.rm = TRUE),      
    radius = 18,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_2013

#----------------------------------Heatmap Per Capita Deaths (2014)-------------------------------------------------
gunViolence_2014 = filter(gunViolence, year == "2014")

heatmap_perCapita_2014 = leaflet(gunViolence_2014) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence_2014$per_hthous_killed, na.rm = TRUE),      
    radius = 18,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_2014

#----------------------------------Heatmap Per Capita Deaths (2015)-------------------------------------------------
gunViolence_2015 = filter(gunViolence, year == "2015")

heatmap_perCapita_2015 = leaflet(gunViolence_2015) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence_2015$per_hthous_killed, na.rm = TRUE),      
    radius = 18,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_2015

#----------------------------------Heatmap Per Capita Deaths (2016)-------------------------------------------------
gunViolence_2016 = filter(gunViolence, year == "2016")

heatmap_perCapita_2016 = leaflet(gunViolence_2016) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence_2016$per_hthous_killed, na.rm = TRUE),      
    radius = 18,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_2016

#----------------------------------Heatmap Per Capita Deaths (2017)-------------------------------------------------
gunViolence_2017 = filter(gunViolence, year == "2017")

heatmap_perCapita_2017 = leaflet(gunViolence_2017) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence_2017$per_hthous_killed, na.rm = TRUE),      
    radius = 18,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_2017

#----------------------------------Heatmap Per Capita Deaths (2018)-------------------------------------------------
gunViolence_2018 = filter(gunViolence, year == "2018")

heatmap_perCapita_2018 = leaflet(gunViolence_2018) %>%
  addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence_2018$per_hthous_killed, na.rm = TRUE),      
    radius = 18,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_2018

#----------------------------------U.S. Geographical Region Divisions-------------------------------------------------
# Source: https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States
#-------------------------------------Southern U.S.A-------------------------------------------------
US_South = c("Alabama", 'Arkansas', 
             "Delaware", "Florida", "Georgia", 
             "Kentucky", "Louisiana", "Maryland", 
             "Mississippi", "North Carolina", "Oklahoma", 
             "South Carolina", "Tennessee", "Texas", "Virginia", 
            "West Virginia")

gunViolence_south = filter(gunViolence, state %in% US_South)
heatmap_perCapita_south = leaflet(gunViolence_south) %>%
  addTiles() %>% setView(lng = -86.8104, lat = 33.5186, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = ~gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),      
    radius = 25,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_south

#------------------------------------Midwestern U.S.A.-------------------------------------------------
US_midwest = c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", 
  "North Dakota", "Ohio", "South Dakota", "Wisconsin")

gunViolence_mw = filter(gunViolence, state %in% US_midwest)
heatmap_perCapita_midwest = leaflet(gunViolence_mw) %>%
  addTiles() %>% setView(lng = -93.5366, lat = 42.129, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = ~gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),      
    radius = 25,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_midwest

#------------------------------------Western U.S.A.-------------------------------------------------
US_west = c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", 
                "Wyoming", "California", "Oregon", "Washington")

gunViolence_west = filter(gunViolence, state %in% US_west)
heatmap_perCapita_west = leaflet(gunViolence_west) %>%
  addTiles() %>% setView(lng = -111.876183, lat = 40.758701, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = ~gunViolence$per_hthous_killed,  
    blur = 20,    
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),      
    radius = 25,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_west

#------------------------------------Northeastern U.S.A.-------------------------------------------------
US_northeast = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", 
                 "Vermont", "New Jersey", "New York", "Pennsylvania")

gunViolence_northeast = filter(gunViolence, state %in% US_northeast)
heatmap_perCapita_northeast = leaflet(gunViolence_northeast) %>%
  addTiles() %>% setView(lng = -74.0060, lat = 40.7128, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = ~gunViolence$per_hthous_killed,  
    blur = 25,    
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),      
    radius = 20,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_northeast

#------------------------------------Alaska, U.S.A.-------------------------------------------------
gunViolence_alaska = filter(gunViolence, state == "Alaska")

heatmap_perCapita_ak = leaflet(gunViolence_alaska) %>%
  addTiles() %>% setView(lng = -151.1940, lat = 64.55206, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = ~gunViolence_alaska$per_hthous_killed,  
    blur = 25,    
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),      
    radius = 20,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_ak

#------------------------------------Hawaii, U.S.A.-------------------------------------------------
gunViolence_hawaii = filter(gunViolence, state == "Hawaii")

heatmap_perCapita_hi = leaflet(gunViolence_hawaii) %>%
  addTiles() %>% setView(lng = -157, lat = 20, zoom = 3.5) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = ~gunViolence$per_hthous_killed,  
    blur = 25,    
    max = max(gunViolence$per_hthous_killed, na.rm = TRUE),      
    radius = 20,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html_perCapita, position = "bottomleft")
heatmap_perCapita_hi

#----------------------------------Map of Incidents: Deaths ≥ 10-------------------------------------------------
highCasuality = filter(gunViolence, n_killed >= 10)
map_highCasuality = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addAwesomeMarkers(lng = highCasuality$longitude, lat = highCasuality$latitude, label = highCasuality$n_killed)
map_highCasuality 

# With heatmap:
# Create pop-ups for incidents
LVshooting = paste(sep = "<br/>",
                   "Las Vegas Shooting",
                   "1 October 2017",
                   "59+ Fatalities",
                   "Las Vegas, NV 89119")
Umpqua = paste(sep = "<br/>",
               "Umpqua Community College Shooting",
               "1 October 2015",
               "10 Fatalities",
               "Roseburg, OR 97470")
NavyYard = paste(sep = "<br/>",
                 "Washington Navy Yard Shooting",
                 "16 September 2013",
                 "11 Fatalities",
                 "Washington, DC 20376")
SanBernadinoAttack = paste(sep = "<br/>",
                           "San Bernardino Attack",
                           "2 December 2015",
                           "16 Fatalities",
                           "San Bernardino, CA 92408")
Sutherland = paste(sep = "<br/>",
                   "Sutherland Springs Church Shooting",
                   "5 November 2017",
                   "27 Fatalities",
                   "Sutherland Springs, TX 78161")
Pulse = paste(sep = "<br/>",
              "Pulse Nightclub Shooting",
              "50 Fatalities",
              "12 June 2016",
              "Orlando, FL 32806")
Parkland = paste(sep = "<br/>",
                 "Marjory Stoneman Douglas High School Shooting",
                 "14 February 2018",
                 "17 Fatalities",
                 "Parkland, FL 33076")

heatmap_massFatalities = heatmap %>% addPopups(lng = -115.1717, lat = 36.0950,
                                        LVshooting, options = popupOptions(closeButton = TRUE)) %>%
  addPopups(lng = -123.2800, lat = 43.2628,
            Umpqua, options = popupOptions(closeButton = TRUE)) %>%
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
heatmap_massFatalities

#----------------------------------Colorado, U.S.A.-------------------------------------------------
gunViolence_CO = filter(gunViolence, state == "Colorado")

heatmap_CO = leaflet(gunViolence_CO) %>%
  addTiles() %>% setView(lng = -105.7821, lat = 38.5501, zoom = 6) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addHeatmap(
    lng = ~longitude,  
    lat = ~latitude,  
    intensity = ~n_killed,  
    blur = 20,    
    max = max(gunViolence_CO$n_killed, na.rm = TRUE),      
    radius = 20,  
    gradient = c("blue", "green", "yellow", "red")  
  ) %>%
  addScaleBar(position = "topright") %>%
  addControl(html = legend_html, position = "bottomleft") %>%
  addCircleMarkers(lng = gunViolence_CO$longitude, lat = gunViolence_CO$latitude,
                   radius = 3, stroke = TRUE, fill = TRUE,
                   fillColor = "orange", color = "black", weight = .8, fillOpacity = 0.6, opacity = 0.5)
heatmap_CO

#-----------------------------------------------Choropleth----------------------------------------------------

# Moved to top of file:
# gun_violence_restrictions <- read_csv("gun_violence_restrictions.csv")

states = geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
class(states)
names(states)


##-##                ##-##
##-## Vanessa's Code ##-## Run with updated gun_violence_restrictions.csv!
##-##                ##-##
gun_violence_restrictions |> 
  mutate_if(is.numeric,as.factor) -> restr #As factor the columns.

states$background_checks_private_sales <- gun_violence_restrictions$background_checks_private_sales
##-##          ##-##
##-##          ##-##

m = leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% addPolygons()

pal = colorBin("Spectral", domain = states$background_checks_private_sales, bins = 2)

states_restr = m %>% addPolygons(
  fillColor = ~pal(background_checks_private_sales),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
states_restr

# Choropleth and Heatmap overlay
combined_map = leaflet(states) %>%
  setView(lng = -96, lat = 37.8, zoom = 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light", accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(data = states,
              fillColor = ~pal(background_checks_private_sales), 
              weight = 2, 
              opacity = 1, 
              color = "black", 
              dashArray = "3", 
              fillOpacity = 0.65,
              group = "Polygons") %>%
  addHeatmap(data = gunViolence, 
             lng = ~longitude, 
             lat = ~latitude, 
             intensity = gunViolence$per_hthous_killed, 
             blur = 20, 
             max = max(gunViolence$per_hthous_killed, na.rm = TRUE), 
             radius = 18, 
             gradient = c("blue", "green", "yellow", "red"), 
             group = "Heatmap") %>%
  addLayersControl(overlayGroups = c("Heatmap", "Polygons"),
                   options = layersControlOptions(collapsed = FALSE))

combined_map



