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


#----------------------------------Datasets----------------------------------------------
gunViolence = read.csv("final_df.csv")
yearly_pop = read_excel("./yearly_pop.xlsx")

#--------------------------------Data Cleaning-------------------------------------------
# Filter out NA values in longitude and latitude
gunViolence = gunViolence %>% 
  filter(!is.na(longitude) & !is.na(latitude))

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
                                                           

#------------------------------Informative Incident Maps-------------------------------------------------

# All incidents of MASS SHOOTINGS, defined by U.S. statute 
# (the Investigative Assistance for Violent Crimes Act of 2012) in the dataset

massShooting = filter(gunViolence, n_killed >= 3)
map_ShootingMass = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addAwesomeMarkers(lng = massShooting$longitude, lat = massShooting$latitude, label = massShooting$n_killed)
map_ShootingMass

# Deaths greater than or equal to ten
highCasuality = filter(gunViolence, n_killed >= 10)
map_highCasuality = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addAwesomeMarkers(lng = highCasuality$longitude, lat = highCasuality$latitude, label = highCasuality$n_killed)
map_highCasuality

# Incidents where there were no reported deaths
noDeaths = filter(gunViolence, gunViolence$n_killed == 0)
map_noDeaths = leaflet() %>% addTiles %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
  addCircleMarkers(lng = noDeaths$longitude, lat = noDeaths$latitude, radius = 1)
map_noDeaths

#-------------------------------------------------------------------------------------------------------


