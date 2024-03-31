# Packages:
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(leaflet)
library(viridis)
library(leaflet.extras)
library(reshape2)

# Read in datasets
gunViolence = read.csv("final_df.csv")
yearly_pop = read_excel("./yearly_pop.xlsx")

gunViolence = na.omit(gunViolence)

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

lm = lm(gunViolence$n_killed ~ gunViolence$year + gunViolence$incident_month + gunViolence$state)


# Assuming 'model' is your linear regression model

# Extract summary information
model_summary <- tidy(lm)

# Create a nice-looking table
kable(model_summary, format = "html", align = "c", caption = "Linear Regression Summary") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


# Load required packages

library(mgcv)
library(rgl)


# Fit the multivariate regression splines model
model <- gam(n_killed ~ s(year, k = 6) + s(incident_day, k = 6) + s(state_population, k = 6), data = gunViolence)

# Create a grid of predictor values for plotting
year_values <- seq(min(gunViolence$year), max(gunViolence$year), length.out = 100)
incident_day_values <- seq(min(gunViolence$incident_day), max(gunViolence$incident_day), length.out = 100)
state_population_values <- seq(min(gunViolence$state_population), max(gunViolence$state_population), length.out = 100)
grid <- expand.grid(year = year_values, incident_day = incident_day_values, state_population = state_population_values)
# Sort the grid dataframe by year and incident_day
grid <- grid[order(grid$year, grid$incident_day, grid$state_population), ]

# Generate predicted values
pred_values <- predict(model, newdata = grid, type = "response")

# Plot
open3d()
persp3d(x = grid$year, y = grid$incident_day, z = grid$state_population, zlim = range(pred_values),
        col = heat.colors(100)[cut(pred_values, 100)], alpha = 0.7,
        xlab = "Year", ylab = "Incident Day of the Month", zlab = "State Population")
axes3d()





