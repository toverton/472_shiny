# Packages:
library(rsample)    
library(ggplot2)  
library(earth)     
library(caret)     
library(vip)       
library(pdp)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(reshape2)

gunViolence = read_csv("final_df.csv")
yearly_pop = read_excel("yearly_pop.xlsx")

gunViolence = na.omit(gunViolence)
#----                                                                            ----#
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
#----                                                                            ----#




# Prepare predictors and response variable
predictors <- gunViolence[, c('state', 'city_or_county', 'year', 'incident_wday', 'incident_month')]
response <- gunViolence$n_killed

# Build MARS model
mars_model <- earth(response ~ predictors)

# Print summary of the model
summary(mars_model)

# Visualize the model
plot(mars_model)

# Make predictions using the trained model
# Replace 'new_data' with your new dataset for prediction
predictions <- predict(mars_model, newdata = new_data)
