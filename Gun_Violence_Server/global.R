library(dplyr)

gun_data<-read.csv("../final_df.csv")
gun_data<-na.omit(gun_data)
cleantable <-gun_data %>% select(
  latitude,
  longitude,
  n_killed,
  n_injured,
  per_hthous_killed,
  date,
  year,
  month = incident_month,
  day = incident_day,
  state_population,
  state
)

gun_data$date = as.Date(gun_data$date)