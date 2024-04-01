library(tidyverse)
library(readxl)
library(reshape2)
library(ggplot2)
library(funtimes)

final_df |> 
  filter(year %in% c(2014, 2015, 2016, 2017)) |> 
  group_by(date) |> 
  summarize(total_killed = sum(n_killed)) -> f_df_14_17
f_df_14_17 |> 
  mutate(year = year(date)) -> f_df_14_17

yearly_pop <- read_excel("./yearly_pop.xlsx")

yearly_pop |> 
  slice(4) |> 
  select(-c(1, 2, 3, 4, 5, 6, 7, 12, 13)) |> 
  rename("2014" = 1, 
         "2015" = 2,
         "2016" = 3,
         "2017" = 4) |> 
  melt(na.rm = FALSE, value.name = "us_population", variable.name = "year") -> us_pop_14_17

us_pop_14_17$year <- as.numeric(as.character(us_pop_14_17$year))

f_df_14_17 |>
  merge(us_pop_14_17, by = "year") |> 
  mutate(deaths_per_mill = (total_killed / us_population)*1000000) -> death_rate_df

rm(yearly_pop, f_df_14_17) #keeping environment clean

death_rate_df |> 
  select(date, deaths_per_mill) -> death_rate_ts
death_rate_ts <- ts(death_rate_ts$deaths_per_mill, death_rate_ts$date)

notrend_test(death_rate_ts)

#write.csv(death_rate_df, "~/Desktop/death_rate_df.csv", row.names = F)

# death_rate_df |> 
#   ggplot() + 
#   geom_line(aes(x = date, y = deaths_per_mill)) +
#   theme_bw()