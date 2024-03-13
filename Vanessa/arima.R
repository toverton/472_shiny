library(tseries)
library(forecast)
library(xts)
library(fpp)

##                                                        ##
##---- Remember to load final_df from data_cleaning.R ----##
##                                                        ##

#creating training and test data frames
final_df |> 
  filter(year != 2013 & year != 2018) |> 
  select(c(date, n_killed)) |>
  group_by(date) |> 
  summarize(total_killed = sum(n_killed)) -> training_df

final_df |> 
  filter(year == 2018) |>
  select(c(date, n_killed)) |>
  group_by(date) |> 
  summarize(total_killed = sum(n_killed)) -> test_df

training_df_ts <- xts(training_df$total_killed, training_df$date) 
test_df_ts <- xts(test_df$total_killed, test_df$date) 

#Testing for stationarity
adf.test(training_df_ts)

#ACF and PACF plots
auto_corr <- acf(training_df_ts, plot = F)
plot(auto_corr, main = "Total Fatalities Series Autocorrelation")

par_auto_corr <- pacf(training_df_ts, plot = F)
plot(par_auto_corr, main = "Total Fatalities Series Partial Autocorrelation")




