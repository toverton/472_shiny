library(tseries)
library(forecast)
library(xts)
library(fpp)
library(astsa)

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
rm(training_df, test_df)

#differencing training and test sets 
diff(training_df_ts, lag = 7, differences = 1) |> 
  na.omit() -> train_df_ts_lagged 
diff(test_df_ts, lag = 7, differences = 1) |> 
  na.omit() -> test_df_ts_lagged 

acf(train_df_ts_lagged)
pacf(train_df_ts_lagged)

sarima_fit <- sarima(training_df_ts, p = 2, d = 0, q = 2, P = 4, D = 1, Q = 1, S = 7)