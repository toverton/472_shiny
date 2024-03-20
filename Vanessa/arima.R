library(tseries)
library(forecast)
library(astsa)

##                                                        ##
##---- Remember to load final_df from data_cleaning.R ----##
##                                                        ##

#creating training and test data frames
final_df |> 
  filter(year != 2013 & year != 2017 & year != 2018) |>
  select(c(date, n_killed)) |>
  group_by(date) |> 
  summarize(total_killed = sum(n_killed)) -> training_df
#mutate_at(c('total_killed'), ~(scale(.) %>% as.vector))

final_df |> 
  filter(year == 2017) |>
  select(c(date, n_killed)) |>
  group_by(date) |> 
  summarize(total_killed = sum(n_killed)) -> test_df
#mutate_at(c('total_killed'), ~(scale(.) %>% as.vector))

training_df_ts <- ts(training_df$total_killed, training_df$date) 
test_df_ts <- ts(test_df$total_killed, test_df$date) 

#differencing training and test sets 
diff(training_df_ts, lag = 7, differences = 1) |> 
  na.omit() -> train_df_ts_lagged 
diff(test_df_ts, lag = 7, differences = 1) |> 
  na.omit() -> test_df_ts_lagged 

#acf and pacf of differenced data
acf(train_df_ts_lagged)
pacf(train_df_ts_lagged)

sarima_fit <- sarima(training_df_ts, p = 0, d = 0, q = 0, P = 0, D = 1, Q = 1, S = 7)
sarima.for(training_df_ts, 364, 1, 0, 1, 0, 1, 1, 7)$pred |>
  as.vector() -> sarima_pred #bAD

test_df_ts |> 
  as.vector() -> test_df_ts
sqrt(mean((test_df_ts - sarima_pred)^2)) -> sarima_rmse

sarima_pred |> 
  as_tibble() -> sarima_pred_df

ggplot() + geom_line(aes(x = training_df$date, y = training_df$total_killed, 
                         color = "Training" )) + 
  geom_line(aes(x = test_df$date, y = sarima_pred_df$value, 
                         color = "Forecasted")) + 
  geom_line(aes(x = test_df$date, y = test_df$total_killed,
                color = "Observed"), alpha = 0.5) + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.title=element_blank()) +
  scale_color_manual(values = c("black", "darkgray", "dodgerblue2")) +
  labs(x = "Date", y = "Total Fatalities")