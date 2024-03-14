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
auto_corr_df <- with(auto_corr, data.frame(lag, acf))

N <- length(training_df_ts)
ciz = -qnorm((1-0.95)/2)/sqrt(N-3)
cir = (exp(2*ciz) - 1) / (exp(2*ciz) + 1)  

auto_corr_df |>
  ggplot(mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = cir), linetype = 2, color = 'firebrick1') +
  geom_hline(aes(yintercept = -cir), linetype = 2, color = 'firebrick1') +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Lag", 
       y = "Autocorrelation", 
       title = "Autocorrelation of Total Fatalities Training Series")

par_auto_corr <- pacf(training_df_ts, plot = F)
plot(par_auto_corr, main = "Total Fatalities Series Partial Autocorrelation")





