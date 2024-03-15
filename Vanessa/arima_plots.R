##                                                        ##
##---- Remember to load final_df from data_cleaning.R ----##
##                                                        ##

##                                                        ##
##----            Also load ARIMA data frames         ----##
##                                                        ##

# ACF and PACF Plots #
#--- Before Diff. ---#

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
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(face="bold", size = 9)) +
  labs(x = "Lag", 
       y = "Autocorrelation", 
       title = "Autocorrelation of Total Fatalities Training Series 
       (Before Differencing)") + 
  scale_x_continuous(breaks = round(seq(min(auto_corr_df$lag), 
                                        max(auto_corr_df$lag), 
                                        by = 7), 
                                    1))
#ggsave("acf_pre_diff.png", width = 12, height = 7)

par_auto_corr <- pacf(training_df_ts, plot = F)
par_auto_corr_df <- with(par_auto_corr, data.frame(lag, acf))

par_auto_corr_df |>
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
        plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(face="bold", size = 9)) +
  labs(x = "Lag", 
       y = "Partial Autocorrelation", 
       title = "Partial Autocorrelation of Total Fatalities Training Series 
       (Before Differencing)") + 
  scale_x_continuous(breaks = round(seq(min(auto_corr_df$lag), 
                                        max(auto_corr_df$lag), 
                                        by = 7), 
                                    1))
#ggsave("pacf_pre_diff.png", width = 12, height = 7)
