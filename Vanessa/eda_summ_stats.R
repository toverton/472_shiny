library(tidyverse)
library(ggplot2)
library(plotly)

##                                                        ##
##---- Remember to load final_df from data_cleaning.R ----##
##                                                        ##

state_to_abb <- function(state_name){
  if (state_name == "District of Columbia"){
    return("DC")
  } else {
    return(state.abb[match(state_name, state.name)])
  }
}

final_df |> 
  mutate(state_abb = sapply(final_df$state, state_to_abb)) -> f_df_abb

f_df_abb |>
  group_by(state_abb) |>
  summarize(total = sum(per_hthous_killed)) |>
  ggplot(aes(y = reorder(state_abb, -total), x = total, fill = total)) + 
  geom_col(width = 0.5, position = position_dodge(0.2), show.legend = F) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Fatalities per 100,000", 
       y = "State or District", 
       title = "2013 - 2018 Fatalities per 100,000 by State or District") + 
  scale_fill_gradient(low = "dodgerblue2", high = "firebrick1") + 
  coord_flip()
ggsave("per_cap_plot.png", width = 15, height = 8)

final_df |>
  group_by(incident_month) |>
  summarize(total = sum(n_killed)) |>
  ggplot(aes(x = incident_month, y = total)) + 
  geom_point() +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Month", 
       y = "Total Fatalities", 
       title = "2013 - 2018 Total Fatalties by Month") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), linetype = "dashed")

final_df |>
  filter(year == 2017) |>
  group_by(date) |>
  summarize(total_killed = sum(n_killed), 
            total_injured = sum(n_injured)) |>
  ggplot() + 
  geom_line(aes(x = date, y = total_killed, color = "Killed"), 
            size = 0.5) + 
  geom_line(aes(x = date, y = total_injured, color = "Injured"), 
            size = 0.5) + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_blank(),
        legend.position = c(.92, .35), 
        legend.text=element_text(size = 11)) + 
  scale_color_manual(values = c("#E58EFF", "firebrick1")) + 
  labs(x = "Date", y = "Total Number of People", title = "Total Injuries and Fatalities in 2017")
#ggsave("2017_time_series_plot.png", width = 14, height = 7)