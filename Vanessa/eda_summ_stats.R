library(tidyverse)
library(ggplot2)

##                                                        ##
##---- Remember to load final_df from data_cleaning.R ----##
##                                                        ##

final_df |>
  group_by(state) |>
  summarize(total = sum(per_hthous_killed)) |>
  ggplot(aes(y = reorder(state, -total), x = total, fill = total)) + 
  geom_col(width = 0.5, position = position_dodge(0.2), show.legend = F) +
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Fatalities per 100,000", 
       y = "State or District", 
       title = "2013 - 2018 Fatalities per 100,000 by State") + 
  scale_fill_gradient(low = "dodgerblue2", high = "firebrick1") + 
  coord_flip()
#ggsave("per_cap_plot.png", width = 14, height = 7)

final_df |> 
  filter(per_hthous_killed != 0, state == "District of Columbia") |>
  ggplot(aes(x = log(per_hthous_killed))) + 
  geom_density()