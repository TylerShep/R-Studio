library(cfbfastR)
library(dplyr)
library(ggplot2)



#GET PBP DATA -> JEANTY DATA
pbp_data <- cfbfastR::load_cfb_pbp(2024)

jeanty <- pbp_data %>%
  filter(rusher_player_name == "Ashton Jeanty") %>%
  mutate(start_yard_line_ts = if_else(home != "Boise State", 100 - yard_line, yard_line)) %>%
  mutate(end_yard_line_ts = start_yard_line_ts + yards_gained) %>%
  mutate(first_down = if_else(distance >= yards_gained, 1, 0)) %>%
  mutate(game_number_key = row_number() / 2) %>%
  select(game_number_key, game_play_number, rusher_player_name, def_pos_team, start_yard_line_ts, yard_line, yards_gained, end_yard_line_ts, first_down, scoring_play, home)

#CREATE LEGEND COUNTS & LABELS
rush_type_counts <- jeanty %>%
  mutate(rush_type = case_when(
    yards_gained > 50 ~ '50+',
    yards_gained >= 20 & yards_gained <= 50 ~ '20_to_50',
    yards_gained >= 10 & yards_gained <= 19 ~ '10_to_19',
    yards_gained >= 6 & yards_gained <= 9 ~ '6_to_9',
    yards_gained >= 1 & yards_gained <= 5 ~ '1_to_5',
    TRUE ~ '<=0'
  )) %>%
  group_by(rush_type) %>%
  summarise(count = n())

labels_with_counts <- c(
  '<=0' = bquote('<=0 yds (' * bold(.(rush_type_counts$count[rush_type_counts$rush_type == "<=0"])) * ')'),
  '1_to_5' = bquote('1-5 yds (' * bold(.(rush_type_counts$count[rush_type_counts$rush_type == "1_to_5"])) * ')'),
  '6_to_9' = bquote('6-9 yds (' * bold(.(rush_type_counts$count[rush_type_counts$rush_type == "6_to_9"])) * ')'),
  '10_to_19' = bquote('10-19 yds (' * bold(.(rush_type_counts$count[rush_type_counts$rush_type == "10_to_19"])) * ')'),
  '20_to_50' = bquote('20-50 yds (' * bold(.(rush_type_counts$count[rush_type_counts$rush_type == "20_to_50"])) * ')'),
  '50+' = bquote('50+ yds (' * bold(.(rush_type_counts$count[rush_type_counts$rush_type == "50+"])) * ')')
)


#APPEND (Change year within pbp DS)
all_jeanty_data <- data.frame()
all_jeanty_data <- bind_rows(all_jeanty_data, jeanty)


#PLOT VIZ
ggplot(jeanty) + 
  annotate("rect", xmin = seq(0, 90, by = 10), xmax = seq(10, 100, by = 10),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = c(rep(c("white", "lightgrey"), 5))) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "gray30", alpha = 0.5) +
  annotate("rect", xmin = 100, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "gray30", alpha = 0.5) +
  geom_rect(aes(
    xmin = start_yard_line_ts, 
    xmax = end_yard_line_ts, 
    ymin = game_number_key - 0.5, 
    ymax = game_number_key + 0.5,
    fill = case_when(
      yards_gained > 50 ~ '50+',
      yards_gained >= 20 & yards_gained <= 50 ~ '20_to_50',
      yards_gained >= 10 & yards_gained <= 19 ~ '10_to_19',
      yards_gained >= 6 & yards_gained <= 9 ~ '6_to_9',
      yards_gained >= 1 & yards_gained <= 5 ~ '1_to_5',
      TRUE ~ '<=0'
    )
  )) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), minor_breaks = seq(0, 100, 5)) + 
  scale_fill_manual(
    values = c('<=0' = 'cadetblue3', '1_to_5' = 'chartreuse3', '6_to_9' = 'gold1', 
               '10_to_19' = 'orange', '20_to_50' = 'darkorange3', '50+' = 'red3'),
    breaks = names(labels_with_counts),
    labels = labels_with_counts,
    name = "Rush Type"
  ) +
  labs(title = "Ashton Jeanty Rushing Heat Map:", 
       subtitle = "Yards Rushed by Play, 2024-25 Season", 
       caption = expression(bold("Visualization:") ~ "@BlueChaosStats" ~ bold("Data:") ~ "@cfbfastr")) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),  
    axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.text.x = element_text(color = 'black'),
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank(),  
    panel.grid.major = element_line(color = "gray29", size = 0.2),
    panel.grid.minor = element_line(color = "gray29", size = 0.2, linetype = "longdash"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    plot.caption = element_text(hjust = 1, size = 10, color = "black", margin = margin(t = 10)),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = "black")
  )
