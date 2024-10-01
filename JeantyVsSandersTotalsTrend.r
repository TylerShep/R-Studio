library(cfbfastR)
library(dplyr)
library(ggplot2)
library(reshape2)



# GAMES DATA
game_week <- cfbfastR::load_cfb_schedules() %>%
  filter(season == 2024, completed == TRUE) %>%
  distinct(game_id, season, home_team, away_team, season_type) %>%
  mutate(opponent = if_else(home_team == "Boise State", away_team, home_team))  # New opponent column

print(game_week)

#GET PBP DATA -> RB DATA
bsu_game_stats <- cfbfastR::cfbd_game_player_stats(year = 2024, team = "Boise State") %>%
  filter(name == "Ashton Jeanty", category == "rushing") %>%
  select(game_id, team, name, yds, avg, td) %>%
  mutate(
    yds = as.numeric(yds),
    avg = as.numeric(avg),
    td = as.numeric(td)) %>%
  arrange(game_id) %>%  # Sort by game_id
  mutate(week = row_number()) 
print(bsu_game_stats)


# BARRY SANDSERS 1988 STATS (https://www.espn.com/college-football/story/_/id/24440133/the-untold-stories-barry-sanders-record-setting-1988-season)
sanders_stats <- data.frame(
  Week = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  Name = c("Barry Sanders", "Barry Sanders", "Barry Sanders", "Barry Sanders",
           "Barry Sanders", "Barry Sanders", "Barry Sanders", "Barry Sanders", 
           "Barry Sanders", "Barry Sanders", "Barry Sanders", "Barry Sanders"),
  Opponent = c("Miami, Ohio", "Texas A&M", "Tulsa", "Colorado", "Nebraska", 
               "Missouri", "Kansas State", "Oklahoma", "Kansas", "Iowa State", 
               "Texas Tech", "Wyoming"),
  Yards = c(178, 157, 304, 174, 189, 154, 320, 215, 312, 293, 332, 222),
  Touchdowns = c(2, 2, 5, 4, 4, 2, 3, 2, 5, 4, 4, 5),
  Attempts = c(18, 20, 33, 24, 35, 25, 37, 39, 37, 32, 44, 29)
)
print(sanders_stats)


# COMBINE DATAFRAMES
bsu_stats_with_week <- bsu_game_stats %>%
  left_join(game_week, by = "game_id") %>%
  select(week, name, opponent, yds, avg, td)

combined_stats <- sanders_stats %>%
  left_join(bsu_stats_with_week, by = c("Week" = "week"))

combined_stats <- combined_stats %>%
  mutate(
    Total_Sanders_Yds = cumsum(Yards),
    Total_Sanders_TDs = cumsum(Touchdowns),
    Total_Jeanty_Yds = cumsum(yds), 
    Total_Jeanty_TDs = cumsum(td),
  )

print(combined_stats)

combined_stats_melted <- melt(combined_stats, id.vars = 'Week', 
                              measure.vars = c('Total_Sanders_Yds', 'Total_Jeanty_Yds', 'Total_Sanders_TDs', 'Total_Jeanty_TDs'),
                              variable.name = 'Metric', value.name = 'Value')


#PLOT VIZ
ggplot(combined_stats_melted, aes(x = as.factor(Week))) +
  geom_line(data = subset(combined_stats_melted, Metric %in% c('Total_Sanders_Yds', 'Total_Jeanty_Yds')),
            aes(y = Value, color = Metric, group = Metric), size = 1) +
    geom_point(data = subset(combined_stats_melted, Metric %in% c('Total_Sanders_Yds', 'Total_Jeanty_Yds')),
             aes(y = Value, color = Metric), size = 2) +
    geom_text(data = subset(combined_stats_melted, Metric == 'Total_Jeanty_Yds'),
            aes(y = Value, label = Value), nudge_y = 0.25 ,vjust = -1.2, color = 'blue', size = 3.5, fontface = "bold") +
    geom_bar(data = subset(combined_stats_melted, Metric %in% c('Total_Sanders_TDs', 'Total_Jeanty_TDs')),
           aes(y = Value * 50, fill = Metric), stat = 'identity', position = position_dodge(width = 0.5), width = 0.4) +
    geom_text(data = subset(combined_stats_melted, Metric == 'Total_Jeanty_TDs'),
            aes(y = Value * 50, label = Value), hjust = -0.2, nudge_x = 0.25, color = 'blue', size = 3.5, fontface = "bold") +
    scale_y_continuous(name = 'Yards', 
                     sec.axis = sec_axis(~ . / 50, name = 'Touchdowns')) +
    scale_color_manual(values = c('Total_Sanders_Yds' = 'darkorange1', 'Total_Jeanty_Yds' = 'blue'),
                     labels = c('Total_Sanders_Yds' = 'Sanders', 
                                'Total_Jeanty_Yds' = 'Jeanty')) +
  scale_fill_manual(values = c('Total_Sanders_TDs' = 'darkorange1', 'Total_Jeanty_TDs' = 'blue'),
                    labels = c('Total_Sanders_TDs' = 'Sanders', 
                               'Total_Jeanty_TDs' = 'Jeanty')) +
  labs(x = 'Week #', 
       title = "Barry Sanders '88 vs Asthon Jeanty '24:", 
       subtitle = "Total Yards & Touchdowns per Week", 
       caption = expression(bold("Visualization:") ~ "@BlueChaosStats" ~ bold("Data:") ~ "@cfbfastr")) +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.y = element_text(color = 'black'),
    panel.grid.major = element_line(color = "gray29", size = 0.2, linetype = "dashed"),
    panel.grid.minor = element_line(color = "gray29", size = 0.2, linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    plot.caption = element_text(hjust = 1, size = 10, color = "black", margin = margin(t = 10)),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "left",
    legend.box.just = "left",
    legend.spacing.y = unit(0, "pt"),
    legend.key.size = unit(0.5, "cm"),
    plot.margin = margin(t = 10, r = 10, b = 40, l = 10) 
  ) +
  guides(
    color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL)
  )
