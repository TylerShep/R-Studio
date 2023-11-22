library(ggplot2)
library(ggimage)
library(dplyr)
library(reshape2)
library(scales)

pbp_data_list <- list()

#Get MW Schools
mw_schools <- cfbfastR::cfbd_team_info() %>% 
  filter(conference == "Mountain West") %>% 
  select(school, logo)


#Get Current Game Week
game_week <- cfbfastR::load_cfb_schedules(seasons = 2023) %>%
  filter(completed == TRUE ) %>%
  distinct(season, week)


#Get Play by Play data for ALL MW games, ALL 2023 Weeks
for(w in game_week$week) {
  pbp_data_week <- cfbfastR::cfbd_pbp_data(year = 2023, week = w) %>%
    filter(offense_play %in% mw_schools$school)
    pbp_data_list[[paste0("week_", w)]] <- pbp_data_week
}
pbp_data <- bind_rows(pbp_data_list)  

#Join in MW Schools data
merged_data <- left_join(pbp_data, mw_schools, by = c("offense_play" = "school"))


#Group by team, calc coversions %
#mw_conversion <- merged_data %>%
 # group_by(offense_play, logo) %>%
 # summarise(
   # tot_offense_plays = n_distinct(id_play),
    #first_downs= sum(down == 1 & yards_gained >= distance),
    #first_down_rate = sum(down == 1 & yards_gained >= distance) / tot_offense_plays,
    #second_downs = sum(down == 2 & yards_gained >= distance),
    #second_down_rate = sum(down == 2 & yards_gained >= distance) / tot_offense_plays,
    #third_downs = sum(down == 3 & yards_gained >= distance),
    #third_down_rate = sum(down == 3 & yards_gained >= distance) / tot_offense_plays,
    #fourth_downs = sum(down == 4 & yards_gained >= distance),
    #fourth_down_rate = sum(down == 4 & yards_gained >= distance) / tot_offense_plays,
    #not_coverted = sum(yards_gained < distance) / tot_offense_plays) 

conversion_rates <- merged_data %>%
  group_by(offense_play, logo) %>%
  summarise(
    first_down = sum(down == 1 & yards_gained >= distance)/n(),
    second_down = sum(down == 2 & yards_gained >= distance)/n(),
    third_down = sum(down == 3 & yards_gained >= distance)/n(),
    fourth_down = sum(down == 4 & yards_gained >= distance)/n(),
    no_conversion = 1 - (first_down + second_down + third_down + fourth_down)
  )


#%>%  select(offense_play, logo, first_down_rate, second_down_rate, third_down_rate, fourth_down_rate, not_coverted)

#Plot visualizaiton
library(tidyr)
library(ggimage)

conversion_rates_long <- conversion_rates %>%
  gather(key = "down", value = "conversion", -offense_play)

ggplot(conversion_rates_long, aes(x=offense_play, y=conversion, fill=down)) + 
  geom_bar(stat="identity", position="stack") +
  coord_flip() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title="How often do Mountain West offenses convert to a new series of downs?",
       subtitle="2023 College Football Season, Weeks 1-2",
       x="Team",
       y="Conversion Rate (as percentage)", 
       fill="Down") +
  theme_minimal() + 
  scale_fill_brewer(palette="Set1")

library(grid)
library(jpeg)

ggplot(conversion_rates_long, aes(x=offense_play, y=conversion, fill=down)) + 
  geom_bar(stat="identity", position="stack") +
  coord_flip() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title="How often do Mountain West offenses convert to a new series of downs?",
       subtitle="2023 College Football Season, Weeks 1-2",
       x="",
       y="Conversion Rate (as percentage)", 
       fill="Down") +
  theme_minimal() + 
  theme(axis.text.y= element_blank()) +
  scale_fill_brewer(palette="Set1") +
  geom_image(aes(image = logo), data = mw_schools, nudge_y = 0.5, size = 0.05)
