library(cfbfastR)
library(gt)
library(dplyr)



# GET DATA
pbp_data <- cfbfastR::load_cfb_pbp(year = 2024)
teams_data <- cfbfastR::load_cfb_teams(fbs_only = TRUE)
rushing_plays <- c('Rush', 'Rushing Touchdown')
boise_state_roster <- cfbfastR::cfbd_team_roster(year = 2024, team = "Boise State")


# FBS TEAMS ONLY
fbs_teams <- teams_data %>%
  filter(classification == "fbs") %>%
  select(school, color, logo)


# FILER RUSHING DATA
team_rushing_stats <- pbp_data %>%
  filter(play_type %in% rushing_plays, pos_team != "Boise State") %>%
  group_by(pos_team) %>%
  summarize(
    total_carries = n(),
    total_yards = sum(yards_gained, na.rm = TRUE),
    total_tds = sum(scoring, na.rm = TRUE),
    total_games = n_distinct(week),  # Calculate total games
    unique_rushers = n_distinct(rusher_player_name)  # Count unique rushing players
  )

ashton_stats <- pbp_data %>%
  filter(rusher_player_name == "Ashton Jeanty", play_type %in% rushing_plays) %>%
  summarize(
    total_carries = n(),
    total_yards = sum(yards_gained, na.rm = TRUE),
    total_tds = sum(scoring, na.rm = TRUE),
    total_games = n_distinct(week),  # Calculate total games
    unique_rushers = 1  # Only Ashton Jeanty, so set to 1
  ) %>%
  mutate(pos_team = "Boise State")


# GET AJ IMAGE
aj_headshot_url <- boise_state_roster %>%
  filter(last_name == "Jeanty") %>%
  select(headshot_url)
headshot_url_value <- aj_headshot_url %>% pull(headshot_url)


# COMBINE DATA
combined_stats <- bind_rows(ashton_stats, team_rushing_stats)
combined_teams_data <- fbs_teams %>%
  left_join(combined_stats, by = c("school" = "pos_team")) %>%
  mutate(
    team_player = ifelse(school == "Boise State", "Ashton Jeanty", school),
    image_url = ifelse(team_player == "Ashton Jeanty", headshot_url_value, logo),
    YPC = total_yards / total_carries,  # Calculate Yards Per Carry
    YPG = total_yards / total_games,     # Calculate Yards Per Game
    Yards_Per_Rusher = total_yards / unique_rushers  # Calculate Yards Per Rusher
  ) %>%
  # Arrange the data with Ashton Jeanty at the top, then by descending total_yards
  arrange(desc(total_yards)) %>%
  mutate(rank = ifelse(team_player == "Ashton Jeanty", 1, row_number() + 1)) %>%
  arrange(rank) %>%  # Arrange by rank to put Ashton Jeanty at the top
  select(rank, team_player, image_url, total_carries, total_yards, total_tds, total_games, unique_rushers, YPC, YPG, Yards_Per_Rusher)


# CREATE TABLE
table_plot <- combined_teams_data %>%
  select(-image_url) %>%  # Exclude the image_url column
  gt() %>%
  text_transform(
    locations = cells_body(columns = "team_player"),
    fn = function(x) {
      # Combine team_player name with the image from image_url
      paste0(
        web_image(url = combined_teams_data$image_url, height = 30),
        " ", x
      )
    }
  ) %>%
  cols_label(
    rank = "Rank",
    team_player = "Team/Player",
    total_carries = "Total Carries",
    total_yards = "Total Yards",
    total_tds = "Total TDs",
    total_games = "Total Games",
    unique_rushers = "Unique Rushers",
    YPC = "Yards Per Carry",
    YPG = "Yards Per Game",
    Yards_Per_Rusher = "Yards Per Rusher"
  ) %>%
  fmt_number(
    columns = c("YPC", "YPG", "Yards_Per_Rusher"),  # Format columns to 2 decimal places
    decimals = 2
  ) %>%
  tab_header(
    title = md("**ASHTON JEANTY vs THE FBS:**"),
    subtitle = md("FBS Rushing Breakdown, 2024")
  ) %>%
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    table.border.left.color = "black",
    table.border.right.color = "black",
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    table.border.left.width = px(2),
    table.border.right.width = px(2),
    table.width = pct(100),  # Set table width to 100%
    container.width = pct(100),  # Set container width to 100%
    container.padding.x = px(20),  # Horizontal padding around the table
    container.padding.y = px(20)   # Vertical padding around the table
  ) %>%
  data_color(
    columns = c("YPC", "YPG", "Yards_Per_Rusher"),  # Only color these columns
    colors = scales::col_numeric(
      palette = c("white", "darkgreen"),
      domain = NULL  # Automatically scales the color gradient based on each column's values
    )
  )



print(table_plot)
