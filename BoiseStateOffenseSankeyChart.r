library(cfbfastR)
library(dplyr)
library(ggplot2)
library(networkD3)
library(stringr)
library(tidyr)



# GET PBP DATA -> RB DATA
pbp_data <- cfbfastR::load_cfb_pbp(2024)

pbp_filtered <- pbp_data %>%
  filter(pos_team == "Boise State") %>%
  mutate(
    drive_down = paste(down, "Down", sep = " "),
    player = rusher_player_name,  # Keep all player names
    yards_category = case_when(
      yards_gained >= 1 & yards_gained <= 5 ~ "1-5",
      yards_gained >= 6 & yards_gained <= 10 ~ "6-10",
      yards_gained >= 11 & yards_gained <= 20 ~ "11-20",
      yards_gained >= 21 & yards_gained <= 50 ~ "21-50",
      yards_gained > 50 ~ "50+"
    ),
    play_result = ifelse(str_detect(play_type, "Touchdown"), "TD", "First Down")
  )

# NODE MAPPING
nodes <- data.frame(name = unique(c(
  "BSU Offense",
  unique(pbp_filtered$drive_down),
  unique(pbp_filtered$player),
  unique(pbp_filtered$yards_category),
  unique(pbp_filtered$play_result)
)))

nodes$color_group <- ifelse(nodes$name == "Ashton Jeanty", "blue", "grey")
node_indices <- setNames(1:nrow(nodes), nodes$name)

# LINKS
links_stage1 <- pbp_filtered %>%
  mutate(
    Source = node_indices["BSU Offense"],
    Target = node_indices[drive_down]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage2 <- pbp_filtered %>%
  mutate(
    Source = node_indices[drive_down],
    Target = node_indices[player]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage3 <- pbp_filtered %>%
  mutate(
    Source = node_indices[player],
    Target = node_indices[yards_category]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage4 <- pbp_filtered %>%
  mutate(
    Source = node_indices[yards_category],
    Target = node_indices[play_result]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_sankey <- bind_rows(links_stage1, links_stage2, links_stage3, links_stage4) %>%
  as.data.frame()


#DEBUGGING DATA
links_sankey$Source <- as.integer(links_sankey$Source - 1)
links_sankey$Target <- as.integer(links_sankey$Target - 1)
missing_sources <- setdiff(links_sankey$Source, 0:(nrow(nodes) - 1))
missing_targets <- setdiff(links_sankey$Target, 0:(nrow(nodes) - 1))

if (length(missing_sources) > 0 || length(missing_targets) > 0) {
  print("There are still mismatches in the Source or Target indices!")
  print(paste("Missing Sources:", toString(missing_sources)))
  print(paste("Missing Targets:", toString(missing_targets)))
} else {
  print("All Sources and Targets are now valid.")
}

#STYLING
color_scale <- 'd3.scaleOrdinal()
  .domain(["blue", "grey"])
  .range(["#00008B", "#D3D3D3"])'

# CREATE SANKEY DIAGRAM
sankeyNetwork(
  Links = links_sankey,
  Nodes = nodes,
  Source = 'Source',
  Target = 'Target',
  Value = 'Value',
  NodeID = 'name',
  colourScale = color_scale,
  fontSize = 12,
  nodeWidth = 40,
  nodePadding = 20,
  NodeGroup = 'color_group'
)
