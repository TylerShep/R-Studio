library(cfbfastR)
library(dplyr)
library(ggplot2)
library(networkD3)
library(stringr)
library(tidyr)

# GET PBP DATA -> RB DATA
pbp_data <- cfbfastR::load_cfb_pbp(2024)

pbp_filtered <- pbp_data %>%
  filter(pos_team == "Boise State", down != 0) %>%
  mutate(
    drive_down = paste(down, "Down", sep = " "),
    player = rusher_player_name,  # Keep all player names
    yards_category = case_when(
      yards_gained <= 0 ~ "0 or less yds",
      yards_gained >= 1 & yards_gained <= 5 ~ "1-5 yds",
      yards_gained >= 6 & yards_gained <= 10 ~ "6-10 yds",
      yards_gained >= 11 & yards_gained <= 20 ~ "11-20 yds",
      yards_gained >= 21 & yards_gained <= 50 ~ "21-50 yds",
      yards_gained > 50 ~ "50+ yds"
    ),
    play_result = case_when(scoring == 1 ~ "Touchdown",
                             firstD_by_yards == 1 ~ "First Down",
                             scoring == 0 & firstD_by_yards == 0 ~ "Next Play")
    )

# Prepare data for Sankey diagram with new structure
nodes <- data.frame(name = unique(c(
  unique(pbp_filtered$player),
  unique(pbp_filtered$drive_down),
  unique(pbp_filtered$yards_category),
  unique(pbp_filtered$play_result)
)))

# Add a new column to mark nodes related to "Ashton Jeanty"
nodes$color_group <- ifelse(nodes$name == "Ashton Jeanty", "blue", "grey")

# Map each node to an index
node_indices <- setNames(1:nrow(nodes), nodes$name)

# Create links for Sankey diagram in the new structure
links_stage1 <- pbp_filtered %>%
  mutate(
    Source = node_indices[player],
    Target = node_indices[drive_down]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage2 <- pbp_filtered %>%
  mutate(
    Source = node_indices[drive_down],
    Target = node_indices[yards_category]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage3 <- pbp_filtered %>%
  mutate(
    Source = node_indices[yards_category],
    Target = node_indices[play_result]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

# Combine all stages into one links data frame and convert to plain data frame
links_sankey <- bind_rows(links_stage1, links_stage2, links_stage3) %>%
  as.data.frame()

# Ensure that Source and Target columns are integers and zero-indexed
links_sankey$Source <- as.integer(links_sankey$Source - 1)
links_sankey$Target <- as.integer(links_sankey$Target - 1)

# Add a new column to color links related to "Ashton Jeanty" in any connection
links_sankey$color_group <- ifelse(
  nodes$name[links_sankey$Source + 1] == "Ashton Jeanty" | 
    nodes$name[links_sankey$Target + 1] == "Ashton Jeanty" | 
    nodes$name[links_sankey$Source + 1] %in% pbp_filtered$player[pbp_filtered$rusher_player_name == "Ashton Jeanty"] |
    nodes$name[links_sankey$Target + 1] %in% pbp_filtered$yards_category[pbp_filtered$rusher_player_name == "Ashton Jeanty"] |
    nodes$name[links_sankey$Target + 1] %in% pbp_filtered$play_result[pbp_filtered$rusher_player_name == "Ashton Jeanty"],
  "blue",
  "grey"
)

# Debugging: Identify any remaining mismatches in sources and targets
missing_sources <- setdiff(links_sankey$Source, 0:(nrow(nodes) - 1))
missing_targets <- setdiff(links_sankey$Target, 0:(nrow(nodes) - 1))

if (length(missing_sources) > 0 || length(missing_targets) > 0) {
  print("There are still mismatches in the Source or Target indices!")
  print(paste("Missing Sources:", toString(missing_sources)))
  print(paste("Missing Targets:", toString(missing_targets)))
} else {
  print("All Sources and Targets are now valid.")
}

# Create a color scale using JavaScript for nodes and links based on the 'color_group' column
color_scale <- 'd3.scaleOrdinal()
  .domain(["blue", "grey"])
  .range(["#00008B", "#D3D3D3"])'

# Create the Sankey diagram
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
  NodeGroup = 'color_group',  # Use 'color_group' to set the color for nodes
  LinkGroup = 'color_group'   # Use 'color_group' to set the color for links
)
