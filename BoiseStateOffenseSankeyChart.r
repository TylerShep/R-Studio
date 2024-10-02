library(cfbfastR)
library(dplyr)
library(ggplot2)
library(networkD3)
library(stringr)
library(tidyr)


#GET PBP DATA -> RB DATA
pbp_data <- cfbfastR::load_cfb_pbp(2024)

pbp_filtered <- pbp_data %>%
  filter(pos_team == "Boise State") %>%
  mutate(
    drive_down = paste(down, "Down", sep = " "),
    player = ifelse(str_detect(rusher_player_name, "Ashton Jeanty"), "Ashton Jeanty", "Other Player"),
    yards_category = case_when(
      yards_gained >= 1 & yards_gained <= 5 ~ "1-5",
      yards_gained >= 6 & yards_gained <= 10 ~ "6-10",
      yards_gained >= 11 & yards_gained <= 20 ~ "11-20",
      yards_gained >= 21 & yards_gained <= 50 ~ "21-50",
      yards_gained > 50 ~ "50+"
    ),
    play_result = ifelse(str_detect(play_type, "Touchdown"), "TD", "First Down")
  )

# Prepare data for Sankey diagram
nodes <- data.frame(name = unique(c(
  "BSU Offense",
  unique(pbp_filtered$drive_down),
  unique(pbp_filtered$player),
  unique(pbp_filtered$yards_category),
  unique(pbp_filtered$play_result)
)))

# Map each node to an index
node_indices <- setNames(1:nrow(nodes), nodes$name)

# Create links for Sankey diagram in separate stages with additional checks
links_stage1 <- pbp_filtered %>%
  mutate(
    Source = node_indices["BSU Offense"],
    Target = node_indices[drive_down]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%  # Remove rows with NA indices
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage2 <- pbp_filtered %>%
  mutate(
    Source = node_indices[drive_down],
    Target = node_indices[player]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%  # Remove rows with NA indices
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage3 <- pbp_filtered %>%
  mutate(
    Source = node_indices[player],
    Target = node_indices[yards_category]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%  # Remove rows with NA indices
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

links_stage4 <- pbp_filtered %>%
  mutate(
    Source = node_indices[yards_category],
    Target = node_indices[play_result]
  ) %>%
  filter(!is.na(Source) & !is.na(Target)) %>%  # Remove rows with NA indices
  group_by(Source, Target) %>%
  summarise(Value = n(), .groups = 'drop')

# Combine all stages into one links data frame and convert to plain data frame
links_sankey <- bind_rows(links_stage1, links_stage2, links_stage3, links_stage4) %>%
  as.data.frame()

# Ensure that Source and Target columns are integers and zero-indexed
links_sankey$Source <- as.integer(links_sankey$Source - 1)
links_sankey$Target <- as.integer(links_sankey$Target - 1)

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

# Create a color scale to highlight Ashton Jeanty in dark blue and others in light grey
color_scale <- 'd3.scaleOrdinal() .domain(["Ashton Jeanty", "Other Player"]) .range(["#00008B", "#D3D3D3"])'


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
  nodePadding = 20
)
