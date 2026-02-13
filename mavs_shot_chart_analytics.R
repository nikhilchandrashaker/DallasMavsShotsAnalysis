# ============================================================================
# Dallas Mavericks Shot Chart Analytics System
# Shooting Efficiency by Court Zone + Defensive Points Allowed Analysis
# Season: 2024-2025
# ============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(gridExtra)
library(scales)
library(knitr)

# Set seed for reproducibility
set.seed(2024)

# ============================================================================
# PART 1: DEFINE COURT ZONES
# ============================================================================

# 9 distinct court zones based on NBA shot chart data
court_zones <- data.frame(
  zone_id = 1:9,
  zone_name = c(
    "Restricted Area",           # 0-4 feet from basket
    "Paint (Non-RA)",           # 4-8 feet from basket
    "Short Mid-Range",          # 8-16 feet
    "Long Mid-Range",           # 16 feet to 3PT line
    "Corner 3 (Left)",          # Left corner 3PT
    "Corner 3 (Right)",         # Right corner 3PT
    "Wing 3 (Left)",            # Left wing 3PT
    "Wing 3 (Right)",           # Right wing 3PT
    "Top of Key 3"              # Above the break 3PT
  ),
  zone_type = c(
    "Paint", "Paint", "Mid-Range", "Mid-Range",
    "Three-Point", "Three-Point", "Three-Point", "Three-Point", "Three-Point"
  ),
  typical_fg_pct = c(
    0.68, 0.48, 0.42, 0.40, 0.39, 0.39, 0.37, 0.37, 0.36
  ),
  typical_efg_pct = c(
    0.68, 0.48, 0.42, 0.40, 0.585, 0.585, 0.555, 0.555, 0.54
  ),
  stringsAsFactors = FALSE
)

cat("Court zones defined successfully!\n")
print(kable(court_zones, format = "simple", digits = 3))

# ============================================================================
# PART 2: DALLAS MAVERICKS ROSTER & SHOOTING PROFILES
# ============================================================================

# 15 core rotation players (2024-25 season)
mavs_players <- data.frame(
  player_id = 1:15,
  player_name = c(
    # Stars
    "Luka Doncic", "Kyrie Irving", "Klay Thompson",
    # Key Rotation
    "PJ Washington", "Daniel Gafford", "Dereck Lively II",
    "Naji Marshall", "Quentin Grimes", "Maxi Kleber",
    # Role Players
    "Jaden Hardy", "Spencer Dinwiddie", "Dwight Powell",
    "Olivier-Maxence Prosper", "AJ Lawson", "Markieff Morris"
  ),
  position = c(
    "PG", "PG", "SG",
    "PF", "C", "C",
    "SF", "SG", "PF",
    "SG", "PG", "C",
    "SF", "SG", "PF"
  ),
  role = c(
    "Star", "Star", "Star",
    "Starter", "Starter", "Starter",
    "Rotation", "Rotation", "Rotation",
    "Bench", "Bench", "Bench",
    "Deep Bench", "Deep Bench", "Deep Bench"
  ),
  stringsAsFactors = FALSE
)

cat("\n\nDallas Mavericks Roster Loaded!\n")
cat(sprintf("Total Players: %d\n", nrow(mavs_players)))

# ============================================================================
# PART 3: GENERATE SHOOTING DATA BY ZONE
# ============================================================================

# Function to generate realistic FG% by zone based on player archetype
generate_zone_shooting <- function(player_name, position, role, zone_id, zone_name, zone_type) {
  
  # Base shooting ability by role
  base_skill <- case_when(
    role == "Star" ~ runif(1, 0.05, 0.15),
    role == "Starter" ~ runif(1, 0.00, 0.08),
    role == "Rotation" ~ runif(1, -0.03, 0.05),
    role == "Bench" ~ runif(1, -0.05, 0.03),
    TRUE ~ runif(1, -0.08, 0.00)
  )
  
  # Position-specific modifiers
  position_mod <- case_when(
    # Guards better at 3PT, worse in paint
    position %in% c("PG", "SG") & zone_type == "Three-Point" ~ 0.03,
    position %in% c("PG", "SG") & zone_type == "Paint" ~ -0.05,
    # Centers better in paint, worse at 3PT
    position == "C" & zone_type == "Paint" ~ 0.08,
    position == "C" & zone_type == "Three-Point" ~ -0.15,
    # Forwards balanced
    position %in% c("SF", "PF") & zone_type == "Mid-Range" ~ 0.02,
    TRUE ~ 0
  )
  
  # Player-specific adjustments (stars get bonuses)
  player_mod <- case_when(
    player_name == "Luka Doncic" & zone_name %in% c("Top of Key 3", "Wing 3 (Left)") ~ 0.06,
    player_name == "Luka Doncic" & zone_type == "Paint" ~ 0.05,
    player_name == "Kyrie Irving" & zone_type == "Mid-Range" ~ 0.08,
    player_name == "Kyrie Irving" & zone_name == "Restricted Area" ~ 0.12,
    player_name == "Klay Thompson" & zone_type == "Three-Point" ~ 0.08,
    player_name == "Daniel Gafford" & zone_name == "Restricted Area" ~ 0.15,
    player_name == "Dereck Lively II" & zone_name == "Restricted Area" ~ 0.12,
    player_name == "PJ Washington" & zone_name %in% c("Corner 3 (Left)", "Corner 3 (Right)") ~ 0.05,
    TRUE ~ 0
  )
  
  # Get baseline for this zone
  baseline <- court_zones$typical_fg_pct[court_zones$zone_id == zone_id]
  
  # Calculate final FG%
  final_fg_pct <- baseline + base_skill + position_mod + player_mod
  
  # Ensure realistic bounds
  final_fg_pct <- max(0.15, min(0.85, final_fg_pct))
  
  return(final_fg_pct)
}

# Generate shot attempts by zone
generate_shot_volume <- function(player_name, role, zone_name, zone_type) {
  
  # Base volume by role
  base_volume <- case_when(
    role == "Star" ~ 150,
    role == "Starter" ~ 80,
    role == "Rotation" ~ 50,
    role == "Bench" ~ 30,
    TRUE ~ 15
  )
  
  # Zone popularity (restricted area gets most attempts)
  zone_multiplier <- case_when(
    zone_name == "Restricted Area" ~ 1.5,
    zone_name == "Paint (Non-RA)" ~ 0.8,
    zone_type == "Three-Point" ~ 1.2,
    zone_type == "Mid-Range" ~ 0.4,  # Mid-range less popular in modern NBA
    TRUE ~ 0.5
  )
  
  # Player-specific volume
  if (player_name == "Luka Doncic" && zone_name == "Top of Key 3") zone_multiplier <- zone_multiplier * 1.5
  if (player_name == "Klay Thompson" && zone_type == "Three-Point") zone_multiplier <- zone_multiplier * 1.3
  if (player_name %in% c("Daniel Gafford", "Dereck Lively II") && zone_type == "Paint") zone_multiplier <- zone_multiplier * 2
  
  attempts <- round(base_volume * zone_multiplier * runif(1, 0.7, 1.3))
  attempts <- max(5, attempts)  # Minimum 5 attempts per zone
  
  return(attempts)
}

# Create complete shooting dataset
cat("\n\nGenerating shooting data for all players across all zones...\n")

shooting_data <- expand.grid(
  player_id = mavs_players$player_id,
  zone_id = court_zones$zone_id,
  stringsAsFactors = FALSE
) %>%
  left_join(mavs_players, by = "player_id") %>%
  left_join(court_zones, by = "zone_id") %>%
  rowwise() %>%
  mutate(
    fg_pct = generate_zone_shooting(player_name, position, role, zone_id, zone_name, zone_type),
    attempts = generate_shot_volume(player_name, role, zone_name, zone_type),
    makes = round(attempts * fg_pct),
    points_per_shot = case_when(
      zone_type == "Three-Point" ~ fg_pct * 3,
      TRUE ~ fg_pct * 2
    ),
    efg_pct = case_when(
      zone_type == "Three-Point" ~ (makes * 1.5) / attempts,
      TRUE ~ fg_pct
    )
  ) %>%
  ungroup()

cat(sprintf("Generated %d player-zone combinations!\n", nrow(shooting_data)))

# ============================================================================
# PART 4: OPPONENT DEFENDERS ANALYSIS
# ============================================================================

# Top 20 defenders from opponent teams that Mavs face
defenders <- data.frame(
  defender_id = 1:20,
  defender_name = c(
    # Elite Defenders
    "Anthony Davis", "Rudy Gobert", "Bam Adebayo", "Jaren Jackson Jr.",
    "Draymond Green", "OG Anunoby", "Jrue Holiday", "Alex Caruso",
    # Good Defenders
    "Kawhi Leonard", "Paul George", "Derrick White", "Herb Jones",
    "Lu Dort", "Matisse Thybulle", "Jarrett Allen", "Robert Williams",
    # Solid Defenders
    "Jaden McDaniels", "Dillon Brooks", "Patrick Beverley", "Marcus Smart"
  ),
  team = c(
    "Lakers", "Timberwolves", "Heat", "Grizzlies",
    "Warriors", "Knicks", "Celtics", "Thunder",
    "Clippers", "76ers", "Celtics", "Pelicans",
    "Thunder", "Trail Blazers", "Cavaliers", "Trail Blazers",
    "Timberwolves", "Rockets", "76ers", "Grizzlies"
  ),
  position = c(
    "C", "C", "C", "C",
    "PF", "SF", "SG", "SG",
    "SF", "SF", "SG", "SF",
    "SG", "SF", "C", "C",
    "SF", "SF", "PG", "PG"
  ),
  defensive_tier = c(
    "Elite", "Elite", "Elite", "Elite",
    "Elite", "Elite", "Elite", "Elite",
    "Good", "Good", "Good", "Good",
    "Good", "Good", "Good", "Good",
    "Solid", "Solid", "Solid", "Solid"
  ),
  stringsAsFactors = FALSE
)

# Function to generate defensive FG% allowed by zone
generate_defensive_impact <- function(defender_name, position, tier, zone_name, zone_type) {
  
  # Base defensive impact by tier (negative = better defense)
  base_impact <- case_when(
    tier == "Elite" ~ runif(1, -0.08, -0.04),
    tier == "Good" ~ runif(1, -0.04, -0.01),
    tier == "Solid" ~ runif(1, -0.02, 0.01),
    TRUE ~ runif(1, 0.00, 0.03)
  )
  
  # Position-specific rim protection
  position_mod <- case_when(
    position == "C" & zone_name == "Restricted Area" ~ -0.10,
    position == "C" & zone_name == "Paint (Non-RA)" ~ -0.06,
    position %in% c("SF", "PF") & zone_type == "Paint" ~ -0.03,
    position %in% c("PG", "SG") & zone_type == "Three-Point" ~ -0.02,
    TRUE ~ 0
  )
  
  # Defender-specific strengths
  defender_mod <- case_when(
    defender_name == "Rudy Gobert" & zone_type == "Paint" ~ -0.12,
    defender_name == "Anthony Davis" & zone_type == "Paint" ~ -0.10,
    defender_name == "Bam Adebayo" & zone_type == "Paint" ~ -0.08,
    defender_name == "Jrue Holiday" & zone_type == "Three-Point" ~ -0.05,
    defender_name == "OG Anunoby" & zone_type == "Three-Point" ~ -0.04,
    defender_name == "Alex Caruso" & zone_name %in% c("Wing 3 (Left)", "Wing 3 (Right)") ~ -0.06,
    TRUE ~ 0
  )
  
  total_impact <- base_impact + position_mod + defender_mod
  
  # Get baseline
  baseline <- court_zones$typical_fg_pct[court_zones$zone_name == zone_name]
  
  # FG% allowed
  fg_pct_allowed <- baseline + total_impact
  fg_pct_allowed <- max(0.20, min(0.75, fg_pct_allowed))
  
  return(fg_pct_allowed)
}

# Generate defensive data
cat("\nGenerating defensive data for opponent players...\n")

defensive_data <- expand.grid(
  defender_id = defenders$defender_id,
  zone_id = court_zones$zone_id,
  stringsAsFactors = FALSE
) %>%
  left_join(defenders, by = "defender_id") %>%
  left_join(court_zones, by = "zone_id") %>%
  rowwise() %>%
  mutate(
    fg_pct_allowed = generate_defensive_impact(defender_name, position, defensive_tier, zone_name, zone_type),
    possessions = case_when(
      defensive_tier == "Elite" ~ sample(150:250, 1),
      defensive_tier == "Good" ~ sample(120:200, 1),
      TRUE ~ sample(80:150, 1)
    ),
    points_allowed = case_when(
      zone_type == "Three-Point" ~ fg_pct_allowed * possessions * 3,
      TRUE ~ fg_pct_allowed * possessions * 2
    ),
    defensive_rating = 100 - (fg_pct_allowed * 100)  # Higher = better defense
  ) %>%
  ungroup()

cat(sprintf("Generated %d defender-zone combinations!\n", nrow(defensive_data)))

# ============================================================================
# PART 5: ANALYSIS & QUERY FUNCTIONS
# ============================================================================

# Function to query player shooting by zone
query_player_zones <- function(player) {
  cat("\n=================================================================\n")
  cat(sprintf("SHOOTING PROFILE: %s\n", player))
  cat("=================================================================\n\n")
  
  player_stats <- shooting_data %>%
    filter(player_name == player) %>%
    arrange(desc(fg_pct)) %>%
    select(zone_name, attempts, makes, fg_pct, points_per_shot, efg_pct)
  
  print(kable(player_stats, format = "simple", digits = 3,
              col.names = c("Zone", "FGA", "FGM", "FG%", "PPS", "eFG%")))
  
  # Best zones
  best_zones <- player_stats %>% head(3)
  cat("\nBEST ZONES:\n")
  for(i in 1:nrow(best_zones)) {
    cat(sprintf("  %d. %s: %.1f%% on %d attempts\n", 
                i, best_zones$zone_name[i], best_zones$fg_pct[i]*100, best_zones$attempts[i]))
  }
  
  # Worst zones
  worst_zones <- player_stats %>% tail(3) %>% arrange(fg_pct)
  cat("\nWEAK ZONES:\n")
  for(i in 1:nrow(worst_zones)) {
    cat(sprintf("  %d. %s: %.1f%% on %d attempts\n", 
                i, worst_zones$zone_name[i], worst_zones$fg_pct[i]*100, worst_zones$attempts[i]))
  }
}

# Function to query zone across all players
query_zone_rankings <- function(zone) {
  cat("\n=================================================================\n")
  cat(sprintf("ZONE RANKINGS: %s\n", zone))
  cat("=================================================================\n\n")
  
  zone_stats <- shooting_data %>%
    filter(zone_name == zone) %>%
    arrange(desc(fg_pct)) %>%
    select(player_name, position, role, attempts, fg_pct, points_per_shot) %>%
    head(10)
  
  print(kable(zone_stats, format = "simple", digits = 3,
              col.names = c("Player", "Pos", "Role", "FGA", "FG%", "PPS")))
}

# Function to compare offense vs defense
compare_offense_defense <- function(mavs_player, defender) {
  cat("\n=================================================================\n")
  cat(sprintf("MATCHUP: %s (DAL) vs %s\n", mavs_player, defender))
  cat("=================================================================\n\n")
  
  offense <- shooting_data %>%
    filter(player_name == mavs_player) %>%
    select(zone_name, fg_pct, points_per_shot)
  
  defense <- defensive_data %>%
    filter(defender_name == defender) %>%
    select(zone_name, fg_pct_allowed, defensive_rating)
  
  matchup <- offense %>%
    left_join(defense, by = "zone_name") %>%
    mutate(
      expected_fg_pct = (fg_pct + fg_pct_allowed) / 2,
      advantage = fg_pct - fg_pct_allowed
    ) %>%
    arrange(desc(advantage))
  
  print(kable(matchup, format = "simple", digits = 3,
              col.names = c("Zone", "Mav FG%", "PPS", "Def FG% Allow", "Def Rating", "Expected FG%", "Advantage")))
  
  cat("\nKEY INSIGHTS:\n")
  cat(sprintf("Best matchup zones for %s:\n", mavs_player))
  top_3 <- matchup %>% head(3)
  for(i in 1:3) {
    cat(sprintf("  %s: +%.1f%% advantage\n", top_3$zone_name[i], top_3$advantage[i]*100))
  }
}

# ============================================================================
# PART 6: COMPREHENSIVE ANALYSIS
# ============================================================================

cat("\n\n=================================================================\n")
cat("DALLAS MAVERICKS SHOT CHART ANALYTICS SUMMARY\n")
cat("=================================================================\n\n")

# Overall team shooting by zone
cat("TEAM SHOOTING BY ZONE\n")
cat("---------------------\n")
team_zones <- shooting_data %>%
  group_by(zone_name, zone_type) %>%
  summarise(
    total_attempts = sum(attempts),
    total_makes = sum(makes),
    team_fg_pct = total_makes / total_attempts,
    avg_pps = mean(points_per_shot),
    .groups = "drop"
  ) %>%
  arrange(desc(team_fg_pct))

print(kable(team_zones, format = "simple", digits = 3,
            col.names = c("Zone", "Type", "FGA", "FGM", "FG%", "PPS")))

# Best shooters by zone type
cat("\n\nBEST SHOOTERS BY ZONE TYPE\n")
cat("--------------------------\n")

for(z_type in unique(court_zones$zone_type)) {
  cat(sprintf("\n%s:\n", z_type))
  top_shooters <- shooting_data %>%
    filter(zone_type == z_type) %>%
    group_by(player_name) %>%
    summarise(
      total_attempts = sum(attempts),
      avg_fg_pct = weighted.mean(fg_pct, attempts),
      .groups = "drop"
    ) %>%
    filter(total_attempts >= 50) %>%
    arrange(desc(avg_fg_pct)) %>%
    head(5)
  
  for(i in 1:nrow(top_shooters)) {
    cat(sprintf("  %d. %s: %.1f%% (%d attempts)\n", 
                i, top_shooters$player_name[i], 
                top_shooters$avg_fg_pct[i]*100, 
                top_shooters$total_attempts[i]))
  }
}

# Best defenders by zone
cat("\n\nBEST DEFENDERS BY ZONE\n")
cat("----------------------\n")
best_defenders <- defensive_data %>%
  group_by(defender_name, team) %>%
  summarise(
    avg_fg_allowed = weighted.mean(fg_pct_allowed, possessions),
    total_possessions = sum(possessions),
    .groups = "drop"
  ) %>%
  arrange(avg_fg_allowed) %>%
  head(10)

print(kable(best_defenders, format = "simple", digits = 3,
            col.names = c("Defender", "Team", "Avg FG% Allowed", "Possessions")))

# ============================================================================
# PART 7: VISUALIZATIONS
# ============================================================================

cat("\n\nGenerating visualizations...\n")

# Visualization 1: Team Heat Map by Zone
zone_summary <- shooting_data %>%
  group_by(zone_name) %>%
  summarise(avg_fg_pct = mean(fg_pct)) %>%
  mutate(zone_rank = rank(-avg_fg_pct))

p1 <- ggplot(zone_summary, aes(x = reorder(zone_name, avg_fg_pct), y = avg_fg_pct, fill = avg_fg_pct)) +
  geom_col() +
  scale_fill_viridis(option = "plasma", direction = -1) +
  coord_flip() +
  labs(title = "Dallas Mavericks: Average FG% by Court Zone",
       subtitle = "2024-25 Season",
       x = "Court Zone",
       y = "Average Field Goal %",
       fill = "FG%") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Visualization 2: Luka's Shot Chart
luka_zones <- shooting_data %>%
  filter(player_name == "Luka Doncic") %>%
  arrange(desc(fg_pct))

p2 <- ggplot(luka_zones, aes(x = reorder(zone_name, fg_pct), y = fg_pct, fill = zone_type)) +
  geom_col() +
  scale_fill_manual(values = c("Paint" = "#00538C", "Mid-Range" = "#B8C4CA", "Three-Point" = "#002B5E")) +
  coord_flip() +
  geom_hline(yintercept = 0.45, linetype = "dashed", color = "red") +
  labs(title = "Luka Doncic: Field Goal % by Zone",
       subtitle = "Dashed line = league average",
       x = "Court Zone",
       y = "Field Goal %",
       fill = "Zone Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Visualization 3: Paint vs 3PT Specialists
paint_3pt <- shooting_data %>%
  group_by(player_name, zone_type) %>%
  summarise(avg_fg = weighted.mean(fg_pct, attempts), .groups = "drop") %>%
  filter(zone_type %in% c("Paint", "Three-Point")) %>%
  pivot_wider(names_from = zone_type, values_from = avg_fg) %>%
  filter(!is.na(Paint) & !is.na(`Three-Point`))

p3 <- ggplot(paint_3pt, aes(x = `Three-Point`, y = Paint)) +
  geom_point(size = 4, alpha = 0.7, color = "#00538C") +
  geom_text(aes(label = player_name), vjust = -0.8, size = 3) +
  geom_vline(xintercept = 0.36, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0.55, linetype = "dashed", color = "gray") +
  labs(title = "Mavericks: Paint vs 3-Point Shooting",
       subtitle = "Dashed lines = league averages",
       x = "Three-Point FG%",
       y = "Paint FG%") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Visualization 4: Defensive Heat Map
def_summary <- defensive_data %>%
  group_by(zone_name) %>%
  summarise(avg_fg_allowed = mean(fg_pct_allowed)) %>%
  arrange(avg_fg_allowed)

p4 <- ggplot(def_summary, aes(x = reorder(zone_name, -avg_fg_allowed), y = avg_fg_allowed, fill = avg_fg_allowed)) +
  geom_col() +
  scale_fill_viridis(option = "magma", direction = 1) +
  coord_flip() +
  labs(title = "Opponent Defenders: FG% Allowed by Zone",
       subtitle = "Lower = Better Defense",
       x = "Court Zone",
       y = "Average FG% Allowed",
       fill = "FG% Allowed") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Visualization 5: Top 5 Players Comparison
top_5_players <- c("Luka Doncic", "Kyrie Irving", "Klay Thompson", "PJ Washington", "Daniel Gafford")
top_5_data <- shooting_data %>%
  filter(player_name %in% top_5_players) %>%
  group_by(player_name, zone_type) %>%
  summarise(avg_fg = weighted.mean(fg_pct, attempts), .groups = "drop")

p5 <- ggplot(top_5_data, aes(x = zone_type, y = avg_fg, fill = player_name)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#00538C", "#002B5E", "#B8C4CA", "#8B9DC3", "#DCDCDC")) +
  labs(title = "Top 5 Mavericks: FG% by Zone Type",
       x = "Zone Type",
       y = "Average Field Goal %",
       fill = "Player") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization 6: Volume vs Efficiency
player_summary <- shooting_data %>%
  group_by(player_name) %>%
  summarise(
    total_attempts = sum(attempts),
    avg_fg_pct = weighted.mean(fg_pct, attempts),
    .groups = "drop"
  )

p6 <- ggplot(player_summary, aes(x = total_attempts, y = avg_fg_pct)) +
  geom_point(size = 4, alpha = 0.7, color = "#00538C") +
  geom_text(aes(label = player_name), vjust = -0.8, size = 2.5) +
  labs(title = "Mavericks: Shot Volume vs Efficiency",
       x = "Total Shot Attempts",
       y = "Average FG%") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Save plots
ggsave("mavs_team_zones_heatmap.png", p1, width = 10, height = 6, dpi = 300)
ggsave("mavs_luka_shot_chart.png", p2, width = 10, height = 6, dpi = 300)
ggsave("mavs_paint_vs_3pt.png", p3, width = 10, height = 6, dpi = 300)
ggsave("mavs_defense_heatmap.png", p4, width = 10, height = 6, dpi = 300)
ggsave("mavs_top5_comparison.png", p5, width = 12, height = 6, dpi = 300)
ggsave("mavs_volume_efficiency.png", p6, width = 10, height = 6, dpi = 300)

# Create dashboard
pdf("mavs_shot_chart_dashboard.pdf", width = 16, height = 12)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)
dev.off()

cat("Visualizations saved!\n")

# ============================================================================
# PART 8: EXPORT DATA
# ============================================================================

# Save datasets
write.csv(shooting_data, "mavs_shooting_by_zone.csv", row.names = FALSE)
write.csv(defensive_data, "opponent_defense_by_zone.csv", row.names = FALSE)
write.csv(court_zones, "court_zone_definitions.csv", row.names = FALSE)

cat("\nDatasets saved!\n")

# ============================================================================
# PART 9: EXAMPLE QUERIES
# ============================================================================

cat("\n\n=================================================================\n")
cat("RUNNING EXAMPLE QUERIES\n")
cat("=================================================================\n")

# Query Luka's zones
query_player_zones("Luka Doncic")

# Query restricted area
query_zone_rankings("Restricted Area")

# Query corner 3s
query_zone_rankings("Corner 3 (Right)")

# Compare matchup
compare_offense_defense("Luka Doncic", "Rudy Gobert")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n\n=================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("=================================================================\n")
cat("\nGenerated Files:\n")
cat("  1. mavs_shooting_by_zone.csv - Complete shooting data\n")
cat("  2. opponent_defense_by_zone.csv - Defensive data\n")
cat("  3. court_zone_definitions.csv - Zone reference\n")
cat("  4. mavs_team_zones_heatmap.png - Team performance by zone\n")
cat("  5. mavs_luka_shot_chart.png - Luka's zone breakdown\n")
cat("  6. mavs_paint_vs_3pt.png - Paint vs 3PT scatter\n")
cat("  7. mavs_defense_heatmap.png - Defensive zones\n")
cat("  8. mavs_top5_comparison.png - Top 5 players\n")
cat("  9. mavs_volume_efficiency.png - Volume vs efficiency\n")
cat(" 10. mavs_shot_chart_dashboard.pdf - Complete dashboard\n")
cat("\n=================================================================\n")
cat("\nQUERY FUNCTIONS AVAILABLE:\n")
cat("  query_player_zones('Player Name')  - See player's zone breakdown\n")
cat("  query_zone_rankings('Zone Name')   - See best shooters in zone\n")
cat("  compare_offense_defense('Mav Player', 'Defender') - Matchup analysis\n")
cat("\n=================================================================\n")
