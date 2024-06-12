## Liam Jennings
## EDA Project: WNBA Shooting

## Starter Code
library(tidyverse)
# install.packages("wehoop")
library(wehoop)
wnba_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/wnba_shots.csv")



## load in the library
library(sportyR)

## demo basketball court
sportyR::geom_basketball("WNBA")

## filter out all-star teams
wnba_shots <- wnba_shots |> 
  slice(-grep("^Team", shooting_team))

## Test Data (Wilson, Ionescu, LVA)
### Wilson
wilson <- wnba_shots |> 
  filter(game_type == 2 & shooting_team == "Las Vegas" & abs(coordinate_x) >= 10 & str_like(desc, "A'ja Wilson%")) |> 
  mutate(coordinate_x = abs(coordinate_x),
         player = str_extract(desc, "A'ja Wilson")) |> 
  slice(-grep("^Free Throw", shot_type))

### Ionescu
ionescu_point <- wnba_shots |> 
  filter(game_type == 2 & shooting_team == "New York" 
         & str_like(desc, "Sabrina Ionescu%") & abs(coordinate_x) >= 10) |> 
  mutate(coordinate_x_abs = abs(coordinate_x),
         player = str_extract(desc, "Sabrina Ionescu")) |> 
  slice(-grep("^Free Throw", shot_type))

### Las Vegas Aces
LVA <- wnba_shots |> 
  filter(game_type == 2 & shooting_team == "Las Vegas" & abs(coordinate_x) >= 10) |> 
  mutate(coordinate_x_abs = abs(coordinate_x)) |> 
  slice(-grep("^Free Throw", shot_type))

### New York Liberty 
NYL <- wnba_shots |> 
  filter(game_type == 2 & shooting_team == "New York" & abs(coordinate_x) >= 10) |> 
  mutate(coordinate_x_abs = abs(coordinate_x)) |> 
  slice(-grep("^Free Throw", shot_type))

### Indiana Pacers
IND <- wnba_shots |> 
  filter(game_type == 2 & shooting_team == "Indiana" & abs(coordinate_x) >= 10) |> 
  mutate(coordinate_x_abs = abs(coordinate_x)) |> 
  slice(-grep("^Free Throw", shot_type))

## Make WNBA Court
### Base
wnba_court <- sportyR::geom_basketball("WNBA", display = "offense", rotation = 90)

### New York Liberty court
#### colors = outline and inside arc lines - #6ECEB2, inside arc - #857e82, 
# #b5b7b6, main court - gray, arc - white, paint = #2a2729
# cani_color_league_features(league_code = "WNBA")
nyl_court <- sportyR::geom_basketball("WNBA", display = "offense", rotation = 90,
                         color_updates = list(
                           panel_background = "black",
                           offensive_half_court = "gray",
                           court_apron = "#6ECEB2",
                           center_circle_outline = "white",
                           center_circle_fill = "#6ECEB2",
                           endline = "white",
                           sideline = "white",
                           division_line = "white",
                           two_point_range = "#857e82",
                           painted_area = "#2a2729",
                           lane_boundary = "white",
                           free_throw_circle_fill = "#857e82",
                           inbounding_line = "white",
                           substitution_line = "white",
                           three_point_line = "white",
                           lane_space_mark = "white",
                           free_throw_circle_outline = "#6ECEB2",
                           free_throw_circle_dash = "#6ECEB2",
                           baseline_lower_defensive_box = "white",
                           lane_lower_defensive_box = "#6ECEB2",
                           restricted_arc = "#6ECEB2"
                         ))

### Final product

nyl_court +
  stat_summary_hex(data = NYL, aes(y = coordinate_x_abs, x = coordinate_y, 
                                   z = made_shot, group = -1), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black", alpha = .8) +
  scale_fill_gradient(low = "white", 
                      high = "darkorange3") +
  labs(
    fill = "Shooting Rate",
    title = "New York Libery Shooting Efficiency"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

nyl_court +
  geom_hex(data = NYL, aes(y = coordinate_x_abs, x = coordinate_y), 
           binwidth = c(2, 2), alpha = 0.8, color = "black") +
  scale_fill_gradient(low = "white", 
                      high = "darkorange3") +
  labs(
    fill = "# of Shots",
    title = "New York Libery Shooting Frequency"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20))


nyl_court +
  stat_summary_hex(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y, 
                                   z = made_shot, group = -1), 
                   binwidth = c(2, 2), fun = mean,
                   color = "black", alpha = .8) +
  scale_fill_gradient(low = "white", 
                      high = "darkorange3") +
  labs(
    fill = "Shooting Rate",
    title = "Sabrina Ionescu Shooting Efficiency"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

nyl_court +
  geom_hex(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y), 
           binwidth = c(2, 2), color = "black", alpha = 0.8) +
  scale_fill_gradient(low = "white", 
                      high = "darkorange3") +
  labs(
    fill = "# of Shots",
    title = "Sabrina Ionescu Shooting Frequency"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20))















## Built in wehoop function
glimpse(shot_chart)

shot_chart <- wehoop::wnba_shotchartdetail()$Shot_Chart_Detail

summary(as.numeric(shot_chart$LOC_X))

hist(as.numeric(shot_chart$LOC_X))

summary(as.numeric(shot_chart$LOC_Y))

hist(as.numeric(shot_chart$LOC_Y))

# Convert units
convert_shot <- shot_chart |> 
  mutate(LOC_X = as.numeric(LOC_X),
      LOC_Y = as.numeric(LOC_Y),
      LOC_X_ft = LOC_X / 3,
      LOC_Y_ft = LOC_Y / 3)

convert_shot |> 
  ggplot(aes(LOC_X, LOC_Y)) +
  geom_point()

wilson_sea <- wilson |> 
  filter(home_team_name == "Seattle")

convert_shot |> 
  mutate(LOC_X_ft = LOC_X / 12,
         LOX_Y_FT = LOC_Y / 12) |> 
  ggplot(aes(LOC_X, LOC_Y)) +
  geom_point()

convert_shot |> 
  mutate(LOC_X_ft = LOC_X / 12,
         LOC_Y_ft = LOC_Y / 12) |> 
  ggplot(aes(LOC_X_ft, LOC_Y_ft)) +
  geom_point()

geom_basketball(league = "NBA", display_range = "Offense", rotation = 270, xtrans = 43) +
  ggplot(convert_shot, aes(LOC_X_ft, LOC_Y_ft)) +
  geom_point()
  
wehoop::wnba_shotchartdetail()


wilson__shot <- shot_chart |> 
  mutate(
    LOC_X = as.numeric(LOC_X),
    LOC_Y = as.numeric(LOC_Y),
    LOC_X = abs(LOC_X),
    SHOT_MADE_FLAG = factor(as.numeric(SHOT_MADE_FLAG))
    ) |> 
  filter(abs(LOC_X) >= 10)

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_point(wilson, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3) +
  scale_color_manual(values = c("firebrick", "green3"))










## heat map does not work well with 3-point players
## manually set court locations and use if() to set 1 or 0 for each location and added them up
# Oneil Cruz's average strike zone
# Outside layer
top_zone <- mean(cruz_data$sz_top)
bot_zone <- mean(cruz_data$sz_bot)
left_zone <- -0.95
right_zone <- 0.95
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

# Horizontal lines to break the strike zone into ninths
top_zone <- mean(cruz_data$sz_top)
bot_zone <- mean(cruz_data$sz_bot)
left_third <- -0.95/3
right_third <- 0.95/3
inside_strike_zone_df <- data.frame(
  x = c(left_third, left_third, right_third, right_third, left_third),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)

# Vertical lines to break the strike zone into ninths
top_third <- top_zone - ((top_zone - bot_zone)/3)
bot_third <- ((top_zone - bot_zone)/3) + bot_zone
vertical_strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_third, top_third, top_third, bot_third, bot_third)
)

# Location of pitches on strikeouts
# August 5th, 2022
# 2nd Inning
cruz_strikeouts %>%
  filter(pitch_type %in% c("CH", "CU", "FC", "FF", "FS", "FT", "KC", "SI", "SL"), 
         game_date == "2022-08-05", inning == 2) %>%
  ggplot(aes(x = plate_x, y = plate_z, color = pitch_type)) + 
  geom_point(alpha = 0.75) + 
  geom_path(data = strike_zone_df, aes(x, y), linewidth = 1.5, color = "black") + 
  geom_path(data = inside_strike_zone_df, aes(x, y), color = "black") +
  geom_path(data = vertical_strike_zone_df, aes(x, y), color = "black") +
  geom_text_repel(aes(label = count)) +
  coord_fixed() +
  labs(title = "Location of Pitches Thrown Against Oneil Cruz on August 5th, 2022 (2nd Inning) by Pitch Type",
       caption = "Data courtesy of MLBAM",
       x = "Horizontal Location (feet)",
       y = "Vertical Location (feet)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



## Draw zones on the basketball court
### top corner three
top_line <- 25
bot_line <- 21.75
left_line <- 39
right_line <- 47
top_corner_three <- data.frame(
  x = c(left_line, left_line, right_line, right_line, left_line),
  y = c(bot_line, top_line, top_line, bot_line, bot_line)
)

### bottom corner three
top_line <- -21.75
bot_line <- -25
left_line <- 39
right_line <- 47
bottom_corner_three <- data.frame(
  x = c(left_line, left_line, right_line, right_line, left_line),
  y = c(bot_line, top_line, top_line, bot_line, bot_line)
)

### paint
top_line <- 8
bot_line <- -8
left_line <- 28
right_line <- 47
paint <- data.frame(
  x = c(left_line, left_line, right_line, right_line, left_line),
  y = c(bot_line, top_line, top_line, bot_line, bot_line)
)

### inside the arc
# install.packages("ggforce")
library("ggforce") # to draw a circle
### top arc
top_line <- 21.75
bot_line <- 8
left_line <- 39
right_line <- 47
top_arc <- data.frame(
  x = c(left_line, left_line, right_line, right_line, left_line),
  y = c(bot_line, top_line, top_line, bot_line, bot_line)
)

### bottom arc
top_line <- -8
bot_line <- -21.75
left_line <- 39
right_line <- 47
bot_arc <- data.frame(
  x = c(left_line, left_line, right_line, right_line, left_line),
  y = c(bot_line, top_line, top_line, bot_line, bot_line)
)

### top ar 2
top_line <- 21.75
bot_line <- 8
left_line <-
  right_line <- 39
bot_arc <- data.frame(
  x = c(top_line, bot_line, right_line, right_line, left_line),
  y = c(bot_line, top_line, top_line, bot_line, bot_line)
)


sportyR::geom_basketball("WNBA", xlim = c(0, 47), ylim = c(-25, 25)) +
  # top corner three
  geom_path(data = top_corner_three, aes(x, y), linewidth = 1, color = "black") +
  # bottom corner three
  geom_path(data = bottom_corner_three, aes(x, y), linewidth = 1, color = "black") +
  # paint
  geom_path(data = paint, aes(x, y), linewidth = 1, color = "black") +
  # top arc
  geom_path(data = top_arc, aes(x, y), linewidth = 1, color = "black") +
  # bot arc
  geom_path(data = bot_arc, aes(x, y), linewidth = 1, color = "black")


## May possibly work
wnba_shots |> 
  filter(game_type == 3 & shooting_team == "New York" 
         & abs(coordinate_x) >= 10 & str_like(desc, "Sabrina Ionescu%") &
           coordinate_x >= 28 & coordinate_x <= 47 &
           coordinate_y >= -8 & coordinate_y <= 8) |> 
  mutate(coordinate_x = abs(coordinate_x),
         player = str_extract(desc, "Sabrina Ionescu"),
         paint = ifelse(made_shot == TRUE, 1, 0)) |> 
  slice(-grep("^Free Throw", shot_type)) |> 
  summarize(paint_FG = sum(paint) / nrow())

## Group by shot type
### Shot type manipulation
#### layups - all layups are in the paint
layups <- wnba_shots |> 
  filter(game_type == 3 & shooting_team == "New York" 
         & abs(coordinate_x) >= 10 & str_like(desc, "Sabrina Ionescu%")) |> 
  mutate(coordinate_x = abs(coordinate_x),
         player = str_extract(desc, "Sabrina Ionescu")) |> 
  slice(grep("Layup", shot_type))


sportyR::geom_basketball("WNBA", xlim = c(0, 47), ylim = c(-25, 25)) +
  # top corner three
  geom_path(data = top_corner_three, aes(x, y), linewidth = 1, color = "black") +
  # bottom corner three
  geom_path(data = bottom_corner_three, aes(x, y), linewidth = 1, color = "black") +
  # paint
  geom_path(data = paint, aes(x, y), linewidth = 1, color = "black") +
  geom_point(data = layups, aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.5, size = 1.5)


#### threes
threes <- wnba_shots |> 
  filter(game_type == 3 & shooting_team == "New York" 
         & abs(coordinate_x) >= 10 & str_like(desc, "Sabrina Ionescu%")) |> 
  mutate(coordinate_x = abs(coordinate_x),
         player = str_extract(desc, "Sabrina Ionescu"),
         type = ifelse(grepl("three point", desc), "Three-Pointer", ""),
         type = ifelse(grepl("three point", desc), "Three-Pointer", "")))


sportyR::geom_basketball("WNBA", xlim = c(0, 47), ylim = c(-25, 25)) +
  # top corner three
  geom_path(data = top_corner_three, aes(x, y), linewidth = 1, color = "black") +
  # bottom corner three
  geom_path(data = bottom_corner_three, aes(x, y), linewidth = 1, color = "black") +
  # paint
  geom_path(data = paint, aes(x, y), linewidth = 1, color = "black") +
  geom_point(data = threes, aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.5, size = 1.5)



## Create Sabrina Ionescu Shot Chart
library(viridis)


sabrina <- wnba_shots |> 
  filter(game_type == 3 & shooting_team == "New York" 
         & abs(coordinate_x) >= 10 & str_like(desc, "Sabrina Ionescu%")) |> 
  mutate(coordinate_x = abs(coordinate_x),
         player = str_extract(desc, "Sabrina Ionescu")) |> 
  slice(-grep("^Free Throw", shot_type))

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_point(sabrina, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3) +
  scale_color_manual(values = c("firebrick", "green3"))

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  stat_density_2d(sabrina, mapping = aes(coordinate_x, coordinate_y, fill = made_shot), 
                  geom = "polygon", contour = TRUE, bins = 3, alpha = .5) +
  geom_point(sabrina, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3)

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_density_2d_filled(SEA, mapping = aes(x = coordinate_x, y = coordinate_y, fill = after_stat(level)), 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .5) +
  #geom_point(IND, mapping = aes(x = coordinate_x, y = coordinate_y, color = made_shot)) + 
  #scale_fill_brewer(palette = "RdGy", aesthetics = c("fill", "color")) +
  scale_fill_viridis(aesthetics = c("fill", "color"), discrete = TRUE) +
  theme(legend.position = "none")


####### BY TEAM HEATMAP
######## REGULAR SEASON
team_heat_map <- wnba_shots |> 
  filter(game_type == 2 & abs(coordinate_x) >= 10) |> 
  mutate(coordinate_x = abs(coordinate_x)) |> 
  slice(-grep("^Free Throw", shot_type))

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_density_2d_filled(team_heat_map, mapping = aes(x = coordinate_x, y = coordinate_y, fill = after_stat(level)), 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .5) +
  #geom_point(IND, mapping = aes(x = coordinate_x, y = coordinate_y, color = made_shot)) + 
  #scale_fill_brewer(palette = "RdGy", aesthetics = c("fill", "color")) +
  scale_fill_viridis(aesthetics = c("fill", "color"), discrete = TRUE) +
  theme(legend.position = "none") + 
  facet_wrap(~ shooting_team)

## Las Vegas Aces
LVA <- wnba_shots |> 
  filter(game_type == 2 & shooting_team == "Las Vegas" & abs(coordinate_x) >= 10) |> 
  mutate(coordinate_x = abs(coordinate_x)) |> 
  slice(-grep("^Free Throw", shot_type))

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_point(LVA, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3) +
  scale_color_manual(values = c("firebrick", "green3"))

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_density_2d_filled(LVA, mapping = aes(coordinate_x, coordinate_y, fill = made_shot), 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .5)



LVA |> 
  ggplot(aes(coordinate_x, coordinate_y)) +
  stat_density_2d(geom = "polygon", contour = TRUE,
                  aes(fill = after_stat(level)), color = "black",
                  bins = 15) +
  geom_point(aes(color = made_shot)) +
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) +
  scale_color_manual(values = c("firebrick1", "forestgreen")) +
  # scale_fill_distiller(palette = c, direction = 1) +
  theme_bw()


scale_color_manual(values = c("green4","red3"), aesthetics = "color", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
  scale_fill_manual(values = c("green2","gray20"), aesthetics = "fill", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
  
  ## next steps:
  ### identify zones and aggregate shooting percentage in zone
  ### can do league first or one team, and then group by team for an R Shiny app
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  wnba_shots |> 
  filter(-10 <= coordinate_x & 10 >= coordinate_x) |> 
  select(desc, coordinate_x, coordinate_y, quarter_seconds_remaining)


# install.packages("paletteer")
library(paletteer)
palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", direction = -1)

p1 <- plot_court(court_themes$ppt) + 
  geom_density_2d_filled(player, mapping = aes(x=x,y=y,fill = ..level..,), 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .5)  + 
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) +
  scale_x_continuous(limits = c(-27.5, 27.5)) + 
  scale_y_continuous(limits = c(0, 45)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, size = 22, family = "Comic Sans MS", face = "bold", vjust = -4),
        plot.subtitle = element_text(hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", vjust = -8),
        legend.title = element_blank(),
        legend.text = element_text(hjust = .5, size = 10, family = "Comic Sans MS", face = "bold", colour = "white"),
        plot.caption = element_text(hjust = .5, size = 6, family = "Comic Sans MS", face = "bold", colour = "lightgrey", vjust = 8)) +
  labs(title = "Chris Paul Shot Heatmap",
       subtitle = "2020-21 Playoffs | As of 7/13/21",
       caption = "Tutorial: @DSamangy") 






## Creating NBA Court
# Creating court and plotting

circle_points <- function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(tibble(x = center[1] + radius * cos(angles),
                y = center[2] + radius * sin(angles)))
}

# Court Dimenons & lines
width <- 50
height <- 94 / 2
key_height <- 19
inner_key_width <- 12
outer_key_width <- 16
backboard_width <- 6
backboard_offset = 4
neck_length <- 0.5
hoop_radius <- 0.75
hoop_center_y <- backboard_offset + neck_length + hoop_radius
three_point_radius <- 22.15

# Court themes
court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray20',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray20"
  )
)

# Function to create court based on given dimensions
plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 21.75
    three_point_side_height = 0
  }
  
  court_points = tibble(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , tibble(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , tibble(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , tibble(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = tibble(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  # Final plot creation
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'gray20', color = 'gray20'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

plot_court()





wnba_shots |> 
  filter(home_team_name == "Las Vegas") -> LVA


wnba_shots |> 
  filter(game_type == 3 & shooting_team == "Las Vegas" & qtr == 1 & game_id == 401578574) -> LVA 

sportyR::geom_basketball("WNBA") +
  geom_point(LVA, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3) +
  scale_color_manual(values = c("firebrick", "green3"))

summary(wnba_shots)






## Structure
(wnba_shots)

## shot_type variable
wnba_shots |> 
  select(shot_type) |> 
  unique()

## top 10 shots
wnba_shots |> 
  count(shot_type, name = "freq") |> 
  arrange(desc(freq)) |> 
  slice_head(n = 10) |> 
  mutate(prop = freq / sum(freq),
         shot_type = fct_reorder(shot_type, prop)) |> 
  ggplot(aes(shot_type, prop)) + 
  geom_col(aes(fill = shot_type)) +
  labs(
    x = "Shot Type",
    y = "Percentage of Total Shots",
    title = "Top 10 Shot Types in the WNBA",
    caption = "Data Courtesy of wehoops",
    fill = "Shot Type"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, 
                                  face = "bold")) +
  scale_fill_brewer(palette = "Paired") +
  coord_flip()



## look at play by play
pbp_2023 <- wehoop::load_wnba_pbp() |> 
  filter(shooting_play) |> 
  # make a column to indicate the shooting team
  mutate(shooting_team = ifelse(team_id == home_team_id, 
                                home_team_name,
                                away_team_name)) |> 
  slice_head(n = 20)

# install.packages("sportyR")



sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_point(wilson, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3) +
  scale_color_manual(values = c("firebrick", "green3"))

sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  geom_density_2d_filled(wilson, mapping = aes(coordinate_x, coordinate_y, fill = made_shot), 
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .5) +
  geom_point(wilson, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3)


sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
  stat_density_2d(wilson, mapping = aes(coordinate_x, coordinate_y, fill = after_stat(level)), 
                  geom = "polygon", contour = TRUE, bins = 50, alpha = .5) +
  geom_point(wilson, mapping = aes(coordinate_x, coordinate_y, color = made_shot), alpha = 0.8, size = 3)


## Test court display
nyl_court +
  geom_point(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y), size = 2)

## Create contours of 2D kernel density estimate
### w/o court
ionescu_point |> 
  ggplot(aes(y = coordinate_x_abs, x = coordinate_y)) +
  geom_point(size = 2, alpha = 0.4) +
  geom_density2d() +
  coord_fixed() +
  scale_y_reverse()

### w/ court
nyl_court +
  geom_point(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y), size = 2, alpha = 0.4) +
  # adjust - modifies the multivariate bandwidth
  geom_density2d(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y), adjust = 0.5)


## Heatmap
ionescu_point |> 
  ggplot(aes(y = coordinate_x_abs, x = coordinate_y)) +
  stat_density2d(aes(fill = after_stat(level)),
                 h = 0.6, bins = 60, geom = "polygon") +
  scale_fill_gradient(low = "midnightblue", 
                      high = "gold") +
  scale_y_reverse()


## Turn off contours and use tiles instead
### Divide the space into a grid and color the grid according to high/low values
ionescu_point |> 
  ggplot(aes(y = coordinate_x_abs, x = coordinate_y)) +
  stat_density2d(aes(fill = after_stat(density)),
                 h = 0.6, bins = 60, contour = FALSE,
                 geom = "raster") +
  scale_fill_gradient(low = "white", 
                      high = "royalblue4") +
  theme(legend.position = "bottom") +
  coord_fixed() +
  scale_y_reverse()


## Hexagonal binning
#install.packages('hexbin')
library(hexbin)
### LVA
LVA |> 
  ggplot(aes(y = coordinate_x_abs, x = coordinate_y)) +
  geom_hex(binwidth = c(1, 1)) +
  scale_fill_gradient(low = "midnightblue", 
                      high = "gold") +
  theme(legend.position = "bottom") +
  coord_fixed() +
  scale_y_reverse()


### LVA w/ court
wnba_court +
  #geom_point(data = LVA, aes(y = coordinate_x_abs, x = coordinate_y), size = 2, alpha = 0.4) +
  # adjust - modifies the multivariate bandwidth
  geom_hex(data = LVA, aes(y = coordinate_x_abs, x = coordinate_y), binwidth = c(1, 1)) +
  scale_fill_gradient(low = "midnightblue", 
                      high = "green2")


### NYL w/ court
nyl_court +
  #geom_point(data = LVA, aes(y = coordinate_x_abs, x = coordinate_y), size = 2, alpha = 0.4) +
  # adjust - modifies the multivariate bandwidth
  geom_hex(data = NYL, aes(y = coordinate_x_abs, x = coordinate_y),
           binwidth = c(3, 3), color = "black") +
  scale_fill_gradient(low = "white", 
                      high = "darkred")

## Shooting efficiency
### Can compute a function of another variable inside hexagons with stat_summary_hex()
ionescu_point |> 
  ggplot(aes(y = coordinate_x_abs, x = coordinate_y, 
             z = made_shot, group = -1)) +
  stat_summary_hex(binwidth = c(2, 2), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "midnightblue", 
                      high = "green2") +
  theme(legend.position = "bottom") +
  coord_fixed() +
  scale_y_reverse()

nyl_court +
  stat_summary_hex(data = LVA, aes(y = coordinate_x_abs, x = coordinate_y, 
                                   z = made_shot, group = -1), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black") +
  scale_fill_gradient(low = "midnightblue", 
                      high = "gold") +
  theme(legend.position = "bottom")



wnba_court +
  stat_summary_hex(data = NYL, aes(y = coordinate_x_abs, x = coordinate_y, 
                                   z = made_shot, group = -1), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black", alpha = 0.6) +
  scale_fill_gradient(low = "midnightblue", 
                      high = "green2") +
  theme(legend.position = "bottom")




wnba_court +
  stat_summary_hex(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y, 
                                             z = made_shot, group = -1), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black", alpha = 0.6) +
  scale_fill_gradient(low = "midnightblue", 
                      high = "green2") +
  theme(legend.position = "bottom")

nyl_court +
  stat_summary_hex(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y, 
                                             z = made_shot, group = -1), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black", alpha = 0.6) +
  scale_fill_gradient(low = "white", 
                      high = "firebrick") +
  labs(
    fill = "Shooting %",
    title = "Sabrina Ionescu Shooting %"
  ) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

nyl_court +
  stat_summary_hex(data = NYL, aes(y = coordinate_x_abs, x = coordinate_y, 
                                   z = made_shot, group = -1), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black", alpha = 0.6) +
  scale_fill_gradient(low = "white", 
                      high = "firebrick") +
  labs(
    fill = "Shooting %",
    title = "New York Libery Shooting %"
  ) +
  theme(legend.position = "bottom")

wnba_court +
  #geom_point(data = LVA, aes(y = coordinate_x_abs, x = coordinate_y), size = 2, alpha = 0.4) +
  # adjust - modifies the multivariate bandwidth
  geom_hex(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y), 
           binwidth = c(3, 3), alpha = 0.75) +
  scale_fill_gradient(low = "#1a00ff", 
                      high = "#ff8c00")


wilson_point <- wnba_shots |> 
  filter(game_type == 2 & shooting_team == "Las Vegas" 
         & str_like(desc, "A'ja Wilson%") & abs(coordinate_x) >= 10) |> 
  mutate(coordinate_x_abs = abs(coordinate_x),
         player = str_extract(desc, "A'ja Wilson")) |> 
  slice(-grep("^Free Throw", shot_type))


wnba_court +
  stat_summary_hex(data = wilson_point, aes(y = coordinate_x_abs, x = coordinate_y, 
                                            z = made_shot, group = -1), 
                   binwidth = c(3, 3), fun = mean,
                   color = "black", alpha = 0.6) +
  scale_fill_gradient(low = "#1a00ff", 
                      high = "#ff8c00") +
  theme(legend.position = "bottom")

wnba_court +
  geom_hex(data = wilson_point, aes(y = coordinate_x_abs, x = coordinate_y), 
           binwidth = c(3, 3), alpha = 0.6) +
  scale_fill_gradient(low = "#1a00ff", 
                      high = "#ff8c00") +
  theme(legend.position = "bottom")
