#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sportyR)

### Data
#### Read in the data
wnba_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/wnba_shots.csv")

#### Clean dataset
wnba_shots <- wnba_shots |> 
  # filter by regular season and remove outliers
  filter(game_type == 2 & abs(coordinate_x) >= 10) |> 
  mutate(
    # take the absolute value of the x coordinate to only get one side of the court
    coordinate_x = abs(coordinate_x),
    # rename shooting_team to include team name
    shooting_team_name = case_match(shooting_team, "Atlanta" ~ "Atlanta Dream", 
                              "Chicago" ~ "Chicago Sky", "Connecticut" ~ "Connecticut Sun", 
                              "Dallas" ~ "Dallas Wings", "Indiana" ~ "Indiana Fever", 
                              "Las Vegas" ~ "Las Vegas Aces", "Los Angeles" ~ "Los Angeles Spark", 
                              "Minnesota" ~ "Minnesota Lynx", "New York" ~ "New York Liberty", 
                              "Phoenix" ~ "Phoenix Mercury", "Seattle" ~ "Seattle Storm", 
                              "Washington" ~ "Washington Mystics")
         ) |>
  # remove Free Throws and the All-Star game shots
  slice(-c(grep("^Free Throw", shot_type), grep("^Team", shooting_team)))

# WNBA court (New York Libery)
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

# nyl_court +
#   stat_summary_hex(data = NYL, aes(y = coordinate_x_abs, x = coordinate_y, 
#                                    z = made_shot, group = -1), 
#                    binwidth = c(2, 2), fun = mean,
#                    color = "black", alpha = .8) +
#   scale_fill_gradient(low = "white", 
#                       high = "darkorange3") +
#   labs(
#     fill = "Shooting Rate",
#     title = "New York Libery Shooting Efficiency"
#   ) +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
# 
# nyl_court +
#   geom_hex(data = NYL, aes(y = coordinate_x_abs, x = coordinate_y), 
#            binwidth = c(2, 2), alpha = 0.8, color = "black") +
#   scale_fill_gradient(low = "white", 
#                       high = "darkorange3") +
#   labs(
#     fill = "# of Shots",
#     title = "New York Libery Shooting Frequency"
#   ) +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
# 
# 
# nyl_court +
#   stat_summary_hex(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y, 
#                                              z = made_shot, group = -1), 
#                    binwidth = c(2, 2), fun = mean,
#                    color = "black", alpha = .8) +
#   scale_fill_gradient(low = "white", 
#                       high = "darkorange3") +
#   labs(
#     fill = "Shooting Rate",
#     title = "Sabrina Ionescu Shooting Efficiency"
#   ) +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
# 
# nyl_court +
#   geom_hex(data = ionescu_point, aes(y = coordinate_x_abs, x = coordinate_y), 
#            binwidth = c(2, 2), color = "black", alpha = 0.8) +
#   scale_fill_gradient(low = "white", 
#                       high = "darkorange3") +
#   labs(
#     fill = "# of Shots",
#     title = "Sabrina Ionescu Shooting Frequency"
#   ) +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WNBA Team Shot Heat Maps"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("teams",
                        "Select a Team",
                        choices = c("Atlanta Dream", "Chicago Sky",
                                    "Connecticut Sun", "Dallas Wings", 
                                    "Indiana Fever", "Las Vegas Aces",
                                    "Los Angeles Spark", "Minnesota Lynx",
                                    "New York Liberty", "Phoenix Mercury",
                                    "Seattle Storm", "Washington Mystics"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), 
                        plotOutput("frequency"), plotOutput("efficiency"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # frequency plot
    output$frequency <- renderPlot({
      
      # create data
      team_data <- wnba_shots |> 
        filter(shooting_team_name == input$teams)
      
      # plot
      nyl_court +
        stat_summary_hex(data = team_data, aes(x = coordinate_y, y = coordinate_x, 
                                         z = made_shot, group = -1), 
                         binwidth = c(2, 2), fun = mean,
                         color = "black", alpha = .8) +
        scale_fill_gradient(low = "white", 
                            high = "darkorange3") +
        labs(
          fill = "Shooting Rate",
          title = paste(input$teams, "Shooting Efficiency")
        ) +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
    })

    # shooting efficiency plot
    output$efficiency <- renderPlot({
      # create data
      team_data <- wnba_shots |> 
        filter(shooting_team_name == input$teams)
      
      # plot
      nyl_court +
        geom_hex(data = team_data, aes(x = coordinate_y, y = coordinate_x), 
                 binwidth = c(2, 2), alpha = 0.8, color = "black") +
        scale_fill_gradient(low = "white", 
                            high = "darkorange3") +
        labs(
          fill = "# of Shots",
          title = paste(input$teams, "Shooting Frequency")
        ) +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
