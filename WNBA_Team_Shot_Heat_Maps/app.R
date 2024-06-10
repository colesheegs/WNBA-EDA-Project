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
library(viridis)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WNBA Team Shot Heat Maps"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("teams",
                        "Select a Team",
                        choices = c("Atlanta", "Chicago",
                                    "Connecticut", "Dallas", 
                                    "Indiana", "Las Vegas",
                                    "Los Angeles", "Minnesota",
                                    "New York", "Phoenix",
                                    "Seattle", "Washington"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("heatMap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$heatMap <- renderPlot({
      
      # read in data
      wnba_shots <- read_csv("https://raw.githubusercontent.com/36-SURE/36-SURE.github.io/main/data/wnba_shots.csv")
      
      
      # create data
      team_heat_map <- wnba_shots |> 
        filter(game_type == 2 & abs(coordinate_x) >= 10 & shooting_team == input$teams) |> 
        mutate(coordinate_x = abs(coordinate_x)) |> 
        slice(-grep("^Free Throw", shot_type))
      
      # create plot
      sportyR::geom_basketball("WNBA", xlim = c(0, 47)) +
        geom_density_2d_filled(team_heat_map, mapping = aes(x = coordinate_x, y = coordinate_y, fill = after_stat(level)), 
                               contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .5) +
        scale_fill_viridis(aesthetics = c("fill", "color"), discrete = TRUE) +
        theme(legend.position = "none")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
