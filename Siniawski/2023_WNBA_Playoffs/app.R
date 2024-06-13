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
library(wehoop)




# Define UI for application that draws a histogram
ui <- fluidPage(
    h1("2023 WNBA Playoff Data"),
    selectInput(inputId = "sel_team",
                label = "Choose a Team:",
                choices = c("")),
    plotOutput("distPlot")
    )

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$distPlot <- renderPlot({
      
      pbp <- load_wnba_pbp(2023)
      pbp <- wnba_pbp |> 
        filter(shooting_play) |> 
        # make a column to indicate the shooting team
        mutate(shooting_team = ifelse(team_id == home_team_id, 
                                      home_team_name,
                                      away_team_name)) |> 
        select(game_id, game_play_number, game_type = season_type,
               desc = text, shot_type = type_text, 
               made_shot = scoring_play, shot_value = score_value, 
               coordinate_x, coordinate_y, shooting_team, 
               home_team_name, away_team_name, home_score, away_score, qtr,
               quarter_seconds_remaining = start_quarter_seconds_remaining,
               game_seconds_remaining = start_game_seconds_remaining)
      
      df <- pbp |> filter(shooting_team %in% input$sel_team & game_type == 3) |> 
          group_by(shooting_team, time_group) |> summarise(num_shots = n(), total_shots_made = sum(made_shot)) |> 
          mutate(prop = round(total_shots_made / num_shots, 3))
      
      g <- ggplot(df, aes(x = time_group, y = prop))
      g + geom_line(alpha = 0.4) + geom_smooth(se = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)







