#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
library("tidyverse")
library("jsonlite")
library("httr")
library("ggplot2")
library("bslib")
rm(list=ls())

#Creating Functions to Read in Data
team_stats_query <- function(season, team){
  url <- paste0("https://api-nba-v1.p.rapidapi.com/players/statistics?", #base of URL
                "&season=", season, #adding season to URL
                "&team=", team #adding team to URL
  )
  
  response <- httr::GET(url, add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 
                                         'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), 
                        content_type("application/octet-stream"))
  
  parsed_nba_info <- fromJSON(rawToChar(response$content))
  
  #Performing Data Cleaning
  team_data1 <- as_tibble(parsed_nba_info$response) #pulling response
  team_data1$first_name <- team_data1$player$firstname
  team_data1$last_name <- team_data1$player$lastname
  team_data1$min1 <- strptime(team_data1$min, format = "%M:%S")
  team_data1$minutes1 <- round_date(team_data1$min1, unit = "1 minute")
  team_data1$minutes <- minute(team_data1$minutes1)
  team_data1$starter <- ifelse(is.na(team_data1$pos), NA, 1)
  
  team_data <- team_data1 %>%
    group_by(first_name, last_name) %>%
    mutate(games_not_played = sum(minutes < 1 | is.na(minutes))) %>%
    mutate(games = n()) %>%
    mutate(games_played = n() - games_not_played) %>%
    mutate(games_started = sum(starter, na.rm = TRUE)) %>%
    mutate(games_start_pct = games_started/games_played) %>%
    mutate(overall_ppg = sum(points, na.rm = TRUE)/games_played) %>%
    mutate(overall_rpg = sum(totReb, na.rm = TRUE)/games_played) %>%
    mutate(overall_mpg = sum(minutes, na.rm = TRUE)/games_played) %>%
    fill(starter, .direction = "downup") %>%
    select(first_name, last_name, overall_ppg, overall_rpg, overall_mpg, games, games_played, games_not_played, games_started, games_start_pct, starter) %>%
    distinct()
  
  team_data$likely_starter <- ifelse(team_data$games_start_pct > 0.5, 1, 0)
  return(team_data)
}


ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Player Statistics Data"),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      sliderInput("size", "Size of Points on Graph",
                  min = 1, max = 10, value = 5, step = 1),
      
      checkboxInput("games_start_pct", h4("Percent Games Started", style = "color:black;"))
    ),
    
    # Show outputs?msle
    mainPanel(
      plotOutput("teamPlot1"),
      plotOutput("teamPlot2"),
      textOutput("info"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  #get data for only order specified
  getData <- reactive({
    newData <- team_data
    newData
  })
  
  #create plot
  output$teamPlot1 <- renderPlot({
    #get data
    teamData <- getData()
    
    #base plotting object
    g <- ggplot(teamData, aes(x = overall_mpg, y = overall_ppg, label = last_name)) + geom_text(hjust = 0, nudge_x = 0.20)
    
    if (input$games_start_pct) {
      g + geom_point(size = input$size, aes(col = games_start_pct))
    } else {
      g + geom_point(size = input$size)
    }
  })
  
  output$teamPlot2 <- renderPlot({
    #get data
    teamData <- getData()
    
    #base plotting object
    g <- ggplot(teamData, aes(x = overall_mpg, y = overall_rpg, label = last_name)) + geom_text(hjust = 0, nudge_x = 0.20)
    
    if (input$games_start_pct) {
      g + geom_point(size = input$size, aes(col = games_start_pct))
    } else {
      g + geom_point(size = input$size)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
