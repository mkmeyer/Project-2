#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library("tidyverse")
library("jsonlite")
library("httr")
library("ggplot2")
library("bslib")

ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Player Statistics Data"),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h3("Select the position:"),
      selectizeInput("pos", "Position", selected = "SG", choices = levels(as.factor(players_stats_data$pos))),
      
      br(),
      
      h3("Select the team:"),
      selectizeInput("team_code", "Team", selected = "SAC", choices = levels(as.factor(players_stats_data$team_code)))
    ),
    
    # Show outputs?msle
    mainPanel(
      plotOutput("playersPlot"),
      textOutput("info"),
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  #get data for only order specified
  getData <- reactive({
    positions <- input$pos
    teams <- input$team_code
    
    newData <- players_stats_data %>% filter(pos == positions & team_code == teams)
    newData
  })
  
  #create plot
  output$playersPlot <- renderPlot({
    #get data
    playerstatsData <- getData()
    
    #base plotting object
    g <- ggplot(playerstatsData, aes(x = points)) 
    g + geom_histogram()
  })
  
  #create text info
  output$info <- renderText({
    #get data
    playerstatsData <- getData()
    
    #paste info out
    paste("The average minutes played for", playerstatsData$first_name[1], playerstatsData$last_name[1], "is", round(mean(playerstatsData$minutes, na.rm = TRUE), 2), "and the average points scored is", round(mean(playerstatsData$points, na.rm = TRUE), 2), sep = " ")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
