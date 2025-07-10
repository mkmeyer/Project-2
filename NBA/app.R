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
  titlePanel("NBA Players Data"),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h3("Select the position:"),
      selectizeInput("leagues_standard_pos", "Position", selected = "G", choices = levels(as.factor(players_data$leagues_standard_pos))),
      
      br(),
      
      sliderInput("size", "Size of Points on Graph",
                  min = 1, max = 10, value = 5, step = 1),
      checkboxInput("birth_country", h4("birth_country", style = "color:black;"))
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
    positions <- input$leagues_standard_pos
    
    newData <- players_data %>% filter(leagues_standard_pos == positions)
    newData
  })
  
  #create plot
  output$playersPlot <- renderPlot({
    #get data
    playersData <- getData()
    
    #base plotting object
    g <- ggplot(playersData, aes(x = weight_pounds, y = height_meters, label = lastname)) + geom_text(hjust = 0, nudge_x = 0.20)
    
    if (input$birth_country) {
      g + geom_point(size = input$size, aes(col = birth_country))
    } else {
      g + geom_point(size = input$size)
    }
  })
  
  #create text info
  output$info <- renderText({
    #get data
    playersData <- getData()
    
    #paste info out
    paste("The average body height for the", input$leagues_standard_pos, "position is", round(mean(playersData$height_meters, na.rm = TRUE), 2), "meters", "and the average weight is", round(mean(playersData$weight_pounds, na.rm = TRUE), 2), "pounds", sep = " ")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
