#Loading libraries required for app execution
library("shiny")
library("tidyverse")
library("jsonlite")
library("httr")
library("ggplot2")
library("bslib")

#Creating Functions to Read in the Data
#Function to read in Team Standings Data
standings_query <- function(season, team){
  standings_url <- paste0("https://api-nba-v1.p.rapidapi.com/standings?league=standard", #base of URL
                "&season=", season, #adding season to URL
                "&team=", team #adding team to URL
                )
  
  standings_response <- httr::GET(standings_url, add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 
                                         'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), 
                        content_type("application/octet-stream")) #supplying necessary key, host, and content type information
  
  parsed_standings_info <- fromJSON(rawToChar(standings_response$content)) #parsing the returned standings data
  
  standings_data <- as_tibble(parsed_standings_info$response) #pulling tibble from parsed standings data
  
  #Recoding nested variables in order to access them
  #I kept running into errors attempting this with "rename" from tidyverse, so I did baseR coding
  standings_data$team_id <- standings_data$team$id
  standings_data$team_name <- standings_data$team$name
  standings_data$conference_name <- standings_data$conference$name
  standings_data$conference_rank <- as.numeric(standings_data$conference$rank)
  standings_data$division_rank <- as.numeric(standings_data$division$rank)
  standings_data$win_pct <- as.numeric(standings_data$win$percentage)
  standings_data$loss_pct <- as.numeric(standings_data$loss$percentage)
  standings_data$win_total <- as.numeric(standings_data$win$total)
  standings_data$loss_total <- as.numeric(standings_data$loss$total)
  standings_data$conference_win <- as.numeric(standings_data$conference$win)
  standings_data$conference_loss <- as.numeric(standings_data$conference$loss)
  standings_data$division_win <- as.numeric(standings_data$division$win)
  standings_data$division_loss <- as.numeric(standings_data$division$loss)
  standings_data$win_home <- as.numeric(standings_data$win$home)
  standings_data$loss_home <- as.numeric(standings_data$loss$home)
  standings_data$win_away <- as.numeric(standings_data$win$away)
  standings_data$loss_away <- as.numeric(standings_data$loss$away)
  
  #converting the dataset into long form for better plotting use
  standingsdata_long <- standings_data |>
    pivot_longer(cols = 19:28, #selecting numeric columns
                 names_to = "Metric", 
                 values_to = "Count") |>
    select(c(1, 12:20))
  
  return(standingsdata_long)
}

#Player Statistics Data
players_query <- function(season, team){
  players_url <- paste0("https://api-nba-v1.p.rapidapi.com/players?", #base of URL
                "&season=", season, #adding season to URL
                "&team=", team #adding team to URL
  )
  
  players_response <- httr::GET(players_url, add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 
                                         'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), 
                        content_type("application/octet-stream")) #supplying necessary key, host, and content type information
  
  parsed_players_info <- fromJSON(rawToChar(players_response$content)) #parsing the returned players data
  
  players_data1 <- as_tibble(parsed_players_info$response) #pulling response from parsed players data

  #Recoding nested variables in order to access them
  #I kept running into errors attempting this with "rename" from tidyverse, so I did baseR coding
  players_data1$birth_date <- players_data1$birth$date
  players_data1$birth_country <- players_data1$birth$country
  players_data1$nba_start <- players_data1$nba$start
  players_data1$nba_pro <- players_data1$nba$pro
  players_data1$height_inches <- as.numeric(players_data1$height$feet) * 12 + as.numeric(players_data1$height$inches)
  players_data1$weight_pounds <- as.numeric(players_data1$weight$pounds)
  players_data1$pos <- players_data1$leagues$standard$pos
  
  #Selecting necessary variables for plots
  players_data <- players_data1 |>
    select(id, firstname, lastname, birth_date, birth_country, nba_start, nba_pro, height_inches, weight_pounds, pos)
  
  return(players_data)
}

#Team Statistics Data
team_stats_query <- function(season, team){
  teams_url <- paste0("https://api-nba-v1.p.rapidapi.com/players/statistics?", #base of URL
                "&season=", season, #adding season to URL
                "&team=", team #adding team to URL
  )
  
  teams_response <- httr::GET(teams_url, add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 
                                         'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), 
                        content_type("application/octet-stream")) #supplying necessary key, host, and content type information
  
  parsed_teams_info <- fromJSON(rawToChar(teams_response$content)) #parsing the returned teams data
  
  team_data1 <- as_tibble(parsed_teams_info$response) #pulling response from parsed teams data
  
  #Recoding nested variables in order to access them
  #I kept running into errors attempting this with "rename" from tidyverse, so I did baseR coding
  team_data1$first_name <- team_data1$player$firstname
  team_data1$last_name <- team_data1$player$lastname
  team_data1$min1 <- strptime(team_data1$min, format = "%M:%S") #converting minutes played from character to time
  
  team_data <- team_data1 |>
    mutate(minutes1 = round_date(min1, unit = "1 minute")) |> #rounding minutes played to the nearest minute
    mutate(minutes = minute(minutes1)) |> #
    mutate(starter = ifelse(is.na(pos), NA, 1)) |>
    group_by(first_name, last_name) |>
    mutate(games_not_played = sum(minutes < 1 | is.na(minutes))) |>
    mutate(games = n()) |>
    mutate(games_played = n() - games_not_played) |>
    mutate(games_started = sum(starter, na.rm = TRUE)) |>
    mutate(games_start_pct = games_started/games_played) |>
    mutate(overall_ppg = sum(points, na.rm = TRUE)/games_played) |>
    mutate(overall_rpg = sum(totReb, na.rm = TRUE)/games_played) |>
    mutate(overall_mpg = sum(minutes, na.rm = TRUE)/games_played) |>
    fill(starter, .direction = "downup") |>
    select(first_name, last_name, overall_ppg, overall_rpg, overall_mpg, games, 
           games_played, games_not_played, games_started, games_start_pct, starter) |>
    distinct()
  
  team_data$likely_starter <- ifelse(team_data$games_start_pct > 0.5, 1, 0)
  
  return(team_data)
}

#Creating a list of options of seasons and positions
seasons <- c(2015:2024)
positions <- c("G", "SF", "SG", "PF", "C", "F", "F-C", "F-G", "G-F")

#Creating list of options of NBA teams
url <- "https://api-nba-v1.p.rapidapi.com/teams"
response <- VERB("GET", url, add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), content_type("application/octet-stream"))
content(response, "text")
teams <- fromJSON(rawToChar(response$content))
team_data <- as_tibble(teams$response) #pulling response
nba_teams <- team_data[(team_data$nbaFranchise == TRUE), ]
nba_teams_list <- paste(nba_teams$id, nba_teams$name, sep = " ")

ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Statistics"),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h3("Select the season:"),
      selectizeInput("seasons", "Season", selected = 2020, choices = levels(as.factor(seasons))),
      
      h3("Select the team:"),
      selectizeInput("teams", "Team", selected = "1 Atlanta Hawks", choices = levels(as.factor(nba_teams_list))),
      
      h3("Select the position:"),
      selectizeInput("positions", "Position", selected = "G", choices = levels(as.factor(positions))),
      
      sliderInput("size", "Size of Points on Graph",
                  min = 1, max = 10, value = 5, step = 1),
      
      checkboxInput("games_start_pct", h4("Percent Games Started", style = "color:black;"))
    ),
    
    navset_card_underline(
      # About tab----
      nav_panel("About", HTML(paste0(
        "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://images.ctfassets.net/h8q6lxmb5akt/5qXnOINbPrHKXWa42m6NOa/421ab176b501f5bdae71290a8002545c/nba-logo_2x.png' width = '186'></a>",
        "<p>The purpose of this app is to explore NBA data. I have been a college basketball fan and WNBA basketball fan for a few years, but I have often felt overwhelmed trying to learn about the NBA. There are so many teams and so much history! My goal with this app is to describe and display information for different combinations of teams, seasons, and positions in order to visualize trends in the NBA and for specific teams and seasons.</p>",
        "<p>This data is sourced from <a href = 'https://rapidapi.com/api-sports/api/api-nba' >Rapid API</a>. I found this using the <a href = 'https://github.com/public-apis/public-apis?tab=readme-ov-file#sports--fitness' >GitHub</a> link provided for this project. I had to make an API key for this project, but did not need to pay for a subscription. I ended up making so many calls that I purchased one, but I was too far into the project to turn back. This likely wouldn't be an issue for a user, but was just because I was calling the API so much for trial and error. This data contains information on standings, teams, and players from roughly the years of 2014-2024. There are occasionally years of missing data from certain teams.</p>",
        "<p>The About tab contains information on the broader project. The Data Download tab displays and downloads the data tables retrieves from the API given certain user selected inputs. The Data Exploration tab contains data visualization based on several metrics. There are plots for wins and losses, points scored and rebounds by minute played, and player body dimensions by position. </p>"))),
      
      # Data Download Tab--
      nav_panel("Data Download", tableOutput("standings_table"), tableOutput("team_table"), tableOutput("players_table")),
      
      # Data Exploration Tab ----
      nav_panel("Data Exploration", plotOutput("standingsPlot"), plotOutput("teamPlot1"), plotOutput("teamPlot2"), plotOutput("playersPlot"), textOutput("info"))
    )
  )
)


server <- function(input, output, session) {
  
  #Get standings data based on user inputted season and team
  getstandingsData <- reactive({
    season <- input$seasons
    team <- as.numeric(gsub("([0-9]+).*$", "\\1", input$teams))
    
    newstandingsData <- standings_query(season, team) 
    newstandingsData
  })
  
  #Get team data based on user inputted season and team
  getteamData <- reactive({
    season <- input$seasons
    team <- as.numeric(gsub("([0-9]+).*$", "\\1", input$teams))

    newteamData <- team_stats_query(season, team) 
    newteamData
  })
  
  #Get player data based on user inputted season, team, and position
  getplayersData <- reactive({
    season <- input$seasons
    team <- as.numeric(gsub("([0-9]+).*$", "\\1", input$teams))
    positions <- input$positions

    newplayersData <- players_query(season, team) |> filter(pos == positions)
    newplayersData
  })
  
  #Display data
  #create output of observations
  output$standings_table <- renderTable({
    #get data
    standingstableData <- getstandingsData()
    #write csv
    write.csv(standingstableData, "standingsData.csv")
    #display data
    standingstableData
  })
  
  output$team_table <- renderTable({
    #get data
    teamtableData <- getteamData()
    #write csv
    write.csv(teamtableData, "teamData.csv")
    #display data
    teamtableData
  })
  
  output$players_table <- renderTable({
    #get data
    playerstableData <- getplayersData()
    #write csv
    write.csv(playerstableData, "playersData.csv")
    #display data
    head(playerstableData)
  })
  
  #create a standings data table table
  
  output$standingsPlot <- renderPlot({
    #get data
    standingsData <- getstandingsData()
    
    #base plotting object
    g <- ggplot(standingsData, aes(x = Metric, y = Count)) + geom_bar(stat = "identity") + labs(title = "Wins and Losses")
    g
  })
  
  #create plot
  output$teamPlot1 <- renderPlot({
    #get data
    teamData <- getteamData()

    #base plotting object
    g <- ggplot(teamData, aes(x = overall_mpg, y = overall_ppg, label = last_name)) + geom_text(hjust = 0, nudge_x = 0.20) + labs(title = "Points Scored by Minutes Played per Game", x = "Average Minutes Played Per Game", y = "Average Points Scored Per Game", colour = "Percent Games Started")

    if (input$games_start_pct) {
      g + geom_point(size = input$size, aes(col = games_start_pct))
    } else {
      g + geom_point(size = input$size)
    }
  })

  output$teamPlot2 <- renderPlot({
    #get data
    teamData <- getteamData()

    #base plotting object
    g <- ggplot(teamData, aes(x = overall_mpg, y = overall_rpg, label = last_name)) + geom_text(hjust = 0, nudge_x = 0.20) + labs(title = "Rebounds by Minutes Played per Game", x = "Average Minutes Played Per Game", y = "Average Rebounds Per Game", colour = "Percent Games Started")

    if (input$games_start_pct) {
      g + geom_point(size = input$size, aes(col = games_start_pct))
    } else {
      g + geom_point(size = input$size)
    }
  })
  
  #create plot
  output$playersPlot <- renderPlot({
      #get data
      playersData <- getplayersData()
      
      #base plotting object
      g <- ggplot(playersData, aes(x = weight_pounds, y = height_inches, label = lastname)) + 
        geom_point(size = input$size) + geom_text(hjust = 0, nudge_x = 0.20) + labs(title = "Player Body Dimensions", y = "Height (Inches)", x = "Weight (Pounds)")
      
      g
    })
  
  #create text info
  output$info <- renderText({
    #get data
    playersData <- getplayersData()

    #paste info out
    paste("The average body height for the", input$positions, "position on the", input$teams,"is", round(mean(playersData$height_meters, na.rm = TRUE), 2), "meters", "and the average weight is", round(mean(playersData$weight_pounds, na.rm = TRUE), 2), "pounds", sep = " ")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
