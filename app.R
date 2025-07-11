#Loading libraries required for app execution
library("shiny")
library("tidyverse")
library("jsonlite")
library("httr")
library("ggplot2")
library("bslib")
library("maps")

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
  standings_data$total_win <- as.numeric(standings_data$win$total)
  standings_data$total_loss <- as.numeric(standings_data$loss$total)
  standings_data$conference_win <- as.numeric(standings_data$conference$win)
  standings_data$conference_loss <- as.numeric(standings_data$conference$loss)
  standings_data$division_win <- as.numeric(standings_data$division$win)
  standings_data$division_loss <- as.numeric(standings_data$division$loss)
  standings_data$home_win <- as.numeric(standings_data$win$home)
  standings_data$home_loss <- as.numeric(standings_data$loss$home)
  standings_data$away_win <- as.numeric(standings_data$win$away)
  standings_data$away_loss <- as.numeric(standings_data$loss$away)
  
  standings_data <- standings_data |>
    select(c(season, team_name, conference_name, conference_rank, division_rank, win_pct, loss_pct, total_win, total_loss, conference_win, conference_loss, division_win, division_loss, home_win, home_loss, away_win, away_loss))
  
  return(standings_data)
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
    mutate(minutes = minute(minutes1)) |> #extracting only the minutes part of time
    mutate(starter = ifelse(is.na(pos), NA, 1)) |> #classifying starters as those with assigned positions
    group_by(first_name, last_name) |>
    mutate(games_not_played = sum(minutes = 0 | is.na(minutes))) |> #classifying games not played as those with 0 minutes played
    mutate(games = n()) |>
    mutate(games_played = n() - games_not_played) |>
    mutate(games_started = sum(starter, na.rm = TRUE)) |>
    mutate(games_start_pct = games_started/games_played) |>
    mutate(overall_ppg = sum(points, na.rm = TRUE)/games_played) |> #calculating points per game
    mutate(overall_rpg = sum(totReb, na.rm = TRUE)/games_played) |> #calculating rebounds per game
    mutate(overall_apg = sum(assists, na.rm = TRUE)/games_played) |> #calculating assists per game
    mutate(overall_mpg = sum(minutes, na.rm = TRUE)/games_played) |> #calculating minutes per game
    mutate(likely_starter = ifelse(games_start_pct > 0.5, 1, 0)) |> #classifying likely starters as those who started in more than 50% of the games they played
    fill(starter, .direction = "downup") |>
    select(first_name, last_name, pos, overall_ppg, overall_apg, overall_rpg, overall_mpg, points, totReb, assists, games, 
           games_played, games_not_played, games_started, games_start_pct, starter, likely_starter) |>
    distinct()
  
  return(team_data)
}

#Creating a lists of options for user input for seasons and teams
#These options will affect what the API retrieves
seasons <- c(2015:2024)
nbateams_url <- "https://api-nba-v1.p.rapidapi.com/teams" #base URL to extract NBA teams from API
nbateams_response <- httr::GET(nbateams_url, 
                      add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 
                                  'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), 
                      content_type("application/octet-stream")) #accessing the API

teams <- fromJSON(rawToChar(nbateams_response$content))
team_data <- as_tibble(teams$response) #pulling list of teams

nba_teams <- team_data[(team_data$nbaFranchise == TRUE), ] #selecting only NBA teams
nba_teams_list <- paste(nba_teams$id, nba_teams$name, sep = " ") #combining team ID and team name to make the ID both usable in code (the number is needed for API input) and readable to the user

#Creating lists of positions and divisions of play for user input
#These are variables within the data set, and user input will subset the data
positions <- c("All", "G", "SF", "SG", "PF", "C", "F", "F-C", "F-G", "G-F")
divisions <- c("Overall", "Conference", "Division", "Home", "Away")
world_map <- map_data(map = "world")


ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Statistics: Analyzing all 32 Teams from 2015-2024"),
  
  # Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel( #Specifying User Inputs
      h3("Select the season:"),
      selectizeInput("seasons", "Season", selected = 2020, choices = levels(as.factor(seasons))),
      
      h3("Select the team:"), 
      selectizeInput("teams", "Team", selected = "1 Atlanta Hawks", choices = levels(as.factor(nba_teams_list))),
      
      h3("Select the Play Setting:"),
      selectizeInput("divisions", "Play Setting", selected = "Overall", choices = levels(as.factor(divisions))),
      
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
        "<p>This data is sourced from <a href = 'https://rapidapi.com/api-sports/api/api-nba' >Rapid API</a>. I found this using the <a href = 'https://github.com/public-apis/public-apis?tab=readme-ov-file#sports--fitness' >GitHub</a> link provided for this project. I had to make an API key for this project, but did not need to pay for a subscription. I ended up making so many calls that I purchased one, but I was too far into the project to turn back. This likely wouldn't be an issue for a user, but was just because I was calling the API so much for trial and error. This data contains information on standings, teams, and players from roughly the years of 2015-2024. There are occasionally years of missing data from certain teams. Additionally, some teams noted multiple positions for a player (like 'F-C' instead of just 'F' or 'C'). There was a lot of data cleaning involved because sometimes variables were stored within other variables (there was conference$name, conference$rank, conference$win) for example. </p>",
        "<p>The About tab contains information on the broader project. The Data Download tab displays and downloads the data tables retrieved from the API given certain user selected inputs. The first Data Exploration tab contains data visualizations and tables for team wide data. Users can change the team and season supplied in order to access different datasets from the API. Users may also toggle the division to see win/loss records for different settings of play (overall, conference, division, home, away) and can toggle a color feature indicating a player's likelihood of starting the game. The second Data Exploration tab contains data visualizations and tables for player data. As before, users choose the team and season in order to access the data but can also alter the variable position to see different subsets of the data. There are scatterplots of points scored and rebounds by minute played, and player body dimensions by position. There is also a map displaying the countries where players were born.  </p>"))),
      
      # Data Download Tab--
      nav_panel("Data Download", textOutput("standingsdatainfo"), tableOutput("standingsTable"), textOutput("teamdatainfo"), tableOutput("teamTable"), textOutput("playersdatainfo"), tableOutput("playersTable")),
      
      # Data Exploration Tab 1 ----
      nav_panel("Data Exploration-Overall Team Data", textOutput("recordinfo"), tableOutput("recordTable"), plotOutput("standingsPlot"), plotOutput("teamPlot1"), plotOutput("teamPlot2")),
      # Data Exploration Tab 2 ----
      nav_panel("Data Exploration-Position and Player Data", textOutput("positionsinfo"), tableOutput("positionsTable"), plotOutput("playersoverallPlot"), plotOutput("playerssubsetPlot"), textOutput("info"), plotOutput("mapPlot"))
    )
  )
)


server <- function(input, output, session) {
  
  #Get standings data based on user inputted season and team
  getstandingsData <- reactive({
    season <- input$seasons
    team <- as.numeric(gsub("([0-9]+).*$", "\\1", input$teams)) #extracting the team code from the provided input
    
    newstandingsData <- standings_query(season, team) #calling the standings API function to get the data
    newstandingsData
  })
  
  #Get team data based on user inputted season and team
  getteamData <- reactive({
    season <- input$seasons
    team <- as.numeric(gsub("([0-9]+).*$", "\\1", input$teams)) #extracting the team code from the provided input

    newteamData <- team_stats_query(season, team) 
    newteamData
  })
  
  #Get player data based on user inputted season
  getplayersData <- reactive({
    season <- input$seasons
    team <- as.numeric(gsub("([0-9]+).*$", "\\1", input$teams)) #extracting the team code from the provided input

    newplayersData <- players_query(season, team)
    newplayersData
  })
  
  #creating text info explaining the standings data
  output$standingsdatainfo <- renderText({
    paste("This data was downloaded from the standings endpoint using the user provided inputs for team and season") 
  })
  
  #creating the standings data
  output$standingsTable <- renderTable({
    #get data
    standingstableData <- getstandingsData()
    #write csv
    write.csv(standingstableData, "standingsData.csv")
    #display data
    standingstableData
  })

  #creating text info explaining the team data
  output$teamdatainfo <- renderText({
    paste("This data was downloaded from the player statistics endpoint using the user provided inputs for team and season") 
  })

  #creating the team data
  output$teamTable <- renderTable({
    #get data
    teamtableData <- getteamData()
    #write csv
    write.csv(teamtableData, "teamData.csv")
    #display data
    teamtableData
  })

  #creating text info explaining the players data
  output$playersdatainfo <- renderText({
    paste("This data was downloaded from the players endpoint using the user provided inputs for team and season") 
  })

  #creating the playerss data
  output$playersTable <- renderTable({
    #get data
    playerstableData <- getplayersData()
    #write csv
    write.csv(playerstableData, "playersData.csv")
    #display data
    head(playerstableData)
  })
  
  #create text info explaining the records table
  output$recordinfo <- renderText({
    paste("The", input$divisions, "record for the", input$teams,"in", input$seasons, "was", sep = " ") #Adding text explaining the dynamic contingency tables
  })
  
  #create a dynamic standings (records) table
  output$recordTable <- renderTable({
    #get data
    standingsData <- getstandingsData()
    
    #checking the divisions (play settings) input
    if (input$divisions == "Away") {
      away <- standingsData |> select(away_win, away_loss) |>
        mutate(`Win Percent` = (away_win/(away_win + away_loss)*100)) |>
        rename("Wins" = away_win) |>
        rename("Losses" = away_loss)
      away
    } else if (input$divisions == "Home") {
      home <- standingsData |> select(home_win, home_loss) |>
        mutate(`Win Percent` = (home_win/(home_win + home_loss)*100)) |>
        rename("Wins" = home_win) |>
        rename("Losses" = home_loss)
      home
    } else if (input$divisions == "Conference") {
      conference <- standingsData |> select(conference_win, conference_loss) |>
        mutate(`Win Percent` = (conference_win/(conference_win + conference_loss)*100)) |>
        rename("Wins" = conference_win) |>
        rename("Losses" = conference_loss)
      conference
    } else if (input$divisions == "Division") {
      division <- standingsData |> select(division_win, division_loss) |>
        mutate(`Win Percent` = (division_win/(division_win + division_loss)*100)) |>
        rename("Wins" = division_win) |>
        rename("Losses" = division_loss)
      division
      } else {
        overall <- standingsData |> select(total_win, total_loss) |>
          mutate(`Win Percent` = (total_win/(total_win + total_loss)*100)) |>
          rename("Wins" = total_win) |>
          rename("Losses" = total_loss)
        overall
    }
  })
  
  #create a standings plot
  output$standingsPlot <- renderPlot({
    #get data
    standingsData <- getstandingsData()
    
    #converting the dataset into long form for better plotting use
    standingsdata_long <- standingsData |>
      pivot_longer(cols = 9:17, #selecting numeric columns
                   names_to = "Metric", 
                   values_to = "Count")
    
    #creating a bar plot in order to better visually compare the ratio of wins to losses under each play setting
    g <- ggplot(standingsdata_long, aes(x = Metric, y = Count)) + geom_bar(stat = "identity") + labs(title = "Win and Loss Records at All Play Settings")
    g
  })
  
  #create plot
  output$teamPlot1 <- renderPlot({
    #get data
    teamData <- getteamData()

    #creating a scatter plot to assess the relationship between playing time and points scored. Each data point is labeled with a player's last name, so important players can be easily identified
    g <- ggplot(teamData, aes(x = overall_mpg, y = overall_ppg, label = last_name)) + geom_text(hjust = 0, nudge_x = 0.20) + labs(title = "Points Scored by Minutes Played per Game", x = "Average Minutes Played Per Game", y = "Average Points Scored Per Game", colour = "Percent Games Started")

    if (input$games_start_pct) { #allowing the user to adjust the size of the points to help with readability
      g + geom_point(size = input$size, aes(col = games_start_pct))
    } else {
      g + geom_point(size = input$size)
    }
  })

  output$teamPlot2 <- renderPlot({
    #get data
    teamData <- getteamData()

    #creating a scatter plot to assess the relationship between playing time and rebounds. Each data point is labeled with a player's last name, so important players can be easily identified
    g <- ggplot(teamData, aes(x = overall_mpg, y = overall_rpg, label = last_name)) + geom_text(hjust = 0, nudge_x = 0.20) + labs(title = "Rebounds by Minutes Played per Game", x = "Average Minutes Played Per Game", y = "Average Rebounds Per Game", colour = "Percent Games Started")

    if (input$games_start_pct) { #allowing the user to adjust the size of the points to help with readability
      g + geom_point(size = input$size, aes(col = games_start_pct))
    } else {
      g + geom_point(size = input$size)
    }
  })
  
  #create text info to explain the following table
  output$positionsinfo <- renderText({
    paste("Contingency Table and Numerical Summary: Useful Basketball Statistics by Player Position")
  })
  
  output$positionsTable <- renderTable({
    #get data
    teamData <- getteamData()
    
    #grouping the data by position in order to compare stats across position groups
    position_groups <- teamData |>
      group_by(pos) |>
      filter(!is.na(pos)) |>
      mutate(total_points = sum(points, na.rm = TRUE)) |>
      mutate(total_rebounds = sum(totReb, na.rm = TRUE)) |>
      mutate(avg_points = mean(overall_ppg, na.rm = TRUE)) |>
      mutate(avg_assists = mean(overall_apg, ra.rm = TRUE)) |> 
      mutate(avg_rebounds = mean(overall_rpg, ra.rm = TRUE)) |> 
      mutate(total_assists = sum(assists, na.rm = TRUE)) |>
      select(pos, total_points, total_assists, total_rebounds, avg_points, avg_assists, avg_rebounds) |>
      distinct()
    
    position_groups
  })
  
  #creating a plot of the body dimensions of all players
  output$playersoverallPlot <- renderPlot({
      #get data
      playersData <- getplayersData()

      #creating a scatter plot to better show clusters of players
      g <- ggplot(playersData, aes(x = weight_pounds, y = height_inches, label = lastname, col = pos)) + 
        geom_point(size = input$size) + geom_text(hjust = 0, nudge_x = 0.20) + labs(title = "Player Body Dimensions--All Positions", y = "Height (Inches)", x = "Weight (Pounds)")
      
      g
    })
  
  #creating a plot of the body dimensions of players of a certain position
  output$playerssubsetPlot <- renderPlot({
    #get data
    playersData <- getplayersData() |> filter(pos == input$positions)
    
    #creating a scatter plot to better show clusters of players
    g <- ggplot(playersData, aes(x = weight_pounds, y = height_inches, label = lastname)) + 
      geom_point(size = input$size) + geom_text(hjust = 0, nudge_x = 0.20) + labs(title = "Player Body Dimensions--User Selected Position", y = "Height (Inches)", x = "Weight (Pounds)")
    g
  })
  
  #creating dynamic text explaining the average body dimension statistics
  output$info <- renderText({
    #get data
    playersData <- getplayersData()

    #paste info out
    paste("The average body height for the", input$positions, "position on the", input$teams,"is", round(mean(playersData$height_inches, na.rm = TRUE), 2), "inches", "and the average weight is", round(mean(playersData$weight_pounds, na.rm = TRUE), 2), "pounds", sep = " ")
  })
  
  #creating new plot (I was greatly helped by this stack overflow post: https://stackoverflow.com/questions/71858134/create-ggplot2-map-in-r-using-count-by-country)
  output$mapPlot <- renderPlot({
    #get data
    playersData <- getplayersData()
    
    #reorganizing the data into frequencies by country
    mapData <- playersData |>
      select(birth_country) |>
      group_by(birth_country) |>
      mutate(counts = n()) |>
      distinct() |>
      filter(!is.na(birth_country))
    
    #creating the plot
    world_map <- map_data(map = "world")
    
    g <- ggplot(mapData) +
      geom_map(aes(map_id = birth_country, fill = as.factor(counts)), map = world_map) +
      geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
      expand_limits(x = world_map$long, y = world_map$lat) +
      scale_fill_brewer(name = "Counts", palette = "Accent") +
      theme_void() +
      coord_fixed() + 
      labs(title = "Birth Countries of All Players")
    
    g
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
