#Functions to read in the data using API
standings_query <- function(season, team){
  url <- paste0("https://api-nba-v1.p.rapidapi.com/standings?league=standard", #base of URL
                "&season=", season, #adding season to URL
                "&team=", team #adding team to URL
  )
  
  response <- httr::GET(url, add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 
                                         'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), 
                        content_type("application/octet-stream"))
  
  parsed_nba_info <- fromJSON(rawToChar(response$content))
  standings_data <- as_tibble(parsed_nba_info$response) #pulling response
  return(standings_data)
}

players_query <- function(season, team){
  url <- paste0("https://api-nba-v1.p.rapidapi.com/players?", #base of URL
                "&season=", season, #adding season to URL
                "&team=", team #adding team to URL
  )
  
  response <- httr::GET(url, add_headers('x-rapidapi-key' = '9086777ffcmsh8e20ab7c07d978ep14073cjsndbf0615eb5ec', 
                                         'x-rapidapi-host' = 'api-nba-v1.p.rapidapi.com'), 
                        content_type("application/octet-stream"))
  
  parsed_nba_info <- fromJSON(rawToChar(response$content))
  players_data <- as_tibble(parsed_nba_info$response) #pulling response
  return(players_data)
}
