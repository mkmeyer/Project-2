# Project-2: Analyzing NBA Data

This app takes in user inputted information about one of the 32 teams in the NBA over the period from 2015 until 2024 then returns various data tables and graphics displaying standings records as well as trends in scoring, rebounds, minutes played, and player body dimension and position. The goal of this app is to introduce users to NBA teams and players by visualizing winningness and scoring. 

To run this app, the following packages are needed: shiny, tidyverse, jsonlite, httr, ggplot2, bslib, and maps. 
All of those packages can be installed using this line of code:  install.packages(c("shiny", "tidyverse", "jsonlite", "httr", "ggplot2", "bslib", "maps"))

To run the app, use this code: shiny::runGitHub('Project-2', 'mkmeyer')

I also want to provide credit that when I was trying to find a new graph to include and had the idea to make a world map, I was unsure where to begin. I googled and found this (https://stackoverflow.com/questions/71858134/create-ggplot2-map-in-r-using-count-by-country) stack overflow post very helpful.