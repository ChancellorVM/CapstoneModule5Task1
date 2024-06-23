# CapstoneModule5Task1


# ui.R code
# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
            titlePanel("Bike-sharing demand prediction app"), 
            # Create a side-bar layout
            sidebarLayout(
              # Create a main panel to show cities on a leaflet map
              mainPanel(
                leafletOutput("city_bike_map")
              ),
              # Create a side bar to show detailed plots for a city
              sidebarPanel(
                # select drop down list to select city
              ))
  ))


# server.R code
# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  city_weather_bike_df <- data.frame(
    CITY_ASCII = c(),
    LNG = c(),
    LAT = c(),
    TEMPERATURE = c(),
    HUMIDITY = c(),
    BIKE_PREDICTION = c(),
    BIKE_PREDICTION_LEVEL = c(),
    LABEL = c(),
    DETAILED_LABEL = c(),
    FORECASTDATETIME = c()
  )
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  #city_weather_bike_df <- test_weather_data_generation()
  city_weather_bike_df <- test_weather_data_generation()
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
   cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII,LAT,LNG,BIKE_PREDICTION,BIKE_PREDICTION_LEVEL,
             LABEL,DETAILED_LABEL,FORECASTDATETIME,TEMPERATURE) %>%
    summarize(BIKE_PREDICTION = max(BIKE_PREDICTION))
  # Observe drop-down event
  
  # Then render output plots with an id defined in ui.R
  output$city_bike_map <- renderLeaflet({
    leaflet(cities_max_bike) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~LNG,
        lat = ~LAT,
        radius = ~ifelse(BIKE_PREDICTION_LEVEL == "Small", 6, 
                         ifelse(BIKE_PREDICTION_LEVEL == "Medium", 10, 12)),
        color = ~ifelse(BIKE_PREDICTION_LEVEL == "Small", "green", 
                        ifelse(BIKE_PREDICTION_LEVEL == "Medium", "yellow", "red")),
        popup = ~LABEL
      )
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
 })
})

