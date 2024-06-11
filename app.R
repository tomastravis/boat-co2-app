library(shiny)
library(bslib)
library(leaflet)
library(shinydashboard)
library(flexdashboard)
library(data.table)
library(dplyr)
library(bsicons)

# Load helper functions
source("utils/helper_functions.R")
source("utils/app_utils.R")

# Define UI ----
ui <- page_sidebar(
  title = div(img(src = "logo.png", height = "30px"), "Boat CO2"),
  sidebar = sidebar(
    card(
      card_header("Select Boat"),
      div(
        style = "position: relative; z-index: 1050 ; overflow: visible;",
        selectizeInput("selected_boat", "Choose a boat:", choices = boat_names, selected = "MARCO POLO", options = list(dropdownParent = 'body'))
      )),
    card(
      card_header("Route Planning Mode"),
      radioButtons("planning_mode", "Choose route planning mode:",
                   choices = list("Ports" = "ports", "Coordinates" = "coords"),
                   selected = "ports")  # Set "Ports" as the default
    ),
    conditionalPanel(
      condition = "input.planning_mode == 'coords'",
      card(
        card_header("Initial Coordinates"),
        numericInput("lat_start", "Input initial latitude", value = 37.1478730367156),
        numericInput("lon_start", "Initial Longitude", value = -6.997123270950804)
      ),
      card(
        card_header("End Coordinates"),
        numericInput("lat_end", "Input end latitude", value = 24.07814786082897),
        numericInput("lon_end", "Input end Longitude", value = -74.4727176343569)
      )
    ),
    conditionalPanel(
      condition = "input.planning_mode == 'ports'",
      card(
        card_header("Select Initial Port"),
        div(
          style = "position: relative; z-index: 9999 !important; overflow: visible;",
          selectizeInput("initial_port", "Choose initial port:", choices = port_names, selected = "Sevilla", options = list(dropdownParent = 'body'))
        )),
      card(
        card_header("Select End Port"),
        div(
          style = "position: relative; z-index: 1050; overflow: visible;",
          selectizeInput("end_port", "Choose end port:", choices = port_names, options = list(dropdownParent = 'body'))
        ))
    ),
    card(
      card_footer(
        actionButton("calculate_route", "Calculate Route")
      )
    )
  ),
  titlePanel("Boat CO2 Dashboard"),
  textOutput("welcome_message"),
  card(
    leafletOutput("map"),
    fluidRow(
      column(4, valueBoxOutput("nautical_miles")),
      column(4, valueBoxOutput("fuel_consumption")),
      column(4, valueBoxOutput("co2_emissions"))
    )
  ),
  textOutput("selected_boat_name")
)

# Define server logic ----
server <- function(input, output, session) {
  
  output$selected_boat_name <- renderText({
    paste("Selected Boat:", input$selected_boat)
  })
  
  output$welcome_message <- renderText({
    "Welcome to the Boat CO2 Dashboard! Please select a boat and enter the route details to calculate the route."
  })
  
  observeEvent(input$calculate_route, {
    # Clear welcome message
    output$welcome_message <- renderText({ NULL })
    
    if (input$planning_mode == "coords") {
      # Get initial and end coordinates from user input
      lat_start <- input$lat_start
      lon_start <- input$lon_start
      lat_end <- input$lat_end
      lon_end <- input$lon_end
    } else {
      # Get coordinates of selected ports
      initial_port_data <- filter(ports_data, PORT == input$initial_port)
      end_port_data <- filter(ports_data, PORT == input$end_port)
      lat_start <- initial_port_data$LAT
      lon_start <- initial_port_data$LON
      lat_end <- end_port_data$LAT
      lon_end <- end_port_data$LON
    }
    
    # Define origin and destination points
    origin <- c(lon_start, lat_start)
    destination <- c(lon_end, lat_end)
    
    # Find the closest cluster/point/vertex to the origin and destination points
    point_1 <- find_closest_cluster(origin[1], origin[2])
    point_2 <- find_closest_cluster(destination[1], destination[2])
    
    # Get coordinates for the start and end points
    start_coords <- c(dt$lon[dt$V == point_1], dt$lat[dt$V == point_1])
    end_coords <- c(dt$lon[dt$V == point_2], dt$lat[dt$V == point_2])
    
    # Calculate extra distance based on the distances from the origin and destination points to their closest clusters
    distance_1 <- dtHaversine(dt$lat[dt$V == point_1], dt$lon[dt$V == point_1], origin[2], origin[1]) / 1000 / 1.852
    distance_2 <- dtHaversine(dt$lat[dt$V == point_2], dt$lon[dt$V == point_2], destination[2], destination[1]) / 1000 / 1.852
    extra_distance <- distance_1 + distance_2
    
    # Find the best route between the origin and destination points
    best_route <- find_route(gg, point_1, point_2, extra_distance)
    
    # Render the map without the popup
    route_info <- map_route(best_route, input$selected_boat, start_coords, end_coords)
    output$map <- renderLeaflet({
      route_info$map %>%
        addProviderTiles(providers$Stadia.StamenTonerLite, options = providerTileOptions(noWrap = TRUE))
    })
    
    # Update the value in the value boxes to display the route information
    output$nautical_miles <- renderValueBox({
      value_box(
        title = "Nautical Miles",
        value = route_info$nautical_miles,
        showcase = bsicons::bs_icon("bar-chart", fallback = icon("chart-bar")),
        theme = "teal"
      )
    })
    
    output$fuel_consumption <- renderValueBox({
      value_box(
        title = "Fuel Consumption (tonnes)",
        value = route_info$total_fuel_consumption,
        showcase = bsicons::bs_icon("fuel-pump", fallback = icon("gas-pump")),
        theme = "orange"
      )
    })
    
    output$co2_emissions <- renderValueBox({
      value_box(
        title = "CO2 Emissions (tonnes)",
        value = route_info$total_emissions,
        showcase = bsicons::bs_icon("cloud", fallback = icon("cloud")),
        theme = "red"
      )
    })
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
