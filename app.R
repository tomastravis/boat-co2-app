library(shiny)
library(bslib)
library(leaflet)
library(shinydashboard)
library(flexdashboard)
library(data.table)  # For fread function
library(dplyr)  # For filter function

# Load helper functions from the specified file
source("utils/helper_functions.R")
source("utils/app_utils.R")

# Load the CSV file and extract unique boat names
boat_co2_data_list <- fread("data/boat_co2_data_list.csv")
boat_names <- unique(boat_co2_data_list$name)

# Define the map_route function that uses boat name
map_route <- function(x, boat_name) {
  route <- x[[1]]  # Extract route from the input
  
  # Find the relevant boat data from boat_co2_data_list using boat name
  boat_data <- filter(boat_co2_data_list, name == boat_name)
  
  # If no data is found for the given boat name, return a message
  if (nrow(boat_data) == 0) {
    stop("No data found for the given boat name.")
  }
  
  # Extract necessary information from boat_data
  fuel_consumption <- boat_data$fuel_kg_per_nm_consumption
  emissions <- boat_data$c02_kg_per_nm_emissions
  
  # Adjust coordinates for the antimeridian
  route$lon[route$lon_1 > 100 & route$lon < (-100)] <- 179
  route$lon[route$lon_1 < (-100) & route$lon > 100] <- (-179)
  
  # Create a Leaflet map object
  route_map <- leaflet(route) %>%
    addTiles()  # Add default OpenStreetMap map tiles
  
  # Add polylines for each segment of the route
  for (i in 1:nrow(route)) {
    route_map <- route_map %>%
      addPolylines(
        lng = c(route$lon[i], route$lon_1[i]),
        lat = c(route$lat[i], route$lat_1[i]),
        weight = 1  # Set the weight of the polyline
      )
  }
  
  # Calculate total fuel consumption and emissions
  total_fuel_consumption <- fuel_consumption * x[[2]]
  total_emissions <- emissions * x[[2]]
  
  # Add markers for the start and end points of the route
  route_map <- route_map %>%
    addCircleMarkers(lng = route$lon[1], lat = route$lat[1]) %>%
    addCircleMarkers(lng = route$lon[nrow(route)], lat = route$lat[nrow(route)], color = "Red") %>%
    addPopups(
      lng = mean(route$lon, na.rm = TRUE),
      lat = mean(route$lat, na.rm = TRUE),
      popup = paste(
        x[[2]], "nautical miles","<br>",
        "Fuel Consumption (kg):", total_fuel_consumption, "<br>",
        "CO2 Emissions (kg):", total_emissions, "<br>",
        "Boat Name:", boat_name
      )
    )
  
  # Return the map object
  return(route_map)
}

# Define UI ----
ui <- page_sidebar(
  title = "Boat Router",
  sidebar = sidebar(
    card(
      card_header("Select Boat"),
      selectInput("selected_boat", "Choose a boat:", choices = boat_names)  # Add selectInput for boat names
    ),
    card(
      card_header("Initial Coordinates"),
      numericInput("lat_start", "Input initial latitude", value = 37.1478730367156),
      numericInput("lon_start", "Initial Longitude", value = -6.997123270950804)
    ),
    card(
      card_header("End Coordinates"),
      numericInput("lat_end", "Input end latitude", value = 24.07814786082897),
      numericInput("lon_end", "Input end Longitude", value = -74.4727176343569),
      card_footer(
        actionButton("calculate_route", "Calculate Route")
      )
    )
  ),
  titlePanel("Welcome to Open Boat Router"),
  card(
    leafletOutput("map"),
    valueBoxOutput("nautical_miles")  # Add the valueBoxOutput for displaying the nautical miles
  ),
  textOutput("selected_boat_name")  # Output to display the selected boat name
)

# Define server logic ----
server <- function(input, output) {
  
  output$selected_boat_name <- renderText({
    paste("Selected Boat:", input$selected_boat)
  })
  
  observeEvent(input$calculate_route, {
    # Get initial and end coordinates from user input
    lat_start <- input$lat_start
    lon_start <- input$lon_start
    lat_end <- input$lat_end 
    lon_end <- input$lon_end
    
    # Define origin and destination points
    origin <- c(lon_start, lat_start)
    destination <- c(lon_end, lat_end)
    
    # Find the closest cluster/point/vertex to the origin and destination points
    point_1 <- find_closest_cluster(origin[1], origin[2])
    point_2 <- find_closest_cluster(destination[1], destination[2])
    
    # Calculate extra distance based on the distances from the origin and destination points to their closest clusters
    distance_1 <- dtHaversine(dt$lat[dt$V == point_1], dt$lon[dt$V == point_1], origin[2], origin[1]) / 1000 / 1.852
    distance_2 <- dtHaversine(dt$lat[dt$V == point_2], dt$lon[dt$V == point_2], destination[2], destination[1]) / 1000 / 1.852
    extra_distance <- distance_1 + distance_2
    
    # Find the best route between the origin and destination points
    best_route <- find_route(gg, point_1, point_2, extra_distance)
    
    # Update the value in the value_box to display the total nautical miles
    output$nautical_miles <- renderValueBox({
      value_box(
        title = "Nautical Miles",
        value = best_route[[2]],
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "teal"
      )
    })
    
    # Render the map with the new route
    output$map <- renderLeaflet({
      map_route(best_route, input$selected_boat) %>%
        addProviderTiles(providers$Stadia.StamenTonerLite,
                         options = providerTileOptions(noWrap = TRUE))
    })
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
