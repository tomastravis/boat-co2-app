# Load required libraries
library(data.table)  # For efficient data manipulation
library(dplyr)       # For data manipulation and transformation
library(leaflet)     # For interactive maps
library(igraph)      # For graph manipulation and analysis
library(sp)          # For spatial data classes and methods
#library(rgdal)       For spatial data I/O
library(sf)          # For working with spatial data
library(mapview)     # For interactive viewing of spatial data
library(tidyverse)   # For data manipulation and visualization
library(scales)      # For correct number manipulation

### Set map limits
# Define latitude and longitude limits for the map
#lon_max = 179.9999
#lon_min = (-179.9999)
#lat_max = 89.9999
#lat_min = (-89.9999)

# Load Haversine distance function
source("utils/dtHaversine_function.R")

# Load the CSV file and extract unique boat names and port names
boat_co2_data_list <- fread("data/boat_co2_data_list.csv")
boat_names <- unique(boat_co2_data_list$name)
ports_data <- fread("data/ports_data.csv")  # Assuming you have a CSV file with port data
port_names <- unique(ports_data$PORT)

### Shortest route function
find_route <- function(graph, V1, V2, extra_distance) {
  # Find shortest path between two nodes in a graph
  
  # Calculate shortest paths using Dijkstra's algorithm (igraph library -> shortest_paths function)
  aa <- shortest_paths(
    graph,
    from = V1,
    to = V2,
    mode = "all",
    weights = E(graph)$distance,
    output = "vpath",
    predecessors = FALSE,
    inbound.edges = FALSE,
    algorithm = "dijkstra"
  )
  
  # Extract route from the shortest path
  route <- as.data.table(cbind(V = as.vector(aa$vpath[[1]])))
  route <- left_join(route, dt, by = "V")
  
  # Calculate distances between consecutive points in the route
  route$lon_1[2:nrow(route)] <- route$lon[1:(nrow(route) - 1)]
  route$lat_1[2:nrow(route)] <- route$lat[1:(nrow(route) - 1)]
  route[, distance := dtHaversine(lat, lon, lat_1, lon_1)]
  route$distance <- round(route$distance / 1852, 1)  # Convert to nautical miles
  
  # Calculate total distance of the route
  distance <- 10 * round((sum(route$distance, na.rm = TRUE) + extra_distance) / 10, 0)
  
  # Return the route and total distance
  return(list(route, distance))
}

### Function to plot the map with the route
map_route <- function(x, boat_name, start_coords, end_coords) {
  route <- x[[1]]
  
  # Find the relevant boat data from boat_co2_data_list using the boat name
  boat_data <- boat_co2_data_list[boat_co2_data_list$name == boat_name, ]
  
  # If no data is found for the given boat name, return a message
  if (nrow(boat_data) == 0) {
    stop("No data found for the given boat name.")
  }
  
  # Extract necessary information from boat_data
  my_imo <- boat_data$imo
  fuel_consumption <- boat_data$fuel_kg_per_nm_consumption
  emissions <- boat_data$c02_kg_per_nm_emissions
  
  # Adjust coordinates for the antimeridian
  route$lon[route$lon_1 > 100 & route$lon < (-100)] <- 179
  route$lon[route$lon_1 < (-100) & route$lon > 100] <- (-179)
  
  # Create a Leaflet map object
  route_map <- leaflet(route) %>%
    addTiles()
  
  # Add polylines for each segment of the route
  for (i in 1:nrow(route)) {
    route_map <- route_map %>%
      addPolylines(
        lng = c(route$lon[i], route$lon_1[i]),
        lat = c(route$lat[i], route$lat_1[i]),
        weight = 1
      )
  }
  
  # Add markers for the start and end points of the route
  route_map <- route_map %>%
    addCircleMarkers(lng = start_coords[1], lat = start_coords[2], color = "green", radius = 5, label = "Start Point") %>%
    addCircleMarkers(lng = end_coords[1], lat = end_coords[2], color = "red", radius = 5, label = "End Point")
  
  total_fuel_consumption <- fuel_consumption * x[[2]] / 1000
  total_emissions <- emissions * x[[2]] / 1000
  
  list(
    map = route_map,
    nautical_miles = comma(x[[2]], accuracy = 0.1),
    total_fuel_consumption = comma(total_fuel_consumption, accuracy = 0.1),
    total_emissions = comma(total_emissions, accuracy = 0.1)
  )
}



### Function to find the closest cluster/node in a graph
find_closest_cluster <- function(lon, lat) {
  set_points <- dt  # Dataset containing cluster coordinates
  
  # Calculate distances from the input point to each cluster
  lat_1 = lat
  lon_1 = lon
  set_points[, distance := dtHaversine(lat, lon, lat_1, lon_1) / 1000 / 1.852]  # Convert to nautical miles
  
  # Find the closest cluster based on the minimum distance
  closest <- set_points$V[set_points$distance == min(set_points$distance)]
  
  # Return the ID of the closest cluster
  return(closest)
}

