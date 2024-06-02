# Function to calculate the Haversine distance between two points given their latitudes and longitudes, 
# where r is the ecuatorial raius of the Earth in km
dtHaversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137) {
  
  # Convert degrees to radians for trigonometric calculations
  radians <- pi/180
  
  # Convert latitude and longitude values to radians
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  
  # Calculate differences in latitude and longitude
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  
  # Haversine formula for distance calculation
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  
  # Calculate the distance using the Haversine formula
  distance <- 2 * atan2(sqrt(a), sqrt(1 - a)) * r
  
  # Return the calculated distance
  return(distance)
}
