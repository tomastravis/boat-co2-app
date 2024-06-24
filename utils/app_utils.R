library(here)
library(igraph)
source("utils/helper_functions.R")

# Read network data and cluster coordinates from CSV files
network <- fread(here("data", "network.csv"), stringsAsFactors = FALSE)
cluster_coordinates <- fread(here("data", "cluster_coordinates.csv"), stringsAsFactors = FALSE)
boat_co2_data_list <- fread(here("data", "boat_co2_data_list.csv"), stringsAsFactors = FALSE)
ports_data <- fread(here("data","ports_data.csv"), stringsAsFactors = FALSE)

# Load names separately for selectize inputs
boat_names <- unique(boat_co2_data_list$name)
port_names <- unique(ports_data$PORT)

# Select necessary columns from the network data
nn <- dplyr::select(network, cluster, lag_cluster)

# Create an undirected graph from the network data using the 'igraph' package
gg <- graph_from_data_frame(nn, directed = FALSE)

# Assign distances to edges in the graph from the 'distance' column
E(gg)$distance <- network$distance

# Convert cluster information to a data table
dt <- as.data.frame(cbind(V = c(1:length(V(gg))), cluster = names(V(gg))))
dt$cluster <- as.integer(dt$cluster)
dt$V <- as.integer(dt$V)
dt <- left_join(dt, cluster_coordinates, by = "cluster")
dt <- as.data.table(dt) 
