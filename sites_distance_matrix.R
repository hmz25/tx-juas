#install.packages(c("sf", "sp", "geosphere", "TSP"))

# Load the required libraries
library(sf)
library(geosphere)
library(TSP)
library(tidyverse)

sites_2024 <- st_read("~/Desktop/ECOLAB SITES.kml")
double_visit_sites_2025 <- st_read("~/Desktop/ecolab sites 2025.kml")
single_visit_sites_2025 <- st_read("~/Desktop/drone flights.kml")

ecolab_sites <- rbind(sites_2024, double_visit_sites_2025, single_visit_sites_2025)

ecolab_sites <- ecolab_sites |> 
  select("Name", "geometry") |> 
  filter(!(Name %in% c("Williamson 2022-13", "Glimmer (Travis 2023-84)")))

centroids <- st_centroid(ecolab_sites)

# Convert centroids to longitude and latitude
coords <- st_coordinates(centroids)

# Calculate the distance matrix
distance_matrix <- distm(coords, fun = distHaversine)

# Create a TSP object
tsp_instance <- TSP(distance_matrix)

# Solve the TSP
tour <- solve_TSP(tsp_instance)

# Get the optimized order of sites
optimized_order <- as.integer(tour)

# Reorder centroids based on the optimized route
optimized_sites <- centroids[optimized_order, ]

# Optionally, visualize the optimized route
plot(st_geometry(ecolab_sites), col = "blue")
plot(st_geometry(optimized_sites), col = "red", add = TRUE)

