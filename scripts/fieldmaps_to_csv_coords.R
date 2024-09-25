library(tidyverse)
library(sf)

wade_tree_coord <- read_csv("C:/Users/hmz25/Desktop/TX 2024 analysis/wade_trees_csv.csv")

wade_tree_coord$geom <- gsub("Point \\(|\\)", "", wade_tree_coord$geom)
wade_tree_coord_xy <- tidyr::separate(wade_tree_coord, geom, into = c("X", "Y"), sep = " ")
wade_tree_coord_xy$X <- as.numeric(wade_tree_coord_xy$X)
wade_tree_coord_xy$Y <- as.numeric(wade_tree_coord_xy$Y)

wade_tree_coord_sf <- st_as_sf(wade_tree_coord_xy, coords = c("X", "Y"), crs = 3857) # Assuming Web Mercator CRS (EPSG:3857)

#transform to WGS84 (lat/lon)
wade_tree_coord_sf <- st_transform(wade_tree_coord_sf, crs = 4326)

#add lat long columns to datasheet + clean up datasheet
wade_tree_coord_xy$longitude <- st_coordinates(wade_tree_coord_sf)[,1]
wade_tree_coord_xy$latitude <- st_coordinates(wade_tree_coord_sf)[,2]

wade_tree_coord_xy <- wade_tree_coord_xy %>% 
  select("longitude", "latitude")

wade_tree_coord_xy

#plot to check if looks good
points <- st_as_sf(wade_tree_coord_xy, coords = c("longitude", "latitude"), crs = 4326)

plot(st_geometry(points), pch=16, col="navy") #nice

#export csv
write.csv(wade_tree_coord_xy, "C:/Users/hmz25/Desktop/TX 2024 analysis/wade_tree_lat_long.csv")

#repeat for sweeten
sweet_tree_coord <- read_csv("C:/Users/hmz25/Desktop/TX 2024 analysis/sweeten_trees_csv.csv")

sweet_tree_coord$geom <- gsub("Point \\(|\\)", "", sweet_tree_coord$geom)
sweet_tree_coord_xy <- tidyr::separate(sweet_tree_coord, geom, into = c("X", "Y"), sep = " ")
sweet_tree_coord_xy$X <- as.numeric(sweet_tree_coord_xy$X)
sweet_tree_coord_xy$Y <- as.numeric(sweet_tree_coord_xy$Y)

sweet_tree_coord_sf <- st_as_sf(sweet_tree_coord_xy, coords = c("X", "Y"), crs = 3857) 

#transform to WGS84 (lat/lon)
sweet_tree_coord_sf <- st_transform(sweet_tree_coord_sf, crs = 4326)

#add lat long columns to datasheet + clean up datasheet
sweet_tree_coord_xy$longitude <- st_coordinates(sweet_tree_coord_sf)[,1]
sweet_tree_coord_xy$latitude <- st_coordinates(sweet_tree_coord_sf)[,2]

sweet_tree_coord_xy <- sweet_tree_coord_xy %>% 
  select("longitude", "latitude")

sweet_tree_coord_xy

#plot to check if looks good
points <- st_as_sf(sweet_tree_coord_xy, coords = c("longitude", "latitude"), crs = 4326)

plot(st_geometry(points), pch=16, col="navy") #nice

#export csv
write.csv(sweet_tree_coord_xy, "C:/Users/hmz25/Desktop/TX 2024 analysis/sweeten_tree_lat_long.csv")
