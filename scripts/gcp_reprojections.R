library(terra)
library(sf)
library(tidyverse)

#load in ortho for CRS (all orthos are in same crs, so should be able to do any)
ortho <- rast("C:/Users/hmz25/Documents/pix4d/fisher_20250110/3_dsm_ortho/2_mosaic/fisher_20250110_transparent_mosaic_group1.tif")

# plotRGB(ortho)
# crs(ortho)

# #load in GCP file
# gcp <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/emlid data 2025/20250110_fisher.csv")
# 
# #create spatial feature from CSV GCP file 
# gcp_sf <- st_as_sf(gcp, coords = c("Longitude", "Latitude"), crs = 4326)
# 
# #reproject GCP to be in same CRS as ortho 
# gcp_sf_reproj <- st_transform(gcp_sf, crs = terra::crs(ortho))
# 
# #create df for GCP in correct CRS 
# gcp_sf_reproj_df <- cbind(st_coordinates(gcp_sf_reproj), gcp["Ellipsoidal height"])
# 
# #plot to see if align 
# # plotRGB(ortho)
# # plot(gcp_sf_reproj, add = TRUE)
# 
# #save results in a new folder 
# write_csv(gcp_sf_reproj_df, "C:/Users/hmz25/Desktop/2025 GCP corrected/20250110_fisher_corrected.csv")

#define input and output directories
input_dir <- "C:/Users/hmz25/Desktop/emlid data 2025-selected"
output_dir <- "C:/Users/hmz25/Desktop/2025 GCP corrected"

#list all CSV files in the input directory
csv_files <- list.files(input_dir, pattern = "*.csv", full.names = TRUE)

file = csv_files[3]

# Loop through each CSV file
for (file in csv_files) {
  # Read the GCP file
  gcp <- read_csv(file)
  
  # Create spatial feature from CSV GCP file
  gcp_sf <- st_as_sf(gcp, coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Reproject GCP to be in the same CRS as ortho
  gcp_sf_reproj <- st_transform(gcp_sf, crs = terra::crs(ortho))
  
  # Create df for GCP in correct CRS
  gcp_sf_reproj_df <- cbind(st_coordinates(gcp_sf_reproj), gcp["Ellipsoidal height"])
  
  # Construct output filename
  file_name <- tools::file_path_sans_ext(basename(file))
  output_file <- file.path(output_dir, paste0(file_name, "_corrected.csv"))
  
  # Save results
  write_csv(gcp_sf_reproj_df, output_file)
}


# for 2024 gcps -----------------------------------------------------------

#define input and output directories
input_dir <- "C:/Users/hmz25/Desktop/Emlid logs"
output_dir <- "C:/Users/hmz25/Desktop/2024 GCP corrected"

#list all CSV files in the input directory
csv_files <- list.files(input_dir, pattern = "*.csv", full.names = TRUE)

#file = csv_files[3]

# Loop through each CSV file
for (file in csv_files) {
  # Read the GCP file
  gcp <- read_csv(file)
  
  # Create spatial feature from CSV GCP file
  gcp_sf <- st_as_sf(gcp, coords = c("y", "x"), crs = 4326)
  
  # Reproject GCP to be in the same CRS as ortho
  gcp_sf_reproj <- st_transform(gcp_sf, crs = terra::crs(ortho))
  
  # Create df for GCP in correct CRS
  gcp_sf_reproj_df <- cbind(st_coordinates(gcp_sf_reproj), gcp["z"])
  
  # Construct output filename
  file_name <- tools::file_path_sans_ext(basename(file))
  output_file <- file.path(output_dir, paste0(file_name, "_corrected.csv"))
  
  # Save results
  write_csv(gcp_sf_reproj_df, output_file)
}
