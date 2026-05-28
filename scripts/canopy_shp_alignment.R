#script to shift tree canopy shape files to align with corrected orthos

library(sf)
library(dplyr)
library(tidyverse)

# #wd for lab desktop
# setwd("C:/Users/hmz25/Box/Katz lab/texas/")

#wd for hz macbook
setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")

#read in for coordinates used to calculate offset (generated in ortho_alignment script)

# coords <- read_csv("aligned_orthos/gcps_sonora_20250115_transparent_mosaic_group1.csv")  
# #ref_x, ref_y = coords from img to align to, tgt_x, tgt_y = current coords

coords_dir <- "aligned_orthos"
coords_list <- list.files(coords_dir, full.names = F, pattern = ".csv")
coords_list_full <- list.files(coords_dir, full.names = T, pattern = ".csv")

#generate list of site names to pick the right csv to do canopy correction
#canopy files were generated using 2025 imagery, so either of those should work

#build a list of info for each file
coord_file_info <- list()

for (i in seq_along(coords_list)) {
  file_name <- coords_list[i]
  
  site <- str_split_i(file_name, "_", 2)
  date <- as.Date(str_split_i(file_name, "_", 3), "%Y%m%d")
  
  coord_file_info[[i]] <- list(
    site     = site,
    date     = date,
    filename = file_name,
    filepath = coords_list_full[i]
  )
}

#select the earliest 2025 file per site
sites <- unique(sapply(coord_file_info, `[[`, "site"))

earliest_2025 <- list()

for (i in seq_along(sites)) {
  s <- sites[i]
  
  # subset to this site, 2025 only
  site_files <- Filter(function(x) x$site == s & format(x$date, "%Y") == "2025", coord_file_info)
  
  if (length(site_files) == 0) next  # skip if no 2025 files for this site
  
  # pick the one with the minimum date
  dates <- sapply(site_files, `[[`, "date")
  earliest_2025[[s]] <- site_files[[which.min(dates)]]
}

earliest_2025

#select shape file
# shp <- st_read("2025 juas qgis/sono_canopy_seg_fixed.shp")
# plot(shp)

#loop through each shape file to calculate offset from correct coord file 
shp_dir <- "2025 juas qgis"
shp_list <- list.files(shp_dir, full.names = F, pattern = "_fixed.shp")
shp_list_full <- list.files(shp_dir, full.names = T, pattern = "_fixed.shp")

# i = 1

for (i in seq_along(shp_list)) {
  
  #read in shp file
  shp <- st_read(shp_list_full[i])
  # plot(shp$geometry)
  
  #get site name
  site <- str_split_i(basename(shp_list_full[i]), "_", 1)
  
  #pull matching coords file
  coords_file <- earliest_2025[[site]]$filepath
  coords <- read_csv(coords_file)
  
  #reproject shp file to be in same CRS at csv files
  shp_utm <- st_transform(shp, crs = 32614)
  
  # #check shape file CRS
  # st_crs(shp)
  # 
  # #check the range of your coordinates
  # summary(coords)
  # summary(st_coordinates(shp))
  
  #compute the mean offset
  # (ref - tgt) gives the vector to shift from target toward ref
  dx <- mean(coords$ref_x - coords$tgt_x)
  dy <- mean(coords$ref_y - coords$tgt_y)
  
  #shift the shape file based on mean dx and dy
  shp_shifted <- shp_utm %>%
    mutate(geometry = st_geometry(.) + c(dx, dy))
  
  # plot(shp_utm$geometry)
  # plot(shp_shifted$geometry, add = T, col = "red")
  
  #define CRS for shifted shapefile
  st_crs(shp_shifted) <- st_crs(shp_utm)
  
  #transform CRS back to original 
  shp_shifted_wgs <- st_transform(shp_shifted, crs = st_crs(shp))
  
  # plot(shp$geometry)
  # plot(shp_shifted_wgs$geometry, add = T, col = "red")
  
  #save the result
  st_write(shp_shifted_wgs, paste0(site,"_shifted.shp"))
    
  print(i)
}
