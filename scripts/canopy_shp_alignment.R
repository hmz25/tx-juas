#script to shift tree canopy shape files to align with corrected orthos

library(sf)
library(dplyr)

setwd("C:/Users/hmz25/Box/Katz lab/texas/")

#read in data
coords <- read.csv("aligned_orthos/gcps_sonora_20250107_transparent_mosaic_group1.csv")  #ref_x, ref_y = coords from img to align to, tgt_x, tgt_y = current coords
shp <- st_read("your_shapefile.shp")

#compute the mean offset
# (ref - tgt) gives the vector to shift from target toward reference)
dx <- mean(coords$ref_x - coords$tgt_x)
dy <- mean(coords$ref_y - coords$tgt_y)

# Build a translation matrix and apply it
shift_shp <- function(geom, dx, dy) {
  geom + c(dx, dy)
}

shp_shifted <- shp %>%
  mutate(geometry = st_geometry(.) + c(dx, dy))

# Set the CRS (carried over from original)
st_crs(shp_shifted) <- st_crs(shp)

# Save the result
st_write(shp_shifted, "shifted_shapefile.shp")