library(sf)
library(raster)
library(dplyr)
library(terra)
library(sp)

#creating a workflow for overlaying drone ortho with spectral index using lidar data tree polygons

#load segmented juas file
wade_juas_lidar <- st_read("C:/Users/hmz25/Desktop/TX 2024 analysis/TX_final_250624/qgis/juas_lidar_wade.gpkg")

#load orthomosaic image
wade_juas_ortho <- brick("C:/Users/hmz25/Documents/pix4d/Wade_20240103/3_dsm_ortho/2_mosaic/Wade_20240103_transparent_mosaic_group1.tif")
#plotRGB(wade_juas_ortho) #checking to see if it loaded correctly

#reproject lidar data to be in same CRS as ortho
wade_juas_lidar <- st_transform(wade_juas_lidar, crs(wade_juas_ortho))
#compareCRS(wade_juas_lidar, wade_juas_ortho) #TRUE

#create subset of ortho to test workflow
#ortho_extent <- st_read("C:/Users/hmz25/Desktop/TX_final_250624/sample.shp")

#extent(juas_ortho)
#class      : Extent 
# xmin       : 589653.1 
# xmax       : 589970.7 
# ymin       : 3410518 
# ymax       : 3410904

#extent_test <- as(extent(589654, 589659, 3410520, 3410525), 'SpatialPolygons')
#extent_sub <- as(extent(589800, 589850, 3410650, 3410700), 'SpatialPolygons')

#plot(juas_ortho, 1, ext = extent_test) # cropping within raster::plot and only using band 1 (red)

#crs(extent_sub) <- crs(juas_ortho) # "+proj=longlat +datum=WGS84 +no_defs"
#ortho_test <- crop(juas_ortho, extent_sub)
#ortho_test <- aggregate(ortho_test, 10)
#lidar_test <- st_crop(juas_lidar_reproj, extent_sub)

#plot(ortho_test, 1)
#plot(lidar_test, add = TRUE)

#plotRGB(ortho_test, r = 1, g = 2, b = 3, stretch = "lin")
#plot(lidar_test, border = "red", col = NA, add = TRUE)

#plot(juas_ortho)

#define spectral index
orange_index <- function(r, g) {
  index <- ((r-g)/(r+g))
  return(index)
}

red_band <- wade_juas_ortho[[1]]
#plot(red_band)
green_band <- wade_juas_ortho[[2]]
#plot(green_band)

wade_index_map <- overlay(red_band, green_band, fun = orange_index)
#plot(wade_index_map)

#lidar_polygons_sub <- st_crop(juas_lidar_reproj, st_bbox(extent_test))

#create binary mask at resolution of the ortho to mask out everything from ortho that isn't a tree
#juas_lidar_raster <- rasterize(lidar_polygons, juas_ortho, field = 1)
lidar_raster <- raster::rasterize(wade_juas_lidar, wade_index_map, field = 1)
#plot(lidar_raster)

#wade_index_map used to be "spectral_index_map
#lidar_raster used to be "lidar_test_raster"

#wade_treetop_ortho <- mask(wade_juas_ortho, lidar_raster) #wade_juas_ortho used to be "ortho_test", lidar_raster used to be "lidar_test_raster"
treetop_spectral_index <- mask(wade_index_map, lidar_raster)

#wade_treetop_ortho used to be "treetop_ortho"

#define the color ramp from green to orange
coneColors_palette <- colorRampPalette(c("darkgreen", "orange"))

#plot the treetop spectral index with the custom color ramp
#plot(treetop_spectral_index, col = coneColors_palette(5), box = FALSE)

#add the lidar test layer with a border
#plot(lidar_test, border = "black", col = NA, add = TRUE, box = FALSE)

plot(treetop_spectral_index, col = coneColors_palette(5), axes = FALSE, box = FALSE)

# Add the lidar test layer with a border and no box
plot(wade_juas_lidar, border = "black", col = NA, add = TRUE, axes = FALSE, box = FALSE)

# alt option: export the subsetted spectral index and the subsetted polygons and then do it in QGIS

