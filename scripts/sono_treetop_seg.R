library(terra)
library(raster)
library(sf)

#read in sonora ortho
# sono_ortho_2024 <- rast("C:/Users/hmz25/Documents/pix4d/sonora_20240109/3_dsm_ortho/2_mosaic/sonora_20240109_transparent_mosaic_group1.tif")
sono_ortho_2024 <- brick("C:/Users/hmz25/Documents/pix4d/sonora_20240116/3_dsm_ortho/2_mosaic/sonora_20240116_transparent_mosaic_group1.tif")
sono_ortho_2025 <- brick("C:/Users/hmz25/Documents/pix4d/sonora_20250115/3_dsm_ortho/2_mosaic/sonora_20250115_transparent_mosaic_group1.tif")
plotRGB(sono_ortho_2024)
plotRGB(sono_ortho_2025)

# kimb_ortho_24 <- rast("~/pix4d/kimble_20240117/3_dsm_ortho/2_mosaic/kimble_20240117_transparent_mosaic_group1.tif")
# plotRGB(kimb_ortho_24)
# 
# kimb_ortho_25 <- rast("~/pix4d/kimble_20250116/3_dsm_ortho/2_mosaic/kimble_20250116_transparent_mosaic_group1.tif")
# plotRGB(kimb_ortho_25)

#create subsample of tiff to showcase high and low pcd trees 
# extent(sono_ortho)
# sono_sub <- as(extent(351720, 351820, 3352150, 3352250), 'SpatialPolygons') #zoomed out  old code

# sono_sub <- as(extent(351750, 351790, 3352155, 3352180), 'SpatialPolygons') #zoomed in old code

sono_sub <- as(extent(351690, 351732, 3352107, 3352140), 'SpatialPolygons')


# kimb_sub <- (as(extent(397855, 397909, 3397414, 3397476), 'SpatialPolygons'))

crs(sono_sub) <- crs(sono_ortho_2024)

# crs(kimb_sub) <- crs(kimb_ortho_24)

#crop sono ortho to extent of subsample
sono_ortho_sub24 <- crop(sono_ortho_2024, sono_sub)
plotRGB(sono_ortho_sub24)

sono_ortho_sub25 <- crop(sono_ortho_2025, sono_sub)
plotRGB(sono_ortho_sub24)
dim(sono_ortho_sub25)

sono_ortho_sub24_terra <- rast(sono_ortho_sub24)

sono_ortho_sub24_coarser <- terra::aggregate(sono_ortho_sub24_terra, fact=144)
dim(sono_ortho_sub24_coarser)
plotRGB(sono_ortho_sub24_coarser)

# writeRaster(sono_ortho_sub24, filename = "sono_ortho_sub24_2.tif", format = "GTiff")
# writeRaster(sono_ortho_sub25, filename = "sono_ortho_sub25_2.tif", format = "GTiff")

# kimb_24_crop <- crop(kimb_ortho_24, kimb_sub)
# plotRGB(kimb_24_crop)
# 
# kimb_25_crop <- crop(kimb_ortho_25, kimb_sub)
# plotRGB(kimb_25_crop)

#zoom in even more on ortho to segment a few treetops
# sono_sub_zoom <- as(extent(351750, 351790, 3352155, 3352180), 'SpatialPolygons')
# sono_ortho_zoom <- crop(sono_ortho_sub, sono_sub_zoom)
# plotRGB(sono_ortho_zoom)
# sono_ortho_zoom <- aggregate(sono_ortho_zoom, 10)

# #export cropped tiff to manually delineate treetops
# writeRaster(sono_ortho_zoom, filename = "sono_zoom.tif", format = "GTiff")

#load in segmented tree tops
# sono_trees <- st_read("C:/Users/hmz25/Documents/sonora_trees.shp") #from old code

# sono_trees <- st_read("C:/Users/hmz25/Documents/sono_sub_seg.shp") #re-done to align with 24 orthos

sono_trees <- st_read("C:/Users/hmz25/Documents/sono_sub_seg2.shp")

# sono_trees <- st_read("C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis/sono_canopy_seg.shp") #for 2025 delineation

# kimb_trees <- st_read("C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis/kimb_canopy_seg.shp")

#align CRS of tiff and segmented treetops
crs(sono_trees)
crs(sono_ortho_sub25)

sono_trees_reproj <- st_transform(sono_trees, crs(sono_ortho_sub25))

# kimb_trees_reproj <- st_transform(kimb_trees, crs(kimb_24_crop))

compareCRS(sono_trees_reproj, sono_ortho_sub25)

plot(sono_trees_reproj) 

#want polygon 4 and 9

#plot segmented treetops over ortho
plotRGB(sono_ortho_sub24)#, r = 1, g = 2, b = 3, stretch = "lin")
plot(sono_trees_reproj, add = TRUE, border = "white")

#making histograms for focal canopies 

t <- crop(sono_ortho_sub24, sono_trees_reproj[4,])
plotRGB(t)

# plotRGB(kimb_25_crop)
# plot(kimb_trees_reproj, add = TRUE, border = "black")

# filter <- sono_ortho_zoom < 35
# filtered_ortho <- mask(sono_ortho_zoom, filter, maskvalue=1)

names(sono_ortho_sub24) = c("r", "g", "b", "trans")

names(t) <- c("r", "g", "b", "trans")

sono_ortho_crop <- mask(sono_ortho_sub24, sono_trees_reproj)
plotRGB(sono_ortho_crop)

t_crop <- mask(t, sono_trees_reproj[4,])
plotRGB(t_crop)

rf_prediction <- terra::predict(sono_ortho_crop, rf_mask_ortho)
plot(rf_prediction)

t_rf <- terra::predict(t_crop, rf_mask_ortho)


filter <- rf_prediction == 1
filtered_ortho <- mask(sono_ortho_crop, filter, maskvalue=1)

#plotRGB(filtered_ortho)

filt <- t_rf == 1
t_filt <- mask(t_crop, filt, maskvalue =1)
plotRGB(t_filt)

#define spectral index
orange_index <- function(r, g) {
  index <- ((r-g)/(r+g))
  return(index)
}

red_band <- filtered_ortho[[1]]
green_band <- filtered_ortho[[2]]

index_map <- overlay(red_band, green_band, fun = orange_index)

plot(index_map)

#create binary mask at resolution of the ortho to mask out everything from ortho that isn't a tree
treetop_raster <- raster::rasterize(sono_trees_reproj, index_map, field = 1)
plot(treetop_raster)

treetop_index <- mask(index_map, treetop_raster)

#define the color ramp from green to orange
# coneColors_palette <- colorRampPalette(c("lightblue", "orangered"))
coneColors_palette <- colorRampPalette(c("lightgreen", "orangered"))

plot(treetop_index, col = coneColors_palette(100), alpha = 1, axes = FALSE, box = FALSE)

# # Add the lidar test layer with a border and no box
# plot(sono_trees_reproj, border = "black", col = NA, add = TRUE, axes = FALSE, box = FALSE)
# 
# # Load required package
# library(scales)
# 
# # Define a custom color ramp function
# coneColors_palette <- function(values) {
#   # Create a color ramp that transitions from light green to white to dark orange
#   colorRampPalette(c("darkgreen", "white", "orangered3"))(length(values))
# }
# 
# # Generate a sequence of colors with more steps to enhance the gradient
# values <- seq(min(treetop_index[], na.rm = TRUE), max(treetop_index[], na.rm = TRUE), length.out = 100)
# colors <- coneColors_palette(values)
# 
# # Plot with enhanced color contrast, ensuring zero is white
# plot(treetop_index, col = colors, alpha = 1, axes = FALSE, box = FALSE)
# 
# # Add the lidar test layer with a border and no box
# plot(sono_trees_reproj, border = "black", col = NA, add = TRUE, axes = FALSE, box = FALSE)
