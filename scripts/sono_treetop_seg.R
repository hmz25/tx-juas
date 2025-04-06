library(terra)
library(raster)
library(sf)

#read in sonora ortho
sono_ortho <- brick("C:/Users/hmz25/Documents/pix4d/20240109_Sonora_singlegrid/3_dsm_ortho/2_mosaic/20240109_Sonora_singlegrid_transparent_mosaic_group1.tif")
plotRGB(sono_ortho)

#create subsample of tiff to showcase high and low pcd trees 
extent(sono_ortho)
sono_sub <- as(extent(351720, 351820, 3352150, 3352250), 'SpatialPolygons')
sono_sub

crs(sono_sub) <- crs(sono_ortho)

#crop sono ortho to extent of subsample
sono_ortho_sub <- crop(sono_ortho, sono_sub)
plotRGB(sono_ortho_sub)

#zoom in even more on ortho to segment a few treetops
sono_sub_zoom <- as(extent(351750, 351790, 3352155, 3352180), 'SpatialPolygons')
sono_ortho_zoom <- crop(sono_ortho_sub, sono_sub_zoom)
plotRGB(sono_ortho_zoom)
#sono_ortho_zoom <- aggregate(sono_ortho_zoom, 10)

#export cropped tiff to manually delineate treetops
writeRaster(sono_ortho_zoom, filename = "sono_zoom.tif", format = "GTiff")

#load in segmented tree tops
sono_trees <- st_read("C:/Users/hmz25/Documents/sonora_trees.shp")

#align CRS of tiff and segmented treetops
crs(sono_trees)
crs(sono_ortho_zoom)

sono_trees_reproj <- st_transform(sono_trees, crs(sono_ortho_zoom))

compareCRS(sono_trees_reproj, sono_ortho_zoom)

plot(sono_trees_reproj) 

#plot segmented treetops over ortho
plotRGB(sono_ortho_zoom, r = 1, g = 2, b = 3, stretch = "lin")
plot(sono_trees_reproj, add = TRUE, border = "white")

# filter <- sono_ortho_zoom < 35
# filtered_ortho <- mask(sono_ortho_zoom, filter, maskvalue=1)

names(sono_ortho_zoom) = c("r", "g", "b", "trans")

# rf_prediction <- terra::predict(sono_ortho_zoom, rf_mask_ortho)
# plot(rf_prediction)


filter <- rf_prediction == 1
filtered_ortho <- mask(sono_ortho_zoom, filter, maskvalue=1)

#plotRGB(filtered_ortho)

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
coneColors_palette <- colorRampPalette(c("darkgreen", "orange"))

plot(treetop_index, col = coneColors_palette(100), alpha = 1, axes = FALSE, box = FALSE)

# Add the lidar test layer with a border and no box
plot(sono_trees_reproj, border = "black", col = NA, add = TRUE, axes = FALSE, box = FALSE)

# Load required package
library(scales)

# Define a custom color ramp function
coneColors_palette <- function(values) {
  # Create a color ramp that transitions from light green to white to dark orange
  colorRampPalette(c("darkgreen", "white", "orangered3"))(length(values))
}

# Generate a sequence of colors with more steps to enhance the gradient
values <- seq(min(treetop_index[], na.rm = TRUE), max(treetop_index[], na.rm = TRUE), length.out = 100)
colors <- coneColors_palette(values)

# Plot with enhanced color contrast, ensuring zero is white
plot(treetop_index, col = colors, alpha = 1, axes = FALSE, box = FALSE)

# Add the lidar test layer with a border and no box
plot(sono_trees_reproj, border = "black", col = NA, add = TRUE, axes = FALSE, box = FALSE)
