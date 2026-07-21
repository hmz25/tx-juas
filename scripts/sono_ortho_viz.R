#script to apply index to ortho images for presentation viz

library(terra)
library(raster)
library(sf)

#read in orthos
ortho_24 <- brick("F:/aligned_orthos/sonora_20240109_transparent_mosaic_group1_aligned.tif")
ortho_25 <- brick("F:/aligned_orthos/sonora_20250115_transparent_mosaic_group1_aligned.tif")
ortho_26 <- brick("F:/aligned_orthos/sonora_20260112_transparent_mosaic_group1_corrected.tif")
# plotRGB(ortho_24)
# plotRGB(ortho_25)
# plotRGB(ortho_26)

#crop to smaller extent
ortho_sub <- as(extent(351690, 351732, 3352107, 3352140), 'SpatialPolygons')

crs(ortho_sub) <- crs(ortho_24)

#crop ortho to extent of subsample
ortho_sub24 <- crop(ortho_24, ortho_sub)
plotRGB(ortho_sub24)

ortho_sub25 <- crop(ortho_25, ortho_sub)
plotRGB(ortho_sub25)

ortho_sub26 <- crop(ortho_26, ortho_sub)
plotRGB(ortho_sub26)

# writeRaster(ortho_sub24, filename = "F:/sono_ortho_viz/ortho_sub24.tif", format = "GTiff")
# writeRaster(ortho_sub25, filename = "F:/sono_ortho_viz/ortho_sub25.tif", format = "GTiff")
# writeRaster(ortho_sub26, filename = "F:/sono_ortho_viz/ortho_sub26.tif", format = "GTiff")

#read in shape file of canopy segmentation to crop and apply index 
shp <- st_read("C:/Users/HMZ/Box/texas/03_output/corrected_canopy_shp/sono_shifted.shp")

#align CRS of tiff and segmented treetops
crs(shp)
crs(ortho_sub26)

shp_reproj <- st_transform(shp, crs(ortho_sub25))

compareCRS(shp_reproj, ortho_sub25)

plot(shp_reproj, add = T, col = "white") 

#plot segmented treetops over ortho
plotRGB(ortho_sub24)#, r = 1, g = 2, b = 3, stretch = "lin")
plot(shp_reproj, add = TRUE, border = "white")

# names(ortho_sub24) <- c("r", "g", "b", "transparant")  # rename to match what rf_model expects

ortho_ttops <- mask(ortho_sub24, shp_reproj)
plotRGB(ortho_ttops)

pix_df <- as.data.frame(ortho_ttops, xy = TRUE) %>%
  rename(r = sonora_20240109_transparent_mosaic_group1_1,
         g = sonora_20240109_transparent_mosaic_group1_2,
         b = sonora_20240109_transparent_mosaic_group1_3,
         trans = sonora_20240109_transparent_mosaic_group1_4) %>% 
  dplyr::select(-trans)

pix_df$class <- predict(rf_mask, newdata = pix_df)

gam = 2.2

fol_pixels <- pix_df %>%
  filter(class == "yes") %>%
  mutate(
    r_lin = (r / 255)^(1/gam),
    g_lin = (g / 255)^(1/gam),
    b_lin = (b / 255)^(1/gam)
  ) %>%
  mutate(
    sum_lin = r_lin + g_lin + b_lin,
    r_norm_gam = r_lin / sum_lin,
    g_norm_gam = g_lin / sum_lin,
    b_norm_gam = b_lin / sum_lin,
    rg_index = (r_norm_gam - g_norm_gam) / (r_norm_gam + g_norm_gam)
  )

index_r <- rasterFromXYZ(
  fol_pixels[, c("x", "y", "rg_index")],
  crs = crs(ortho_24)
)

# create an empty single-layer raster with the same grid as your masked image
index_r <- raster(ortho_ttops[[1]])

# find which cells your fol_pixels x,y coordinates correspond to
cells <- cellFromXY(index_r, fol_pixels[, c("x", "y")])

# assign the index values to those cells; everything else stays NA
index_r[cells] <- fol_pixels$rg_index

plotRGB(ortho_sub24, r = 1, g = 2, b = 3, stretch = "lin")
plot(st_geometry(shp_reproj), border = "white", lwd = 2, add = TRUE)
plot(index_r, add = TRUE, col = colorRampPalette(brewer.pal(11, "RdYlBu"))(100),
     alpha = 0.8, legend = TRUE)

my_palette <- rev(colorRampPalette(brewer.pal(11, "RdYlBu"))(100))

plotRGB(ortho_sub24, r = 1, g = 2, b = 3, stretch = "lin")
plot(st_geometry(shp_reproj), border = "white", lwd = 2, add = TRUE)
plot(index_r, add = TRUE, col = my_palette, alpha = 0.8, legend = TRUE)


# bump up the right margin so there's room outside the image
par(mar = c(5, 4, 4, 6), xpd = NA)

plotRGB(ortho_sub24, r = 1, g = 2, b = 3, stretch = "lin")
plot(st_geometry(shp_reproj), border = "white", lwd = 2, add = TRUE)

# draw the raster colors but suppress its built-in legend
plot(index_r, add = TRUE, col = my_palette, alpha = 0.8, legend = FALSE)

# draw the legend separately, positioned outside the plot panel
sono_plot <- plot(index_r, legend.only = TRUE, col = my_palette,
     smallplot = c(0.87, 0.90, 0.25, 0.75))  # c(x1, x2, y1, y2) as fraction of device width/height

#try viz with ggplot
library(ggplot2)
library(RStoolbox)   # for ggRGB / fortify-ing rasters
library(sf)
library(dplyr)

# 1. Convert the index raster to a data frame for ggplot
index_df <- as.data.frame(index_r, xy = TRUE) %>%
  rename(rg_index = 3) %>%
  filter(!is.na(rg_index))

# 2. Build the plot: RGB background, index overlay, shapefile outline
ggRGB(ortho_sub24, r = 1, g = 2, b = 3, stretch = "lin", ggLayer = TRUE) %>%
  ggplot() +
  ggRGB(ortho_sub24, r = 1, g = 2, b = 3, stretch = "lin", ggLayer = TRUE) +
  geom_raster(data = index_df, aes(x = x, y = y, fill = rg_index), alpha = 0.8) +
  geom_sf(data = shp_reproj, fill = NA, color = "white", linewidth = 0.8, inherit.aes = FALSE) +
  scale_fill_viridis_c(name = "RG Index") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.margin = margin(5, 20, 5, 5)
  )

## 
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

