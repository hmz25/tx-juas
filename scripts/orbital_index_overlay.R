#code to apply spectral index to orbital reconstructions of trees

#load libraries
library(lidR)
library(terra)
library(sf)
library(raster)
library(dplyr)

#browseVignettes(package = "lidR")

#read in LAS data from orbital reconstruction
wade_t3_las <- readLAS("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/point_cloud/20240103_Wade_tree3_orbital_group1_densified_point_cloud.las",
                   select = "RGB")

#explore the data
print(wade_t3_las)
str(wade_t3_las)
plot(wade_t3_las)
#plot(wade_t3, color = RGB)

#clip LAS to extent of focal tree
##load in shape file
wade_t3_shp <- st_read("C:/Users/hmz25/Desktop/TX 2024 analysis/qgis/wade_t3_extent.shp")

##reproject to same crs
#projection(wade_t3) <- crs(wade_t3_ext)
#wade_t3_las_reproj <- st_transform(wade_t3_las, projection(wade_t3_ext))

wade_t3_shp_reproj <- st_transform(wade_t3_shp, st_crs(wade_t3_las))
compareCRS(wade_t3_las, wade_t3_shp_reproj) #false but it's okay

#clip las point cloud to be just focal tree
wade_t3_las_clip <- clip_roi(wade_t3_las, wade_t3_shp_reproj)
plot(wade_t3_las_clip) #yay!
##can we apply index to the las plot? 
#str(wade_t3_las_clip)

#define las bands
R <- wade_t3_las@data$R
#print(R)
G <- wade_t3_las@data$G
B <- wade_t3_las@data$B

#create orange index
orange_index <- function(R, G) {
  (R - G)/(R + G)
}

#apply index to orbital
index_overlay <- orange_index(R, G)
plot(index_overlay)
print(index_overlay)

##use equation to get number of cones per pixel 
##figure out pixel size in las file
##

#take mean index value of orbital (to compare to mean index value of same tree from ortho) 
mean_index_orbital <- mean(index_overlay)
sum_index_orbital <- sum(index_overlay)

#load in ortho from wade
wade_ortho <- brick("C:/Users/hmz25/Documents/pix4d/20240103_Wade/3_dsm_ortho/2_mosaic/20240103_Wade_transparent_mosaic_group1.tif")
#plotRGB(wade_ortho)

#crop ortho down to smaller size 
wade_ortho_clip <- crop(wade_ortho, extent(wade_t3_shp_reproj))
#plotRGB(wade_ortho_clip)
#plot(wade_t3_ext

#extract t3 from ortho
wade_t3_rgb <- mask(wade_ortho_clip, wade_t3_shp_reproj)
plotRGB(wade_t3_rgb)

#extract pixel values from ortho image
t3_ortho_rgb <- raster::extract(x = wade_ortho_clip, 
                                y = wade_t3_shp_reproj, 
                                df = TRUE)
#head(t3_ortho_rgb)

#clean data frame and add column for spectral index 
t3_rgb_df <- t3_ortho_rgb %>% 
  rename("r" = 2, "g" = 3, "b" = 4) %>% 
  select("r", "g", "b") %>% 
  mutate(index = (r-g)/(r+g))

#take mean index value for t3 from ortho image
mean_index_ortho <- mean(t3_rgb_df$index) #wow they're really close

#visualize index on wade t3

#define index for ortho 
ortho_index <- orange_index <- function(r, g) {
  index <- ((r-g)/(r+g))
  return(index)
}

red_band <- wade_ortho_clip[[1]]
green_band <- wade_ortho_clip[[2]]

index_map <- overlay(red_band, green_band, fun = ortho_index)

plot(index_map)

#mask out everything from ortho image except for tree crown polygon 
t3_raster <- raster::rasterize(wade_t3_shp_reproj, index_map, field = 1)
#plot(t3_raster)

wade_t3_ortho_index <- mask(index_map, t3_raster)
plot(wade_t3_ortho_index)

cone_palette <- colorRampPalette(c("green", "orangered"))

plot(wade_t3_ortho_index, col = cone_palette(100), alpha = 1, axes = FALSE, box = FALSE)

#NEXT STEPS
## same pixel classification for orbital as for ortho? YES
## create loops to do this for all orbital images rather than just one 

