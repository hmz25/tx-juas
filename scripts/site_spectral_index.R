#code for applying spectral index at the site level


# load libraries ----------------------------------------------------------
library(terra)
library(tidyverse)
library(lidR)
library(sf)
library(raster)
library(stars)
library(randomForest)
library(ranger)#install.packages("ranger")


# load orthomosaic images -------------------------------------------------
wade_20240118 <- rast("C:/Users/hmz25/Documents/pix4d/20240118_Wade/3_dsm_ortho/2_mosaic/20240118_Wade_transparent_mosaic_group1.tif")
#image(wade_20240118)
#plotRGB(wade_20240118)
# print(wade_20240118)
# wade_20240118_df <- as.data.frame(wade_20240118)
# 
# wade_20240118_df_filtered <- wade_20240118_df %>% 
#   filter(if_any(c(1:4), ~ . != 0))


##come up with a directory and for loop to load in drone orthos


# segment tree tops using lidar -------------------------------------------

#load in ttop files

#this is using las files, cannot seem to get crs files to align, so trying with the shapefiles from QGIS
#wade_ttops_las <- readLAS("C:/Users/hmz25/Desktop/TX 2024 analysis/TX_final_250624/rs_data/north_centralWade.las")
#plot(wade_ttops_las, map = TRUE)
#print(wade_ttops_las)
#str(wade_ttops_las)

wade_ttops_shp <- st_read("C:/Users/hmz25/Desktop/TX 2024 analysis/qgis/wade_ttops_shp.shp")
#plot(wade_ttops_shp[1])


#clip ttop file to extent of ortho
###don't know if I need to do this when using shp files?

# wade_20240118_extent <- ext(wade_20240118) %>%
#   as.polygons() #convert spatextent into spatvector 
# 
# #convert the SpatVector polygon to an sf object
# wade_20240118_extent_sf <- st_as_sf(wade_20240118_extent) #make into spatial feature to use to crop las file


#align crs of ortho and ttops 
# extent_reproj <- st_transform(wade_20240118_extent_sf, lidR::crs(wade_ttops_las)) #wade extent in projection of las 
# 
# st_crs(wade_20240118_extent_sf)
# lidR::crs(wade_ttops_las)
# # st_crs(wade_20240118_extent_sf)
# # wade_t3_20240118_extent_sf_reproj <- st_transform(wade_20240118_extent_sf, st_crs(wade_ttops_las))
# 
# #clip the las to the extent of the raster
# wade_ttops_las_clipped <- clip_roi(wade_ttops_las, extent_reproj)
# 
# #reproject ortho to be in same crs as las
# wade_20240118_reproj <- project(wade_20240118, as.character(lidR::crs(wade_ttops_las_clipped)))
# # terra::crs(wade_20240118) <- "epsg:6343"
# # #terra::crs(wade_20240118)
# # lidR::crs(wade_ttops_las_clipped) <- "epsg:6343"
# # lidR::crs(wade_ttops_las_clipped)
# # same.crs(wade_ttops_las_clipped, wade_20240118)
# 
# #not working :(
# # wade_20240118_reproj <- project(wade_20240118, lidR::crs(wade_ttops_las_clipped))
# # terra::crs(wade_20240118_reproj)
# # same.crs(wade_20240118_reproj, wade_ttops_las)

st_crs(wade_ttops_shp)
terra::crs(wade_20240118)

wade_ttops_reproj <- st_transform(wade_ttops_shp, terra::crs(wade_20240118))

compareCRS(st_crs(wade_ttops_reproj), terra::crs(wade_20240118))

plot(wade_ttops_reproj[1])

#mask out everything in ortho that isn't a tree

#turn geometries column in shp file into spatraster 
wade_ttop_geom <- as_Spatial(wade_ttops_reproj$geometry)
plot(wade_ttop_geom)

# ext_wade <- ext(wade_20240118)
# 
# ext(wade_20240118) <- ext_wade

wade_ttop_vect <- vect(wade_ttop_geom)
#plot(wade_ttop_vect)
plotRGB(wade_20240118)
plot(wade_ttop_vect, add = TRUE)

wade_mask <- terra::mask(wade_20240118, wade_ttop_vect)
plotRGB(wade_mask)

# filter out non cone and foliage from pictures ---------------------------

##need to check if this is the correct pixel-based classifier... want to do one from ortho images instead?

##run pixel-based classifier 

#load pictures to create training dataset for non-foliage and non-cones
sup_not_twig <- stack("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/not_2.tiff")
#plotRGB(sup_not_twig) 
#sup_not_twig$not_2_1 
#sup_not_twig$not_2_1[1:100]

#dataframe for non-foliage and non-cone pixels
sup_not_twig_df <- as.data.frame(sup_not_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  filter(r != 255) %>% 
  mutate(class = "not") 
#head(sup_not_twig_df)

#load pictures to create training dataset for foliage and cones
sup_yes_twig <- stack("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/yes_1.tif")
#plotRGB(sup_yes_twig) 
#sup_yes_twig$yes_1_1 
#sup_yes_twig$yes_1_1[1:100]

#dataframe for foliage and cone pixels
sup_yes_twig_df <- as.data.frame(sup_yes_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  filter(r != 255) %>% 
  mutate(class = "yes")  
#head(sup_yes_twig_df)

#combine training datasets and randomly select pixels from theme
training_df <- bind_rows(sup_not_twig_df, sup_yes_twig_df) %>%
  dplyr::select(-max) %>%
  sample_n(100000) %>%
  mutate(class = as.factor(class))
#head(training_df)
#str(training_df)

#run a pixel-based classifier
###TRY RANGER INSTEAD OF RF 
set.seed(100)
rf_mask <- randomForest(class ~ ., data = training_df, na.action=na.omit)
#rf_mask

#only retain r, g, b layers from ortho
wade_mask_rgb <- subset(wade_mask, 1:3)

names(wade_mask_rgb) <- colnames(training_df)[1:3]

# #testing on small subset of ortho, it works!
# wade_mask_rgb_ext <- ext(589800, 589850, 3410600, 3410650)
# 
# sub_vect <- as.polygons(wade_mask_rgb_ext)
# 
# # plot(wade_mask_rgb$r)
# # plot(sub_vect, col = 'red', add = TRUE)
# 
# wade_mask_rgb_sub <- crop(wade_mask_rgb, sub_vect)
# 
# wade_rf_mask_subset <- predict(wade_mask_rgb_sub, rf_mask)
# #plot(wade_rf_mask_subset)
# 
# wade_rf_mask_subset <- c(wade_mask_rgb_sub, wade_rf_mask_subset)
# plot(wade_rf_mask_subset)

#apply rf pixel classifier to ortho
###slow af, switch to using ranger?
wade_rf_mask <- predict(wade_mask_rgb, rf_mask)

#combine rgb ortho RasterStack with rf mask RasterStack
wade_rf_mask_rgb <- c(wade_mask_rgb, wade_rf_mask)
# print(wade_rf_mask_rgb)
# plot(wade_rf_mask_rgb)

#create mask layer where only pixels that fall into "yes" class are retained

#testing on a subset first to see if the workflow is correct
mask_test <- ifel(wade_rf_mask_subset$class == "yes", 1, NA)
#plot(mask_test)

mask_filter_test <- mask(wade_rf_mask_subset, mask_test)
#plot(mask_filter_test)

# apply spectral index to filtered tree tops ---------------------------------------

#define spectral index
orange_index <- function(r, g) {
  index <- ((r-g)/(r+g))
  return(index)
}

#create index map and apply it to the filtered ortho 

rg <- mask_filter_test[[1:2]] #new spat raster with only red and green band values
index_map <- lapp(rg, fun = orange_index) #applying index to ortho 

plot(index_map)

# use spectral index to predict PCD ---------------------------------------

#create dataframe for masked ortho 
wade_rf_mask_rgb_df_sub <- as.data.frame(mask_filter_test) %>% 
  na.omit() %>% 
  mutate(orange_index = ((r-g)/(r+g))) %>% 
  dplyr::select(r, g, orange_index) 
#this workflow is successful! now need to apply it to the entire landscape 

