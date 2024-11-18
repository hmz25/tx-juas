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


##come up with a directory and forloop to load in drone orthos


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
#sup_not_twig$not_1_1 
#sup_not_twig$not_1_1[1:100]

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

#testing on small subset of ortho, it works!
wade_mask_rgb_ext <- ext(589800, 589850, 3410600, 3410650)

sub_vect <- as.polygons(wade_mask_rgb_ext)

# plot(wade_mask_rgb$r)
# plot(sub_vect, col = 'red', add = TRUE)

wade_mask_rgb_sub <- crop(wade_mask_rgb, sub_vect)

wade_rf_mask_subset <- predict(wade_mask_rgb_sub, rf_mask)
#plot(wade_rf_mask_subset)

wade_rf_mask_subset <- c(wade_mask_rgb_sub, wade_rf_mask_subset)
plot(wade_rf_mask_subset)

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

##okay now I think I need to apply the index only to the trees that we have cone counts for!!!

#load in orthos

setwd("C:/Users/hmz25/Desktop/")
ortho_dir <- "TX 2024 analysis/site_orthos"
#list.files(ortho_dir)

ortho_files <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)

for (i in seq_along(ortho_files)) {
  
  file_name <- file_path_sans_ext(basename(ortho_files[i]))
  
  assign(file_name, rast(ortho_files[i]))
  
  print(i)
}

#plotRGB(Williamson_20240104)

#load in shapefiles   

shp_dir <- "TX 2024 analysis/qgis/focal_trees_shp"
list.files(shp_dir)

shp_files <- list.files(shp_dir, pattern = ".shp", full.names = TRUE)

for (i in seq_along(shp_files)) {
  
  file_name <- file_path_sans_ext(basename(shp_files[i]))
  
  assign(file_name, st_read(shp_files[i]))
  
  print(i)
}

#in for loop, load in ortho and shp files, align crs, crop ortho so only include trees in shp file???

#align crs of orthos and shp files 

wade_shp_reproj <- st_transform(wade_focal_trees, terra::crs(Wade_20240103))
#compareCRS(st_crs(wade_shp_reproj), terra::crs(Wade_20240103))

# plotRGB(Wade_20240103)
# plot(wade_shp_reproj, add = TRUE, col = "red")

wade_ortho_ttops <- mask(Wade_20240103, wade_shp_reproj)
plotRGB(wade_ortho_ttops)

##RUN PIXEL CLASSIFIER ON MASKED ORTHO

#extract RGB values and corresponding geometry IDs
wade_ortho_ttops_df <- terra::extract(wade_ortho_ttops, wade_shp_reproj, df = TRUE)
#tail(wade_ortho_ttops_df)

#rename columns with red, green, blue bands and so that ID = tree #. Also add index column 
wade_ortho_ttops_df <- wade_ortho_ttops_df %>% 
  dplyr::select(1:4) %>% 
  rename(tree = ID, r = 2, g = 3, b = 4) %>% 
  mutate(orange_index = ((r-g)/(r+g)))
  
#tail(wade_ortho_ttops_df)

#trying to visualize data, come back to this it's not working 
# library(ggplot2)
# ?geom_map
# 
# wade_shp_reproj_plot <- st_coordinates(wade_shp_reproj) %>% 
#   as.data.frame() %>% 
#   dplyr::select(c("X", "Y", "L2")) %>% 
#   rename(x = X,
#          y = Y,
#          id = L2)
# 
# ggplot(wade_ortho_ttops_df) + 
#   geom_map(aes(map_id = ID), map = wade_shp_reproj_plot)

#connect w cone density data

quadrat_cones <- read_csv("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/2024_quadratcones.csv")

#add cones per weight columns
quadrat_cones <- quadrat_cones %>%
  dplyr::select(date_collected,
         site,
         tree,
         quadrat,
         total_mass,
         s1_count, s1_weight,
         s2_count, s2_weight,
         s3_count, s3_weight,
         s4_count, s4_weight,
         s5_count, s5_weight) %>%
  mutate(s1 = (quadrat_cones$s1_count/quadrat_cones$s1_weight),
         s2 = (quadrat_cones$s2_count/quadrat_cones$s2_weight),
         s3 = (quadrat_cones$s3_count/quadrat_cones$s3_weight),
         s4 = (quadrat_cones$s4_count/quadrat_cones$s4_weight),
         s5 = (quadrat_cones$s5_count/quadrat_cones$s5_weight))  

#take mean of cone per weight for each quadrat
quadrat_cones <- quadrat_cones %>%
  dplyr::select(date_collected,
         site,
         tree,
         quadrat,
         total_mass,
         s1,
         s2,
         s3,
         s4,
         s5)

quadrat_cones$cone_mean <- rowMeans(subset(quadrat_cones, select = c("s1", "s2", "s3", "s4", "s5")),
                                    na.rm = TRUE)

#take standard deviation of quadrat cones
quadrat_cones <- quadrat_cones %>%
  rowwise %>%
  mutate(cone_sd = sd(c_across(s1:s5))) %>%
  ungroup()

#calculate total number of cones in each quadrat
quadrat_cones <- quadrat_cones %>%
  mutate(total_cones = cone_mean*total_mass) #total cones = cones per 25cmx25cm (625cm2)

#rename date column and ensure dates are in the right format
quadrat_cones <- rename(quadrat_cones, date = date_collected)
quadrat_cones$date <- mdy(quadrat_cones$date)

# quadrat_cones %>% 
#   group_by(date, site, tree) %>% 
#   summarize(total_cones_tree = sum(total_cones))

#account for pixel size 

cones_per_px_df <- quadrat_cones %>% 
  dplyr::select(date, site, tree, quadrat, total_cones) %>% #gives total cones per 625 cm2 (25cm x 25cm)
  mutate(cones_per_cm = total_cones/625) %>% #cones per cm seems low?
  mutate(cones_per_pix = cones_per_cm*0.982) %>% 
  group_by(date, site, tree) %>% 
  summarise(avg_cones_per_pix = mean(cones_per_pix, na.rm=T))

#create pixel size df for orthos 
res_list <- vector("list", length(ortho_files))

for (i in seq_along(ortho_files)) {
  
  file_name <- file_path_sans_ext(basename(ortho_files[i]))
  
  #load the orthomosaic as a SpatRaster
  ortho <- rast(ortho_files[i])
  
  #resolution in meters per pixel
  resolution <- res(ortho)
  
  #total number of pixels in the ortho
  total_pixels <- ncell(ortho)
  
  #store file name, resolution, and total pixels in a data frame row
  res_list[[i]] <- data.frame(
    file_name = file_name,
    res_x = resolution[1],      # resolution in x-direction
    res_y = resolution[2],      # resolution in y-direction
    total_pixels = total_pixels # total number of pixels
  )
}

#combine rows into a single data frame
site_px_df <- bind_rows(res_list)

#reformat df so it can be joined w the cones df
site_px_df <- site_px_df %>% 
  separate(file_name, into = c("site", "date"), sep = "_") %>%
  mutate(site = tolower(site)) %>%
  mutate(date = ymd(date)) %>% 
  mutate(res_x_cm = res_x * 100,
         res_y_cm = res_y * 100) %>%  #convert m to cm 
  mutate(cm_per_px = res_x_cm * res_y_cm) %>% 
  dplyr::select(site, date, cm_per_px, total_pixels)

#join site_px df with site_cones df 
cones_per_cm2_df <- quadrat_cones %>% 
  dplyr::select(date, site, tree, quadrat, total_cones) %>% #gives total cones per 625 cm2 (25cm x 25cm)
  group_by(date, site, tree) %>% 
  summarize(avg_cone_per_tree = mean(total_cones, na.rm = TRUE)) %>% #gives you avg cones per 625 from all quadrats
  mutate(cones_per_cm = avg_cone_per_tree/625, na.rm = TRUE) #FIX NA RM

total_cones_df <- cones_per_cm2_df %>% 
  left_join(site_px_df, by = c("date", "site")) %>% 
  mutate(cones_per_px = cones_per_cm * cm_per_px) %>% 
  mutate(total_cones = total_pixels * cones_per_px)

# wade_cone_df <- cones_per_px_df %>% 
#   filter(date == "2024-01-03")
# 
# wade_cone_rgb_df <- wade_cone_df %>% 
#   left_join(wade_ortho_ttops_df, by = "tree")

#multiply cones per cm by number or pixels in each tree 
#sum the index
#plot cone density vs index for each tree 

t1_px <- wade_ortho_ttops_df %>% 
  filter(tree == "1")
#tail(t1_px)
nrow(t1_px)

wade_ortho_ttops_df %>% 
  group_by(tree) %>% 
  summarize(sum_index = abs(sum(orange_index)))

quadrat_cones %>% 
  dplyr::select(date, site, tree, quadrat, total_cones) %>% #gives total cones per 625 cm2 (25cm x 25cm)
  mutate(cones_per_cm = total_cones/625) %>% #cones per cm seems low?
  mutate(cones_per_pix = cones_per_cm*0.982) %>% 
  group_by(date, site, tree) %>% 
  summarise(avg_cones_per_pix = mean(cones_per_pix, na.rm=T)) 

#need to figure out what datapoints i actually want to fall on the graphs and do the data manipulation that way
## sum of spectral reading vs sum cones based on site and on tree? I think that's what I want 

res()
  
