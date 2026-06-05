#script to extract pixel values + calculate index values for all trees at each site across the years

#set up work environment

library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(randomForest)
library(exactextractr)

setwd("C:/Users/hmz25/Box/Katz lab/texas/")

# load in rf model to filter foliage vs non foliage pixels --------------------
rf_mask <- get(load("03_output/rf_mask_2026.RData"))


# set dir for ortho images ----------------------------------------------------
ortho_dir <- "03_output/aligned_orthos"
ortho_list <- list.files(ortho_dir, pattern = ".tif$", full.names = FALSE)
ortho_list <- ortho_list[15:19] #subset to sonora for testing
ortho_list_full_dir <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)
ortho_list_full_dir <- ortho_list_full_dir[15:19]

# ortho <- rast("2025 orthos/sonora_20250115_transparent_mosaic_group1.tif")
# plotRGB(ortho)

# set dir for segmented canopy shape files --------------------------------------------

shp_dir <- "03_output/corrected_canopy_shp"
shp_list <- list.files(shp_dir, pattern = ".shp$", full.names = FALSE)
shp_list_full_dir <- list.files(shp_dir, pattern = ".shp$", full.names = TRUE)

# # checking to see if the correct files
# test <- st_read(shp_list_full_dir[11])
# plot(test)

# loop through each site and shape file to calculate mean index --------

index_df <- data.frame()

i = 1

#added orthos, trying new code 
for (i in seq_along(ortho_list)) {
  
  #extract site name and date
  ortho_name <- basename(ortho_list[i])
  site_prefix <- substr(ortho_name, 1, 4)  # first 4 characters
  site_name <- str_extract(ortho_name, "^[^_]+")
  flight_date <- str_match(ortho_name, "^[^_]+_([^_]+)_")[,2]
  
  #find matching shapefile (first 4 letters match)
  match_index <- which(substr(shp_list, 1, 4) == site_prefix)
  
  if (length(match_index) == 0) {
    next
  }
  
  shp_path <- shp_list_full_dir[match_index[1]]  
  
  #load ortho and shapefile
  ortho <- rast(ortho_list_full_dir[i]) #plotRGB(ortho)
  names(ortho) <- c("r", "g", "b", "transparant")
  
  shp <- st_read(shp_path, quiet = TRUE, fid_column_name = "tree")
  shp_reproj <- st_transform(shp, crs(ortho)) #plot(shp_reproj, add = T, col = "red")
  
  #cropping the polygons so don't pick up any soil 
  shp_reproj_crop <- st_buffer(shp_reproj, dist = -0.50) #plot(shp_reproj_crop, add = T, col = "white")
  
  #extract pixel values from canopy shapefiles 
  extracted_values <- exact_extract(ortho, shp_reproj_crop,
                                    coverage_area = TRUE, 
                                    include_cols = "tree", 
                                    include_xy = TRUE)
  
  # #testing visually to see if it's doing the right thing
  # 
  # str(extracted_values[[1]])
  # test_df <- extracted_values[[1]] %>%
  #   dplyr::select(x, y, everything())
  # 
  # raster_template <- rasterFromXYZ(test_df[, c("x", "y", "r")])
  # r_layer <- rasterFromXYZ(test_df[, c("x", "y", "r")])
  # g_layer <- rasterFromXYZ(test_df[, c("x", "y", "g")])
  # b_layer <- rasterFromXYZ(test_df[, c("x", "y", "b")])
  # 
  # rgb_stack <- stack(r_layer, g_layer, b_layer)
  # names(rgb_stack) <- c("r", "g", "b")
  # 
  # plotRGB(rgb_stack)
  
  #combine all tree pixel values into df 
  extracted_df <- bind_rows(extracted_values) %>% 
    dplyr::select(-transparant) 
  
  #predict if pixel is cone or foliage
  extracted_df$class <- predict(rf_mask_ortho, extracted_df)
  
  #filter for cone/foliage pixels
  fol_pixels <- extracted_df %>%
    filter(class == "yes")
  
  # #visual check
  # 
  # fol_pixels_test_df <- fol_pixels %>%
  #   filter(tree %in% 0)
  # 
  # fol_pixels_template <- rasterFromXYZ(fol_pixels_test_df[, c("x", "y", "r")])
  # r_layer <- rasterFromXYZ(fol_pixels_test_df[, c("x", "y", "r")])
  # g_layer <- rasterFromXYZ(fol_pixels_test_df[, c("x", "y", "g")])
  # b_layer <- rasterFromXYZ(fol_pixels_test_df[, c("x", "y", "b")])
  # 
  # fol_pixels_stack <- stack(r_layer, g_layer, b_layer)
  # names(fol_pixels_stack) <- c("r", "g", "b")
  # 
  # plotRGB(fol_pixels_stack)
  
  #compute index
  index_df <- fol_pixels %>%
    mutate(rg_index = (r - g) / (r + g),
           site = site_name,
           date = flight_date) %>% 
    dplyr::select(-class)
  
  site_index_df <- rbind(site_index_df, index_df)
  
  # site_index_df <- rbind(site_index_df,
  #                             data.frame(site = site_name,
  #                                        date = flight_date,
  #                                        rg_index = index_df$rg_index))
  
  print(i)
}

#print df 
site_index_df

# #test to see if right number of tree canopies
# wind_site_index <- site_index_df %>% 
#   filter(site == "windmill")
# 
# unique(wind_site_index$tree)
# #yes!!!!

#check code to see if it's producing right values
#select 1 tree, visualize it, calculate mean index of canopy 
#choosing tree 1 from cath (which is 1 in qgis but 0 in R based on exactextractr naming)

# #load in raster of cath (make sure to specify date) and shapefile for only the test tree
# rast_test <- rast("C:/Users/hmz25/Box/Katz lab/texas/orthos/cathedral_20240105_transparent_mosaic_group1.tif")
# plotRGB(rast_test)
# 
# shp_test <- st_read("C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis/cath_canopy_test.shp")
# shp_test_reproj <- st_transform(shp_test, crs(rast_test))
# 
# plot(shp_test_reproj, add=TRUE, col="blue")
# 
# #make sure it matches df
# 
# # #build df of tree 0 to confirm it matches 
# # test_df <- site_index_df %>%
# #   filter(site %in% "cathedral",
# #          tree %in% 0)
# # 
# # coord_x <- mean(test_df$x)
# # coord_y <- mean(test_df$y)
# # 
# # points(coord_x, coord_y, add = TRUE, col="red")
# # #matches!
# 
# #crop raster to shapefile
# rast_test_sub <- crop(rast_test, shp_test_reproj)
# plotRGB(rast_test_sub)
# 
# #mask raster by shapefile
# rast_test_mask <- mask(rast_test_sub, shp_test_reproj)
# plotRGB(rast_test_mask)
# 
# #run pixel classifier
# names(rast_test_mask) <- c("r", "g", "b", "transp")
# 
# rast_test_mask <- subset(rast_test_mask, "transp", negate = T) #remove transparant layer
# 
# rast_test_filt <- predict(rast_test_mask, rf_mask_ortho)
# plot(rast_test_filt)
# 
# filt <- rast_test_filt == 1
# rast_test_filt <- mask(rast_test_mask, filt, maskvalue=1)
# plotRGB(rast_test_filt)
# 
# #calculate index and compare 
# test_index_df <- as.data.frame(rast_test_filt) %>% 
#   mutate(rg_index = (r-g)/(r+g)) %>% 
#   summarize(mean_index = mean(rg_index)) #-0.02158547
# 
# site_index_df %>% 
#   filter(site %in% "cathedral",
#          tree %in% 0,
#          date %in% "20240105") %>% 
#   summarize(mean_index = mean(rg_index)) #-0.02158548 
# 
# #YAY!


# site_index_df %>%
#   filter(site %in% "cathedral",
#          tree %in% 0,
#          date %in% "20240105") %>%
#   summarize(mean_r = mean(r),
#             mean_g = mean(g),
#             mean_b = mean(b))
# 
# (113.0567-116.4378)/(113.0567+116.4378)

#save output in Box folder
write_csv(site_index_df, file = "C:/Users/hmz25/Box/Katz lab/texas/rg_index_df.csv", append = F)