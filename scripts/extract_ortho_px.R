#script to extract pixel values + calculate index values for all trees at each site across the years

#set up work environment

library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(randomForest)
library(exactextractr)
library(mmstat4)

#lab desktop
setwd("C:/Users/hmz25/Box/Katz lab/texas/")

# #hz laptop
# setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")

# #mo comp
# setwd("C:/Users/HMZ/Box/texas")

# load in rf model to filter foliage vs non foliage pixels --------------------
load("03_output/rf_mask_2026.RData") #restores rf_mask_ortho into the environment
# rf_mask_ortho$err.rate[nrow(rf_mask_ortho$err.rate), "OOB"]

# set dir for ortho images ----------------------------------------------------
# ortho_dir <- "03_output/aligned_orthos"
ortho_dir <- "F:/aligned_orthos"
ortho_list <- list.files(ortho_dir, pattern = ".tif$", full.names = FALSE)
# ortho_list <- ortho_list[27:31] #subset to sonora for testing
ortho_list <- ortho_list[c(11, 18, 38)] #add in one-off sites
ortho_list_full_dir <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)
# ortho_list_full_dir <- ortho_list_full_dir[27:31]
ortho_list_full_dir <- ortho_list_full_dir[c(11, 18, 38)]

# ortho <- rast("C:/Users/hmz25/Box/Katz lab/texas/03_output/aligned_orthos/cathedral_20241230_transparent_mosaic_group1_aligned.tif")
# plotRGB(ortho)

## make sure all files are uncorrupted ----------------------------------------------

# list.raster.files(path = ortho_list_full_dir, return_rasters = FALSE)

# checkFiles(ortho_list_full_dir, open = 0)

# #create df to store results
# load_status <- data.frame(
#   file = basename(ortho_list_full_dir),
#   status = character(length(ortho_list_full_dir)),
#   error = character(length(ortho_list_full_dir)),
#   stringAsFactors = F
# )
#
# # i = 1
#
# #loop thru raster files
# for (i in seq_along(ortho_list_full_dir)) {
#   file <- ortho_list_full_dir[i]
#
#   tryCatch({
#     temp_rast <- rast(file)
#
#     load_status$status[i] <- "success"
#     load_status$error[i] <- "NA"
#
#   }, error = function(e) {
#     load_status$status[i] <- "fail"
#     load_status$error[i] <- e$message
#   })
#
#   print(i)
# }


# load in canopy segmentation file --------------------------------------------

all_canopy_seg <- st_read("01_data/all_trees_canopy_seg_shp.shp")
# str(all_canopy_seg)
# unique(length(all_canopy_seg$poly_id))

#filter out pinchotii + live oak
all_canopy_seg <- all_canopy_seg %>%
  filter(!grepl("Live oak", notes, ignore.case = T),
         !grepl("Pinchotii", notes, ignore.case = T))

#trim data frame to use in for loop
all_canopy_seg_sub <- all_canopy_seg %>%
  dplyr::select(site, poly_id, poly_st, geometry)

# load in per-site canopy shapefiles - fallback for sites whose corrected geometry
# hasn't been merged into the master file above yet -----------------------------
shp_dir <- "01_data/canopy segmentation"
shp_list <- list.files(shp_dir, pattern = "_fixed.shp$", full.names = FALSE)
shp_list_full_dir <- list.files(shp_dir, pattern = "_fixed.shp$", full.names = TRUE)

# loop through each site and shape file to extract pixel vals --------

#create output for files
output_dir <- "F:/ortho_px_output"  #adjust path if needed
# dir.create(output_dir, showWarnings = FALSE)

# i = 1

for (i in seq_along(ortho_list)) {

  #extract site name and date
  ortho_name <- basename(ortho_list[i])
  site_name <- substr(str_extract(ortho_name, "^[^_]+"),1,4)
  flight_date <- str_match(ortho_name, "^[^_]+_([^_]+)_")[,2]

  #try the master canopy segmentation file first
  shp_site <- all_canopy_seg_sub %>%
    filter(poly_st == site_name)

  if (nrow(shp_site) > 0) {
    shp_source <- "master"
  } else {
    #fall back to this site's per-site fixed shapefile
    match_index <- which(substr(shp_list, 1, 4) == site_name)

    if (length(match_index) == 0) {
      next
    }

    shp_site <- st_read(shp_list_full_dir[match_index[1]],
                         quiet = TRUE, fid_column_name = "tree")
    shp_site$polygon_id <- paste(site_name, flight_date, seq_len(nrow(shp_site)), sep = "_")
    shp_source <- "fixed"
  }

  #load ortho
  ortho <- rast(ortho_list_full_dir[i]) #plotRGB(ortho)
  names(ortho) <- c("r", "g", "b", "transparant")

  #reproject shapefile to match ortho CRS
  shp_reproj <- st_transform(shp_site, crs(ortho))
  # plot(shp_reproj, add = T, col = "red")

  #crop polygons
  shp_reproj_crop <- st_buffer(shp_reproj, dist = -0.75)
  # plot(shp_reproj, add = T, col = "white")

  #remove empty geometries
  shp_reproj_crop <- shp_reproj_crop[!st_is_empty(shp_reproj_crop), ]

  if (nrow(shp_reproj_crop) == 0) {
    next
  }

  #add site column if this came from a fixed shapefile (master already has it)
  if (!"site" %in% names(shp_reproj_crop)) {
    shp_reproj_crop$site <- site_name
  }

  #crop to ortho extent
  shp_reproj_crop_sv <- vect(shp_reproj_crop)
  shp_reproj_crop_sub <- crop(shp_reproj_crop_sv, ortho)
  shp_reproj_crop_sub_sf <- st_as_sf(shp_reproj_crop_sub)

  if (nrow(shp_reproj_crop_sub_sf) == 0) {
    next
  }

  #extract pixel values from ortho, including whichever id columns this source has, plus xy + coverage
  id_cols <- intersect(c("site", "poly_id", "poly_st", "tree", "polygon_id"),
                        names(shp_reproj_crop_sub_sf))

  extracted_values <- exact_extract(ortho, shp_reproj_crop_sub_sf,
                                    include_cols = id_cols,
                                    include_xy = TRUE,
                                    coverage_area = TRUE)

  #str(extracted_values)

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

  #remove geometry column from each dataframe before binding
  #can't handle NULL geometries
  extracted_values_clean <- lapply(extracted_values, function(x) {
    x$geometry <- NULL
    x
  })

  #combine all tree pixel values into df
  extracted_df <- bind_rows(extracted_values_clean) %>%
    dplyr::select(-transparant)

  # length(unique(extracted_df$poly_id))

  #predict if pixel is cone or foliage
  extracted_df$class <- predict(rf_mask_ortho, extracted_df)

  #count non-foliage pixels per polygon, keyed on whichever id column this source uses
  poly_id_col <- if (shp_source == "master") "poly_id" else "polygon_id"

  nonfoliage_counts <- extracted_df %>%
    group_by(across(all_of(poly_id_col))) %>%
    summarize(n_nonfoliage = sum(class != "yes"), .groups = "drop")

  #filter for foliage pixels and attach each polygon's non-foliage pixel count
  fol_pixels <- extracted_df %>%
    filter(class == "yes") %>%
    left_join(nonfoliage_counts, by = poly_id_col)

  # length(unique(fol_pixels$poly_id))

  # #visual check
  #
  # #get unique combinations of site, flight_date, and focal tree
  # unique_combos <- extracted_df %>%
  #   distinct(site, fcl_tr_)
  #
  # #pick one combination (e.g., first row)
  # test_combo <- unique_combos[2, ]
  #
  # #subset to that one tree
  # fol_pixels_test_df <- fol_pixels %>%
  #   filter(site == test_combo$site,
  #          fcl_tr_ == test_combo$fcl_tr_)
  #
  # fol_pixels_test_df_plot <- fol_pixels_test_df %>%
  #   mutate(color = rgb(r, g, b, maxColorValue = 255))
  #
  # #plot
  # ggplot(fol_pixels_test_df_plot, aes(x = x, y = y)) +
  #   geom_point(aes(color = color), size = 1) +
  #   scale_color_identity() +
  #   coord_equal() +
  #   theme_void() +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   ggtitle(paste(test_combo$site, test_combo$flight_date, "Tree", test_combo$fcl_tr_))

  #create final data frame
  px_df <- fol_pixels %>%
    mutate(flight_date = flight_date) %>%
    dplyr::select(-class)

  #fill in whichever id columns this source didn't produce, so every saved file
  #has the same columns regardless of source
  for (missing_col in setdiff(c("poly_id", "poly_st", "tree", "polygon_id"), names(px_df))) {
    px_df[[missing_col]] <- NA
  }

  # length(unique(index_df$poly_id))

  #save each index df as rds to assemble out of the for loop
  output_filename <- file.path(output_dir, paste0(site_name, "_", flight_date, "_pixels.rds"))
  saveRDS(px_df, output_filename)
  print(paste("Saved:", output_filename))

  print(i)

  #clean up
  rm(ortho, shp_reproj, shp_reproj_crop, shp_reproj_crop_sv,
     shp_reproj_crop_sub, shp_reproj_crop_sub_sf, extracted_values,
     extracted_df, nonfoliage_counts, fol_pixels, px_df)

  gc()
  tmpFiles(current = TRUE, remove = TRUE)
  tmpFiles(orphan = TRUE, remove = TRUE)
}




##

focal_tree_df <- read_csv("01_data/FieldMaps data 2026/focal_trees_2026.csv")
head(focal_tree_df)

unique(site_index_df$site)
unique(site_index_df$date)

site_index_df_pheno <- site_index_df %>%
  mutate(year = substr(date, 1, 4)) %>%
  filter(2025 %in% year) %>%
  group_by(site, tree) %>%
  filter(n_distinct(date) == 2) %>%
  ungroup()

site_index_df_pheno %>%
  filter("sonora" %in% site) %>%
  ggplot() +
  geom_boxplot(aes(x = tree, y = mean_rg_index, col = date))

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

#####

#new script

#script to extract pixel values + calculate index values for all trees at each site across the years

#set up work environment

library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(randomForest)
library(exactextractr)
library(mmstat4)

#lab desktop
setwd("C:/Users/hmz25/Box/Katz lab/texas/")

# #hz laptop
# setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")

# #mo comp
# setwd("C:/Users/HMZ/Box/texas")

# load in rf model to filter foliage vs non foliage pixels --------------------
load("03_output/rf_mask_2026.RData") #restores rf_mask_ortho into the environment
# rf_mask_ortho$err.rate[nrow(rf_mask_ortho$err.rate), "OOB"]

# set dir for ortho images ----------------------------------------------------
# ortho_dir <- "03_output/aligned_orthos"
ortho_dir <- "F:/aligned_orthos"
ortho_list <- list.files(ortho_dir, pattern = ".tif$", full.names = FALSE)
# ortho_list <- ortho_list[27:31] #subset to sonora for testing
ortho_list <- ortho_list[c(11, 18, 38)] #add in one-off sites
ortho_list_full_dir <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)
# ortho_list_full_dir <- ortho_list_full_dir[27:31]
ortho_list_full_dir <- ortho_list_full_dir[c(11, 18, 38)]

# ortho <- rast("C:/Users/hmz25/Box/Katz lab/texas/03_output/aligned_orthos/cathedral_20241230_transparent_mosaic_group1_aligned.tif")
# plotRGB(ortho)

## make sure all files are uncorrupted ----------------------------------------------

# list.raster.files(path = ortho_list_full_dir, return_rasters = FALSE)

# checkFiles(ortho_list_full_dir, open = 0)

# #create df to store results
# load_status <- data.frame(
#   file = basename(ortho_list_full_dir),
#   status = character(length(ortho_list_full_dir)),
#   error = character(length(ortho_list_full_dir)),
#   stringAsFactors = F
# )
#
# # i = 1
#
# #loop thru raster files
# for (i in seq_along(ortho_list_full_dir)) {
#   file <- ortho_list_full_dir[i]
#
#   tryCatch({
#     temp_rast <- rast(file)
#
#     load_status$status[i] <- "success"
#     load_status$error[i] <- "NA"
#
#   }, error = function(e) {
#     load_status$status[i] <- "fail"
#     load_status$error[i] <- e$message
#   })
#
#   print(i)
# }


# load in canopy segmentation file --------------------------------------------

all_canopy_seg <- st_read("01_data/all_trees_canopy_seg_shp.shp")
# str(all_canopy_seg)
# unique(length(all_canopy_seg$poly_id))

#filter out pinchotii + live oak
all_canopy_seg <- all_canopy_seg %>%
  filter(!grepl("Live oak", notes, ignore.case = T),
         !grepl("Pinchotii", notes, ignore.case = T))

#trim data frame to use in for loop
all_canopy_seg_sub <- all_canopy_seg %>%
  dplyr::select(site, poly_id, poly_st, geometry)

# load in per-site canopy shapefiles - fallback for sites whose corrected geometry
# hasn't been merged into the master file above yet -----------------------------
shp_dir <- "01_data/canopy segmentation"
shp_list <- list.files(shp_dir, pattern = "_fixed.shp$", full.names = FALSE)
shp_list_full_dir <- list.files(shp_dir, pattern = "_fixed.shp$", full.names = TRUE)

# loop through each site and shape file to extract pixel vals --------

#create output for files
output_dir <- "F:/ortho_px_output"  #adjust path if needed
# dir.create(output_dir, showWarnings = FALSE)

# i = 1

for (i in seq_along(ortho_list)) {
  
  #extract site name and date
  ortho_name <- basename(ortho_list[i])
  site_name <- substr(str_extract(ortho_name, "^[^_]+"),1,4)
  flight_date <- str_match(ortho_name, "^[^_]+_([^_]+)_")[,2]
  
  #try the master canopy segmentation file first
  shp_site <- all_canopy_seg_sub %>%
    filter(poly_st == site_name)
  
  if (nrow(shp_site) > 0) {
    shp_source <- "master"
  } else {
    #fall back to this site's per-site fixed shapefile
    match_index <- which(substr(shp_list, 1, 4) == site_name)
    
    if (length(match_index) == 0) {
      next
    }
    
    shp_site <- st_read(shp_list_full_dir[match_index[1]],
                        quiet = TRUE, fid_column_name = "tree")
    shp_site$polygon_id <- paste(site_name, flight_date, seq_len(nrow(shp_site)), sep = "_")
    shp_source <- "fixed"
  }
  
  #load ortho
  ortho <- rast(ortho_list_full_dir[i]) #plotRGB(ortho)
  names(ortho) <- c("r", "g", "b", "transparant")
  
  #reproject shapefile to match ortho CRS
  shp_reproj <- st_transform(shp_site, crs(ortho))
  # plot(shp_reproj, add = T, col = "red")
  
  #crop polygons
  shp_reproj_crop <- st_buffer(shp_reproj, dist = -0.75)
  # plot(shp_reproj, add = T, col = "white")
  
  #remove empty geometries
  shp_reproj_crop <- shp_reproj_crop[!st_is_empty(shp_reproj_crop), ]
  
  if (nrow(shp_reproj_crop) == 0) {
    next
  }
  
  #add site column if this came from a fixed shapefile (master already has it)
  if (!"site" %in% names(shp_reproj_crop)) {
    shp_reproj_crop$site <- site_name
  }
  
  #crop to ortho extent
  shp_reproj_crop_sv <- vect(shp_reproj_crop)
  shp_reproj_crop_sub <- crop(shp_reproj_crop_sv, ortho)
  shp_reproj_crop_sub_sf <- st_as_sf(shp_reproj_crop_sub)
  
  if (nrow(shp_reproj_crop_sub_sf) == 0) {
    next
  }
  
  #extract pixel values from ortho, including whichever id columns this source has, plus xy + coverage
  id_cols <- intersect(c("site", "poly_id", "poly_st", "tree", "polygon_id"),
                       names(shp_reproj_crop_sub_sf))
  
  extracted_values <- exact_extract(ortho, shp_reproj_crop_sub_sf,
                                    include_cols = id_cols,
                                    include_xy = TRUE,
                                    coverage_area = TRUE)
  
  #str(extracted_values)
  
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
  
  #remove geometry column from each dataframe before binding
  #can't handle NULL geometries
  extracted_values_clean <- lapply(extracted_values, function(x) {
    x$geometry <- NULL
    x
  })
  
  #combine all tree pixel values into df
  extracted_df <- bind_rows(extracted_values_clean) %>%
    dplyr::select(-transparant)
  
  # length(unique(extracted_df$poly_id))
  
  #predict if pixel is cone or foliage
  extracted_df$class <- predict(rf_mask_ortho, extracted_df)
  
  #count non-foliage pixels per polygon, keyed on whichever id column this source uses
  poly_id_col <- if (shp_source == "master") "poly_id" else "polygon_id"
  
  nonfoliage_counts <- extracted_df %>%
    group_by(across(all_of(poly_id_col))) %>%
    summarize(n_nonfoliage = sum(class != "yes"), .groups = "drop")
  
  #filter for foliage pixels and attach each polygon's non-foliage pixel count
  fol_pixels <- extracted_df %>%
    filter(class == "yes") %>%
    left_join(nonfoliage_counts, by = poly_id_col)
  
  # length(unique(fol_pixels$poly_id))
  
  # #visual check
  #
  # #get unique combinations of site, flight_date, and focal tree
  # unique_combos <- extracted_df %>%
  #   distinct(site, fcl_tr_)
  #
  # #pick one combination (e.g., first row)
  # test_combo <- unique_combos[2, ]
  #
  # #subset to that one tree
  # fol_pixels_test_df <- fol_pixels %>%
  #   filter(site == test_combo$site,
  #          fcl_tr_ == test_combo$fcl_tr_)
  #
  # fol_pixels_test_df_plot <- fol_pixels_test_df %>%
  #   mutate(color = rgb(r, g, b, maxColorValue = 255))
  #
  # #plot
  # ggplot(fol_pixels_test_df_plot, aes(x = x, y = y)) +
  #   geom_point(aes(color = color), size = 1) +
  #   scale_color_identity() +
  #   coord_equal() +
  #   theme_void() +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   ggtitle(paste(test_combo$site, test_combo$flight_date, "Tree", test_combo$fcl_tr_))
  
  #create final data frame
  px_df <- fol_pixels %>%
    mutate(flight_date = flight_date) %>%
    dplyr::select(-class)
  
  #fill in whichever id columns this source didn't produce, so every saved file
  #has the same columns regardless of source
  for (missing_col in setdiff(c("poly_id", "poly_st", "tree", "polygon_id"), names(px_df))) {
    px_df[[missing_col]] <- NA
  }
  
  # length(unique(index_df$poly_id))
  
  #save each index df as rds to assemble out of the for loop
  output_filename <- file.path(output_dir, paste0(site_name, "_", flight_date, "_pixels.rds"))
  saveRDS(px_df, output_filename)
  print(paste("Saved:", output_filename))
  
  print(i)
  
  #clean up
  rm(ortho, shp_reproj, shp_reproj_crop, shp_reproj_crop_sv,
     shp_reproj_crop_sub, shp_reproj_crop_sub_sf, extracted_values,
     extracted_df, nonfoliage_counts, fol_pixels, px_df)
  
  gc()
  tmpFiles(current = TRUE, remove = TRUE)
  tmpFiles(orphan = TRUE, remove = TRUE)
}
