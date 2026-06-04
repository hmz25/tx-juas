#script to extract digital number pixel values from cropped + masked quadrat images 

#set up environment
library(tidyverse)
library(dplyr)
library(terra)
library(raster)

#set working directory

#lab desktop
setwd("C:/Users/hmz25/Box/Katz lab/texas")

#hz laptop
setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")

#define path to cropped + masked quadrat images
img_folder <- "tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics"
img_list <- list.files(img_folder, full.names = FALSE, pattern = ".tif")
img_list_dir <- list.files(img_folder, full.names = TRUE, pattern = ".tif")

# test <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/gun_t10_tree.tif")
# plot(test)

#initiate data frame to store DN pixel values for each pixel in each quadrat 
quadrat_px_df <- data.frame()

# i = 1

for (i in seq_along(img_list)) {
  #remove the file extension to use as object name
  file_name <- tools::file_path_sans_ext(img_list[i])
  
  #define different file naming convention for female quadrat pics (all are on tree)
  if (str_detect(file_name, "female")){
    site <- str_split_i(file_name, "_", 1)
    
    date <- str_split_i(file_name, "_", 2)
    
    tree <- str_split_i(file_name, "_", 3)
    
    quadrat_location <- "tree"
  } else {
    
    #extract site name (everything before "_t")
    site <- str_split_i(file_name, "_", 1)
    
    #extract tree number (digits after "t")
    tree <- str_split_i(file_name, "_", 2)
    
    #extract quadrat location
    quadrat_location <- str_split_i(file_name, "_", 3)
    
  }
  
  #determine if image has been WB adjusted 
  adjustment <- if_else(str_detect(file_name, "_adj"), "adj", "unadj")
  
  #load the image raster
  photo_i <- rast(img_list_dir[i])
  # plotRGB(photo_i)
  
  #rename raster columns 
  names(photo_i) <- c("r", "g", "b", "rf_mask")
  
  #convert to data frame
  photo_i_df <- as.data.frame(photo_i, na.rm = FALSE) #keep NA values 
  
  #add site, tree, photo ID columns to df 
  photo_i_df$photo <- file_name
  photo_i_df$site <- site
  photo_i_df$tree <- tree
  photo_i_df$quadrat_location <- quadrat_location
  photo_i_df$adjustment <- adjustment
  
  #add to data frame of all pixels in each quadrat 
  quadrat_px_df <- rbind(quadrat_px_df, photo_i_df)
  
  print(i) 
}

quadrat_px_df #result is data frame with all pixel values from each quadrat 
#rf_mask values = 1 is not cone/fol, 2 is cone/fol 

#save output df as csv

output_dir <- "03_output"
  
write_csv(quadrat_px_df, paste0(output_dir,"/quadrat_px_df.csv"))
