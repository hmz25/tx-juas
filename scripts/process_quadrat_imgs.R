#script to process (filter + mask) aerial quadrat images from 2026 field season using rf pixel classifier

library(tidyverse)
library(dplyr)
library(terra)
library(stringr)
library(raster)
library(readr)
library(purrr)
#library(matrixStats)
#install.packages("randomForest")
library(randomForest)
library(lubridate)
library(ggplot2)
library(janitor)
# install.packages("hms")
library(hms)
#install.packages("ggpubr")
library(ggpubr)

setwd("C:/Users/hmz25/Box/")

# exploring random forest pixel classifier to mask out non cones/non foliage --------
#load in rf mask (created in rf_mask script)
rf_mask <- get(load("Katz lab/texas/rf_mask_2026.RData"))
# print(rf_mask)

#apply model to a single photo
photo_i <- rast("Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/cathedral_t3_tree.tif")
#plotRGB(photo_i)

#make sure layer names for picture match the training df column names 
names(photo_i) <- c("r", "g", "b")
photo_i_mask <- predict(photo_i, rf_mask)
plot(photo_i_mask)

photo_j <- rast("Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/fisher_t6_tree.tif")
plotRGB(photo_j)
names(photo_j) <- c("r", "g", "b")
photo_j_mask <- predict(photo_j, rf_mask)
plot(photo_j_mask)

#add mask layer back to original photo
photo_i <- c(photo_i, photo_i_mask)
plot(photo_i)
# plot(photo_i$class) 
# plot(photo_i$r)

# applying rf pixel classifier to all aerial drone images and saving in new folder ---------------------

photo_dir <- "C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics"
photo_list <- list.files(photo_dir, full.names = FALSE, pattern = ".tif")
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE, pattern = ".tif")

# create new sub folder for masked white balanced pics
output_subfolder <- file.path(photo_dir, "masked_cropped_quadrat_pics")
if (!dir.exists(output_subfolder)) {
  dir.create(output_subfolder, recursive = TRUE, showWarnings = TRUE)
}

i = 7

for(i in 1:length(photo_list)){
  photo_i <- raster::stack(photo_list_full_dir[i])  #plotRGB(photo_i)
  
  photo_i_df <- as.data.frame(photo_i) %>%
    rename(c("r" = 1 , "g" = 2, "b" = 3))  #head(photo_i_df)
  
  photo_i_mask_df <- predict(rf_mask, photo_i_df)
  # head(photo_i_mask_df)
  # photo_i_df <- photo_i_df %>%
  #   mutate(is_foreground = photo_i_mask_df,
  #          is_foreground_numeric = case_when( is_foreground == "yes" ~ 1,
  #                                             is_foreground == "not" ~ 0))
  # #head(photo_i_df)
  
  photo_i <- addLayer(photo_i, photo_i[[3]])
  photo_i[[4]] <- photo_i_mask_df # photo_i_df$is_forground
  # plot(photo_i[[4]])
  
  save_file_name = paste0(output_subfolder,"/",photo_list[i])
  raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
  print(i)
}

test <- stack("Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/gun_t10_tree.tif")
plotRGB(test)
plot(test)
