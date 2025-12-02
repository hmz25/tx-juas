#script to filter and mask quadrat images using rf pixel classifier

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

#apply model to a single photo
photo_i <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/wade_t7.tif")
#plotRGB(photo_i)

#make sure layer names for picture match the training df column names 
names(photo_i) <- colnames(training_df)[1:3]
photo_i_mask <- predict(photo_i, rf_mask)
plot(photo_i_mask)

photo_j <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/fish_t10.tif")
plotRGB(photo_j)
names(photo_j) <- colnames(training_df)[1:3]
photo_j_mask <- predict(photo_j, rf_mask)
plot(photo_j_mask)

#add mask layer back to original photo
photo_i <- c(photo_i, photo_i_mask)
plot(photo_i)
# plot(photo_i$class) 
# plot(photo_i$r)

# applying rf pixel classifier to all handheld drone images and saving in new folder ---------------------

photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025"
photo_list <- list.files(photo_dir, full.names = FALSE, pattern = ".tif")
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE, pattern = ".tif")

# create new sub folder for masked white balanced pics
output_subfolder <- file.path(photo_dir, "masked_handheld_quadrat_pics")
if (!dir.exists(output_subfolder)) {
  dir.create(output_subfolder, recursive = TRUE, showWarnings = TRUE)
}

i = 2

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
  
  save_file_name = paste0("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/",photo_list[i])
  raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
  print(i)
}

cath_female <- raster::stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_female.tif")
plotRGB(cath_female)
plot(cath_female)

cath_t9 <-raster::stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_t9.tif")
plotRGB(cath_t9)
plot(cath_t9)

cath_t7 <-raster::stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_t7.tif")
plotRGB(cath_t7)
plot(cath_t7)

