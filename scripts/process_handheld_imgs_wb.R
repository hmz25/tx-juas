#script to filter and mask quadrat images that have been white balanced using rf pixel classifier

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


# applying rf pixel mask to all white balanced handheld drone images and saving in new folder ---------------------

photo_dir <- "Katz Lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced"
photo_list <- list.files(photo_dir, full.names = FALSE, pattern = ".tif")
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE, pattern = ".tif")

# create new sub folder for masked white balanced pics
output_subfolder <- file.path(photo_dir, "masked_wb")
if (!dir.exists(output_subfolder)) {
  dir.create(output_subfolder, recursive = TRUE, showWarnings = TRUE)
}

#eventually need to add in rf object that has been trained on wb photos

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
  
  #save masked img
  save_file_name <- paste0("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_wb/",
                           tools::file_path_sans_ext(photo_list[i]), 
                           "_wb.tif")
  raster::writeRaster(photo_i, filename = save_file_name, overwrite = TRUE)
  print(i)
}

#examining picys
cath_female <- raster::stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_wb/cath_female_wb.tif")
plotRGB(cath_female)
plot(cath_female)

cath_t9 <- raster::stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_wb/cath_t9_wb.tif")
plotRGB(cath_t9)
plot(cath_t9)

cath_t9_orig <- raster::stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/corrected_masked_handheld_quadrat_pics/cath_t9.tif")
plotRGB(cath_t9_orig)

filt <- cath_t9$cath_t9_wb_4 == 1
cath_t9_filt <- mask(cath_t9, filt, maskvalue = 1)
plotRGB(cath_t9_filt)

cath_t9_filt_index <- (cath_t9_filt$cath_t9_wb_1 - cath_t9_filt$cath_t9_wb_2)/(cath_t9_filt$cath_t9_wb_1 + cath_t9_filt$cath_t9_wb_2)
plot(cath_t9_filt_index, zlim = c(-0.4, 0.2))
cellStats(cath_t9_filt_index, stat = 'mean')
cellStats(cath_t9_filt_index, stat = 'sum')

filt <- cath_t9_orig$cath_t9_4 == 1
cath_t9_orig_filt <- mask(cath_t9_orig, filt, maskvalue = 1)
plotRGB(cath_t9_orig_filt)

cath_t9_orig_filt_index <- (cath_t9_orig_filt$cath_t9_1 - cath_t9_orig_filt$cath_t9_2)/(cath_t9_orig_filt$cath_t9_1 + cath_t9_orig_filt$cath_t9_2)
plot(cath_t9_orig_filt_index, zlim = c(-0.4, 0.2))
cellStats(cath_t9_orig_filt_index, stat = 'mean')
cellStats(cath_t9_orig_filt_index, stat = 'sum')

