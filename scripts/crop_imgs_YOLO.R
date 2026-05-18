#script to crop iPhone quadrat images for labeling for YOLO

library(raster)
library(terra)
# install.packages("SpaDES")
library(SpaDES)
# install.packages("imageRy")
library(imageRy)

setwd("C:/Users/hmz25/Box/Katz lab/texas/")



# crop iPhone images ------------------------------------------------------

input_folder  <- "tx 2026 drone pics/2026 quadrat pics/iphone_quadrat_pics_2026/cropped_iphone_quadrat_pics_2026"
output_folder <- "tx 2026 drone pics/2026 quadrat pics/YOLO_training_imgs/"

files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
# files

# i = 1
# j = 1

for (i in seq_along(files)) {
  
  #load in iPhone quadrat img
  r <- rast(files[i])
  # plotRGB(r)
  
  #split into 3 by 3 sqaures
  splits <- splitRaster(r, 3, 3)
  
  #save each crop as new tiff in new folder
  for (j in seq_along(splits)) {
    img_name <- tools::file_path_sans_ext(basename(files[i]))
    
    writeRaster(splits[[j]], filename = paste0(output_folder,img_name,"_",j,".png"))
  }
  
  print(i)
}

# r <- rast(files[1])
# plotRGB(r)
# 
# splits <- splitRaster(r, 3, 3)
# plotRGB(splits[[1]])
# plotRGB(splits[[2]])
# plotRGB(splits[[3]])

# crop up-close drone images ------------------------------------------------------

input_folder  <- "tx 2026 drone pics/2026 quadrat pics/handheld_drone_pics/cropped_handheld_drone_pics"
output_folder <- "tx 2026 drone pics/2026 quadrat pics/YOLO_training_imgs/drone/"

files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
# files

# i = 1
# j = 1

for (i in seq_along(files)) {
  
  #load in iPhone quadrat img
  r <- rast(files[i])
  # plotRGB(r)
  
  #split into 3 by 3 sqaures
  splits <- splitRaster(r, 3, 3)
  
  #save each crop as new tiff in new folder
  for (j in seq_along(splits)) {
    img_name <- tools::file_path_sans_ext(basename(files[i]))
    
    writeRaster(splits[[j]], filename = paste0(output_folder,img_name,"_",j,".png"))
  }
  
  print(i)
}





