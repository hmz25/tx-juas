#script to crop images for labeling for YOLO

library(raster)
library(terra)
# install.packages("SpaDES")
library(SpaDES)
# install.packages("imageRy")
library(imageRy)

setwd("C:/Users/hmz25/Box/Katz lab/texas/")


#load in img

#crop into 3 random sections

#save each crop as new tiff in new folder

input_folder  <- "tx 2026 drone pics/2026 quadrat pics/iphone_quadrat_pics_2026"
output_folder <- "tx 2026 drone pics/2026 quadrat pics/YOLO_training_imgs/"

files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
# files

# i = 1
# j = 1

for (i in seq_along(files)) {
  r <- rast(files[i])
  
  splits <- splitRaster(r, 3, 3)
  
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


