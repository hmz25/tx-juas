library(terra)
library(imager)
library(tidyverse)
library(exifr)

img_20240103 <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2024 drone pics/DJI_202401031340_010/DJI_20240103134316_0015_V_capturepro.JPG")
plotRGB(img_20240103)


img_20240103_grey <- load.image("C:/Users/hmz25/Box/Katz lab/texas/tx 2024 drone pics/DJI_202401031340_010/DJI_20240103134316_0015_V_capturepro.JPG") %>% 
  grayscale()

plot(img_20240103_grey)

avg_brightness <- mean(img_20240103_grey)

img_20240108_grey <- load.image("C:/Users/hmz25/Box/Katz lab/texas/tx 2024 drone pics/DJI_202401081200_026/DJI_20240108120600_0019_V_capturepro.JPG") %>% 
  grayscale()

plot(img_20240108_grey)

avg_brightness <- mean(img_20240108_grey)

# exif_info <- read_exif("C:/Users/hmz25/Box/Katz lab/texas/tx 2024 drone pics/DJI_202401031340_010/DJI_20240103134316_0015_V_capturepro.JPG")
# exif_info <- read_exif("C:/Users/hmz25/Box/Katz lab/texas/tx 2024 drone pics/DJI_202401081200_026/DJI_20240108120600_0019_V_capturepro.JPG")
exif_info <- read_exif("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/DJI_202412311320_129_wade/DJI_20241231132341_0007_V.JPG")


#looking at how index changes between wb and not wb
no_wb <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/cath_t6.tif")
plotRGB(no_wb)

names(no_wb) <- c("r", "g", "b")
no_wb$index <- (no_wb$r - no_wb$g)/(no_wb$r + no_wb$g)
plot(no_wb$index)

wb <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/cath_t6.tif")
plotRGB(wb)

names(wb) <- c("r", "g", "b")
wb$index <- (wb$r - wb$g)/(wb$r + wb$g)
plot(wb$index)

hist(wb$index)
hist(no_wb$index)

#for aerial image
no_wb <- rast("C:/Users/hmz25/Desktop/DJI_20250115124007_0072_V copy.jpg")
plotRGB(no_wb)

names(no_wb) <- c("r", "g", "b")
no_wb$index <- (no_wb$r - no_wb$g)/(no_wb$r + no_wb$g)
plot(no_wb$index)

wb <- rast("C:/Users/hmz25/Desktop/DJI_20250115124007_0072_V copy_wb.tif")
plotRGB(wb)

names(wb) <- c("r", "g", "b")
wb$index <- (wb$r - wb$g)/(wb$r + wb$g)
plot(wb$index)

hist(wb$index)
hist(no_wb$index)
