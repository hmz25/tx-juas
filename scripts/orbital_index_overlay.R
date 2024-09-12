#code to apply spectral index to orbital reconstructions of trees

#load libraries
library(lidR)
library(terra)
library(sf)
library(raster)

#browseVignettes(package = "lidR")

#read in LAS data from orbital reconstruction
wade_t3_lidar <- readLAS("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/point_cloud/20240103_Wade_tree3_orbital_group1_densified_point_cloud.las",
                   select = "RGB")

#explore the data
print(wade_t3_lidar)
str(wade_t3_lidar)
plot(wade_t3_lidar)
#plot(wade_t3, color = RGB)

#clip LAS to extent of focal tree
##load in shape file
wade_t3_ext <- st_read("C:/Users/hmz25/Desktop/TX 2024 analysis/qgis/wade_t3_extent.shp")

##reproject to same crs
#projection(wade_t3) <- crs(wade_t3_ext)
wade_t3_lidar_reproj <- st_transform(wade_t3_lidar, projection(wade_t3_))

wade_t3_ext_reproj <- st_transform(wade_t3_ext, st_crs(wade_t3_lidar))
compareCRS(wade_t3_lidar, wade_t3_ext_reproj)


wade_t3_lidar_clip <- clip_roi(wade_t3_lidar, wade_t3_ext_reproj)
plot(wade_t3_lidar_clip)

