
getwd()

library(sf)
library(exifr)# install.packages("exifr")
library(dplyr)

#load in shape file
shp <- st_read("Katz lab/texas/focal_trees_2025_shp/focal_trees_2025_shp.shp")

#select site for analysis
shp_clean <- shp |> 
  mutate(site = tolower(substr(site, 1, 4))) 

site_df <- shp_clean |> 
  filter(site == "gun") 

#extract GPS data from images
image_folder <- "/Volumes/Seagate Portable/DJI_202501131305_179_gunsiteboundary"
image_files <- list.files(image_folder, pattern = "\\.JPG$", full.names = TRUE)

exif_data <- read_exif(image_files, tags = c("GPSLongitude", "GPSLatitude", "FileName"))

#convert to numeric coordinates
exif_data <- exif_data %>%
  mutate(
    GPSLongitude = as.numeric(GPSLongitude),
    GPSLatitude = as.numeric(GPSLatitude))

#convert image GPS points to sf object
image_sf <- st_as_sf(exif_data, coords = c("GPSLongitude", "GPSLatitude"), crs = 4326)

st_crs(image_sf)
st_crs(site_df)

site_df <- st_transform(site_df, crs = 4326)

st_bbox(site_df) 
st_bbox(image_sf)

ggplot() +
  geom_sf(data = image_sf, color = "blue", size = 2, alpha = 0.5) +  #images in blue
  geom_sf(data = site_df, color = "red", size = 3, shape = 4) +  #focal trees in red 
  coord_sf() +
  theme_minimal() 

#spatial join to find overlaps
images_with_trees <- st_join(image_sf, site_df, join = st_is_within_distance, dist = 20)

#filter overlapping images
img_overlap <- images_with_trees %>%
  dplyr::select(FileName, focal_tree) %>%
  filter(!is.na(focal_tree))

