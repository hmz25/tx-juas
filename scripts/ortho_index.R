##script to apply index to segmented ortho images

library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(randomForest)
# install.packages("exactextractr")
library(exactextractr)

setwd("C:/Users/hmz25/Box/Katz lab/texas/")

# train rf model to filter foliage vs non foliage pixels --------------------

#load pictures to create training dataset for non-foliage and non-cones
not_twig <- rast("not_fol_ortho.tif")
#plotRGB(not_twig) 
#not_twig$not_fol_ortho_1 
#not_twig$not_fol_ortho_1[1:100] 

#dataframe for non-foliage and non-cone pixels
not_twig_df <- as.data.frame(not_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>%
  filter(r != 255) %>% 
  mutate(class = "not") 
#head(not_twig_df)

#load pictures to create training dataset for foliage and cones
yes_twig <- rast("yes_fol_ortho.tif")
#plotRGB(yes_twig) 
#yes_twig$yes_fol_ortho_1 
#yes_twig$yes_fol_ortho_1[1:100] 

#dataframe for foliage and cone pixels
yes_twig_df <- as.data.frame(yes_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>%
  filter(r != 255) %>% 
  mutate(class = "yes")  
#head(yes_twig_df)

#combine training datasets and randomly select pixels from theme
training_df_ortho <- bind_rows(not_twig_df, yes_twig_df) %>%
  sample_n(100000, replace = TRUE) %>%
  mutate(class = as.factor(class))
#head(training_df_ortho)
#str(training_df_ortho)

#run a pixel-based classifier
set.seed(100)
rf_mask_ortho <- randomForest(class ~ ., data = training_df_ortho, na.action=na.omit)
#rf_mask_ortho

# set dir for ortho images ----------------------------------------------------

ortho_dir <- "2025 orthos"
ortho_list <- list.files(ortho_dir, full.names = FALSE)
ortho_list_full_dir <- list.files(ortho_dir, full.names = TRUE)

# ortho <- rast("2025 orthos/sonora_20250115_transparent_mosaic_group1.tif")
# plotRGB(ortho)

# set dir for segmented canopy shape files --------------------------------------------

shp_dir <- "2025 juas qgis"
shp_list <- list.files(shp_dir, pattern = ".shp", full.names = FALSE)
shp_list_full_dir <- list.files(shp_dir, pattern = ".shp", full.names = TRUE)

# shp <- st_read("2025 juas qgis/sono_canopy_seg.shp")


# loop through each site and shape file to calculate mean index --------


site_mean_index_df <- data.frame()

# i = 1

for (i in seq_along(ortho_list)){
  
  #extract site name from filename (before the first "_")
  site_name <- str_extract(basename(ortho_list[i]), "^[^_]+")
  
  #load in ortho for the site
  ortho <- rast(ortho_list_full_dir[i])
  # plotRGB(ortho)
  
  #rename ortho columns 
  names(ortho) <- c("r", "g", "b", "transparant")
  
  #load in the shapefile for the site
  shp <- st_read(shp_list_full_dir[i])
  
  #align the crs of ortho and shape file
  shp_reproj <- st_transform(shp, crs(ortho))
  # crs(ortho) == crs(shp_reproj)
  # plot(shp_reproj, add = TRUE, col = "red")
  
  #extract values from ortho that are within the canopy shapefiles
  extracted_values <- exact_extract(ortho, shp_reproj)
  #mask instead of exact extract? to visualize 
  
  #convert extracted values to df
  extracted_df <- bind_rows(extracted_values)
  
  #apply rf classifier to extracted pixels
  extracted_df$class <- predict(rf_mask_ortho, extracted_df)
  # extracted_df$class == "yes"
  
  #filter df for foliage pixels 
  fol_pixels <- extracted_df %>% 
    filter(class == "yes")
  
  #calculate mean index value for the site 
  site_mean_index <- fol_pixels %>%
    mutate(rg_index = (r - g) / (r + g)) %>%
    summarize(mean_index = mean(rg_index, na.rm = TRUE)) %>%
    pull(mean_index)
  
  #add the mean site values and site name to the df
  site_mean_index_df <- rbind(site_mean_index_df, 
                              data.frame(site = site_name, mean_index = site_mean_index))
  
  print(n)
  
}

site_mean_index_df

ggplot(data = site_mean_index_df, aes(x = site, y = mean_index)) +
  geom_boxplot()


# compare this year index with 2024 smap data for each site ---------------

#read in smap data
smap_df <- read_csv("tx_SMAP_2024.csv")

#add in county data to site mean index df 
tx_counties <- read_csv("tx_site_metadata - Sheet1.csv") %>% 
  rename(site = site_name)

site_mean_index_join_df <- site_mean_index_df %>% 
  left_join(tx_counties, by = "site") %>% 
  select(-c(county.x, county.y, coords.x, coords.y, county.x.x, coords)) %>% 
  rename(county = county.y.y) %>% 
  as.data.frame()

write_csv(site_mean_index_join_df, "site_smap_df.csv")

#join smap df with index df 
site_index_smap <- smap_df %>% 
  rename(county = NAME) %>% 
  left_join(site_mean_index_df, by = "county") %>% 
  rename(mean_smap = mean)

ggplot(site_index_smap) +
  geom_point(aes(x = mean_smap, y = mean_index)) +
  theme_minimal()

#run linear model 
index_smap_lm <- lm(data = site_index_smap, mean_index ~ mean_smap)
summary(index_smap_lm)













