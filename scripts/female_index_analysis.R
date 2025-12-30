#script to visualize index on female trees to see how values compare to male trees

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)

setwd("C:/Users/hmz25/Box/")


# load in rf mask to filter out non-cone/foliage pixels from canop --------

rf_mask_ortho <- get(load("Katz lab/texas/rf_mask_ortho.RData"))
# rf_mask_ortho

# set dir for ortho images ----------------------------------------------------
ortho_dir <- "Katz lab/texas/female tree index analysis/imgs"
ortho_list <- list.files(ortho_dir, pattern = ".tif$", full.names = FALSE)
ortho_list_full_dir <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)

# ortho <- rast(ortho_list_full_dir[1])
# plotRGB(ortho)

# set dir for segmented canopy shape files --------------------------------------------

shp_dir <- "Katz lab/texas/female tree index analysis/shp"
shp_list <- list.files(shp_dir, pattern = ".shp$", full.names = FALSE)
shp_list_full_dir <- list.files(shp_dir, pattern = ".shp$", full.names = TRUE)

# shp <- st_read(shp_list_full_dir[1])
# plot(shp)

# loop through each site and shape file to calculate mean index --------

female_index_df <- data.frame()

i = 5

for (i in seq_along(ortho_list)) {
  
  #extract site name and date
  ortho_name <- basename(ortho_list[i])
  site_prefix <- substr(ortho_name, 1, 4)  # first 4 characters
  site_name <- str_extract(ortho_name, "^[^_]+")
  flight_date <- str_match(ortho_name, "^[^_]+_([^_]+)_")[,2]
  
  #find matching shapefile (first 4 letters match)
  match_index <- which(substr(shp_list, 1, 4) == site_prefix)
  
  if (length(match_index) == 0) {
    next
  }
  
  shp_path <- shp_list_full_dir[match_index[1]]  # Use first match if multiple
  
  #load ortho and shapefile
  ortho <- rast(ortho_list_full_dir[i]) #plotRGB(ortho)
  names(ortho) <- c("r", "g", "b", "transparant")
  
  
  shp <- st_read(shp_path, quiet = TRUE) #plot(shp)
  shp_reproj <- st_transform(shp, crs(ortho)) #plot(shp_reproj, add = T, col = "red")
  
  #cropping the polygons so don't pick up as much soil 
  shp_reproj_crop <- st_buffer(shp_reproj, dist = -0.25) #plot(shp_reproj_crop, add = T, col = "white")
  
  #crop ortho to canopy
  ortho_crop <- crop(ortho, shp_reproj_crop)
  # plotRGB(ortho_crop)
  
  #filter out pixel values that aren't within the canopy
  ortho_mask <- mask(ortho_crop, shp_reproj_crop)
  # plotRGB(ortho_mask)
  
  #change names of ortho to match rf 
  ortho_mask <- ortho_mask[[1:3]]
  names(ortho_mask) <- c("r", "g", "b")
  
  #run random forest pixel classifier on cropped ortho 
  ortho_rf <- terra::predict(ortho_mask, rf_mask_ortho)
  # plot(ortho_rf)
  
  #filter out pixels that aren't foliage or cones
  filt <- ortho_rf == 1
  ortho_filt <- mask(ortho_mask, filt, maskvalue =1)
  # plotRGB(ortho_filt)
  
  #calculate index values on filtered canopy, compare with index values in the df above 
  ortho_filt$index <- (ortho_filt$r - ortho_filt$g)/(ortho_filt$r + ortho_filt$g)
  # plot(ortho_filt$index)
  
  female_i_index_df <- as.data.frame(ortho_filt)
  # mean(female_i_index_df$index)
  
  female_i_index_df <- female_i_index_df %>% 
    mutate(name = ortho_name,
           site = site_name,
           date = flight_date,
           mean_rg_index = mean(index))
  
  female_index_df <- rbind(female_i_index_df, female_index_df)
  
  print(i)
  
}

#print df 
female_index_df

female_index_df <- female_index_df %>% 
  mutate(tree = "female")

female_index_df_sub <- female_index_df %>% 
  group_by(site, date) %>% 
  summarize(mean_index = mean(index)) 

ggplot(female_index_df) + 
  geom_histogram(aes(x = index, fill = site), alpha = 0.5) +
  ggtitle("distribution of index values for female tree canopies") + 
  theme_classic()

ggplot(female_index_df) + 
  geom_violin(aes(x = site, y = index), alpha = 0.5) +
  ggtitle("distribution of index values for female tree canopies") + 
  theme_classic() 


# loop through male focal trees to see how index changes ------------------

male_shp_dir <- "Katz lab/texas/female tree index analysis/male_shp"
male_shp_list <- list.files(male_shp_dir, pattern = ".shp$", full.names = FALSE)
male_shp_list_full_dir <- list.files(male_shp_dir, pattern = ".shp$", full.names = TRUE)

# shp <- st_read(male_shp_list_full_dir[1])
# plot(shp)

# loop through each site and shape file to calculate mean index --------

male_index_df_25 <- data.frame()

# i = 8

for (i in seq_along(ortho_list)) {
  
  #extract site name and date
  ortho_name <- basename(ortho_list[i])
  site_prefix <- substr(ortho_name, 1, 4)  # first 4 characters
  site_name <- str_extract(ortho_name, "^[^_]+")
  flight_date <- str_match(ortho_name, "^[^_]+_([^_]+)_")[,2]
  
  #find matching shapefile (first 4 letters match)
  match_index <- which(substr(male_shp_list, 1, 4) == site_prefix)
  
  if (length(match_index) == 0) {
    next
  }
  
  shp_path <- male_shp_list_full_dir[match_index[1]]  # Use first match if multiple
  
  #load ortho and shapefile
  ortho <- rast(ortho_list_full_dir[i]) #plotRGB(ortho)
  names(ortho) <- c("r", "g", "b", "transparant")
  
  
  shp <- st_read(shp_path, quiet = TRUE) #plot(shp)
  shp_reproj <- st_transform(shp, crs(ortho)) #plot(shp_reproj, add = T, col = "red")
  
  #cropping the polygons so don't pick up as much soil 
  shp_reproj_crop <- st_buffer(shp_reproj, dist = -0.25) #plot(shp_reproj_crop, add = T, col = "white")
  
  #crop ortho to canopy
  ortho_crop <- crop(ortho, shp_reproj_crop)
  # plotRGB(ortho_crop)
  
  #filter out pixel values that aren't within the canopy
  ortho_mask <- mask(ortho_crop, shp_reproj_crop)
  # plotRGB(ortho_mask)
  
  #change names of ortho to match rf 
  ortho_mask <- ortho_mask[[1:3]]
  names(ortho_mask) <- c("r", "g", "b")
  
  #run random forest pixel classifier on cropped ortho 
  ortho_rf <- terra::predict(ortho_mask, rf_mask_ortho)
  # plot(ortho_rf)
  
  #filter out pixels that aren't foliage or cones
  filt <- ortho_rf == 1
  ortho_filt <- mask(ortho_mask, filt, maskvalue =1)
  # plotRGB(ortho_filt)
  
  #calculate index values on filtered canopy, compare with index values in the df above 
  ortho_filt$index <- (ortho_filt$r - ortho_filt$g)/(ortho_filt$r + ortho_filt$g)
  # plot(ortho_filt$index)
  
  male_i_index_df <- as.data.frame(ortho_filt)
  # mean(male_i_index_df$index)
  
  male_i_index_df <- male_i_index_df %>% 
    mutate(name = ortho_name,
           site = site_name,
           date = flight_date,
           mean_rg_index = mean(index))
  
  male_index_df_25 <- rbind(male_i_index_df, male_index_df_25)
  
  print(i)
  
}

#print df 
male_index_df_25

male_index_df_25 <- male_index_df_25 %>% 
  mutate(year = 2025)

male_index_df_sub_25 <- male_index_df_25 %>% 
  group_by(site, date) %>% 
  summarize(mean_index = mean(index)) 

index_df_join <- male_index_df_25 %>% 
  mutate(tree = "male") %>% 
  rbind(female_index_df)

ggplot(index_df_join) +
  geom_violin(aes(x = substr(site,1 ,4), y = index, fill = tree)) +
  ggtitle("2025 female vs male canopy index values") +
  xlab("site") + 
  theme_classic()


# examining index values on male trees for 2024 field season --------------

# fish <- rast("C:/Users/hmz25/Box/Katz lab/texas/orthos/fisher_20240112_transparent_mosaic_group1.tif")
# plotRGB(fish)
# 
# fish_tree <- st_read("C:/Users/hmz25/Box/Katz lab/texas/female tree index analysis/male_shp/fisher_male_tree_seg.shp")
# fish_tree <- st_transform(fish_tree, crs(fish))
# 
# plot(fish_tree, add = T, col = "red")

ortho_dir <- "Katz lab/texas/female tree index analysis/imgs_2024"
ortho_list <- list.files(ortho_dir, pattern = ".tif$", full.names = FALSE)
ortho_list_full_dir <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)

male_shp_dir <- "Katz lab/texas/female tree index analysis/male_shp_2024"
male_shp_list <- list.files(male_shp_dir, pattern = ".shp$", full.names = FALSE)
male_shp_list_full_dir <- list.files(male_shp_dir, pattern = ".shp$", full.names = TRUE)

# shp <- st_read(male_shp_list_full_dir[1])
# plot(shp)

# loop through each site and shape file to calculate mean index --------

male_index_df_24 <- data.frame()

# i = 7

for (i in seq_along(ortho_list)) {
  
  #extract site name and date
  ortho_name <- basename(ortho_list[i])
  site_prefix <- substr(ortho_name, 1, 4)  # first 4 characters
  site_name <- str_extract(ortho_name, "^[^_]+")
  flight_date <- str_match(ortho_name, "^[^_]+_([^_]+)_")[,2]
  
  #find matching shapefile (first 4 letters match)
  match_index <- which(substr(male_shp_list, 1, 4) == site_prefix)
  
  if (length(match_index) == 0) {
    next
  }
  
  shp_path <- male_shp_list_full_dir[match_index[1]]  # Use first match if multiple
  
  #load ortho and shapefile
  ortho <- rast(ortho_list_full_dir[i]) #plotRGB(ortho)
  names(ortho) <- c("r", "g", "b", "transparant")
  
  
  shp <- st_read(shp_path, quiet = TRUE) #plot(shp)
  shp_reproj <- st_transform(shp, crs(ortho)) #plot(shp_reproj, add = T, col = "red")
  
  #cropping the polygons so don't pick up as much soil 
  shp_reproj_crop <- st_buffer(shp_reproj, dist = -0.25) #plot(shp_reproj_crop, add = T, col = "white")
  
  #crop ortho to canopy
  ortho_crop <- crop(ortho, shp_reproj_crop)
  # plotRGB(ortho_crop)
  
  #filter out pixel values that aren't within the canopy
  ortho_mask <- mask(ortho_crop, shp_reproj_crop)
  # plotRGB(ortho_mask)
  
  #change names of ortho to match rf 
  ortho_mask <- ortho_mask[[1:3]]
  names(ortho_mask) <- c("r", "g", "b")
  
  #run random forest pixel classifier on cropped ortho 
  ortho_rf <- terra::predict(ortho_mask, rf_mask_ortho)
  # plot(ortho_rf)
  
  #filter out pixels that aren't foliage or cones
  filt <- ortho_rf == 1
  ortho_filt <- mask(ortho_mask, filt, maskvalue =1)
  # plotRGB(ortho_filt)
  
  #calculate index values on filtered canopy, compare with index values in the df above 
  ortho_filt$index <- (ortho_filt$r - ortho_filt$g)/(ortho_filt$r + ortho_filt$g)
  # plot(ortho_filt$index)
  
  male_i_index_df <- as.data.frame(ortho_filt)
  # mean(male_i_index_df$index)
  
  male_i_index_df <- male_i_index_df %>% 
    mutate(name = ortho_name,
           site = site_name,
           date = flight_date,
           mean_rg_index = mean(index))
  
  male_index_df_24 <- rbind(male_i_index_df, male_index_df_24)
  
  print(i)
  
}

#print df 
male_index_df_24

male_index_df_24 <- male_index_df_24 %>% 
  mutate(year = 2024)

male_index_df_sub <- male_index_df_24 %>% 
  group_by(site, date) %>% 
  summarize(mean_index = mean(index)) 

index_df_join <- male_index_df_24 %>% 
  mutate(tree = "male") %>% 
  rbind(female_index_df)

ggplot(index_df_join) +
  geom_violin(aes(x = substr(site,1,4), y = index, fill = tree)) + 
  ggtitle("2025 female canopy vs 2024 male canopy index values") + 
  xlab("site") + 
  theme_classic()

male_index_df_comb <- male_index_df_24 %>% 
  rbind(male_index_df_25) %>% 
  mutate(year = as.factor(year)) %>% 
  filter(!site == "gun")

ggplot(male_index_df_comb) +
  geom_violin(aes(x = substr(site, 1, 4), y = index, fill = year)) + 
  ggtitle("2024 vs 2025 male canopy index values") + 
  xlab("site") + 
  theme_classic()
