#script to analyze spectral index on same trees at different phenological stages


# load libraries ----------------------------------------------------------
library(terra)
library(tidyverse)
library(lidR)
library(sf)
library(randomForest)
library(ranger)
library(tools)
library(raster)

# load in photos from different days --------------------------------------

### starting with cropped masked handheld pics
### should we only be looking at focal trees?

setwd("C:/Users/hmz25/")
# handheld_dir <- "Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level"
# #list.files(handheld_dir)
# 
# date_collected_csv <- read.csv("Box/texas/pollen_production/TX jan 24/data analysis/cone processing 24 - counts.csv") %>% 
#   dplyr::select(date_collected, site, tree, quadrat) %>% 
#   mutate(quadrat = as.integer(quadrat)) %>% 
#   mutate(site = str_replace(site, "cell tower", "cell")) 
# 
# #str(date_collected_csv)
# 
# handheld_files <- list.files(handheld_dir, pattern = ".tif$", full.names = TRUE)
# 
# # for (i in seq_along(handheld_files)) {
# #   
# #   file_name <- file_path_sans_ext(basename(handheld_files[i]))
# #   
# #   assign(file_name, rast(handheld_files[i]))
# #   
# #   print(i)
# # }
# 
# for (i in seq_along(handheld_files)) {
#   #extract file name
#   file_name <- file_path_sans_ext(basename(handheld_files[i]))
#   
#   #parse site, tree, and quadrat
#   site <- str_extract(file_name, "^[^_]+") #text before first '_'
#   tree <- as.integer(str_extract(file_name, "(?<=_t)\\d+")) #number after '_t'
#   quadrat <- as.integer(str_extract(file_name, "(?<=_q)\\d+")) #number after '_q'
#   
#   #match with date metadata
#   matched_metadata <- date_collected_csv %>%
#     filter(site == !!site, tree == !!tree, quadrat == !!quadrat)
#   
#   if (nrow(matched_metadata) == 1) {
#     date_collected <- matched_metadata$date_collected
#   } else {
#     warning(paste("metadata mismatch for file:", file_name))
#     next
#   }
#   
#   #create new name
#   new_file_name <- paste0(site, "_t", tree, "_q", quadrat, "_", date_collected)
#   
#   #read the raster and assign with the new name
#   assign(new_file_name, rast(handheld_files[i]))
#   
#   print(i)
# }
# plotRGB(`cathedral_t3_q1_01/05/2024`)

#i'm dumb, we need to look at ortho pics

ortho_dir <- "Desktop/TX 2024 analysis/site_orthos"
#list.files(ortho_dir)

ortho_files <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)

for (i in seq_along(ortho_files)) {
  
  file_name <- file_path_sans_ext(tolower(basename(ortho_files[i])))
  
  assign(file_name, rast(ortho_files[i]))
  
  print(i)
}

#plotRGB(williamson_20240104)
#crs(celltower_20240111)

# load in shape files of focal trees -------------------------------------

### could also assign random focal trees using qgis instead of just looking at focal trees
shp_dir <- "Desktop/TX 2024 analysis/qgis/focal_trees_shp"
list.files(shp_dir)

shp_files <- list.files(shp_dir, pattern = ".shp", full.names = TRUE)

for (i in seq_along(shp_files)) {
  #extract the base name of the file without extension
  file_name <- file_path_sans_ext(basename(shp_files[i]))
  
  #read the shapefile
  shp_data <- st_read(shp_files[i])
  
  #reproject the shapefile to match the CRS of the raster
  shp_reproj <- st_transform(shp_data, terra::crs(wade_20240103))
  
  #assign the reprojected shapefile to a variable in the global environment
  assign(file_name, shp_reproj)
  
  print(i)
}

#compareCRS(crs(glimmer_focal_trees), crs(glimmer_20240102))
#plotRGB(glimmer_20240102)
#plot(glimmer_focal_trees, add = TRUE, col = "red")
glimmer_focal_trees


# use shape files to extract focal trees from orthos ----------------------

#create list of orthos and shape file names
ortho_names <- ls(pattern = "^[A-Za-z]+_\\d{8}$") 
shapefile_names <- ls(pattern = "_focal_trees$") 

ortho_name = "cathedral_20240105"

#loop over ortho images and extract only parts of image with focal trees
for (ortho_name in ortho_names) {
  #get site names from orthos
  site_name <- str_extract(ortho_name, "^[A-Za-z]+") 
  
  #find matching shape file 
  shapefile_name <- shapefile_names[grepl(paste0("^", site_name, "_focal_trees$"), shapefile_names)]
  
  #check if a matching shape file was found
  if (length(shapefile_name) == 1) {
    #get the raster and shape file objects
    raster <- get(ortho_name) #plotRGB(raster)
    shapefile <- get(shapefile_name) #plot(shapefile, add = TRUE, col = "red")
    
    #crop and mask the ortho using the shape file
    cropped_raster <- crop(raster, shapefile) #plotRGB(cropped_raster) #cropped_raster
    #do we need to crop??
    masked_raster <- mask(cropped_raster, shapefile) #plotRGB(masked_raster) #masked_raster
    
    #rasterize sf object to assign tree id to pixels in raster 
    id_raster <- rasterize(vect(shapefile), masked_raster, field = "id", background = NA) #plot(id_raster) 
    
    # Combine the masked raster with the ID raster
    final_raster <- c(masked_raster, id_raster)
    names(final_raster)[nlyr(final_raster)] <- "tree_id" #name tree id layer
    
    #create a new name for the processed raster
    new_name <- paste0(ortho_name, "_cropped")
    
    #assign the cropped and masked raster to a new object
    assign(new_name, final_raster)
    
    #save the new raster to disk
    # writeRaster(masked_raster, filename = paste0(new_name, ".tif"), format = "GTiff", overwrite = TRUE)
    
    print(paste("processed:", new_name))
  } else {
    warning(paste("no matching shapefile found for raster:", ortho_name))
  }
}
#williamson fails because we don't have focal trees from williamson

# plotRGB(wade_20240103_cropped) plot(wade_20240103_cropped)
# plotRGB(sweeten_20240116_cropped)
# plotRGB(sonora_20240109_cropped)

# run pixel classifier on cropped orthos ------------------------------------------

#load pictures to create training dataset for non-foliage and non-cones
not_twig <- stack("Box/texas/pollen_production/TX jan 24/data analysis/not_ortho.tif")
#plotRGB(not_twig) 
#not_twig$not_ortho_1 
#not_twig$not_ortho_1[1:100] #all 255

#dataframe for non-foliage and non-cone pixels
not_twig_df <- as.data.frame(not_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>%
  filter(r != 255) %>% 
  mutate(class = "not") 
#head(not_twig_df)

#load pictures to create training dataset for foliage and cones
yes_twig <- stack("Box/texas/pollen_production/TX jan 24/data analysis/yes_ortho.tif")
#plotRGB(yes_twig) 
#yes_twig$yes_ortho_1 
#yes_twig$yes_ortho_1[1:100] #also all 255

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
#str(rf_mask_ortho)

#create new list for all cropped rasters
cropped_orthos <- ls(pattern = "^[A-Za-z]+_\\d{8}_cropped$")


# ortho_names
# i = "kimble_20240117_cropped" #plotRGB(i)
# plotRGB(i)
# plot(i)
# 
# i = 7

for (i in seq_along(cropped_orthos)) {
  #get ortho from R environment 
  ortho <- mget(cropped_orthos)[[i]] #plotRGB(ortho)
  
  #subset r, g, b, and tree id layers from the ortho
  ortho_sub <- subset(ortho, c(1, 2, 3, 5))
  
  #rename ortho layers to match rf training columns 
  names(ortho_sub) <- c("r", "g", "b", "tree_id") #colnames(training_df_ortho)[1:3]
  
  #use the rf to predict if pixels are foliage vs not foliage 
  ortho_rf_layer <- predict(ortho_sub, rf_mask_ortho) #plot(ortho_rf_layer)
  
  #combine ortho and new foliage vs not foliage predicted layer
  ortho_sub_rf <- c(ortho_sub, ortho_rf_layer)
  
  #use rf layer to retain only pixels that are foliage 
  ortho_mask <- ifel(ortho_sub_rf$class == "yes", 1, NA)
  
  #mask out all pixels from the ortho that are not foliage 
  filtered_ortho <- mask(ortho_sub_rf, ortho_mask) #plot(filtered_ortho) plotRGB(filtered_ortho)
  
  #create a new name for the processed ortho
  new_name <- paste0(cropped_orthos[i], "_masked")
  
  #assign the filtered ortho to a new object
  assign(new_name, filtered_ortho)
  
  print(i)
  
}

# plotRGB(glimmer_20240102_cropped_masked)
# plot(kimble_20240110_cropped_masked)
# plotRGB(kimble_20240110_cropped_masked)
# plotRGB(kimble_20240117_cropped_masked)

# compare how spectral index changes based on phenology ------------------------

#spectral index on different dates

masked_orthos <- ls(pattern = "^[A-Za-z]+_\\d{8}_cropped_masked$")

# masked_orthos[1]
# test <- get(masked_orthos[i])
# plotRGB(test)

# i = 7

combined_df <- data.frame()

for (i in seq_along(masked_orthos)) {
  
  masked_ortho <- get(masked_orthos[i])
  
  ortho_df <- as.data.frame(masked_ortho) %>% 
    rename(r = 1, g = 2, b = 3, tree_id = 4, foliage = 5) %>% 
    mutate(orange_index = ((r-g)/(r+g))) %>% 
    mutate(site = paste0(masked_orthos[i])) %>% 
    dplyr::select(-foliage)
  
  combined_df <- rbind(combined_df, ortho_df)
  
  print(i)
}

combined_df_clean <- combined_df %>% 
  separate(site, into = c("site", "date", "cropped", "masked"), sep = "_") %>% 
  dplyr::select(-c("cropped","masked")) %>% 
  filter(!is.na(tree_id)) %>% 
  mutate(tree_id = as.character(tree_id))#%>% 
  #mutate(date = ymd(date))

# str(combined_df_clean)

df_test <- combined_df_clean %>%
  filter(site == 'kimble') %>%
  arrange(tree_id) %>%
  mutate(date = ymd(date)) 

str(df_test)

# ggplot(df_test, aes(x = as.factor(tree_id), y = orange_index, group = as.factor(date), fill = as.factor(date))) +
#   geom_boxplot() +
#   theme_minimal()

# ggplot(df_test, aes(x = tree_id, y = orange_index, group = interaction(tree_id, date), fill = interaction(tree_id, date))) +
#   geom_boxplot() +
#   theme_minimal()
# 
# ggplot(df_test, aes(x = tree_id, y = orange_index, group = interaction(tree_id, date))) + geom_violin(aes(fill = as.factor(date)))

ggplot(combined_df_clean, aes(x = tree_id, y = orange_index, group = interaction(tree_id, date))) + 
  geom_violin(aes(fill = as.factor(date))) +
  facet_wrap(~site)


# ggplot(combined_df_clean, aes(x = tree_id, y = orange_index, group = date, fill = date)) +
#   geom_boxplot() +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   facet_wrap(~site)

#sd of spectral index at different dates?


# compare how spectral index changes based on time of day -----------------

time_metadata <- read_csv("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/2024 TX drone pics metadata.csv")
# time_metadata

# library(janitor)

time_metadata <- time_metadata %>%
  clean_names() %>%
  filter(!is.na(time)) %>%
  dplyr::select(site, date, time)
  

