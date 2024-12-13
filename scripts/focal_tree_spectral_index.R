##script to apply the index only to the trees that we have cone counts for!!!

#load libraries
library(terra)
library(tidyverse)
library(dplyr)
library(lidR)
library(sf)
library(raster)
library(stars)
library(randomForest)
library(ranger)
library(tools)

#load in orthos

setwd("C:/Users/hmz25/Desktop/")
ortho_dir <- "TX 2024 analysis/site_orthos"
#list.files(ortho_dir)

ortho_files <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)

for (i in seq_along(ortho_files)) {
  
  file_name <- file_path_sans_ext(basename(ortho_files[i]))
  
  assign(file_name, rast(ortho_files[i]))
  
  print(i)
}

#plotRGB(Williamson_20240104)

#load in shape files   

shp_dir <- "TX 2024 analysis/qgis/focal_trees_shp"
list.files(shp_dir)

shp_files <- list.files(shp_dir, pattern = ".shp", full.names = TRUE)

for (i in seq_along(shp_files)) {
  
  file_name <- file_path_sans_ext(basename(shp_files[i]))
  
  assign(file_name, st_read(shp_files[i]))
  
  print(i)
}

#in for loop, load in ortho and shp files, align crs, crop ortho so only include trees in shp file???

#align crs of orthos and shp files 

wade_shp_reproj <- st_transform(wade_focal_trees, terra::crs(Wade_20240103))
#compareCRS(st_crs(wade_shp_reproj), terra::crs(Wade_20240103))

# plotRGB(Wade_20240103)
# plot(wade_shp_reproj, add = TRUE, col = "red")

wade_ortho_ttops <- mask(Wade_20240103, wade_shp_reproj)
plotRGB(wade_ortho_ttops)

##RUN PIXEL CLASSIFIER ON MASKED ORTHO

#extract RGB values and corresponding geometry IDs
wade_ortho_ttops_df <- terra::extract(wade_ortho_ttops, wade_shp_reproj, df = TRUE)
#tail(wade_ortho_ttops_df)

#rename columns with red, green, blue bands and so that ID = tree #. Also add index column 
test_ortho_rgb_df <- wade_ortho_ttops_df %>% 
  dplyr::select(1:4) %>% 
  rename(tree = ID, r = 2, g = 3, b = 4) %>% 
  mutate(orange_index = ((r-g)/(r+g))) %>% 
  mutate(site = "wade") %>% 
  group_by(site, tree) %>% 
  summarize(sum_index = abs(sum(orange_index)))

#tail(wade_ortho_ttops_df)

#trying to visualize data, come back to this it's not working 
# library(ggplot2)
# ?geom_map
# 
# wade_shp_reproj_plot <- st_coordinates(wade_shp_reproj) %>% 
#   as.data.frame() %>% 
#   dplyr::select(c("X", "Y", "L2")) %>% 
#   rename(x = X,
#          y = Y,
#          id = L2)
# 
# ggplot(wade_ortho_ttops_df) + 
#   geom_map(aes(map_id = ID), map = wade_shp_reproj_plot)

#connect w cone density data

quadrat_cones <- read_csv("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/2024_quadratcones.csv")

#add cones per weight columns
quadrat_cones <- quadrat_cones %>%
  dplyr::select(date_collected,
                site,
                tree,
                quadrat,
                total_mass,
                s1_count, s1_weight,
                s2_count, s2_weight,
                s3_count, s3_weight,
                s4_count, s4_weight,
                s5_count, s5_weight) %>%
  mutate(s1 = (quadrat_cones$s1_count/quadrat_cones$s1_weight),
         s2 = (quadrat_cones$s2_count/quadrat_cones$s2_weight),
         s3 = (quadrat_cones$s3_count/quadrat_cones$s3_weight),
         s4 = (quadrat_cones$s4_count/quadrat_cones$s4_weight),
         s5 = (quadrat_cones$s5_count/quadrat_cones$s5_weight))  

#take mean of cone per weight for each quadrat
quadrat_cones <- quadrat_cones %>%
  dplyr::select(date_collected,
                site,
                tree,
                quadrat,
                total_mass,
                s1,
                s2,
                s3,
                s4,
                s5)

quadrat_cones$cone_mean <- rowMeans(subset(quadrat_cones, select = c("s1", "s2", "s3", "s4", "s5")),
                                    na.rm = TRUE)

#take standard deviation of quadrat cones
quadrat_cones <- quadrat_cones %>%
  rowwise %>%
  mutate(cone_sd = sd(c_across(s1:s5))) %>%
  ungroup()

#calculate total number of cones in each quadrat
quadrat_cones <- quadrat_cones %>%
  mutate(total_cones = cone_mean*total_mass) #total cones = cones per 25cmx25cm (625cm2)

#rename date column and ensure dates are in the right format
quadrat_cones <- rename(quadrat_cones, date = date_collected)
quadrat_cones$date <- mdy(quadrat_cones$date)

# quadrat_cones %>% 
#   group_by(date, site, tree) %>% 
#   summarize(total_cones_tree = sum(total_cones))

#account for pixel size 

cones_per_px_df <- quadrat_cones %>% 
  dplyr::select(date, site, tree, quadrat, total_cones) %>% #gives total cones per 625 cm2 (25cm x 25cm)
  mutate(cones_per_cm = total_cones/625) %>% #cones per cm seems low?
  mutate(cones_per_pix = cones_per_cm*0.982) %>% 
  group_by(date, site, tree) %>% 
  summarise(avg_cones_per_pix = mean(cones_per_pix, na.rm=T))

#create pixel size df for orthos 
res_list <- vector("list", length(ortho_files))

for (i in seq_along(ortho_files)) {
  
  file_name <- file_path_sans_ext(basename(ortho_files[i]))
  
  #load the orthomosaic as a SpatRaster
  ortho <- rast(ortho_files[i])
  
  #resolution in meters per pixel
  resolution <- res(ortho)
  
  #total number of pixels in the ortho
  total_pixels <- ncell(ortho)
  
  #store file name, resolution, and total pixels in a data frame row
  res_list[[i]] <- data.frame(
    file_name = file_name,
    res_x = resolution[1],      # resolution in x-direction
    res_y = resolution[2],      # resolution in y-direction
    total_pixels = total_pixels # total number of pixels
  )
}

#combine rows into a single data frame
site_px_df <- bind_rows(res_list)

#reformat df so it can be joined w the cones df
site_px_df <- site_px_df %>% 
  separate(file_name, into = c("site", "date"), sep = "_") %>% 
  mutate(site = tolower(site)) %>%
  mutate(site = str_replace(site, "celltower", "cell tower")) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(res_x_cm = res_x * 100,
         res_y_cm = res_y * 100) %>%  #convert m to cm 
  mutate(cm_per_px = res_x_cm * res_y_cm) %>% 
  dplyr::select(site, date, cm_per_px, total_pixels)

#join site_px df with site_cones df 
cones_per_cm2_df <- quadrat_cones %>% 
  dplyr::select(date, site, tree, quadrat, total_cones) %>% #gives total cones per 625 cm2 (25cm x 25cm)
  group_by(date, site, tree) %>% 
  summarize(avg_cone_per_tree = mean(total_cones, na.rm = TRUE)) %>% #gives you avg cones per 625 from all quadrats
  mutate(cones_per_cm = avg_cone_per_tree/625) 

total_cones_df <- cones_per_cm2_df %>% 
  left_join(site_px_df, by = c("date", "site")) %>% 
  mutate(cones_per_px = cones_per_cm * cm_per_px) %>% 
  mutate(total_cones = total_pixels * cones_per_px) %>% 
  as.data.frame()

#now take sum of spectral index for each tree and join df and plot 
test_spectral_cone_df <- test_ortho_rgb_df %>% 
  left_join(total_cones_df, by = c("site", "tree"))


# wade_cone_df <- cones_per_px_df %>% 
#   filter(date == "2024-01-03")
# 
# wade_cone_rgb_df <- wade_cone_df %>% 
#   left_join(wade_ortho_ttops_df, by = "tree")

#multiply cones per cm by number or pixels in each tree 
#sum the index
#plot cone density vs index for each tree 

t1_px <- wade_ortho_ttops_df %>% 
  filter(tree == "1")
#tail(t1_px)
nrow(t1_px)

wade_ortho_ttops_df %>% 
  group_by(tree) %>% 
  summarize(sum_index = abs(sum(orange_index)))

quadrat_cones %>% 
  dplyr::select(date, site, tree, quadrat, total_cones) %>% #gives total cones per 625 cm2 (25cm x 25cm)
  mutate(cones_per_cm = total_cones/625) %>% #cones per cm seems low?
  mutate(cones_per_pix = cones_per_cm*0.982) %>% 
  group_by(date, site, tree) %>% 
  summarise(avg_cones_per_pix = mean(cones_per_pix, na.rm=T)) 

#need to figure out what data points i actually want to fall on the graphs and do the data manipulation that way
## sum of spectral reading vs sum cones based on site and on tree? I think that's what I want 
