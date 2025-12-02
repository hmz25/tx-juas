library(tidyverse)
library(data.table) # install.packages("data.table")
library(lubridate)
library(janitor)

#note for HZ -- went in and manually added canopy area and height for cal trees directly to google sheets
##loading in separate dfs for 2024 + 2025 because same tree was named differently in years 1 and 2

#load in calibration trees from 2024 + clean df
cal_trees_24 <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/calibration_trees_2024.csv")

cal_trees_24_clean <- cal_trees_24 %>% 
  filter(!is.na(focal_tree_n_24)) %>%
  rename(location = site) %>% #to fix pivoting issues
  pivot_longer(cols = starts_with("s"),
               names_to = c("sample_n", ".value"),
               names_pattern = "s(\\d+)_(.*)") %>% 
  rename(site = location) #switching back to naming convention

# cal_trees_24_clean %>% 
#   filter(site == "wade") %>% 
#   mutate(branch = as.character(branch)) %>% 
#   ggplot() + 
#   geom_boxplot(aes(x = branch, y = count/weight)) + 
#   theme_classic()

#load in calibration trees for 2025 + clean df

##for 2025, match the cones/g with the cones/g from the counts (don't yet have separate cone counts for cal trees as with 2024)

cal_trees_25 <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/calibration_trees_2025.csv")

cal_trees_25 <- cal_trees_25 %>% 
  rename(tree = focal_tree_number) %>% 
  group_by(date_collected, site, tree, total_branches, canopy_area, height) %>% 
  summarize(mean_branch_weight = mean(total_weight))

##cone counts for the focal trees 
cone_counts_25 <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/cone processing 25 - counts.csv")

##join with branch count/weight data 
cal_trees_25_clean <- cal_trees_25 %>% 
  mutate(tree = as.character(tree)) %>% 
  left_join(cone_counts_25, by = c("site", "tree")) %>% 
  dplyr::select(-subsample_weight) %>% 
  rename(location = site) %>%   
  pivot_longer(
    cols = starts_with("s"),
    names_to = c("sample_n", ".value"),
    names_pattern = "s(\\d+)_(.*)") %>% 
  rename(site = location)

#calculate cones/g for entire tree based on weight of branch
#weight_dry is calculated by using the wet:dry conversion factor that was calculated in "foliar_moisture" script 

cal_tree_24_cone_per_g <- cal_trees_24_clean %>%
  mutate(weight_dry = weight-(weight*0.431)) %>% 
  mutate(cones_per_g = count / weight_dry) %>%
  group_by(`date collected`, site, `cone density`, focal_tree_n_24, focal_tree_n_25, total_branches, canopy_area, height) %>%
  summarise(
    mean_cones_per_g_branch = mean(cones_per_g * `total branch mass`),
    .groups = "drop"
  ) %>%
  mutate(total_cones_per_g = mean_cones_per_g_branch * total_branches)

cal_tree_25_cone_per_g <- cal_trees_25_clean %>%
  mutate(weight_dry = weight-(weight*0.431)) %>%
  mutate(cones_per_g = count / weight_dry) %>%
  group_by(date_collected.x, site, tree, total_branches, mean_branch_weight, total_mass, canopy_area, height) %>% 
  summarize(mean_cones_per_g_branch = mean(cones_per_g*total_mass),
            .groups = "drop") %>% 
  mutate(total_cones_per_g = mean_cones_per_g_branch * total_branches)

cal_trees_cones_per_g <- cal_tree_24_cone_per_g %>% 
  mutate(height = as.character(height)) %>% 
  bind_rows(cal_tree_25_cone_per_g)

#calculate cones/g for these trees based on allometric equations

##using canopy area allometric eq
# y = ax^b
# ln(y) = ln(a) + b*ln(x)
# y = biomass 
# a and b = estimated constants
# x = size (defined by canopy area)

#according to allometry analysis script...
#a = 1.612571  
#b = 1.06618

cal_trees_cones_per_g_allom <- cal_trees_cones_per_g %>% 
  mutate(biomass_kg = 1.612571*(canopy_area^1.06618))

#bring in the index value, which gives estimates cones/g

rg_index_df <- fread("C:/Users/hmz25/Box/Katz lab/texas/rg_index_df_20250729.csv")
head(rg_index_df)

#calculate rg threshold sum (as in spectral_index_handheld_2025 script), since that's what we use to build linear regression
# #create threshold value so that all values below threshold below that value (not cones) are dropped 
# photo_i_index_rg_thresh <- photo_i_index_rg_dif #plot(photo_i)
# photo_i_index_rg_thresh[photo_i_index_rg_dif[] < 0.05] <- 0
# photo_i_index_rg_thresh[photo_i_index_rg_dif[] > 0.05] <- 1
# photo_i_index_rg_thresh_mean <- cellStats(photo_i_index_rg_thresh, "mean") #calculate  #plot(photo_i_index_rg_thresh)
# photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean* 
#   ((ncell(photo_i_index_rg_thresh) - summary(photo_i_index_rg_thresh)[6]) / ncell(photo_i_index_rg_thresh)) #multiply by portion of cells not NA

# rg_index_sub <- rg_index_df %>% 
#   filter(site %in% "sonora",
#          date %in% "20250115",
#          tree %in% "280")

# rg_index_sub_test <- rg_index_sub %>% 
#   mutate(rg_index_thresh = if_else(rg_index < 0.05, 0, 1),
#          rg_index_thresh_mean = mean(rg_index_thresh),
#          rg_index_thresh_sum = rg_index_thresh_mean*((length(rg_index_thresh) - sum(rg_index_thresh == 0))/length(rg_index_thresh)))

rg_index_thresh_df <- rg_index_df %>% 
  group_by(site, tree, date) %>% 
  mutate(mean_index = mean(rg_index),
         rg_index_thresh = if_else(rg_index < 0.05, 0, 1),
         rg_index_thresh_mean = mean(rg_index_thresh),
         rg_index_thresh_sum = rg_index_thresh_mean*((length(rg_index_thresh) - sum(rg_index_thresh == 0))/length(rg_index_thresh)))

rg_index_year_df <- rg_index_thresh_df %>% 
  mutate(year = substr(as.character(date), 1, 4))

rg_index_2024 <- rg_index_year_df %>% 
  filter(year == 2024)

# rg_index_2024_wade_t5 <- rg_index_2024 %>% 
#   filter(site %in% "wade",
#          tree %in% "5")
# 
# unique(rg_index_2024_wade_t5$date)

#only keeping the correct image from 2024 for wade t5
rg_index_2024 <- rg_index_2024 %>%
  filter(
    if_else(site == "wade" & tree == 5,
            date == 20240103,    # keep only these
            TRUE)                # keep everything else
  )

rg_index_2025 <- rg_index_year_df %>% 
  filter(year == 2025)

# wade_t5_index <- rg_index_2024 %>% 
#   filter(site %in% "wade",
#          tree %in% 5)
  

#join index values with calibration data
head(cal_trees_cones_per_g_allom)

df_2024 <- cal_trees_cones_per_g_allom %>% 
  filter(!is.na(focal_tree_n_24)) %>% 
  dplyr::select_if(~ !all(is.na(.))) %>% 
  dplyr::select(-focal_tree_n_25) %>% 
  rename(tree = focal_tree_n_24) %>% 
  clean_names() %>% 
  mutate(year = substr(as.character(date_collected), 6,9))

# df_2024_index <- rg_index_2024 %>% 
#   group_by(site, tree) %>% 
#   summarize(mean_index = mean(rg_index)) %>% 
#   right_join(df_2024, by = c("site", "tree")) %>% 
#   mutate(tree = as.character(tree))

df_2024_index <- rg_index_2024 %>% 
  group_by(site, tree) %>% 
  summarize(mean_index_thresh_sum = mean(rg_index_thresh_sum)) %>% 
  right_join(df_2024, by = c("site", "tree")) %>% 
  mutate(tree = as.character(tree))

df_2025 <- cal_trees_cones_per_g_allom %>% 
  filter(!is.na(tree)) %>% #get rid of trees from 2024
  select_if(~ !all(is.na(.))) %>% #get rid of unnecessary columns
  rename(date_collected = date_collected.x)

df_tree_cones_combined <- df_2024 %>% 
  mutate(tree = as.character(tree)) %>% 
  bind_rows(df_2025) %>% 
  mutate(site = substr(site, 1, 4))

# df_2025_index <- rg_index_2025 %>% 
#   mutate(tree = as.character(tree)) %>% 
#   group_by(site, tree) %>% 
#   summarize(mean_index = mean(rg_index)) %>% 
#   right_join(df_2025, by = c("site", "tree"))

df_2025_index <- rg_index_2025 %>% 
  mutate(tree = as.character(tree)) %>% 
  group_by(site, tree) %>% 
  summarize(mean_index_thresh_sum = mean(rg_index_thresh_sum)) %>% 
  right_join(df_2025, by = c("site", "tree"))

# test <- cal_trees_cones_per_g_allom %>% 
#   mutate(date = coalesce(`date collected`, date_collected.x),
#          focal_tree_n_25 = as.character(focal_tree_n_25), 
#          focal_tree_n_25 = coalesce(tree, focal_tree_n_25))

df_index_combined <- bind_rows(df_2024_index, df_2025_index) %>% 
  dplyr::select(site, tree, date_collected, mean_index_thresh_sum, total_branches, canopy_area, mean_cones_per_g_branch, total_cones_per_g, biomass_kg) %>% 
  mutate(est_total_cones_per_g = (3.418 + 151.475*mean_index_thresh_sum)*(biomass_kg*1000))

ggplot(df_index_combined) +
  geom_point(aes(x = total_cones_per_g, y = est_total_cones_per_g)) +
  scale_x_continuous(labels = scales::number_format())

ggplot(df_index_combined) +
  geom_label(aes(x = total_cones_per_g, y = est_total_cones_per_g, label = paste0(site,tree))) +
  scale_x_continuous(labels = scales::number_format())

#examining outlier

wade_ortho <- rast("C:/Users/hmz25/Box/Katz lab/texas/orthos/wade_20240103_transparent_mosaic_group1.tif")
plotRGB(wade_ortho)

wade_shp <- st_read("C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis/wade_canopy_seg.shp")

wade_shp <- st_transform(wade_shp, crs=st_crs(wade_ortho))

wade_t5 <- wade_shp[wade_shp$id_number == 9,1]
plot(wade_t5)

plot(wade_t5, add = T, col = "red")

wade_ortho_crop <- crop(wade_ortho, wade_t5)
plotRGB(wade_ortho_crop)

wade_t5_ortho <- mask(wade_ortho_crop, wade_t5)
plotRGB(wade_t5_ortho)

wade_t5_ortho <- wade_t5_ortho[[1:3]]
names(wade_t5_ortho) <- c("r", "g", "b")

wade_t5_ortho_rf <- terra::predict(wade_t5_ortho, rf_mask_ortho)

plot(wade_t5_ortho_rf)

filt <- wade_t5_ortho_rf == 1
wade_t5_ortho_filt <- mask(wade_t5_ortho, filt, maskvalue =1)
plotRGB(wade_t5_ortho_filt)

#calculate index values on filtered canopy, compare with index values in the df above 
wade_t5_ortho_filt$index <- (wade_t5_ortho_filt$r-wade_t5_ortho_filt$g)/(wade_t5_ortho_filt$r + wade_t5_ortho_filt$g)
plot(wade_t5_ortho_filt$index)

wade_t5_df <- as.data.frame(wade_t5_ortho_filt)
mean(wade_t5_df$index)

wade_t5_df <- wade_t5_df %>% 
  mutate(rg_index_thresh = if_else(index < 0.05, 0, 1),
       rg_index_thresh_mean = mean(rg_index_thresh),
       rg_index_thresh_sum = rg_index_thresh_mean*((length(rg_index_thresh) - sum(rg_index_thresh == 0))/length(rg_index_thresh)))

est_cones_wade_t5 <- (3.418 + 151.475*0.00747147)*(81.46096*1000)

#examine other high pcd tree (kimble 5)
kimb_ortho <- rast("C:/Users/hmz25/Box/Katz lab/texas/orthos/kimble_20240110_transparent_mosaic_group1.tif")
plotRGB(kimb_ortho)

#note for HZ -- orthos were so off between years that had to make a different canopy segmentation for this tree viz

kimb_shp <- st_read("C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis/kimb_canopy_seg_2024.shp")

kimb_t5 <- st_transform(kimb_shp, crs=st_crs(kimb_ortho))

# kimb_t5 <- kimb_shp[kimb_shp$id_number == 8,1]
# plot(kimb_t5)

plot(kimb_t5, add = T, col = "red")

kimb_ortho_crop <- crop(kimb_ortho, kimb_t5)
plotRGB(kimb_ortho_crop)

kimb_t5_ortho <- mask(kimb_ortho_crop, kimb_t5)
plotRGB(kimb_t5_ortho)

kimb_t5_ortho <- kimb_t5_ortho[[1:3]]
names(kimb_t5_ortho) <- c("r", "g", "b")

kimb_t5_ortho_rf <- terra::predict(kimb_t5_ortho, rf_mask_ortho)

plot(kimb_t5_ortho_rf)

filt <- kimb_t5_ortho_rf == 1
kimb_t5_ortho_filt <- mask(kimb_t5_ortho, filt, maskvalue =1)
plotRGB(kimb_t5_ortho_filt)

#calculate index values on filtered canopy, compare with index values in the df above 
kimb_t5_ortho_filt$index <- (kimb_t5_ortho_filt$r-kimb_t5_ortho_filt$g)/(kimb_t5_ortho_filt$r + kimb_t5_ortho_filt$g)
plot(kimb_t5_ortho_filt$index)

kimb_t5_df <- as.data.frame(kimb_t5_ortho_filt)
mean(kimb_t5_df$index)

kimb_t5_df <- kimb_t5_df %>% 
  mutate(rg_index_thresh = if_else(index < 0.05, 0, 1),
         rg_index_thresh_mean = mean(rg_index_thresh),
         rg_index_thresh_sum = rg_index_thresh_mean*((length(rg_index_thresh) - sum(rg_index_thresh == 0))/length(rg_index_thresh)))

est_cones_kimb_t5 <- (3.418 + 151.475*0.001158474)*(66.63666*1000)

##getting vastly different #s, so going to do a different way

#loop through each ortho, crop to shape file, filter with rf, extract index values, calculate index (mean + thresh), link with cone density data 

#directory for orthos corresponding to closest dates of calibration tree data collection
cal_ortho_dir <- "C:/Users/hmz25/Box/Katz lab/texas/calibration tree analysis/cal_tree_orthos"
cal_ortho_list <- list.files(cal_ortho_dir, full.names = FALSE)
cal_ortho_list_full <- list.files(cal_ortho_dir, full.names = TRUE)

#director for manually segmented canopies of calibration trees from correct orthos to avoid misalignment 
cal_shp_dir <- "C:/Users/hmz25/Box/Katz lab/texas/calibration tree analysis/cal_tree_shp"
cal_shp_list <- list.files(cal_shp_dir, pattern = ".shp$", full.names = FALSE)
cal_shp_list_full <- list.files(cal_shp_dir, pattern = ".shp$", full.names = TRUE)

cal_trees_index_df <- data.frame()

i = 4

#added orthos, trying new code 
for (i in seq_along(cal_ortho_list)) {
  
  #extract site name and date
  ortho_name <- basename(cal_ortho_list[i])
  site_prefix <- substr(ortho_name, 1, 4)  # first 4 characters
  # tree_num <- str_extract(cal_shp_list[i], "(?<=^[^_]*_[^_]*_[^_]*_)\\d+")
  # site_name <- str_extract(ortho_name, "^[^_]+")
  # flight_date <- str_match(ortho_name, "^[^_]+_([^_]+)_")[,2]
  
  #find matching shapefile (first 4 letters match)
  match_index <- which(substr(cal_shp_list, 1, 4) == site_prefix)
  
  if (length(match_index) == 0) {
    next
  }
  
  shp_path <- cal_shp_list_full[match_index[1]]  # Use first match if multiple
  
  #load ortho and shapefile
  ortho <- rast(cal_ortho_list_full[i]) #plotRGB(ortho)
  
  shp <- st_read(shp_path, quiet = TRUE)
  shp_reproj <- st_transform(shp, crs(ortho)) #plot(shp_reproj, add = T, col = "red")
  
  #crop ortho to canopy
  ortho_crop <- crop(ortho, shp_reproj)
  # plotRGB(ortho_crop)
  
  #filter out pixel values that aren't within the canopy
  ortho_mask <- mask(ortho_crop, shp_reproj)
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
  # ortho_filt$index_thresh <- ifel(ortho_filt$index < 0.05, 0, 1)
  # plot(ortho_filt$index_thresh)
  # global(ortho_filt$index, fun = "mean", na.rm = TRUE)
  # global(ortho_filt$index_thresh, fun = "mean", na.rm = TRUE)
  
  ortho_index_df <- as.data.frame(ortho_filt)
  # mean(ortho_index_df$index)
  
  ortho_index_df <- ortho_index_df %>% 
    mutate(name = ortho_name,
           mean_rg_index = mean(index),
           rg_index_thresh = if_else(index < 0.05, 0, 1), 
           rg_index_thresh_mean = mean(rg_index_thresh),
           rg_index_thresh_sum = rg_index_thresh_mean*((length(rg_index_thresh) - sum(rg_index_thresh == 0))/length(rg_index_thresh)))
  
  cal_trees_index_df <- rbind(cal_trees_index_df, ortho_index_df)
  
  print(i)
  
}

#print df 

cal_trees_index_df_clean <- cal_trees_index_df %>% 
  group_by(name, rg_index_thresh_mean, rg_index_thresh_sum) %>% 
  summarize(mean_index = mean(index)) %>% 
  mutate(site = substr(name, 1, 4))

#join index df with cone/allometry df
final_df <- left_join(df_tree_cones_combined, cal_trees_index_df_clean, by = "site") %>% 
  dplyr::select(-c(cone_density, total_branches, height, year, mean_branch_weight, total_mass)) %>% 
  rename(manual_est_cones = total_cones_per_g) %>% 
  mutate(index_est_cones = (3.418 + 151.475*rg_index_thresh_sum)*(biomass_kg*1000))

plot_limits <- c(min(final_df$manual_est_cones), max(final_df$manual_est_cones))

ggplot(final_df) +
  geom_point(aes(x = manual_est_cones, y = index_est_cones)) + 
  scale_x_continuous(limits = plot_limits) +
  scale_y_continuous(limits = plot_limits) +
  theme_classic()
