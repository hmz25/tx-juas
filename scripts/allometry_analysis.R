library(lidR)
library(terra)
library(googlesheets4)
library(dplyr)
library(janitor)
library(sf)
# install.packages("lwgeom")
library(lwgeom)

# las <- readLAS("C:/Users/hmz25/Desktop/orbital_las/sonora_t1_orbital_2025_group1_densified_point_cloud.las")
# las <- readLAS("C:/Users/hmz25/Desktop/orbital_las/fisher_t1_orbital_2025_group1_densified_point_cloud.las")
# las <- readLAS("C:/Users/hmz25/Desktop/orbital_las/fisher_t2_orbital_2025_group1_densified_point_cloud.las")
# las <- readLAS("C:/Users/hmz25/Desktop/orbital_las/fisher_t7_orbital_2025_group1_densified_point_cloud.las")
las <- readLAS("C:/Users/hmz25/Desktop/orbital_las/wade_t3_orbital_2025_group1_densified_point_cloud.las")

plot(las)

# install.packages("lidRviewer")
# library(lidRviewer)
# view_cloud(las)

# ?rasterize_terrain
# rasterize_terrain(las)
# ?classify_ground
# install.packages("RCSF")

#classify ground points (using off the shelf algorithm suggested in help)
test_csf <- csf(TRUE, 1, 1, time_step = 1)
test_ground <- classify_ground(las, test_csf)

plot(test_ground, color = "Classification")

ground <- rasterize_terrain(test_ground, res = 0.1)

plot(ground)

surface <- rasterize_canopy(test_ground, res = 0.1)
plot(surface)

writeRaster(surface, "wade_t3_lidar_rast.tiff", overwrite = T)

canopy_height <- surface-ground

plot(canopy_height)

# writeRaster(canopy_height, "wade_t3_lidar_rast.tiff")

max(canopy_height)

# quantile(canopy_height, probs = c(0.95), na.rm = TRUE)
quantile(canopy_height[canopy_height>1], probs = seq(0,1, by = 0.02), na.rm = TRUE)

#watershed segmentation for canopy area?
# install.packages("BiocManager")
# BiocManager::install("EBImage")

chm <- rasterize_canopy(las, res = 0.5, p2r(0.3), pkg = "raster")
plot(chm)
# str(chm)

# crowns <- EBImage::watershed(chm)()
test_segment <- segment_trees(las, EBImage::watershed(chm))
test_segment <- segment_trees(las, lidR::watershed(chm, th_tree = 2))
plot(test_segment)

crowns <- lidR::watershed(chm)()
plot(crowns)

test_segment <- segment_trees(surface, lidR::watershed(chm, th_tree = 2))

gs4_auth()

allometry_csv <- read_sheet("https://docs.google.com/spreadsheets/d/1vUpeKFIuQkiJlMMKNm4MMqOa2ySK5lp1udSLZhnAlQg/edit?gid=0#gid=0")

allometry_csv_clean <- allometry_csv %>% 
  separate_longer_delim(cols = dbh_above_5cm, delim = ",") %>% 
  mutate(dbh_above_5cm = as.numeric(dbh_above_5cm)) %>% 
  group_by(site, tree, height_nikon, height_las, canopy_area_ortho, canopy_area_las, stems_above_5cm, dbh_below_5cm, stems_below_5cm) %>% 
  summarize(mean_dbh_above = mean(dbh_above_5cm)) %>% 
  separate_longer_delim(cols = dbh_below_5cm, delim = ",") %>%
  mutate(dbh_below_5cm = as.numeric(dbh_below_5cm)) %>% 
  group_by(site, tree, height_nikon, height_las, canopy_area_ortho, canopy_area_las, stems_above_5cm, stems_below_5cm, mean_dbh_above) %>%
  summarize(mean_dbh_below = mean(dbh_below_5cm)) %>% 
  group_by(site, tree, height_nikon, height_las, canopy_area_ortho, canopy_area_las) %>% 
  summarize(total_dbh_above = mean_dbh_above*stems_above_5cm,
            total_dbh_below = mean_dbh_below*stems_below_5cm) %>% 
  mutate(total_dbh = total_dbh_above + total_dbh_below) %>% 
  select(-canopy_area_las) %>% 
  filter(!is.na(height_las))
 
 #below code is for ba, not bd
  # #convert cm dbh measurements to inches
  # mutate(total_dbh_above = total_dbh_above*0.393701,
  #        total_dbh_below = total_dbh_below*0.393701) %>% 
  # #convert dbh to basal area (BA (sq. ft.) = .005454 x [DBH (in.)]^2)
  # mutate(ba_above = (total_dbh_above)^2*0.005454,
  #        ba_below = (total_dbh_below)^2*0.005454) %>% 
  # mutate(total_ba = (ba_above + ba_below))

biomass_estimates <- allometry_csv_clean %>% 
  #from Reemts et al., 2013... ln(y) = ln(a) + bln(x)
  #y = biomass, x = size (BD^2H)
  #a and b = estimated constants, a = 0.086, b = 0.891
  # these are constants for total biomass, but can estimate for just 1-hr fuel 
  mutate(biomass = (log(0.086) + 0.891*log(total_dbh^2*height_nikon))) %>% 
  mutate(biomass = exp(biomass))


# Reemts 2013 data --------------------------------------------------------

#df with weights
biomass_df <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/Reemts_JuniperusAsheiAllometryData_Weights.csv") %>% 
  clean_names()

biomass_df <- biomass_df %>% 
  mutate(dry_weight_kg = dry_weight*0.001) %>% 
  rename(dry_weight_g = dry_weight)

#df with allometry measurements
allom_measure_df <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/Reemts_JuniperusAsheiAllometryData_TreeCharacteristics.csv") %>% 
  clean_names()

#clean data
allom_measure_df <- allom_measure_df %>% 
  #convert from cm to m
  mutate(crown_widest_cm = crown_widest_cm*0.01,
         crown_perpendicular_cm = crown_perpendicular_cm*0.01) %>% 
  #rename diameter at root collar to "basal diameter" from literature 
  rename(basal_diameter = diam_root_collar_cm) %>% 
  #calculate area + volume based on equations in reemts 
  mutate(canopy_area = (pi*(crown_widest_cm/2)*(crown_perpendicular_cm/2)), 
         canopy_volume = (4/3)*pi*(crown_widest_cm/2)*(crown_perpendicular_cm/2)*(tree_height_m/2)) %>% 
  #convert tree height in m to cm
  mutate(tree_height_cm = tree_height_m*100,
         #calculate BD^2H 
         bd2h = (basal_diameter^2)*tree_height_m) 

#check to make sure everything matches literature
range(allom_measure_df$basal_diameter)
range(allom_measure_df$tree_height_m)

# #filter dataset as done in methods?
# # analyses focused on trees <15 cm in basal diameter, <2.9 m tall, <7 m2 in  canopy area, and <10 m3 in volume (Table 1)
# # all trees were included in analyses using BD2H
# 
# allom_measure_df_filt <- allom_measure_df %>%
#   filter(tree_height_m < 2.9,
#          basal_diameter < 15,
#          canopy_area < 7,
#          canopy_volume < 10)
# 
# nrow(allom_measure_df_filt)
# 
# #according to table 1 in paper, different Ns were used for each analysis...
# #all 33 trees were used for BD2H so focus on that for replication 

# log transform data
# "all data were log-transformed prior to analysis to stabilize variances"
allom_measure_log_df <- allom_measure_df %>%
  mutate(log_canopy_area = log(canopy_area),
         log_canopy_volume = log(canopy_volume),
         log_height_m = log(tree_height_m),
         log_height_cm = log(tree_height_cm),
         log_basal_diameter = log(basal_diameter),
         log_bd2h = log(bd2h))

#combine dfs
allometry_combined_df <- biomass_df %>% 
  left_join(allom_measure_log_df, by = "tree_id")

#according to REEMTS, no outliers found

#trying to reproduce results
allometry_analysis_df <- allometry_combined_df %>% 
  group_by(tree_id, basal_diameter, tree_height_m, tree_height_cm, 
           canopy_area, canopy_volume, bd2h, log_basal_diameter, log_height_m, 
           log_height_cm, log_canopy_area, log_canopy_volume, log_bd2h) %>% 
  summarize(total_biomass_g = sum(dry_weight_g),
            total_biomass_kg = sum(dry_weight_kg)) %>% 
  #take log of biomass after summing 
  mutate(log_total_biomass_g = log(total_biomass_g),
         log_total_biomass_kg = log(total_biomass_kg))

# ln(y) = ln(a) + b*ln(x)
# y = biomass
# x = size (defined by BD, H, canopy area, etc)
# a and b = estimated constants


BD2H_lm <- lm(log_total_biomass_kg ~ log_bd2h,
              data = allometry_analysis_df)

summary(BD2H_lm)

#calculate RMSE
BD2H_RMSE <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(BD2H_lm))^2))
print(BD2H_RMSE)

# y = ax^b
# y = biomass 
# a and b = estimated constants
# x = size (defined by BD^2 + H)


# ggplot(allometry_crown_biomass_df, aes(x = log_basal_diameter, y = log_total_biomass)) +
#   geom_point() +
#   labs(x = "log basal diameter (cm)", y = "log biomass (kg)",
#        title = "BD2H allom eq") + 
#   theme_classic()

ggplot(allometry_crown_biomass_df, aes(x = basal_diameter, y = total_biomass*0.001)) +
  geom_point() +
  labs(x = "basal diameter (cm)", y = "biomass (kg)",
       title = "BD2H allom eq") + 
  theme_classic()



# Tolleson 2019 data ------------------------------------------------------

#df with biomass
tolleson_biomass_df <- read_csv("C:/Users/hmz25/Desktop/Tolleson_biomass.csv") %>% 
  clean_names()


tolleson_allometry_df <- read_csv("C:/Users/hmz25/Desktop/Tolleson_allometry.csv") %>% 
  clean_names()

tolleson_join_df <- left_join(tolleson_allometry_df, tolleson_biomass_df, by = "tree_id")

##need to figure out how to merge rows?

#clean df + log transform data + add identifier column for combining data 
tolleson_allometry_df <- tolleson_join_df %>% 
  #match col names with Reemts data
  rename(bd2h = basal_diameter2_x_height_cm3,
         dry_weight_kg = grand_total_kg,
         canopy_area = canopy_area_m2,
         canopy_volume =canopy_volume_m3,
         tree_height_m = height_m) %>% 
  mutate(dry_weight_g = dry_weight_kg*1000,
         tree_height_cm = tree_height_m*100,
         log_basal_diameter = log(basal_diameter_cm),
         log_canopy_area = log(canopy_area),
         log_canopy_volume = log(canopy_volume),
         log_height_m = log(tree_height_m),
         log_height_cm = log(tree_height_cm),
         log_basal_diameter = log(basal_diameter_cm),
         log_bd2h = log(bd2h))

tolleson_crown_df <- tolleson_allometry_df %>% 
  dplyr::select(tree_id:x1_hr_fuels_kg, dry_weight_kg:log_bd2h) %>% 
  mutate(total_biomass_g = x1_hr_fuels_kg*100) %>% 
  dplyr::select(-c(total_weight_kg, dry_weight_kg, dry_weight_g, height_category, widest_canopy_diameter_m)) %>% 
  rename(total_biomass_kg = x1_hr_fuels_kg,
         basal_diameter = basal_diameter_cm) %>% 
  mutate(log_total_biomass_g = log(total_biomass_g),
         log_total_biomass_kg = log(total_biomass_kg),
         data_source = "tolleson")
  

# fitting my own allometric eqs based on different variables  ------------

#different fuel class definitions
## 1hrL = living twigs, leaves, and cones <6 mm in diameter
## 1hrD = dead twigs, leaves, and cones <6 mm in diameter
## 10hrL = living branches 6–25 mm in diameter
## 10hrD = dead branches 6–25 mm in diameter
## 100hrL = living branches >25 mm in diameter
## 100hrD = dead branches >25 mm in diameter

allometry_crown_biomass_df <- allometry_combined_df %>% 
  filter(category %in% c("1hrL", "1hrD")) %>% 
  group_by(tree_id, basal_diameter, tree_height_m, tree_height_cm, 
           canopy_area, canopy_volume, bd2h, log_basal_diameter, log_height_m, 
           log_height_cm, log_canopy_area, log_canopy_volume, log_bd2h) %>% 
  summarize(total_biomass_g = sum(dry_weight_g),
            total_biomass_kg = sum(dry_weight_kg)) %>% 
  #take log of biomass after summing 
  mutate(log_total_biomass_g = log(total_biomass_g),
         log_total_biomass_kg = log(total_biomass_kg),
         data_source = "reemts")

#combine tolleson and reemts data
allometry_crown_biomass_df <- allometry_crown_biomass_df %>% 
  bind_rows(tolleson_crown_df)

# #separate analysis, figure out wet:dry conversion
# percent_moisture_df <- allometry_combined_df %>% 
#   filter(category %in% "1hrL") %>% 
#   mutate(weight_retained = dry_weight_g/initial_weight,
#          weight_lost = 1-weight_retained)
# 
# hist(percent_moisture_df$weight_lost)
# 
# percent_moisture_df <- allometry_combined_df %>% 
#   filter(category %in% "1hrL") %>% 
#   mutate(perc_water = ((initial_weight - dry_weight_g)/initial_weight)*100) %>% 
#   ggplot() +
#   geom_boxplot(x = tree_id, y = perc_water)
# 
# ggplot(percent_moisture_df) +
#   geom_boxplot(aes(x = tree_id, y = perc_water)) +
#   theme_classic() + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#fit linear models

# #test to fit model from 1hr fuel in paper
# BD2H_1hr_lm <- lm(log_total_biomass_kg ~ log_bd2h,
#                   data = allometry_crown_biomass_df)
# 
# summary(BD2H_1hr_lm)
# #very close

canopy_area_lm <- lm(log_total_biomass_kg ~ log_canopy_area,
                    data = allometry_crown_biomass_df)

summary(canopy_area_lm)

#calculate RMSE
canopy_area_rmse <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(canopy_area_lm))^2))

ggplot(allometry_crown_biomass_df, aes(x = log_canopy_area, y = log_total_biomass_kg)) +
  geom_point() +
  labs(x = "log crown area (m)", y = "log biomass (kg)",
       title = "canopy area") + 
  theme_classic() + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + #add R-squared and p-value
  annotate("text", x = -1.4, y = 3.60, label = paste0("RMSE = ", round(canopy_area_rmse, 2))) + 
  geom_smooth(method = "lm")

ggplot(allometry_crown_biomass_df, aes(x = exp(log_canopy_area), y = exp(log_total_biomass_kg))) +
  geom_point() +
  labs(x = "crown area (m)", y = "biomass (kg)",
       title = "canopy area") + 
  theme_classic() + 
  annotate("text", x = 1.8, y = 75, label = paste0("R² = ", round(summary(canopy_area_lm)$r.squared, 2))) +
  annotate("text", x = 2.5, y = 70, label = paste0("p-val < 0.001 ")) +
  annotate("text", x = 2.3, y = 65, label = paste0("RMSE = ", round(canopy_area_rmse, 2))) + 
  geom_smooth(method = "lm")


# #p-val
# summary(canopy_area_lm)$coefficients[2,4]

  
#canopy area and height
allometry_crown_biomass_df <- allometry_crown_biomass_df %>% 
  #create new term with combinations of canopy area and height
  mutate(canopy_area_add_height = canopy_area+tree_height_m,
         canopy_area_multiply_height = canopy_area*tree_height_m) %>% 
  #take log of combinations of canopy area and height
  mutate(log_canopy_area_add_height = log(canopy_area_add_height),
         log_canopy_area_multiply_height = log(canopy_area_multiply_height))

#canopy area + height
canopy_area_add_height_lm <- lm(log_total_biomass_kg ~ log_canopy_area_add_height,
                    data = allometry_crown_biomass_df)

summary(canopy_area_add_height_lm)

#calculate RMSE
canopy_area_height_rmse <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(canopy_area_add_height_lm))^2))


ggplot(allometry_crown_biomass_df, aes(x = log_canopy_area_add_height, y = log_total_biomass_kg)) +
  geom_point() +
  labs(x = "canopy area + height", y = "log biomass (kg)",
       title = "canopy area + height") + 
  theme_classic() + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + #add R-squared and p-value
  annotate("text", x = 0.18, y = 3.60, label = paste0("RMSE = ", round(canopy_area_height_rmse, 2))) + 
  geom_smooth(method = "lm")

ggplot(allometry_crown_biomass_df, aes(x = exp(log_canopy_area_add_height), y = exp(log_total_biomass_kg))) +
  geom_point() +
  labs(x = "crown area + height", y = "biomass (kg)",
       title = "canopy area + height") + 
  theme_classic() + 
  annotate("text", x = 1.8, y = 75, label = paste0("R² = ", round(summary(canopy_area_add_height_lm)$r.squared, 2))) +
  annotate("text", x = 3, y = 70, label = paste0("p-val < 0.001 ")) +
  annotate("text", x = 3, y = 65, label = paste0("RMSE = ", round(canopy_area_height_rmse, 2))) + 
  geom_smooth(method = "lm")

#canopy area * height 
#not as good as add height

canopy_area_multiply_height_lm <- lm(log_total_biomass_kg ~ log_canopy_area_multiply_height,
                                data = allometry_crown_biomass_df)

summary(canopy_area_multiply_height_lm)

#calculate RMSE
canopy_area_times_height_rmse <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(canopy_area_multiply_height_lm))^2))

ggplot(allometry_crown_biomass_df, aes(x = log_canopy_area_multiply_height, y = log_total_biomass_kg)) +
  geom_point() +
  labs(x = "canopy area * height", y = "log biomass (kg)",
       title = "canopy area * height") +
  theme_classic() +   
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + #add R-squared and p-value
  annotate("text", x = -1.5, y = 3.60, label = paste0("RMSE = ", round(canopy_area_times_height_rmse, 2))) + 
  geom_smooth(method = "lm")

#crown volume
crown_volume_lm <- lm(log_total_biomass_kg ~ log_canopy_volume,
                             data = allometry_crown_biomass_df)

summary(crown_volume_lm)

#calculate rmse
canopy_volume_rmse <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(crown_volume_lm))^2))


ggplot(allometry_crown_biomass_df, aes(x = log_canopy_volume, y = log_total_biomass_kg)) +
  geom_point() +
  labs(x = "log crown volume (m3)", y = "log biomass (kg)",
       title = "volume") + 
  theme_classic() + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + #add R-squared and p-value
  annotate("text", x = -2, y = 3.60, label = paste0("RMSE = ", round(canopy_volume_rmse, 2))) + 
  geom_smooth(method = "lm")

#crown volume + height
allometry_crown_biomass_df <- allometry_crown_biomass_df %>% 
  #create new term with combinations of canopy volume and height
  mutate(canopy_vol_add_height = canopy_volume+tree_height_m,
         canopy_vol_multiply_height = canopy_volume*tree_height_m) %>% 
  #take log of combinations of canopy volume and height
  mutate(log_canopy_vol_add_height = log(canopy_vol_add_height),
         log_canopy_vol_multiply_height = log(canopy_vol_multiply_height))

#crown volume + height
crown_volume_add_height_lm <- lm(log_total_biomass_kg ~ log_canopy_vol_add_height,
                           data = allometry_crown_biomass_df)

summary(crown_volume_add_height_lm)

#calculate rmse
canopy_volume_add_height_rmse <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(crown_volume_add_height_lm))^2))

ggplot(allometry_crown_biomass_df, aes(x = log_canopy_vol_add_height, y = log_total_biomass_kg)) +
  geom_point() +
  labs(x = "log crown volume + height", y = "log biomass (kg)",
       title = "volume + height") + 
  theme_classic() + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + #add R-squared and p-value
  annotate("text", x = 0.1, y = 3.60, label = paste0("RMSE = ", round(canopy_volume_add_height_rmse, 2))) + 
  geom_smooth(method = "lm")

#canopy area * volume 
crown_volume_multiply_height_lm <- lm(log_total_biomass_kg ~ log_canopy_vol_multiply_height,
                                      data = allometry_crown_biomass_df)

summary(crown_volume_multiply_height_lm)

#calculate rmse
canopy_volume_multiply_height_rmse <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(crown_volume_multiply_height_lm))^2))

ggplot(allometry_crown_biomass_df, aes(x = log_canopy_vol_multiply_height, y = log_total_biomass_kg)) +
  geom_point() +
  labs(x = "log crown volume * height", y = "log biomass (kg)",
       title = "volume * height") + 
  theme_classic() + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + #add R-squared and p-value
  annotate("text", x = -2.2, y = 3.60, label = paste0("RMSE = ", round(canopy_volume_multiply_height_rmse, 2))) + 
  geom_smooth(method = "lm")


# applying allometric equations to focal trees ----------------------------

#calculate canopy biomass (1 hr fuel load, kg) for focal trees
#using canopy area 
## from model summary, intercept = 0.48, log_canopy_area = 1.066
## a = exp(0.47783) = 1.613
## b = 1.066
## y = ax^b 
## biomass = 1.613*canopy area^1.066

#extract canopy area for focal trees 

#load in canopy segmentation files
canopy_seg_dir <- "C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis"
canopy_seg_files <- list.files(canopy_seg_dir, pattern = "_canopy_seg_fixed.shp")
canopy_seg_files_full <- list.files(canopy_seg_dir, pattern = "_canopy_seg_fixed.shp", full.names = T)

all_canopy_seg <- canopy_seg_files_full %>%
  lapply(st_read) %>%
  do.call(rbind, .)

st_is_valid(all_canopy_seg)
st_is_valid(all_canopy_seg, reason = TRUE)


#load in focal tree file
focal_trees_shp <- st_read("C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis/focal_trees_2025_shp.shp")
# plot(focal_trees_shp[1])

focal_trees_geom <- focal_trees_shp$geometry

#check CRS alignment
st_crs(all_canopy_seg)
st_crs(focal_trees_geom)

#create a buffer around focal trees to account for fieldmaps points not always intersecting exactly with canopy 
focal_trees_buff <- st_buffer(focal_trees_geom, dist = 0.85)

#keep only canopy polygons that intersect the 3m buffer
# canopy_focal <- st_filter(all_canopy_seg, focal_trees_geom, .predicate = st_intersects)
# plot(canopy_focal)
# count(canopy_focal)

canopy_focal <- st_filter(all_canopy_seg, focal_trees_buff, .predicate = st_intersects)
count(canopy_focal) #correct number

canopy_focal$canopy_area <- st_area(canopy_focal$geometry)

# test_join <- st_join(focal_trees_shp, all_canopy_seg)

test_join <- st_join(canopy_focal, focal_trees_shp)


canopy_area_df <- test_join$canopy_area %>% 
  as.data.frame() %>% 
  rename(canopy_area = ".") %>% 
  mutate(canopy_area = str_extract(canopy_area, "^[^ ]+"),
         data_source = "my data",
         canopy_area = as.double(canopy_area))

# allom_measure_df <- allom_measure_df %>% 
#   mutate(data_source = "reemts data")

combined_canopy_area_df <- bind_rows(allometry_crown_biomass_df, canopy_area_df)

ggplot(combined_canopy_area_df) +
  geom_point(aes(x = canopy_area, y = canopy_area, col = data_source), alpha = 0.6) + 
  theme_classic()

ggplot(combined_canopy_area_df) +
  geom_point(aes(x = canopy_area, y = data_source)) + 
  theme_classic()


