#script to generate allometric equations from collaborator data

library(googlesheets4)
library(tidyverse)
library(dplyr)
library(janitor)
library(sf)
# install.packages("lwgeom")
library(lwgeom)

setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")

# Reemts 2013 data --------------------------------------------------------

#df with weights
biomass_df <- read_csv("01_data/Reemts_JuniperusAsheiAllometryData_Weights.csv") %>% 
  clean_names()

biomass_df <- biomass_df %>% 
  mutate(dry_weight_kg = dry_weight*0.001) %>% 
  rename(dry_weight_g = dry_weight)

#df with allometry measurements
allom_measure_df <- read_csv("01_data/Reemts_JuniperusAsheiAllometryData_TreeCharacteristics.csv") %>% 
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

#according to Reemts, no outliers found

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

# ggplot(allometry_crown_biomass_df, aes(x = basal_diameter, y = total_biomass*0.001)) +
#   geom_point() +
#   labs(x = "basal diameter (cm)", y = "biomass (kg)",
#        title = "BD2H allom eq") + 
#   theme_classic()



# Tolleson 2019 data ------------------------------------------------------

# #df with biomass
# tolleson_biomass_df <- read_csv("01_data/Tolleson_biomass.csv") %>% 
#   clean_names()
# 
# 
# tolleson_allometry_df <- read_csv("C:/Users/hmz25/Desktop/Tolleson_allometry.csv") %>% 
#   clean_names()

tolleson_data = "https://docs.google.com/spreadsheets/d/1HLkMgOifuGNixM4AMVN5ItbSdZAwToHq7_xqJu942oQ/edit?gid=42346687#gid=42346687"

tolleson_biomass_df <- read_sheet(tolleson_data, sheet = "weights_combined") %>% 
  clean_names()


tolleson_allometry_df <- read_sheet(tolleson_data, sheet = "allometry") %>% 
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

#extract data for formula string
coefs <- coef(canopy_area_lm)
a <- coefs[1]
b <- coefs[2]

#in the form ln(y) = ln(a) + b*ln(x)

equation <- paste0("ln(biomass) = ln(", round(a, 3), ") + ", round(b, 3), " * ln(canopy_area)")
print(equation)

#back transform equation
a <- exp(coefs[1])  # Back-transform the intercept
b <- coefs[2]       # Slope stays the same

equation <- paste0("biomass = ", round(a, 3), " * canopy_area^", round(b, 3))
print(equation)

equation <- bquote(biomass == .(round(a, 3)) * canopy_area^.(round(b, 3)))
print(equation)

#calculate RMSE
canopy_area_rmse <- sqrt(mean((allometry_analysis_df$log_total_biomass_kg - predict(canopy_area_lm))^2))

#in ln
ggplot(allometry_crown_biomass_df, aes(x = log_canopy_area, y = log_total_biomass_kg)) +
  geom_point() +
  labs(x = "log crown area (m)", y = "log biomass (kg)",
       title = "canopy area") + 
  theme_classic() + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) + #add R-squared and p-value
  annotate("text", x = -1.4, y = 3.60, label = paste0("RMSE = ", round(canopy_area_rmse, 2))) + 
  geom_smooth(method = "lm")

#back transform for better viz
ggplot(allometry_crown_biomass_df, aes(x = exp(log_canopy_area), y = exp(log_total_biomass_kg))) +
  geom_point() +
  labs(x = "canopy area (m)", y = "biomass (kg)",
       title = "allometric equation for J. ashei based on canopy area") + 
  theme_classic() + 
  annotate("text", x = 1.8, y = 70, label = paste0("R² = ", round(summary(canopy_area_lm)$r.squared, 3))) +
  annotate("text", x = 2.5, y = 65, label = paste0("p-val < 0.001 ")) +
  annotate("text", x = 2.3, y = 60, label = paste0("RMSE = ", round(canopy_area_rmse, 2))) + 
  geom_smooth(method = "lm")

ggplot(allometry_crown_biomass_df, aes(x = exp(log_canopy_area), y = exp(log_total_biomass_kg))) +
  geom_point() +
  labs(x = "canopy area (m²)", y = "crown biomass (kg)",
       title = expression(paste("canopy area allometric equation for ", italic("J. ashei"), ""))) + 
  theme_classic() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)) +
  annotate("text", x = 1.8, y = 70, label = paste0("R² = 0.90")) +
  annotate("text", x = 2.5, y = 65, label = paste0("p-val < 0.001 ")) +
  annotate("text", x = 2.3, y = 60, label = paste0("RMSE = ", round(canopy_area_rmse, 2))) + 
  geom_smooth(method = "lm") +
  theme(title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 16))

ggplot(allometry_crown_biomass_df, aes(x = exp(log_canopy_area), y = exp(log_total_biomass_kg))) +
  geom_point() +
  labs(x = "canopy area (m)", y = "biomass (kg)",
       title = "allometric equation for crown biomass ~ canopy area") + 
  theme_classic() + 
  annotate("text", x = 1.5, y = 74, label = paste0("R² = ", round(summary(canopy_area_lm)$r.squared, 2))) +
  annotate("text", x = 2.5, y = 70, label = paste0("p-val < 0.001 ")) +
  annotate("text", x = 2.3, y = 65, label = paste0("RMSE = ", round(canopy_area_rmse, 2))) + 
  annotate("text", x = 0, y = 80, 
           label = paste0("y == ", round(a, 2), " *x^", round(b, 3)),
           parse = TRUE, hjust = 0, vjust = 1) +
  geom_smooth(method = "lm")

canopy_area_allom_eq <- ggplot(allometry_crown_biomass_df, aes(x = exp(log_canopy_area), y = exp(log_total_biomass_kg))) +
  geom_point() +
  labs(x = "canopy area (m)", y = "biomass (kg)",
       title = "allometric equation for crown biomass ~ canopy area") + 
  theme_classic() + 
  annotate("text", x = 1.8, y = 75, 
           label = paste0("y == ", round(a, 2), " *x^", round(b, 3)),
           parse = TRUE, hjust = 0, vjust = 1) +
  annotate("text", x = 1.8, y = 69, label = paste0("R² = ", round(summary(canopy_area_lm)$r.squared, 2)), hjust = 0) +
  annotate("text", x = 1.8, y = 65, label = paste0("p-val < 0.001 "), hjust = 0) +
  annotate("text", x = 1.8, y = 60, label = paste0("RMSE = ", round(canopy_area_rmse, 2)), hjust = 0) + 
  geom_smooth(method = "lm")

ggsave(canopy_area_allom_eq, filename = "03_output/canopy_area_allom_eq.png",
       width = 6, height = 5, dpi = 300)

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


