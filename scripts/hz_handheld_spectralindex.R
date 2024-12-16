
# load libraries ----------------------------------------------------------

library(raster)
library(readr)
#library(tidyverse)
library(terra)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
#library(matrixStats)
#install.packages("randomForest")
library(randomForest)
library(lubridate)
library(ggplot2)
library(janitor)
# install.packages("hms")
library(hms)
#install.packages("ggpubr")
library(ggpubr)


# set working directory ---------------------------------------------------

#set wd
setwd("C:/Users/hmz25")



# manual cone density data ------------------------------------------------

#add in cone density per quadrat data

quadrat_cone_raw <- read_csv("Box/texas/pollen_production/TX jan 24/data analysis/2024_quadratcones.csv")

#add cones per weight columns
quadrat_cones <- quadrat_cone_raw %>%
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
  mutate(s1 = (quadrat_cone_raw$s1_count/quadrat_cone_raw$s1_weight),
         s2 = (quadrat_cone_raw$s2_count/quadrat_cone_raw$s2_weight),
         s3 = (quadrat_cone_raw$s3_count/quadrat_cone_raw$s3_weight),
         s4 = (quadrat_cone_raw$s4_count/quadrat_cone_raw$s4_weight),
         s5 = (quadrat_cone_raw$s5_count/quadrat_cone_raw$s5_weight))  

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

#rename site column to match the site column for the images
quadrat_cones$site <- substr(quadrat_cones$site, 1, 4)



# process drone image files -----------------------------------------------

#create a function to load image files
list.files("Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels") 

image_dir <- "Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels/"
quadrat_files <- list.files(image_dir)#[1]

test <- stack("Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels/kimble_t3_q3.tif")
plotRGB(test)


test2 <- test
test2$kimble_t3_q3_3[test2$kimble_t3_q3_1 > 100 & test2$kimble_t3_q3_2 > 100 & test2$kimble_t3_q3_3 > 100] <- 255
test2$kimble_t3_q3_1[test2$kimble_t3_q3_1/test2$kimble_t3_q3_2 > 1.5] <- 255
plotRGB(test2)

test3 <- test$kimble_t3_q3_1/test$kimble_t3_q3_3
plot(test3)



# rf pixel mask -----------------------------------------------------------


### create a model to mask non foliage and non cones

#training dataset for non-foliage and non-cones
sup_not_twig <- stack("Box/texas/pollen_production/TX jan 24/data analysis/not_2.tiff")
#plotRGB(sup_not_twig) #sup_not_twig$not_2_1 #sup_not_twig$not_2_1[1:100]

sup_not_twig_df <- as.data.frame(sup_not_twig) %>%
  filter(not_2_1 != 255) %>%  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  mutate(class = "not") 
#head(sup_not_twig_df)

#training dataset for foliage and cones
sup_yes_twig <- stack("Box/texas/pollen_production/TX jan 24/data analysis/yes_1.tif")
#plotRGB(sup_yes_twig) #sup_yes_twig$yes_1_1 #sup_yes_twig$yes_1_1[1:100]

sup_yes_twig_df <- as.data.frame(sup_yes_twig) %>%
  filter(yes_1_1 != 255) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  mutate(class = "yes")  #head(sup_yes_twig_df)

#combine training datasets and randomly select pixels from theme
training_df <- bind_rows(sup_not_twig_df, sup_yes_twig_df) %>%
  dplyr::select(-max) %>%
  sample_n(100000) %>%
  mutate(class = as.factor(class))
# head(training_df)
# str(training_df)

#run a pixel-based classifier
set.seed(100)
rf_mask <- randomForest(class ~ ., data = training_df, na.action=na.omit)
rf_mask


### apply model to a single photo
photo_i <- stack("Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels/sonora_t6_q8.tif")
#plotRGB(photo_i)

#make sure layer names for picture match the training df column names 
names(photo_i) <- colnames(training_df)[1:3]
photo_i_mask <- predict(photo_i, rf_mask)
plot(photo_i_mask)

#add mask layer back to original photo
photo_i <- stack(photo_i, photo_i_mask)
plot(photo_i)



# applying rf pixel mask to all handheld drone images ---------------------


### apply model to all cropped handheld photos and save in a new folder
# photo_dir <- "Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels"
# photo_list <- list.files(photo_dir, full.names = FALSE)
# photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)
# 
# 
# for(i in 1:length(photo_list)){
#   photo_i <- stack(photo_list_full_dir[i])  #plotRGB(test)
#   
#   photo_i_df <- as.data.frame(photo_i) %>%
#     rename(c("r" = 1 , "g" = 2, "b" = 3))  #head(test_df)
#   
#   photo_i_mask_df <- predict(rf_mask, photo_i_df)
#   # head(photo_i_mask_df)
#   # photo_i_df <- photo_i_df %>%
#   #   mutate(is_foreground = photo_i_mask_df,
#   #          is_foreground_numeric = case_when( is_foreground == "yes" ~ 1,
#   #                                             is_foreground == "not" ~ 0))
#   # #head(photo_i_df)
#   
#   photo_i <- addLayer(photo_i, photo_i[[3]])
#   photo_i[[4]] <- photo_i_mask_df # photo_i_df$is_forground
#   
#   save_file_name = paste0("Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/",photo_list[i])
#   raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
#   print(i)
# }


# calculate rgb values and an index for each photo ------------------------

photo_dir <- "Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)

photo_masked_df <- data.frame()

for(i in 1:length(photo_list)){
  photo_i <- stack(photo_list_full_dir[i]) #plot(photo_i) #photo_i <- stack(photo_list_full_dir[2])
  
  photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
  photo_i_index_rg_dif <- (photo_i[[1]] - photo_i[[2]])/(photo_i[[1]] + photo_i[[2]]) #calculate index 
  
  photo_i_rg_dif <- cellStats(photo_i_index_rg_dif, "mean") #take mean index value 
  
  #create threshold value so that all values below threshold below that value (not cones) are dropped 
  photo_i_index_rg_thresh <- photo_i_index_rg_dif #plot(photo_i)
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] < 0.05] <- 0
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] > 0.05] <- 1
  photo_i_index_rg_thresh_mean <- cellStats(photo_i_index_rg_thresh, "mean") #calculate  #plot(photo_i_index_rg_thresh)
  photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean* 
    ((ncell(photo_i_index_rg_thresh) - summary(photo_i_index_rg_thresh)[6]) / ncell(photo_i_index_rg_thresh)) #multiply by portion of cells not NA
  #(ncell(photo_i_index_rg_thresh) - ncell(photo_i_index_rg_thresh, na.omit = TRUE)) #plotRGB(photo_i) #ncell(photo_i) #cellStats(photo_i_index_rg_thresh, "mean")
  
  
  photo_i_r <- cellStats(photo_i[[1]], "mean")
  photo_i_g <- cellStats(photo_i[[2]], "mean")
  photo_i_b <- cellStats(photo_i[[3]], "mean")
  photo_i_mask <- cellStats(photo_i[[4]], "mean") - 1 #1 = background, 2 = foreground
  
  photo_i_rgb <- data.frame(photo_list[i], photo_i_r, photo_i_g, photo_i_b, photo_i_mask, photo_i_rg_dif, 
                            photo_i_index_rg_thresh_mean, photo_i_index_rg_thresh_sum)
  photo_masked_df <- bind_rows(photo_masked_df, photo_i_rgb)
  print(i)
}


# combine cone + spectral data frames to analyze index --------------------


#add values to dataframe that has the number of cones per quadrat
photo_masked_df_join <- photo_masked_df %>%
  mutate(site = substr(photo_list.i., 1, 4),
         tree = as.numeric(stringr::str_sub(photo_list.i., -8, -8)),
         quadrat = stringr::str_sub(photo_list.i., -5, -5))

quadrat_cones_rgb <- left_join(quadrat_cones, photo_masked_df_join) %>%
  filter(!is.na(total_cones)) %>%
  filter(!is.na(photo_i_r)) %>% mutate(photo_i_mask = photo_i_mask - 1)

quadrat_cones_rgb_test <- quadrat_cones_rgb %>%  
  filter((photo_i_r + photo_i_g + photo_i_b) > 0)

#quadrat_cones_rgb_test <- quadrat_cones_rgb_test %>% filter(photo_i_mask > 0.5) #applying this filter results in no values 

ggplot(quadrat_cones_rgb_test, aes(x=photo_i_index_rg_thresh_sum, y = total_cones/(0.25*0.25), col = site)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

# quadrat_cones_rgb_test_treeID <- quadrat_cones_rgb_test %>% 
#   mutate(tree_id = paste(site, tree, sep = "_")) %>% 
#   as.data.frame()
# 
# ggplot(quadrat_cones_rgb_test_treeID, aes(x = photo_i_index_rg_thresh_sum, y = total_cones/(0.25*0.25), col = tree_id)) +
#   geom_point(alpha = 0.5) + 
#   theme_bw() + 
#   geom_smooth(method = "lm", se = FALSE) +
#   xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()

fit <- lm(quadrat_cones_rgb_test$total_cones  ~ quadrat_cones_rgb_test$photo_i_index_rg_thresh_sum   )
summary(fit)

#fit$residuals
quadrat_cones_rgb_test  <- quadrat_cones_rgb_test %>%
  mutate(photo_i_rg_dif_resid = fit$residuals,
         photo_i_rg_dif_pred = fit$fitted.values)



cor(quadrat_cones_rgb_test)
quadrat_cones_rgb_test %>% dplyr::select(10:23, - photo_list.i.) %>% 
  PerformanceAnalytics::chart.Correlation(.)

# test <- raster::stack("Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/kimble_t6_q7.tif")
# plot(test)
# 
# test2 <- test
# test2[[1]][test2[[4]] == 1] <- NA
# test2[[2]][test2[[4]] == 1] <- NA
# test2_index_rg_dif <- (test2[[1]] - test2[[2]])/(test2[[1]] + test2[[2]])
# 
# plot(test2_index_rg_dif)


# visualizing how phenology impacts the spectral index --------------------

#are we going to have an issue where we can't analyze how pheno impacts the index bc we don't have repeated handheld pics for focal trees? 

#load in phenology data from field maps
fieldmaps_pheno_csv <- read_csv("Box/texas/pollen_production/TX jan 24/data analysis/fieldmaps_pheno.csv")

pheno_csv <- fieldmaps_pheno_csv %>% 
  dplyr::select(date_and_t, misc, pheno, site, tree_n) %>% 
  dplyr::select(site, tree_n, date_and_t, pheno, everything()) %>% 
  filter(!is.na(site), !is.na(tree_n)) %>% 
  rename(tree = tree_n,
         date = date_and_t) %>% 
  mutate(site = str_replace(site, "Berber", "glimmer"),
         site = tolower(site),
         site = substr(site, 1, 4),
         date = mdy(date)) 

#join phenology data with cone density data
pheno_cone_density_df <- quadrat_cones_rgb_test %>% 
  mutate(tree = as.character(tree)) %>% 
  left_join(pheno_csv, by = c("site", "tree", "date")) %>% 
  dplyr::select(date, site, tree, quadrat, total_cones, photo_i_index_rg_thresh_sum, pheno) %>% 
  separate(pheno, into = c("percent_cones_open", "pollen_release"), sep = "%") 

#create df with binned pheno measurements 
pheno_cone_density_sub <- pheno_cone_density_df %>%
  filter(!is.na(percent_cones_open)) %>%
  mutate(pheno = case_when(percent_cones_open <= 25 ~ "<=25",
                           percent_cones_open > 25 & percent_cones_open <= 50 ~ "25-50",
                           percent_cones_open > 50 & percent_cones_open <= 75 ~ "50-75",
                           percent_cones_open > 75 ~ "> 75"))

#visualize how phenology impacts performance of spectral index 
ggplot(pheno_cone_density_sub, aes(x=photo_i_index_rg_thresh_sum, y = total_cones/(0.25*0.25), col = pheno)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + 
  ylab(cone~density~(cones/m^2)) + 
  ggtitle("index performance based on phenology") +
  ggthemes::theme_few() +
  scale_colour_discrete(limits = c("<=25", "25-50", "> 75"),
                        name = "pheno (percent cones open)",
                        labels = c("<=25", "25-75", "> 75")) + #no focal tree data w cones open btwn 50-75
  stat_cor(aes(label = ..rr.label..))


# visualize cone density vs spectral index based on time of day -----------

#load in time metadata
time_metadata <- read_csv("Box/texas/pollen_production/TX jan 24/data analysis/2024 TX drone pics time metadata.csv")
# time_metadata

time_metadata <- time_metadata %>%
  clean_names() %>%
  dplyr::select(site, date, time, tree) %>%
  filter(!is.na(tree)) %>%
  mutate(site = substring(site, 1, 4),
         date = mdy(date))

#join time metadata w cone density data 
tod_cone_density_df <- quadrat_cones_rgb_test %>%  
  left_join(time_metadata, by = c("site", "date", "tree")) %>% 
  dplyr::select(date, site, tree, quadrat, total_cones, photo_i_index_rg_thresh_sum, time) 

#bin time of day by proximity to solar noon (which is ~12:30 in TX in jan)
solar_noon_cone_df <- tod_cone_density_df %>% 
  mutate(solar_noon = case_when(
    time <= as_hms("10:30:00") ~ "> 2 hrs before",
    time > as_hms("10:30:00") & time <= as_hms("14:30:00") ~ "within 2 hrs",
    time > as_hms("14:30:00") ~ "> 2 hrs after"))

#visualize how time of day impacts spectral index performance
ggplot(solar_noon_cone_df, aes(x=photo_i_index_rg_thresh_sum, y = total_cones/(0.25*0.25), col = solar_noon)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + 
  ylab(cone~density~(cones/m^2)) + 
  ggtitle("index performance at different times of day") +
  ggthemes::theme_few() +
  scale_colour_discrete(name = "time to solar noon") +
  stat_cor(aes(label = ..rr.label..))


# visualize how sky conditions impact performance of spectral index -------

#load in data from RTMA NOAA data (retrieved from google earth engine, downloaded to CSV) 
cloud_cover_df <- read_csv("Box/texas/pollen_production/TX jan 24/data analysis/TX_cloud_cover_2024.csv")

cloud_cover_hr <- cloud_cover_df %>% 
  group_by(site, date) %>%  
  mutate(time = as_hms((row_number() - 1) * 3600)) %>%  #convert to hours from midnight
  ungroup() %>% 
  dplyr::select(cloud_cover, site, date, time) %>% 
  mutate(site = substr(site, 1, 4))

# library(fuzzyjoin)

# test_df <- tod_cone_density_df %>%
#   fuzzy_left_join(
#     cloud_cover_hr,
#     by = c("site", "date", "time"),
#     match_fun = list(`==`, `==`, ~ abs(as.numeric(.x - .y)) <= 500)
#   )

# tod_cone_density_df_rounded <- tod_cone_density_df %>% 
#   mutate(rounded_time = round_date(as.POSIXct(time, format = "%H:%M:%S"), unit = "hour"))

tod_cone_density_df_rounded <- tod_cone_density_df %>% 
  mutate(rounded_time = as_hms(round(as.numeric(time) / 3600) * 3600)) %>% #divide by 3600 (seconds per hour), round the result, and multiply back by 3600 to get seconds rounded to the nearest hour.
  dplyr::select(-time) %>% 
  rename(time = rounded_time)

cloud_cover_cone_density <- tod_cone_density_df_rounded %>% 
  left_join(cloud_cover_hr, by = c("site", "date", "time")) %>% 
  mutate(percent_cloud = case_when(
    cloud_cover <= 25 ~ "< 25%",
    cloud_cover > 25 & cloud_cover <= 50 ~ "25-50%",
    cloud_cover > 50 & cloud_cover <= 75 ~ "50-75%",
    cloud_cover > 75 ~ "> 75%"))

#visualize how cloud cover impacts spectral index
ggplot(cloud_cover_cone_density, aes(x=photo_i_index_rg_thresh_sum, y = total_cones/(0.25*0.25), col = percent_cloud)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + 
  ylab(cone~density~(cones/m^2)) + 
  ggtitle("index performance under different cloud cover") +
  ggthemes::theme_few() +
  scale_colour_discrete(limits = c("< 25%", "25-50%", "50-75%", "> 75%"),
                        name = "percent cloud cover") +
  stat_cor(aes(label = ..rr.label..))

install.packages("ggpubr")
library(ggpubr)


# see how index fit changes if using weight instead of area of qua --------
ggplot(quadrat_cones_rgb_test, aes(x=photo_i_index_rg_thresh_sum, y = total_cones/total_mass)) + #, col = site))  
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + ggthemes::theme_few() +
  stat_cor(aes(label = ..rr.label..))


