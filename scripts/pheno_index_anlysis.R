##script to analyze index as a function of cone phenology
##using close-up iphone images of cones to see how index values change at the same sites at different dates
##images have been white balanced

library(terra)
library(dplyr)
library(tidyverse)
library(randomForest)
library(raster)

# #load in filtered, wb, handheld drone pics
# photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/"
# photo_list <- list.files(photo_dir, full.names = FALSE)
# photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)
# 
# setwd("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/")
# 
# pic <- rast("sono_t1.tif")
# plot(pic)
# plotRGB(pic)
# 
# #read exif data to see the date the pics were taken? not sure if possible with exif data in the pics 
# 
# #visualize how index changes on them over time


#using iphone pics 

photo_dir <- "C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/white_balanced"
list.files(photo_dir)

fish_jan10 <- rast("C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/white_balanced/fisher_t10_20250110.tif")
plotRGB(fish_jan10)

fish_jan3 <- rast("C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/white_balanced/fisher_t6_20250103.tif")
plotRGB(fish_jan3)

#apply index to images

names(fish_jan10) <- c("r", "g", "b")
fish_jan10$index <- (fish_jan10$r-fish_jan10$g)/(fish_jan10$r+fish_jan10$g)
plot(fish_jan10$index)
terra::plot(fish_jan10$index, main = "fish jan 10")

names(fish_jan3) <- c("r", "g", "b")
fish_jan3$index <- (fish_jan3$r-fish_jan3$g)/(fish_jan3$r+fish_jan3$g)
plot(fish_jan3$index)
terra::plot(fish_jan3$index, main = "fish jan 3")

#classifier for fol vs not fol (from spectral index script)
iphone_rf_prediction <- terra::predict(fish_jan10, rf_mask)
plot(iphone_rf_prediction)

filter <- iphone_rf_prediction == 1
filtered_img <- mask(fish_jan10, filter, maskvalue=1)

plotRGB(filtered_img)
plot(filtered_img$index)
plot(filtered_img$index, range = c(0, 0.7))


#classifier for just cones 

#training dataset for cones
rf_iphone_cones <- rast("C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/yes_cone.tif")
plotRGB(rf_iphone_cones)

cones_df <- as.data.frame(rf_iphone_cones) %>% 
  rename(c("r" = 1 , "g" = 2, "b" = 3)) %>% 
  filter(r != 255) %>% #not sure if we need this?
  mutate(class = "yes") 

head(cones_df)

#training dataset for not cones
rf_iphone_not_cones <- rast("C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/not_cone.tif")
plotRGB(rf_iphone_not_cones)

not_cones_df <- as.data.frame(rf_iphone_not_cones) %>% 
  rename(c("r" = 1 , "g" = 2, "b" = 3)) %>% 
  filter(r != 255) %>% #not sure if we need this? 
  mutate(class = "not")

#combine training datasets and randomly select pixels from them
training_df_iphone <- bind_rows(cones_df, not_cones_df) %>%
  sample_n(100000, replace = TRUE) %>%
  mutate(class = as.factor(class))
# head(training_df_iphone)
# str(training_df_iphone)

#run a pixel-based classifier
set.seed(100)
iphone_rf_mask <- randomForest(class ~ ., data = training_df_iphone, na.action=na.omit)
iphone_rf_mask

#apply model to a single photo
plotRGB(fish_jan10)
fish_jan10_mask <- predict(fish_jan10, iphone_rf_mask)
plot(fish_jan10_mask)

#add mask layer back to original photo
fish_jan10 <- c(fish_jan10, fish_jan10_mask)
plot(fish_jan10)
# plot(fish_jan10$class) 

#visualize
iphone_rf_prediction <- terra::predict(fish_jan10, iphone_rf_mask)
plot(iphone_rf_prediction)

filter <- iphone_rf_prediction == 1
filtered_img <- mask(fish_jan10, filter, maskvalue=1)

plotRGB(filtered_img)
plot(filtered_img$index)
plot(filtered_img$index, range = c(0.1, 0.5))

iphone_rf_prediction <- terra::predict(fish_jan3, iphone_rf_mask)
plot(iphone_rf_prediction)

filter <- iphone_rf_prediction == 1
filtered_img <- mask(fish_jan3, filter, maskvalue=1)

plotRGB(filtered_img)
plot(filtered_img$index)
plot(filtered_img$index, range = c(0.1, 0.5))

#run some stats on it 

# # applying rf pixel mask to all iphone images and saving in new folder -------------------------------------------------

photo_dir <- "C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/white_balanced"
photo_list <- list.files(photo_dir, pattern = ".tif", full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, pattern = ".tif", full.names = TRUE)

# i = 7

for(i in 1:length(photo_list)){
  photo_i <- raster::stack(photo_list_full_dir[i])  #plotRGB(photo_i)

  photo_i_df <- as.data.frame(photo_i) %>%
    rename(c("r" = 1 , "g" = 2, "b" = 3))   #head(photo_i_df)

  photo_i_mask_df <- predict(iphone_rf_mask, photo_i_df)
  # head(photo_i_mask_df)

  photo_i <- addLayer(photo_i, photo_i[[3]])
  photo_i[[4]] <- photo_i_mask_df 
  # plot(photo_i)
  # plotRGB(photo_i)
  # plot(photo_i[[4]])

  save_file_name = paste0("C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/white_balanced/masked_iphone_pics/",photo_list[i])
  raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
  print(i)
}

test_iphone_pheno <- rast("C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/white_balanced/masked_iphone_pics/cathedral_t9_20250110.tif")
plotRGB(test_iphone_pheno)
plot(test_iphone_pheno)

# apply index to all masked iphone photos --------------------------------------

photo_dir <- "C:/Users/hmz25/Box/Katz lab/texas/iphone_pheno_test/white_balanced/masked_iphone_pics/"
photo_list <- list.files(photo_dir, pattern = ".tif", full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, pattern = ".tif", full.names = TRUE)

photo_masked_iphone_df <- data.frame()


for(i in 1:length(photo_list)){
  photo_i <- stack(photo_list_full_dir[i]) #plot(photo_i) 
  
  photo_i_df <- as.data.frame(photo_i) %>% 
    rename(r = 1, 
           g = 2, 
           b = 3,
           mask = 4) %>% 
    filter(mask == 2) %>% 
    mutate(rg_index = (r-g)/(r+g)) %>% 
    mutate(picture = basename(photo_list[i]))
  
  photo_masked_iphone_df <- bind_rows(photo_masked_iphone_df, photo_i_df)
  
  print(i)
}

head(photo_masked_iphone_df)

#clean df
photo_masked_iphone_df_clean <- photo_masked_iphone_df %>% 
  separate(picture, into = c("site", "tree", "date"), sep = "_") %>% 
  mutate(tree = sub("^t", "", tree),
         date = sub("\\.tif$", "", date))

#do some stats

#randomly sample a set number of cone pixels from each date at each site
set.seed(123)
cone_px_df <- photo_masked_iphone_df_clean %>% 
  group_by(site, date, tree) %>% 
  slice_sample(n = 75, replace = FALSE)

#how does mean index value changes based on date
mean_index_by_date <- cone_px_df %>% 
  group_by(site, date) %>% 
  summarize(mean_index = mean(rg_index))

cone_px_df %>% 
  group_by(site, date) %>% 
  filter(!site %in% c("cathedral", "gun", "sonora", "wade")) %>% 
  ggplot() +
  geom_boxplot(aes(x = site, y = rg_index, col = date)) + 
  theme_classic()
  
#paired t test?

#do another analysis where we compare the same focal trees between flight dates based on how many cones are open?
#so analysis would be what is the mean index value as a function of how many cones are open? again a paired t-test? 

#spectral index df with pheno
cones_per_g_index_df <- read_csv("C:/Users/hmz25/Box/cones_per_g_index.csv")

focal_pheno <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/focal_trees_2025.csv")

focal_pheno_clean <- focal_pheno %>%
  dplyr::select(date_time, site, focal_tree_number, percent_cones_open, x, y) %>%
  mutate(site = tolower(site)) %>%
  mutate(site = substr(site, 1, 4))

pheno_df <- focal_pheno_clean %>% 
  dplyr::filter(!is.na(percent_cones_open)) %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  dplyr::select(-time) %>% 
  mutate(site = gsub(" ", "", site)) %>% 
  rename(tree = focal_tree_number)

index_pheno_df <- pheno_df %>% 
  left_join(cones_per_g_index_df, by = c("site", "tree")) %>% 
  drop_na(photo_i_rg_dif) %>%
  #create column for percent open grouping
  mutate(percent_open_group = case_when(
    percent_cones_open <= 24 ~ "0-24%",
    percent_cones_open <= 75 ~ "25–75%",
    percent_cones_open > 75  ~ "75-100%"
  ))

ggplot(index_pheno_df, aes(x=photo_i_index_rg_thresh_sum, y = total_cones_per_fol, col = percent_open_group)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  ggthemes::theme_few()

# Compute R² per group
r2_df <- index_pheno_df %>%
  group_by(percent_open_group) %>%
  summarise(
    r2 = summary(lm(total_cones_per_fol ~ photo_i_index_rg_thresh_sum))$r.squared,
    x = max(photo_i_index_rg_thresh_sum, na.rm = TRUE),
    y = max(total_cones_per_fol, na.rm = TRUE)
  )

# Plot with text labels
ggplot(index_pheno_df, aes(x = photo_i_index_rg_thresh_sum, 
                           y = total_cones_per_fol, 
                           color = percent_open_group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(data = r2_df,
            aes(x = 0.075, y = c(12,11.5,11), 
                label = paste0("R² = ", round(r2, 2))),
            hjust = 1, vjust = 1, size = 4) +
  theme_bw() +
  labs(col = "percent cones open")+ 
  xlab("spectral index") +
  ylab(expression(cone~density~(cones/g))) +
  theme_classic()


#just looking at some diff resolution of pics
jan2023 <- rast("C:/Users/hmz25/Box/texas/pollen_production/TX jan 23/jan 5 drone/quadrats/j5_q1_2.tif")
plotRGB(jan2023)
dim(jan2023)

jan2023 <- rast("C:/Users/hmz25/Box/texas/pollen_production/TX jan 23/jan 5 drone/quadrats/j5_q2_2.tif")
plotRGB(jan2023)
dim(jan2023)

jan2024 <- rast("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/cropped quadrat pics/cell_t2_q1.tif")
plotRGB(jan2024)
dim(jan2024)

jan2024 <- rast("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/cropped quadrat pics/cathedral_t1_q2.tif")
plotRGB(jan2024)
dim(jan2024)

jan2025 <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/cropped_quadrat_pics/cath_t10.tif")
plotRGB(jan2025)
dim(jan2025)

jan2025_handheld <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/cath_t10.tif")
plotRGB(jan2025_handheld)

jan2024_handheld <- rast("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels/cathedral_t1_q2.tif")
plotRGB(jan2024_handheld)





