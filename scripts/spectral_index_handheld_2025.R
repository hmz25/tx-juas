library(tidyverse)
library(dplyr)
library(terra)
library(stringr)
library(raster)
library(readr)
library(purrr)
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

setwd("C:/Users/hmz25/Box/")


# manual cone data processing ---------------------------------------------

#load in cone count data 
cone_df <- read_csv("Katz lab/texas/cone processing 25 - counts.csv")

cone_df_clean <- cone_df |> 
  rename(name_site = site,
         total_subsample_weight = subsample_weight) |>  
  pivot_longer(
    cols = starts_with("s"),
    names_to = c("sample_n", ".value"),
    names_pattern = "s(\\d+)_(.*)") |> 
  mutate(site = substr(name_site, 1, 4)) |> 
  dplyr::select(date_collected, site, tree, total_mass, total_subsample_weight, sample_n, weight, count, notes) %>% 
  mutate(tree = as.character(tree))

ggplot(cone_df_clean, aes(x = as.factor(tree), y = count)) +
  geom_boxplot() +
  facet_wrap( ~ site)

# cone_var_df <- cone_df_clean |>
#   group_by(site, tree) |>
#   summarise(cone_var = sd(count))

# exploring quadrat images ------------------------------------------------

#examining foliage values
list.files("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025") 

image_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/"
quadrat_files <- list.files(image_dir)#[1]

test <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/cath_t6.tif")
plotRGB(test)


test2 <- test
test2$cath_t6_3[test2$cath_t6_1 > 100 & test2$cath_t6_2 > 100 & test2$cath_t6_3 > 100] <- 255
test2$cath_t6_1[test2$cath_t6_1/test2$cath_t6_2 > 1.5] <- 255
plotRGB(test2)

test3 <- test$cath_t6_1/test$cath_t6_3
plot(test3)


# random forest pixel classifier to mask out non cones/non foliage --------

#training dataset for non-foliage and non-cones
sup_not_twig <- rast("Katz lab/texas/tx 2025 drone pics/not_fol.tif")
#plotRGB(sup_not_twig) #sup_not_twig$not_fol_1 #sup_not_twig$not_fol_1[1:100]

sup_not_twig_df <- as.data.frame(sup_not_twig) %>% 
  filter(not_fol_1 != 255) %>%  
  rename(c("r" = 1 , "g" = 2, "b" = 3)) %>% 
  mutate(class = "not") 
#head(sup_not_twig_df)

#training dataset for foliage and cones
sup_yes_twig <- rast("Katz lab/texas/tx 2025 drone pics/yes_fol.tif")
#plotRGB(sup_yes_twig) #sup_yes_twig$yes_fol_1 #sup_yes_twig$yes_fol_1[1:100]

sup_yes_twig_df <- as.data.frame(sup_yes_twig) %>%
  filter(yes_fol_1 != 255) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>% 
  mutate(class = "yes")  #head(sup_yes_twig_df)

#combine training datasets and randomly select pixels from them
training_df <- bind_rows(sup_not_twig_df, sup_yes_twig_df) %>%
  sample_n(100000) %>%
  mutate(class = as.factor(class))
# head(training_df)
# str(training_df)

#run a pixel-based classifier
set.seed(100)
rf_mask <- randomForest(class ~ ., data = training_df, na.action=na.omit)
rf_mask

#apply model to a single photo
photo_i <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/sono_t8.tif")
#plotRGB(photo_i)

#make sure layer names for picture match the training df column names 
names(photo_i) <- colnames(training_df)[1:3]
photo_i_mask <- predict(photo_i, rf_mask)
plot(photo_i_mask)

#add mask layer back to original photo
photo_i <- c(photo_i, photo_i_mask)
plot(photo_i)
# plot(photo_i$class) 
# plot(photo_i$r)

# applying rf pixel mask to all handheld drone images and saving in new folder ---------------------
# photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025"
# photo_list <- list.files(photo_dir, full.names = FALSE)
# photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)
# 
# #i = 20
# 
# for(i in 1:length(photo_list)){
#   photo_i <- raster::stack(photo_list_full_dir[i])  #plotRGB(photo_i)
# 
#   photo_i_df <- as.data.frame(photo_i) %>%
#     rename(c("r" = 1 , "g" = 2, "b" = 3))  #head(photo_i_df)
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
#   save_file_name = paste0("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/",photo_list[i])
#   raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
#   print(i)
# }

# kimb_t4 <- raster::stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/kimb_t4.tif")
# plotRGB(kimb_t4)
# plot(kimb_t4)


# test <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/sono_t8.tif")
# plotRGB(test)
# plot(test)

# fish_t8 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t8.tif")
# plotRGB(fish_t8)
# plot(fish_t8)
# 
# fish_t9 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t9.tif")
# plotRGB(fish_t9)
# plot(fish_t8)


# applying rf pixel mask to all iphone images and saving in new folder -------------------------------------------------

###DO I HAVE TO DO SEPARATE RF FOR IPHONE PICS 

photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/iphone_quadrat_pics"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)

#i = 7

for(i in 1:length(photo_list)){
  photo_i <- raster::stack(photo_list_full_dir[i])  #plotRGB(photo_i)
  
  photo_i_df <- as.data.frame(photo_i) %>%
    rename(c("r" = 1 , "g" = 2, "b" = 3))   #head(photo_i_df)
  
  photo_i_mask_df <- predict(rf_mask, photo_i_df)
  # head(photo_i_mask_df)
  # photo_i_df <- photo_i_df %>%
  #   mutate(is_foreground = photo_i_mask_df,
  #          is_foreground_numeric = case_when( is_foreground == "yes" ~ 1,
  #                                             is_foreground == "not" ~ 0))
  # #head(photo_i_df)
  
  photo_i <- addLayer(photo_i, photo_i[[3]])
  photo_i[[4]] <- photo_i_mask_df # photo_i_df$is_forground
  # plot(photo_i)
  
  save_file_name = paste0("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics/",photo_list[i])
  raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
  print(i)
}

# cath_t5 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics/cath_t5.tif")
# plotRGB(cath_t5)
# plot(cath_t5)

# apply index to all masked drone photos -----------------------------------------------

photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)

photo_masked_df <- data.frame()

# i = 22

# photo_i = stack("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_t10.tif") # high pcd

# photo_i = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t8.tif")

for(i in 1:length(photo_list)){
  photo_i <- stack(photo_list_full_dir[i]) #plot(photo_i) 
  
  photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
  photo_i_index_rg_dif <- (photo_i[[1]] - photo_i[[2]])/(photo_i[[1]] + photo_i[[2]]) #calculate index 
  
  # plotRGB(photo_i)
  # #color_ramp <- colorRampPalette(c("darkgreen", "orange"))
  # plot(photo_i_index_rg_dif, col = color_ramp(25))
  
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


# apply index to all masked iphone photos --------------------------------------
photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)

photo_masked_iphone_df <- data.frame()

for(i in 1:length(photo_list)){
  photo_i <- stack(photo_list_full_dir[i]) #plot(photo_i) 
  
  photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
  photo_i_index_rg_dif <- (photo_i[[1]] - photo_i[[2]])/(photo_i[[1]] + photo_i[[2]]) #calculate index 
  
  # plotRGB(photo_i)
  # #color_ramp <- colorRampPalette(c("darkgreen","white","orange"))
  # plot(photo_i_index_rg_dif, col = color_ramp(25))
  
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
                            photo_i_index_rg_thresh_mean, photo_i_index_rg_thresh_sum) %>% 
    mutate(source = "iphone")
  
  photo_masked_iphone_df <- bind_rows(photo_masked_iphone_df, photo_i_rgb)
  
  print(i)
}


# combine cone count data and rgb data ------------------------------------

#combine iphone and drone rgb dfs
combined_photo_masked_df <- bind_rows(photo_masked_df, photo_masked_iphone_df)


#calculate total number of cones in each quadrat
cone_density_df <- cone_df_clean %>% 
  mutate(cone_per_g = count*weight,
         std = sd(cone_per_g)) %>% 
  group_by(site, tree, total_mass) %>% 
  summarize(mean_cone_dens = mean(cone_per_g)) %>% 
  summarize(total_cones = mean_cone_dens*total_mass) 


#join rgb data frame with cone density data frame
photo_masked_df_join <- combined_photo_masked_df %>%
  mutate(site = str_extract(photo_list.i., "^[^_]+"),
         tree = as.character(str_extract(photo_list.i., "(?<=_t)\\d+"))) %>% 
  filter(!photo_list.i. %in% c("cell_t8.tif", 
                               "fish_t9.tif",
                               "fish_t8.tif",
                               "rock_t1.tif",
                               "swee_t4.tif",
                               "wade_t8.tif",
                               "wade_t9.tif",
                               "cath_t10.tif",
                               "cath_t9.tif",
                               "cath_t7.tif",
                               "kimb_t1.tif",
                               "swee_t5.tif",
                               "kimb_t9.tif",
                               "gun_t4.tif"))

quadrat_cones_rgb <- left_join(cone_density_df, photo_masked_df_join) %>%
  filter(!is.na(total_cones)) %>%
  filter(!is.na(photo_i_r)) %>% 
  mutate(photo_i_mask = photo_i_mask - 1)

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

#testing just the raw index values without a threshold 
ggplot(quadrat_cones_rgb, aes(x=photo_i_rg_dif, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few()

mod <- lm(photo_i_rg_dif ~ total_cones, data = quadrat_cones_rgb)
summary(mod)

quadrat_cones_rgb_test <- quadrat_cones_rgb %>%  
  filter((photo_i_r + photo_i_g + photo_i_b) > 0)

#quadrat_cones_rgb_test <- quadrat_cones_rgb_test %>% filter(photo_i_mask > 0.5) #applying this filter results in no values 

ggplot(quadrat_cones_rgb_test, aes(x=photo_i_index_rg_thresh_sum, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)


fit <- lm(quadrat_cones_rgb_test$total_cones  ~ quadrat_cones_rgb_test$photo_i_index_rg_thresh_sum   )
summary(fit)
r_sq <- summary(fit)$r.squared

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = total_cones, label = paste(site, tree))) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few() +
  annotate("text", 
           x = min(quadrat_cones_rgb$photo_i_index_rg_thresh_sum), 
           y = max(quadrat_cones_rgb$total_cones), 
           label = paste("R² =", round(r_sq, 3)), 
           hjust = 0, size = 4, color = "black")

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = total_cones, label = paste(site, tree))) + 
  geom_point(alpha = 0.5) + 
  geom_label() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few() +
  annotate("text", 
           x = min(quadrat_cones_rgb$photo_i_index_rg_thresh_sum), 
           y = max(quadrat_cones_rgb$total_cones), 
           label = paste("R² =", round(r_sq, 3)), 
           hjust = 0, size = 4, color = "black")

##examining outliers

#med-high pcd, low index
kimb_t1 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/kimb_t1.tif")
plotRGB(kimb_t1)
#blurry... maybe exclude

#med-high pcd, low index 
swee_t5 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/swee_t5.tif")
plotRGB(swee_t5)
#bad quality-- blurry+ lots of shadows, exclude 

#v high pcd, med index
kimb_t9 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/kimb_t9.tif")
plotRGB(kimb_t9)
#blurry but not a ton of shadows, EXPLORE FURTHER

#med-high index, very low pcd
gun_t5 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics/gun_t5.tif")
plotRGB(gun_t5)
#shadow and lots of twigs + gaps
plot(gun_t5[[4]])
#not really filtering out twig

gun_t5[[1]][gun_t5[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
gun_t5[[2]][gun_t5[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
gun_t5_index <- (gun_t5[[1]] - gun_t5[[2]])/(gun_t5[[1]] + gun_t5[[2]]) #calculate index 
plot(gun_t5_index, col = color_ramp(25))
plot(gun_t5_index, col = custom_palette)
#looks like the yellow ish foliage are making the index value higher

#shadow
fish_t8 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t8.tif")
fish_t8[[1]][fish_t8[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
fish_t8[[2]][fish_t8[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
fish_t8_index <- (fish_t8[[1]] - fish_t8[[2]])/(fish_t8[[1]] + fish_t8[[2]]) #calculate index 
plot(fish_t8_index, col = color_ramp(25))
plot(fish_t8_index, col = custom_palette)

#v high index, medium pcd
gun_t4 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics/gun_t4.tif")
plotRGB(gun_t4)
#some shadow but overall just ver sparse 

#high pcd
fish_t10 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t10.tif")
fish_t10[[1]][fish_t10[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
fish_t10[[2]][fish_t10[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
fish_t10_index <- (fish_t10[[1]] - fish_t10[[2]])/(fish_t10[[1]] + fish_t10[[2]]) #calculate index 
plot(fish_t10_index, col = color_ramp(25))

rock_t1 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/rock_t1.tif")
rock_t1[[1]][rock_t1[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
rock_t1[[2]][rock_t1[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
rock_t1_index <- (rock_t1[[1]] - rock_t1[[2]])/(rock_t1[[1]] + rock_t1[[2]]) #calculate index 
plot(rock_t1_index, col = color_ramp(25))
plotRGB(rock_t1)
plot(rock_t1_index, col = custom_palette)

range_vals <- range(values(fish_t10_index), values(rock_t1_index), na.rm = TRUE)
plot(fish_t10_index, col = color_ramp(25), zlim = range_vals, main = "Fish T10 Index")
plot(rock_t1_index, col = color_ramp(25), zlim = range_vals, main = "Rock T1 Index")

#trying out different color scales
n <- 20

# Split evenly (or slightly biased toward the positive side if needed)
n_neg <- n / 2
n_pos <- n / 2

# Colors
neg_colors <- rep("darkgreen", n_neg)
# pos_colors <- colorRampPalette(c("orange", "orangered"))(n_pos)
pos_colors <- rep("orange", n_pos)

# Final palette
custom_palette <- c(neg_colors, pos_colors)

plot(fish_t10_index, col = custom_palette, zlim = range_vals, main = "Fish T10 Index")
plot(rock_t1_index, col = custom_palette, zlim = range_vals, main = "Rock T1 Index")

#look up r colors 