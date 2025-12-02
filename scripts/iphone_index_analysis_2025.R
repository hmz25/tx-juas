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
  #to avoid col names starting with s for pivoting to longer format
  rename(name_site = site,
         total_subsample_weight = subsample_weight) |>  
  pivot_longer(
    cols = starts_with("s"),
    names_to = c("sample_n", ".value"),
    names_pattern = "s(\\d+)_(.*)") |> 
  mutate(site = substr(name_site, 1, 4)) |> 
  dplyr::select(date_collected, site, tree, total_mass, total_subsample_weight, sample_n, weight, count, notes) %>% 
  mutate(tree = as.character(tree))

cone_df_clean %>% 
  filter(!tree %in% "female") %>% 
  ggplot(aes(x = as.factor(tree), y = count)) +
  geom_boxplot() +
  facet_grid(~site) +
  theme_classic() +
  xlab("focal tree") + ylab("cone count") + 
  theme(axis.text.x = element_text(size = 5))

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

# # applying rf pixel mask to all iphone images and saving in new folder -------------------------------------------------
# 
# ###DO I HAVE TO DO SEPARATE RF FOR IPHONE PICS 
# 
# photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/iphone_quadrat_pics"
# photo_list <- list.files(photo_dir, full.names = FALSE)
# photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)
# 
# #i = 7
# 
# for(i in 1:length(photo_list)){
#   photo_i <- raster::stack(photo_list_full_dir[i])  #plotRGB(photo_i)
#   
#   photo_i_df <- as.data.frame(photo_i) %>%
#     rename(c("r" = 1 , "g" = 2, "b" = 3))   #head(photo_i_df)
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
#   # plot(photo_i)
#   
#   save_file_name = paste0("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics/",photo_list[i])
#   raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
#   print(i)
# }
# 
# # cath_t5 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics/cath_t5.tif")
# # plotRGB(cath_t5)
# # plot(cath_t5)

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
    (ncell(photo_i_index_rg_thresh) - ncell(photo_i_index_rg_thresh, na.omit = TRUE)) #plotRGB(photo_i) #ncell(photo_i) #cellStats(photo_i_index_rg_thresh, "mean")
  
  # photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean* 
  #   ((ncell(photo_i_index_rg_thresh) - global(is.na(photo_i_index_rg_thresh)[6])) / ncell(photo_i_index_rg_thresh)) #multiply by portion of cells not NA
  
  
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

#calculate cone per g in each quadrat

#find avg weight of cones
g_cone_df <- read_csv("Katz lab/texas/cone processing 25 - cone_g.csv")

g_cone_df_clean <- cone_g_df %>% 
  mutate(g_cone = cone_weight/cone_count) %>% 
  summarize(mean_g_cone = mean(g_cone))

#from analysis above, mean g of cone = 0.002314149

cone_g_df <- cone_df_clean %>% 
  dplyr::select(-notes) %>% 
  mutate(g_cones = count*0.002314149, #multiply cone count by weight of each cone
         g_fol = weight-g_cones, #calculate g fol + twig in sample by subtracting total weight of cones
         cones_per_fol = g_cones/g_fol) %>% #calculate ratio of g cone/g twig + fol 
  group_by(site, tree, total_mass) %>% 
  summarize(mean_cones_per_fol = mean(cones_per_fol)) %>% #calculate mean ratio of g cone/g twig + fol 
  summarize(total_cones_per_fol = mean_cones_per_fol*total_mass) #calculate total g cone/g twig + fol for sample

# # old code, calculating total cones in sample rather than ratio of cones/g fol + twig
# cone_density_df <- cone_df_clean %>%
#   mutate(cone_per_g = count*weight,
#          std = sd(cone_per_g)) %>%
#   group_by(site, tree, total_mass) %>%
#   summarize(mean_cone_dens = mean(cone_per_g)) %>%
#   summarize(total_cones = mean_cone_dens*total_mass)


#join rgb data frame with cone density data frame

#for df including iphone pics
# photo_masked_df_join <- combined_photo_masked_df %>%
#   mutate(site = str_extract(photo_list.i., "^[^_]+"),
#          tree = as.character(str_extract(photo_list.i., "(?<=_t)\\d+"))) %>% 
#   filter(!photo_list.i. %in% c("cell_t8.tif", 
#                                "fish_t9.tif",
#                                "fish_t8.tif",
#                                "rock_t1.tif",
#                                "swee_t4.tif",
#                                "wade_t8.tif",
#                                "wade_t9.tif",
#                                "cath_t10.tif",
#                                "cath_t9.tif",
#                                "cath_t7.tif",
#                                "kimb_t1.tif",
#                                "swee_t5.tif",
#                                "kimb_t9.tif",
#                                "gun_t4.tif"))