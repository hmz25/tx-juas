#script to apply index to processed handheld images and compare to manual cone density data 

##to see how images were processed, refer to "process_handheld_imgs"
##or "process_handheld_imgs_wb" scripts 

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

# apply index to all masked/filtered drone photos -----------------------------------------------

#can change to come from either wb pics or not wb pics 
photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics"
photo_list <- list.files(photo_dir, pattern = ".tif", full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, pattern = ".tif", full.names = TRUE)

photo_masked_df <- data.frame()

# i = 22

# test <- stack(photo_list_full_dir[3])
# plotRGB(test)


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
  photo_i_index_rg_thresh_mean <- cellStats(photo_i_index_rg_thresh, "mean") #calculate mean index value  #plot(photo_i_index_rg_thresh)
  photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean* 
    ((ncell(photo_i_index_rg_thresh) - summary(photo_i_index_rg_thresh)[6]) / ncell(photo_i_index_rg_thresh)) #multiply by portion of cells not NA
  #(ncell(photo_i_index_rg_thresh) - ncell(photo_i_index_rg_thresh, na.omit = TRUE)) #plotRGB(photo_i) #ncell(photo_i) #cellStats(photo_i_index_rg_thresh, "mean")
  photo_i_index_rg_sum <- cellStats(photo_i_index_rg_thresh, "sum") #calculate index sum
  
  photo_i_r <- cellStats(photo_i[[1]], "mean")
  photo_i_g <- cellStats(photo_i[[2]], "mean")
  photo_i_b <- cellStats(photo_i[[3]], "mean")
  photo_i_mask <- cellStats(photo_i[[4]], "mean") - 1 #1 = background, 2 = foreground
  
  photo_i_rgb <- data.frame(photo_list[i], photo_i_r, photo_i_g, photo_i_b, photo_i_mask, photo_i_rg_dif, 
                            photo_i_index_rg_thresh_mean, photo_i_index_rg_thresh_sum, photo_i_index_rg_sum)
  
  photo_masked_df <- bind_rows(photo_masked_df, photo_i_rgb)
  
  print(i)
}

head(photo_masked_df)

#eventually save df to Box


# combine cone count data and rgb data ------------------------------------

#calculate cone density (cone/g) for each focal tree 
##this calculates # cones/g cone + foliage
cone_density_df <- cone_df_clean %>%
  mutate(cones_per_g = count/weight,
         std = sd(cones_per_g)) %>%
  group_by(site, tree, total_mass) %>%
  summarize(mean_cones_per_g = mean(cones_per_g)) # %>%
  #don't think I need to do this part bc it would give total cones in the sample, not cones/g
  #summarize(total_cones = mean_cone_dens*total_mass)

# ##this calculates g cone/g foliage
# ###find avg weight of cones
# g_cone_df <- read_csv("Katz lab/texas/cone processing 25 - cone_g.csv")
# 
# g_cone_df_clean <- cone_g_df %>% 
#   mutate(g_cone = cone_weight/cone_count) %>% 
#   summarize(mean_g_cone = mean(g_cone))
# 
# ### from analysis above, mean g of cone = 0.002314149
# 
# cone_g_df <- cone_df_clean %>% 
#   dplyr::select(-notes) %>% 
#   mutate(g_cones = count*0.002314149, #multiply cone count by weight of each cone
#          g_fol = weight-g_cones, #calculate g fol + twig in sample by subtracting total weight of cones
#          cones_per_fol = g_cones/g_fol) %>% #calculate ratio of g cone/g twig + fol 
#   group_by(site, tree, total_mass) %>% 
#   summarize(mean_cones_per_fol = mean(cones_per_fol)) # %>% #calculate mean ratio of g cone/g twig + fol 
#   # summarize(total_cones_per_fol = mean_cones_per_fol*total_mass) #calculate total g cone/g twig + fol for sample

# # old code, calculating total cones in sample rather than ratio of cones/g fol + twig
# cone_density_df <- cone_df_clean %>%
#   mutate(cone_per_g = count*weight,
#          std = sd(cone_per_g)) %>%
#   group_by(site, tree, total_mass) %>%
#   summarize(mean_cone_dens = mean(cone_per_g)) %>%
#   summarize(total_cones = mean_cone_dens*total_mass)

#join rgb data frame with cone density data frame

#old code when female trees weren't included in analysis
# photo_masked_df_join <- photo_masked_df %>%
#   mutate(site = str_extract(photo_list.i., "^[^_]+"),
#          tree = as.character(str_extract(photo_list.i., "(?<=_)\\d+")))

photo_masked_df_join <- photo_masked_df %>% 
  separate(photo_list.i., into = c("site", "tree"), sep = "_") %>% 
  mutate(tree = sub("^t", "", tree),
         tree = sub("\\.tif$", "", tree))

# photo_masked_df_join <- photo_masked_df %>% 
#   separate(photo_list.i., into = c("site", "tree", "wb"), sep = "_") %>% 
#   dplyr::select(-wb) %>% 
#   mutate(tree = sub("^t", "", tree),
#          tree = sub("\\.tif$", "", tree))

quadrat_cones_rgb <- left_join(cone_density_df, photo_masked_df_join) %>%
  filter(!is.na(mean_cones_per_g)) %>%
  filter(!is.na(photo_i_r)) %>% 
  mutate(photo_i_mask = photo_i_mask - 1)

# quadrat_cones_rgb <- left_join(cone_g_df, photo_masked_df_join) %>%
#   filter(!is.na(mean_cones_per_fol)) %>%
#   filter(!is.na(photo_i_r)) %>% 
#   mutate(photo_i_mask = photo_i_mask - 1)

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_sum, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

# ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = mean_cones_per_fol)) + 
#   geom_point(alpha = 0.5) + 
#   theme_bw() + 
#   geom_smooth(method = "lm", se = FALSE) +
#   xlab("spectral index") + ylab(cone~density~(cones/g)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)


#rm outliers
photo_masked_df_join <- photo_masked_df %>%
  mutate(site = str_extract(photo_list.i., "^[^_]+"),
         tree = as.character(str_extract(photo_list.i., "(?<=_t)\\d+"))) %>% 
  filter(!photo_list.i. %in% c("cell_t8.tif", 
                               "fish_t10.tif",
                               "cath_t9.tif",
                               "cath_t7.tif",
                               "rock_t1.tif",
                               "kimb_t1.tif",
                               "swee_t5.tif",
                               "cath_t10.tif")) #cath t10 def just has pcd estimated incorrectly

#old rm outliers
# photo_masked_df_join <- photo_masked_df %>%
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


quadrat_cones_rgb <- left_join(cone_density_df, photo_masked_df_join) %>%
  filter(!is.na(mean_cones_per_g)) %>%
  filter(!is.na(photo_i_r)) %>% 
  mutate(photo_i_mask = photo_i_mask - 1)

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

# ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = mean_cones_per_fol)) + 
#   geom_point(alpha = 0.5) + 
#   theme_bw() + 
#   geom_smooth(method = "lm", se = FALSE) +
#   xlab("spectral index") + ylab(cone~density~(cones/g)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

fit <- lm(quadrat_cones_rgb$mean_cones_per_g  ~ quadrat_cones_rgb$photo_i_index_rg_thresh_sum)
summary(fit)
r_sq <- summary(fit)$r.squared

#calculate equation for cone density from linear model
coefficients <- coef(fit)
# print(coefficients)

intercept <- coefficients[1]
slope <- coefficients[2]

equation_string <- paste0("y_hat = ", round(intercept, 4), " + ", round(slope, 4), " * x")
print(equation_string)

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  ggthemes::theme_few() +
  annotate("text", 
           x = min(quadrat_cones_rgb$photo_i_index_rg_thresh_sum), 
           y = max(quadrat_cones_rgb$mean_cones_per_g), 
           label = paste("R² =", round(r_sq, 2)), 
           hjust = 0, size = 4, color = "black") # + 
  # annotate("text",
  #          x = 0.055, 
  #          y = 35,
  #          label = equation_string)

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = mean_cones_per_g, label = paste(site, tree))) + 
  geom_point(alpha = 0.5) + 
  geom_label() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few()

# #testing just the raw index values without a threshold
# ggplot(quadrat_cones_rgb, aes(x=photo_i_rg_dif, y = mean_cones_per_g)) +
#   geom_point(alpha = 0.5) +
#   theme_bw() +
#   geom_smooth(method = "lm", se = FALSE) +
#   xlab("spectral index") + ylab(cone~density~(cones/m^2)) +
#   ggthemes::theme_few()


# quadrat_cones_rgb_test <- quadrat_cones_rgb %>%  
#   filter((photo_i_r + photo_i_g + photo_i_b) > 0)
# 
# #quadrat_cones_rgb_test <- quadrat_cones_rgb_test %>% filter(photo_i_mask > 0.5) #applying this filter results in no values 


#using cones per g df 
quadrat_cones_rgb <- left_join(cone_g_df, photo_masked_df_join) %>%
  filter(!is.na(total_cones_per_fol)) %>%
  filter(!is.na(photo_i_r)) %>% 
  mutate(photo_i_mask = photo_i_mask - 1)

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_mean, y = total_cones_per_fol)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + ggthemes::theme_few()

fit <- lm(quadrat_cones_rgb$total_cones_per_fol  ~ quadrat_cones_rgb$photo_i_index_rg_thresh_sum   )
summary(fit)
r_sq <- summary(fit)$r.squared

write_csv(quadrat_cones_rgb, "cones_per_g_index.csv")


##examining outliers from not wb

cath_t10 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_t10.tif")
plotRGB(cath_t10)

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
plotRGB(fish_t8)
# fish_t8[[1]][fish_t8[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
# fish_t8[[2]][fish_t8[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
# fish_t8_index <- (fish_t8[[1]] - fish_t8[[2]])/(fish_t8[[1]] + fish_t8[[2]]) #calculate index 
# plot(fish_t8_index, col = color_ramp(25))
# plot(fish_t8_index, col = custom_palette)

#v high index, medium pcd
#gun t 4
gun_t4 <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_iphone_quadrat_pics/gun_t4.tif")
plotRGB(gun_t4)
#some shadow but overall just very sparse 

#fish 10 -- not the right tree?? exclude 
fish_t10 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t10.tif")
plotRGB(fish_t10)
# fish_t10[[1]][fish_t10[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
# fish_t10[[2]][fish_t10[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
# fish_t10_index <- (fish_t10[[1]] - fish_t10[[2]])/(fish_t10[[1]] + fish_t10[[2]]) #calculate index 
# plot(fish_t10_index)#, col = color_ramp(25))

# test <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t10.tif")
# plotRGB(test)

#fish t9 -- overall fine? 
fish_t9 = rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t9.tif")
plotRGB(fish_t9)
fish_t9[[1]][fish_t9[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
fish_t9[[2]][fish_t9[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
fish_t9_index <- (fish_t9[[1]] - fish_t9[[2]])/(fish_t9[[1]] + fish_t9[[2]]) #calculate index
plot(fish_t9_index)

#rocky tree 1 -- super shadowy, exclude 
rock_t1 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/rock_t1.tif")
plotRGB(rock_t1)
# rock_t1[[1]][rock_t1[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
# rock_t1[[2]][rock_t1[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
# rock_t1_index <- (rock_t1[[1]] - rock_t1[[2]])/(rock_t1[[1]] + rock_t1[[2]]) #calculate index 
# plot(rock_t1_index, col = color_ramp(25))

plot(rock_t1_index, col = custom_palette)

#cath tree 9 -- not the right pic!
cath_t9 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_t9.tif")
plotRGB(cath_t9)

test <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/kimb_female.tif")
plotRGB(test)

#cath tree 7 -- not the right pic!
cath_t7 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_t7.tif")
plotRGB(cath_t7)

test <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/fish_female.tif")
plotRGB(test)

##examining outliers from wb


#cell tree 8 -- 
cell_t8 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cell_t8.tif")
plotRGB(cell_t8)
cell_t8[[1]][cell_t8[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
cell_t8[[2]][cell_t8[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
plotRGB(cell_t8)
cell_t8_index <- (cell_t8[[1]] - cell_t8[[2]])/(cell_t8[[1]] + cell_t8[[2]]) #calculate index
plot(cell_t8_index)
cell_t8_index <- (cell_t8[[1]] - cell_t8[[2]])/(cell_t8[[1]] + cell_t8[[2]] + cell_t8[[3]]) #calculate index
plot(cell_t8_index)

cell_t8 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_wb/cell_t8_wb.tif")
plotRGB(cell_t8)
cell_t8[[1]][cell_t8[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
cell_t8[[2]][cell_t8[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
plotRGB(cell_t8)
cell_t8_index <- (cell_t8[[1]] - cell_t8[[2]])/(cell_t8[[1]] + cell_t8[[2]]) #calculate index
plot(cell_t8_index, zlim = c(-0.4, 0.2))

#kimb t3
kimb_t3 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/kimb_t3.tif")
plotRGB(kimb_t3)
kimb_t3[[1]][kimb_t3[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
kimb_t3[[2]][kimb_t3[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
plotRGB(kimb_t3)
kimb_t3_index <- (kimb_t3[[1]] - kimb_t3[[2]])/(kimb_t3[[1]] + kimb_t3[[2]]) #calculate index
plot(kimb_t3_index)

kimb_t3 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_wb/kimb_t3_wb.tif")
plotRGB(kimb_t3)
kimb_t3[[1]][kimb_t3[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
kimb_t3[[2]][kimb_t3[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
plotRGB(kimb_t3)
kimb_t3_index <- (kimb_t3[[1]] - kimb_t3[[2]])/(kimb_t3[[1]] + kimb_t3[[2]]) #calculate index
plot(kimb_t3_index)

cath_t10 = stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cath_t10.tif")
plotRGB(cath_t10)
cath_t10[[1]][cath_t10[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
cath_t10[[2]][cath_t10[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
plotRGB(cath_t10)
cath_t10_index <- (cath_t10[[1]] - cath_t10[[2]])/(cath_t10[[1]] + cath_t10[[2]]) #calculate index
plot(cath_t10_index)
cath_t10_index <- (cath_t10[[1]] - cath_t10[[2]])/(cath_t10[[1]] + cath_t10[[2]] + cath_t10[[3]]) #calculate index
plot(cath_t10_index)



#wade t7

#kimb t5

#kimb t6

#rock t1 

#wade t8

#rock t2






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
