#script to create index using 2026 drone images and manual cone count data

library(tidyverse)
library(dplyr)
library(terra)
# library(magick) #install.packages("magick")
library(stringr)
library(raster)
library(tidyterra)
library(ggplot2)
library(patchwork) #install.packages("patchwork")
library(ggpubr)

setwd("C:/Users/hmz25/Box/Katz lab/texas")


# manual cone data processing ---------------------------------------------

#load in cone count data 
cone_df <- read_csv("cone processing 26 - counts.csv")

cone_df_clean <- cone_df |> 
  rename(name_site = site,
         total_subsample_weight = subsample_weight) |>  
  pivot_longer(
    cols = starts_with("s"),
    names_to = c("sample_n", ".value"),
    names_pattern = "s(\\d+)_(.*)") |> 
  mutate(site = substr(name_site, 1, 4)) |> 
  dplyr::select(date_collected, site, tree, quadrat, quadrat_location, total_mass, total_subsample_weight, sample_n, weight, count, notes) |> 
  filter(!is.na(weight))

# ggplot(cone_df_clean, aes(x = as.factor(tree), y = count)) +
#   geom_boxplot() +
#   facet_wrap( ~ site) + 
#   labs(x = "focal tree") +
#   theme_minimal()
# 
# cone_df_clean |> 
#   mutate(site_tree = paste(site,tree, sep = "_")) |> 
#   ggplot() +
#   geom_boxplot(aes(x = reorder(site_tree, count), y = count)) +
#   xlab("tree ID") +
#   ylab("cone count") +
#   theme_minimal()

cone_density_df <- cone_df_clean |> 
  mutate(cones_per_g = count/weight) |> 
  group_by(date_collected, site, tree, quadrat_location, total_mass) |> 
  summarize(mean_cones_per_g = mean(cones_per_g)) |> 
  mutate(total_cones = mean_cones_per_g * total_mass) 

# cone_density_df |> 
#   filter(quadrat_location == "tree") |> 
#   View()

# quadrat image processing ------------------------------------------------

###EVENTUALLY UPDATE BASE NAME EXTRACTION TO INCLUDE _2 FOR QUADRATS WITH MORE THAN 1 PICTURE 

#path to filtered quadrat images
img_folder <- "tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics"
img_list <- list.files(img_folder, full.names = FALSE, pattern = ".tif")
img_list_dir <- list.files(img_folder, full.names = TRUE, pattern = ".tif")

quadrat_px_df <- data.frame()

# i = 1

for (i in seq_along(img_list)) {
  #remove the file extension to use as object name
  file_name <- tools::file_path_sans_ext(img_list[i])
  
  if (str_detect(file_name, "female")){
    site <- str_split_i(file_name, "_", 1)
    
    date <- str_split_i(file_name, "_", 2)
    
    tree <- str_split_i(file_name, "_", 3)
    
    quadrat_location <- "tree"
  } else {
  
  #extract site name (everything before "_t")
  site <- str_split_i(file_name, "_", 1)
  
  #extract tree number (digits after "t")
  tree <- str_split_i(file_name, "_", 2)
  
  #extract quadrat location
  quadrat_location <- str_split_i(file_name, "_", 3)
  
  }
  
  #determine if img has been WB adjusted 
  adjustment <- if_else(str_detect(file_name, "_adj"), "adj", "unadj")
  
  #load the raster
  photo_i <- rast(img_list_dir[i])
  # plotRGB(photo_i)
  
  #rename raster columns 
  names(photo_i) <- c("r", "g", "b", "rf_mask")
  
  #convert to data frame
  photo_i_df <- as.data.frame(photo_i, na.rm = FALSE) #keep NA values 
  
  #add site, tree, photo ID columns to df 
  photo_i_df$photo <- file_name
  photo_i_df$site <- site
  photo_i_df$tree <- tree
  photo_i_df$quadrat_location <- quadrat_location
  photo_i_df$adjustment <- adjustment
  
  #add to data frame of all pixels in each quadrat 
  quadrat_px_df <- rbind(quadrat_px_df, photo_i_df)
  
  print(i) 
}

quadrat_px_df #result is data frame with all pixel values from each quadrat 
#rf_mask values = 1 is not cone/fol, 2 is cone/fol 

#normalize rgb values to make the brightness invariant 
quadrat_px_df_norm <- quadrat_px_df |> 
  mutate(r_norm = r/(r+g+b),
         g_norm = g/(r+g+b),
         b_norm = b/(r+g+b))

#take mean of rgb values, then calculate the index 
index_df <- quadrat_px_df_norm |> 
  filter(rf_mask == 2,
         adjustment == "unadj") |> 
  group_by(photo, site, tree, quadrat_location) |> 
  summarize(r_mean = mean(r),
            g_mean = mean(g),
            b_mean = mean(b),
            r_norm_mean = mean(r_norm),
            g_norm_mean = mean(g_norm),
            b_norm_mean = mean(b_norm)) |> 
  group_by(photo, site, tree, quadrat_location, across(r_mean:b_norm_mean)) |>
  summarize(mean_index = (r_mean - g_mean)/(r_mean + g_mean),
            mean_norm_index = (r_norm_mean - g_norm_mean)/(r_norm_mean + g_norm_mean),
            mean_new_index = (r_mean - g_mean)/(r_mean/g_mean),
            mean_new_norm_index = (r_norm_mean - g_norm_mean)/(r_norm_mean/g_norm_mean)) |> 
  mutate(site = substr(site, 1, 4),
         tree = if_else(tree == "female", tree, str_remove(tree, "^t")))


# combine quadrat pixel data with cone count data -------------------------

str(index_df)
str(cone_df_clean)

cone_index_df <- cone_density_df |> 
  mutate(tree = as.character(tree)) |> 
  full_join(index_df, cone_density_df, by = c("site", "tree", "quadrat_location")) |> 
  #add info into female quadrat
  mutate(total_cones = if_else(tree == "female", 0, total_cones),
         mean_cones_per_g = if_else(tree == "female", 0, mean_cones_per_g),
         date_collected = if_else(
           tree == "female",
           format(lubridate::ymd(str_split_i(photo, "_", 2)), "%m/%d/%Y"),
           date_collected)
         ) |>  
  drop_na(mean_index) |> 
  filter(quadrat_location == "tree", #limit analysis to on-tree quadrats
         !site == "good", #photos taken at 150 ft
         !photo %in% c("windmill_t1_tree", "celltower_t2_tree", "wade_t5_tree")) #exclude excessively shaded quadrat imgs + use wade_t5_tree_2 in analysis


# add in grey card px info and calculate calibrated index -----------------

grey_card_px_df <- read_csv("grey_card_px_df.csv")

grey_card_px_df_join <- grey_card_px_df |> 
  mutate(date_collected = format(ymd(date), "%m/%d/%Y"),
         site = substr(site, 1, 4),
         r_norm = r/(r+g+b),
         g_norm = g/(r+g+b),
         b_norm = b/(r+g+b)) |> 
  group_by(site, date_collected) |> 
  summarize(mean_r_grey = mean(r),
            mean_g_grey = mean(g),
            mean_b_grey = mean(b),
            mean_r_grey_norm = mean(r_norm),
            mean_g_grey_norm = mean(g_norm),
            mean_b_grey_norm = mean(b_norm))

cone_index_reflect_df <- cone_index_df |> 
  left_join(grey_card_px_df_join, join_by(site, date_collected)) |> 
  #add in reflectance value calculations
  mutate(r_reflect = (0.18/mean_r_grey)*r_mean,
         g_reflect = (0.18/mean_g_grey)*g_mean,
         b_reflect = (0.18/mean_b_grey)*b_mean,
         r_reflect_norm = (0.18/mean_r_grey_norm)*r_norm_mean,
         g_reflect_norm = (0.18/mean_g_grey_norm)*g_norm_mean,
         b_reflect_norm = (0.18/mean_b_grey_norm)*b_norm_mean) |> 
  #calculate index using reflectance values
  mutate(reflect_old_index = (r_reflect - g_reflect)/(r_reflect + g_reflect),
         reflect_new_index = (r_reflect - g_reflect)/(r_reflect/g_reflect),
         reflect_new_index_norm = (r_reflect_norm - g_reflect_norm)/(r_reflect_norm/g_reflect_norm))

# write_csv(cone_index_reflect_df, "cone_density_index_df_2026.csv")

# cone_index_reflect_df |> 
#   select(date_collected, site, tree, mean_cones_per_g, total_cones, reflect_new_index_norm) |> 
#   View()



# visualize data ----------------------------------------------------------

#correlation of index values with on-tree quadrat cone density

mod <- lm(mean_norm_index ~ total_cones, data = cone_index_reflect_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_index_reflect_df, aes(x = mean_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones~(cones/g)) + 
  annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.085, y = 16400, label = paste("p-val =",p_val)) +
  ggthemes::theme_few() 

ggplot(cone_index_reflect_df, aes(x = mean_index, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  annotate("text", x = -0.085, y = 65, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.082, y = 62, label = paste("p-val =",p_val)) + 
  ggthemes::theme_few() 

mod <- lm(mean_norm_index ~ total_cones, data = cone_index_reflect_df) 

summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_index_df, aes(x = mean_norm_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones) + 
  annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.085, y = 16300, label = paste("p =",p_val)) +
  ggthemes::theme_few() 

# ggscatter(cone_index_df_tree, x = "mean_norm_index", y = "total_cones", add = "reg.line") +
#   stat_cor(label.x = -0.10, label.y = 17000) +
#   stat_regline_equation(label.x = -0.10, label.y = 16500)

# ggplot(cone_index_df_tree, aes(x = mean_norm_index, y = total_cones)) +
#   geom_point(alpha = 0.5) +
#   geom_label(label = cone_index_df_tree$photo) +
#   xlab("spectral index") + ylab(total~cones~(cones/g)) +
#   ggthemes::theme_few()

ggplot(cone_index_reflect_df, aes(x = mean_norm_index, y = total_cones)) +
  geom_point(alpha = 0.5) +
  geom_label(label = paste(cone_index_df$site, cone_index_df$tree, sep = " ")) +
  ggtitle("tree quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

ggplot(cone_index_reflect_df, aes(x = mean_norm_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE, aes(col = site)) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones) + 
  ggthemes::theme_few() 

mod <- lm(mean_new_index ~ total_cones, data = cone_index_reflect_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_index_reflect_df, aes(x = mean_new_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index ((R-G)/(R/G))") + ylab(total~cones) + 
  annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.085, y = 16300, label = paste("p =",p_val)) +
  ggthemes::theme_few() 

mod <- lm(mean_new_norm_index ~ total_cones, data = cone_index_reflect_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_index_reflect_df, aes(x = mean_new_norm_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones) + 
  annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.085, y = 16300, label = paste("p =",p_val)) +
  ggthemes::theme_few()

ggplot(cone_index_reflect_df, aes(x = mean_new_norm_index, y = total_cones)) +
  geom_point(alpha = 0.5) +
  geom_label(label = paste(cone_index_df$site, cone_index_df$tree, sep = " ")) +
  ggtitle("tree quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

mod <- lm(reflect_new_index ~ total_cones, data = cone_index_reflect_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 10)

ggplot(cone_index_reflect_df, aes(x = reflect_new_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones) + 
  annotate("text", x = -0.02, y = 20000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.02, y = 19300, label = paste("p =",p_val)) +
  ggthemes::theme_few()

mod <- lm(reflect_old_index ~ total_cones, data = cone_index_reflect_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

mod <- lm(reflect_new_index_norm ~ total_cones, data = cone_index_reflect_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 10)

ggplot(cone_index_reflect_df, aes(x = reflect_new_index_norm, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones) + 
  annotate("text", x = -0.065, y = 20000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.065, y = 19300, label = paste("p < 0.01")) +
  ylim(0,20000) +
  ggthemes::theme_few()

ggplot(cone_index_reflect_df, aes(x = reflect_new_index_norm, y = total_cones)) + 
  geom_point(alpha = 0.5, aes(col = site)) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE, aes(col = site)) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones) + 
  ylim(0,20000) + 
  ggthemes::theme_few() 

ggplot(cone_index_reflect_df, aes(x = reflect_new_index_norm, y = total_cones)) +
  geom_point(alpha = 0.5) +
  geom_label(label = paste(cone_index_df$site, cone_index_df$tree, sep = " ")) +
  ggtitle("tree quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()


#looking at outliers

img <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/goodguys_t3_tree.tif")
plotRGB(img)
plot(img)

# cone_index_df_test <- cone_index_reflect_df |>
#   filter(!photo %in% c("sweeten_t4_tree",
#                        "sweeten_t10_tree",
#                        "sweeten_t5_tree",
#                        "kimble_t1_tree"))

cone_index_df_test <- cone_index_reflect_df |>
  filter(!photo %in% c("sweeten_t4_tree",
                       "sweeten_t10_tree",
                       "kimble_t8_tree",
                       "kimble_t1_tree",
                       "kimble_t12_tree"))

mod <- lm(reflect_new_index ~ total_cones, data = cone_index_df_test) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

#t test to see if index values between males + females differ
female_index_df <- cone_index_reflect_df |> 
  filter(tree == "female")

male_index_df <- cone_index_reflect_df |> 
  filter(tree != "female")

t.test(female_index_df$reflect_new_index, male_index_df$reflect_new_index)

#visualizing index on close up pics of imgs

close_img <- rast("tx 2026 drone pics/2026 quadrat pics/handheld_drone_pics/cropped_handheld_drone_pics/gun_t1.tif")
close_img <- rast("tx 2026 drone pics/2026 quadrat pics/handheld_drone_pics/cropped_handheld_drone_pics/fisher_t6.tif")
plotRGB(close_img)

names(close_img) <- c("r", "g", "b")
close_img$index <- (close_img$r - close_img$g)/(close_img$r / close_img$g)
plot(close_img$index)

# old code exploring img adjustments --------------------------------------



p_unfilt_unadj <- cone_index_df |> 
  filter(adjustment %in% "unadj",
         rf_classification %in% "unfilt") |> 
  ggplot(aes(x = mean_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("unfiltered + unadjusted quadrat images") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

p_unfilt_adj <- cone_index_df |> 
  filter(adjustment %in% "adj",
         rf_classification %in% "unfilt") |> 
  ggplot(aes(x = mean_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("unfiltered + adjusted quadrat images") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

p_filt_unadj <- cone_index_df |> 
  filter(adjustment %in% "unadj",
         rf_classification %in% "filt") |> 
  ggplot(aes(x = mean_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("filtered + unadjusted quadrat images") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

p_filt_adj <- cone_index_df |> 
  filter(adjustment %in% "adj",
         rf_classification %in% "filt") |> 
  ggplot(aes(x = mean_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("filtered + adjusted quadrat images") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

cone_index_df |> 
  filter(quadrat_location == "ground",
         adjustment == "unadj") |> 
  ggplot(aes(x = mean_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("ground quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

cone_index_df |> 
  filter(quadrat_location == "tree",
         adjustment == "unadj") |> 
  ggplot(aes(x = mean_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("tree quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

cone_index_df |> 
  filter(quadrat_location == "tree",
         adjustment == "unadj") |> 
  ggplot(aes(x = mean_norm_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("tree quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

cone_index_df_label <- cone_index_df|> 
  filter(quadrat_location == "tree",
         adjustment == "unadj") 

p_label <- ggplot(cone_index_df_label, aes(x = mean_norm_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_label(label = paste(cone_index_df_label$site, cone_index_df_label$tree, sep = " ")) +
  ggtitle("tree quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  theme_classic()

ggplot(cone_index_df, aes(x = mean_index, y = total_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_label(label = cone_index_df$photo) +
  xlab("spectral index") + ylab(cone~density~(cones/g)) +
  ggthemes::theme_few()
  

# ggplot(cone_index_df, aes(x = mean_index, y = total_cones_per_g)) + 
#   geom_point(alpha = 0.5) + 
#   geom_label(label = cone_index_df$photo) +
#   xlab("spectral index") + ylab(cone~density~(cones/g)) + 
#   ggthemes::theme_few()

wade_t3 <- stack("~/Desktop/2026 quadrat pics/cropped quadrat pics/wade_3.tif")
plotRGB(wade_t3)

names(wade_t3) <- c("r", "g", "b")
wade_t3$index <- (wade_t3$r-wade_t3$g)/(wade_t3$r + wade_t3$g)
plot(wade_t3$index)



img <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/cathedral_t3_tree.tif")
plotRGB(img)

names(img) <- c("r", "g", "b", "rf")
img$index <- (img$r-img$g)/(img$r + img$g)
plot(img$index)

mean_index <- print(round(cellStats(img$index, "mean"), 5)) 


img <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t7_tree.tif")
plot(img)
plotRGB(img)

names(img) <- c("r", "g", "b", "rf_mask")

img_filt <- mask(img, img$rf_mask, maskvalue = 2, inverse = TRUE)
plotRGB(img_filt)

index <- overlay(img_filt$r, img_filt$g,
                 fun = function(r, g) (r - g)/(r + g))


plot(index, col = cm.colors(n = 10))

cellStats(index, stat = "mean")

# ndyi_index <- overlay(img_filt$g, img_filt$b,
#                       fun = function(g, b) (g - b)/(g + b))
# 
# plot(ndyi_index, col = cm.colors(n = 10))

#looking at adjusted image

img_adj <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t7_tree_adj.tif")
plot(img_adj)
plotRGB(img_adj)

names(img_adj) <- c("r", "g", "b", "rf_mask")

img_adj_filt <- mask(img_adj, img_adj$rf_mask, maskvalue = 2, inverse = TRUE)
plotRGB(img_adj_filt)

index_adj <- overlay(img_adj_filt$r, img_adj_filt$g,
                 fun = function(r, g) (r - g)/(r + g))


plot(index_adj, col = cm.colors(n = 10))

cellStats(index_adj, stat = "mean")


#trying patchwork 

img <- rast("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t7_tree.tif")
plot(img)
plotRGB(img)

names(img) <- c("r", "g", "b", "rf_mask")

img_filt <- mask(img, img$rf_mask, maskvalue = 2, inverse = TRUE)
plotRGB(img_filt)

p_rgb_filt <- ggplot() +
  geom_spatraster_rgb(data = img_filt) +
  ggtitle("masked aerial drone image") + 
  theme_void()
  
index <- (img_filt$r - img_filt$g)/(img_filt$r + img_filt$g)


p_index <- ggplot() + 
  geom_spatraster(data = index) + 
  scale_fill_hypso_c(palette = "arctic_hypso") + 
  ggtitle("index applied to drone image") +
  labs(fill = "index value") +
  theme_void() 

index_df <- as.data.frame(index)

p_hist <- ggplot(index_df) +
  geom_histogram(aes(x = r)) +
  labs(x = "index value") + 
  theme_classic()

(p_rgb_filt + p_index) / p_hist

global(index, fun = "mean")


#look at RGB histograms between cone and not gone



