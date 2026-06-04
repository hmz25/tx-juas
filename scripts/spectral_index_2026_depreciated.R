#script to create index using 2026 drone images and manual cone count data

library(tidyverse)
library(dplyr)
library(terra)
library(stringr)
library(raster)
library(tidyterra)
library(ggplot2)
library(nnet)
library(lubridate)

#lab desktop
setwd("C:/Users/hmz25/Box/Katz lab/texas")

#hz laptop
setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")


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

#path to filtered quadrat images
img_folder <- "tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics"
img_list <- list.files(img_folder, full.names = FALSE, pattern = ".tif")
img_list_dir <- list.files(img_folder, full.names = TRUE, pattern = ".tif")

# test <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/gun_t10_tree.tif")
# plot(test)

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
  
  #determine if image has been WB adjusted 
  adjustment <- if_else(str_detect(file_name, "_adj"), "adj", "unadj")
  
  #load the image raster
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

#apply gamma correction 
# https://en.wikipedia.org/wiki/Gamma_correction 

# test <- read_exif("tx 2026 drone pics/2026 quadrat pics/DJI_20260122121727_0323_V.JPG")
# unique(colnames(test))
# str(test)
# 
# grep("Ga", names(test), value = TRUE, ignore.case = TRUE) #gamma not provided in EXIF info 

#raise pixel values to the power of 1/gamma 
#but first bit-depth scale
gam <- 2.2

quadrat_px_df_norm <- quadrat_px_df_norm |>
  #decode gamma so DNs reflect true linear light proportions
  mutate(r_lin = (r / 255)^(1/gam),
         g_lin = (g / 255)^(1/gam),
         b_lin = (b / 255)^(1/gam)) |>
  #make brightness invariant
  mutate(r_norm_gam = r_lin / (r_lin + g_lin + b_lin),
         g_norm_gam = g_lin / (r_lin + g_lin + b_lin),
         b_norm_gam = b_lin / (r_lin + g_lin + b_lin)) |> 
  #convert back to 8bit but not brightness invariant
  mutate(r_test = 255*(r/255)^(1/gam),
         g_test = 255*(g/255)^(1/gam),
         b_test = 255*(b/255)^(1/gam)) |> 
  #8bit and brightness invariant 
  mutate(r_norm_8bit = r_norm_gam*255,
         g_norm_8bit = g_norm_gam*255,
         b_norm_8bit = b_norm_gam*255)

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
            b_norm_mean = mean(b_norm),
            r_norm_gam_mean = mean(r_norm_gam),
            g_norm_gam_mean = mean(g_norm_gam),
            b_norm_gam_mean = mean(b_norm_gam)) |>
  group_by(photo, site, tree, quadrat_location, across(r_mean:b_norm_mean)) |>
  summarize(mean_index = (r_mean - g_mean)/(r_mean + g_mean),
            mean_norm_index = (r_norm_mean - g_norm_mean)/(r_norm_mean + g_norm_mean),
            mean_new_index = (r_mean - g_mean)/(r_mean/g_mean),
            mean_new_norm_index = (r_norm_mean - g_norm_mean)/(r_norm_mean/g_norm_mean),
            mean_norm_gam_index = (r_norm_gam_mean - g_norm_gam_mean)/(r_norm_gam_mean + g_norm_gam_mean),
            mean_new_norm_gam_index = (r_norm_gam_mean - g_norm_gam_mean)/(r_norm_gam_mean/g_norm_gam_mean)) |>
  mutate(site = substr(site, 1, 4),
         tree = if_else(tree == "female", tree, str_remove(tree, "^t")))

# index_df_test <- quadrat_px_df |> 
#   filter(rf_mask == 2,
#          adjustment == "unadj") |> 
#   group_by(photo, site, tree, quadrat_location) |> 
#   summarize(r_mean = mean(r),
#             g_mean = mean(g),
#             b_mean = mean(b)) |> 
#   group_by(photo, site, tree, quadrat_location, across(r_mean:b_mean)) |>
#   mutate(mean_norm_r = r_mean/(r_mean+g_mean+b_mean),
#          mean_norm_g = g_mean/(r_mean+g_mean+b_mean),
#          mean_norm_b = b_mean/(r_mean+g_mean+b_mean),
#          mean_index = (r_mean - g_mean)/(r_mean + g_mean),
#          mean_norm_index = (mean_norm_r - mean_norm_g)/(mean_norm_r + mean_norm_g),
#          mean_new_index = (r_mean - g_mean)/(r_mean/g_mean),
#          mean_new_norm_index = (mean_norm_r - mean_norm_g)/(mean_norm_r/mean_norm_g)) |>  
#   mutate(site = substr(site, 1, 4),
#          tree = if_else(tree == "female", tree, str_remove(tree, "^t")))


# combine quadrat pixel data with cone count data -------------------------

str(index_df)
str(cone_df_clean)

#create df with cone counts and pixel values 
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
         !photo %in% c("windmill_t1_tree", 
                       "celltower_t2_tree", 
                       "wade_t5_tree",
                       "gun_t5_tree",
                       "kimble_t2_tree",
                       "rocky_t3_tree",
                       "kimble_t2_tree")) #exclude excessively shaded quadrat imgs + use wade_t5_tree_2 in analysis


# visualize data ----------------------------------------------------------

#correlation of index values with on-tree quadrat cone density

#model for old normalized index 
mod <- lm(mean_norm_index ~ total_cones, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

mod <- lm(mean_norm_index ~ mean_cones_per_g, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_index_df, aes(x = mean_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones~(cones/g)) + 
  annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.085, y = 16400, label = paste("p-val =",p_val)) +
  ggthemes::theme_few() 

ggplot(cone_index_df, aes(x = mean_index, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(cone~density~(cones/g)) + 
  annotate("text", x = -0.085, y = 65, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.082, y = 62, label = paste("p-val =",p_val)) + 
  ggthemes::theme_few() 

#model for new index
mod <- lm(mean_new_index ~ total_cones, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

mod <- lm(mean_new_index ~ mean_cones_per_g, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_index_df, aes(x = mean_new_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index ((R-G)/(R/G))") + ylab(total~cones) + 
  annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.085, y = 16300, label = paste("p =",p_val)) +
  ggthemes::theme_few() 

#model for new index calculated with normalized bands
mod <- lm(mean_new_norm_index ~ total_cones, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

mod <- lm(mean_new_norm_index ~ mean_cones_per_g, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

#trying to fit logistic model 
mod_logistic <- multinom(mean_new_norm_index ~ mean_cones_per_g, data = cone_index_df, family = "binomial")
summary(mod_logistic)


# ggplot(cone_index_df, aes(x = mean_new_norm_index, y = total_cones)) + 
#   geom_point(alpha = 0.5) + 
#   theme_bw() + 
#   geom_smooth(method = "lm", se = TRUE) +
#   ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
#   xlab("spectral index") + ylab(total~cones) + 
#   annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
#   annotate("text", x = -0.085, y = 16300, label = paste("p =",p_val)) +
#   ggthemes::theme_few()
# 
# ggplot(cone_index_df, aes(x = mean_new_norm_index, y = total_cones)) +
#   geom_point(alpha = 0.5) +
#   geom_label(label = paste(cone_index_df$site, cone_index_df$tree, sep = " ")) +
#   ggtitle("tree quadrat images index") +
#   xlab("spectral index") + ylab(cone~density~(cones/g)) + 
#   theme_classic()

ggplot(cone_index_df, aes(x = mean_new_norm_index, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  annotate("text", x = -0.175, y = 70, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.178, y = 68, label = paste("p < 0.01")) +
  ggthemes::theme_few()

ggplot(cone_index_df, aes(x = mean_new_norm_index, y = mean_cones_per_g)) +
  geom_point(alpha = 0.5) +
  geom_label(label = paste(cone_index_df$site, cone_index_df$tree, sep = " ")) +
  ggtitle("tree quadrat images index") +
  xlab("spectral index") + ylab(cone~density~(cones/g)) +
  theme_classic()

#model for new index calculated with normalized + gamma corrected bands
mod <- lm(mean_new_norm_gam_index ~ total_cones, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

mod <- lm(mean_new_norm_gam_index ~ mean_cones_per_g, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

#model for old index calculated with normalized + gamma corrected bands
mod <- lm(mean_norm_gam_index ~ total_cones, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

mod <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)


# further filtering out non-foliage + cone pixels -------------------------

## for grey pixels, filter if R 70-206 and G 72-205 and B 69-188
## for shadow pixels, filter if R 22-55 and G 22-52 and B 0-19
## also filter if R and G above 225 and B above 210

#first visualize on a couple imgs 
# img <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t12_tree.tif")
# img <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/cathedral_t9_tree.tif")
img <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/fisher_t1_tree.tif")
plotRGB(img)

names(img) <- c("r", "g", "b", "rf")
# plot(img$rf)

# img_filt <- mask(img, img$rf, maskvalue = 2, inverse = TRUE)
# plotRGB(img_filt)

grey_mask <- img$r >= 159 & img$r <= 206 &
  img$g >= 164 & img$g <= 205 &
  img$b >= 141 & img$b <= 188

img[grey_mask, ] <- NA

plotRGB(img)

shadow_mask <- img$r >= 30 & img$r <= 55 &
  img$g >= 33 & img$g <= 52 &
  img$b >= 0 & img$b <= 19

img[shadow_mask, ] <- NA

plotRGB(img)

white_mask <- img$r >= 225 & img$g >= 225 & img$b >= 210 

img[white_mask, ] <- NA

plotRGB(img)

#then edit df to filter them out
quadrat_px_df_norm_filt <- quadrat_px_df_norm |>
  filter(
    # Remove brownish range
    !(r >= 159 & r <= 206 &
        g >= 164 & g <= 205 &
        b >= 141 & b <= 188),
    # Remove dark range
    !(r >= 30 & r <= 55 &
        g >= 33 & g <= 52 &
        b >=  0 & b <= 19),
    # Remove near-white range
    !(r >= 225 & g >= 225 & b >= 210)
  )


## for grey pixels, filter if R 70-206 and G 72-205 and B 69-188
## for shadow pixels, filter if R 22-55 and G 22-52 and B 0-19
## also filter if R and G above 225 and B above 210

# quadrat_px_df_norm_filt <- quadrat_px_df_norm |>
#   filter(
#     # filter grey rock pixels
#     !(r >= 70 & r <= 206 &
#         g >= 72 & g <= 205 &
#         b >= 69 & b <= 188),
#     # filter shadows
#     !(r >= 22 & r <= 55 &
#         g >= 22 & g <= 52 &
#         b >=  0 & b <= 19),
#     # filter over-exposed (white) pixels 
#     !(r >= 235 & g >= 235 & b >= 210)
#   )

index_df_filt <- quadrat_px_df_norm_filt |> 
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

cone_index_df_filt <- cone_density_df |> 
  mutate(tree = as.character(tree)) |> 
  full_join(index_df_filt, cone_density_df, by = c("site", "tree", "quadrat_location")) |> 
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
         !photo %in% c("windmill_t1_tree", 
                       "celltower_t2_tree", 
                       "wade_t5_tree",
                       "gun_t5_tree",
                       "kimble_t2_tree",
                       "rocky_t3_tree")) #exclude excessively shaded quadrat imgs + use wade_t5_tree_2 in analysis


#correlation of index values with on-tree quadrat cone density

#model for new normalized index 

mod <- lm(mean_new_norm_index ~ mean_cones_per_g, data = cone_index_df_filt) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_index_df_filt, aes(x = mean_new_norm_index, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  annotate("text", x = -0.175, y = 70, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.178, y = 68, label = paste("p < 0.01")) +
  ggthemes::theme_few()



# other random analyses ---------------------------------------------------

#t test to see if index values between males + females differ
female_index_df <- cone_index_df |> 
  filter(tree == "female")

male_index_df <- cone_index_df |> 
  filter(tree != "female")

t.test(female_index_df$mean_new_index, male_index_df$mean_new_index)

#visualizing index on close up pics of imgs

close_img <- rast("tx 2026 drone pics/2026 quadrat pics/handheld_drone_pics/cropped_handheld_drone_pics/gun_t1.tif")
close_img <- rast("tx 2026 drone pics/2026 quadrat pics/handheld_drone_pics/cropped_handheld_drone_pics/fisher_t6.tif")
plotRGB(close_img)

names(close_img) <- c("r", "g", "b")
close_img$index <- (close_img$r - close_img$g)/(close_img$r / close_img$g)
plot(close_img$index)

#using wade t7 for visualization
wade_t7 <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t7_tree.tif")
plotRGB(wade_t7)
names(wade_t7) <- c("r", "g", "b", "rf_mask")

#filter out pixels that aren't foliage or cones 
wade_t7_filt <- mask(wade_t7, wade_t7$rf_mask, maskvalue = 2, inverse = TRUE)
plotRGB(wade_t7_filt)

wade_t7_filt$r_norm <- wade_t7_filt$r/(wade_t7_filt$r + wade_t7_filt$g + wade_t7_filt$g)
wade_t7_filt$g_norm <- wade_t7_filt$g/(wade_t7_filt$r + wade_t7_filt$g + wade_t7_filt$g)
wade_t7_filt$b_norm <- wade_t7_filt$b/(wade_t7_filt$r + wade_t7_filt$g + wade_t7_filt$g)

wade_t7_filt$index <- (wade_t7_filt$r_norm - wade_t7_filt$g_norm)/(wade_t7_filt$r_norm/wade_t7_filt$g_norm)
plot(wade_t7_filt$index, axes = FALSE, box = FALSE, col = cm.colors(n = 10))

#examine how much variation there is between female spectral index values

female_index_df <- quadrat_px_df |> 
  filter(tree == "female",
         rf_mask == 2,
         adjustment == "unadj") |> 
  mutate(date = str_split_i(photo, pattern = "_", 2),
         tree_name = paste0(substr(site, 1, 4), substr(date, 5, 8)),
         r_norm = r/(r+g+b),
         g_norm = g/(r+g+b),
         b_norm = b/(r+g+b),
         norm_new_index = (r_norm - g_norm)/(r_norm/g_norm),
         norm_old_index = (r_norm - g_norm)/(r_norm + g_norm))

female_index_df |> 
  ggplot() +
  geom_boxplot(aes(x = tree_name, y = norm_new_index)) + 
  theme_minimal()

kimb_t14 <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/kimble_20260114_female.tif")
plotRGB(kimb_t14)

names(kimb_t14) <- c("r", "g", "b", "rf")
plot(kimb_t14$rf)

kimb_t14_filt <- mask(kimb_t14, kimb_t14$rf, maskvalue = 2, inverse = TRUE)
plotRGB(kimb_t14_filt)
kimb_t14$index <- (kimb_t14_filt$r - kimb_t14_filt$g)/(kimb_t14_filt$r/kimb_t14_filt$g)
plot(kimb_t14$index)

#another way 

#load in just foliage data

#path to filtered quadrat images
img_folder <- "tx 2026 drone pics/cone_v_fol"
img_list <- list.files(img_folder, full.names = FALSE, pattern = ".tif")
img_list_dir <- list.files(img_folder, full.names = TRUE, pattern = ".tif")

px_df <- data.frame()

# i = 1

for (i in seq_along(img_list)) {
  #remove the file extension to use as object name
  file_name <- tools::file_path_sans_ext(img_list[i])
  
  #extract site name (everything before "_t")
  site <- str_split_i(file_name, "_", 1)
  
  #extract tree number (digits after "t")
  tree <- str_split_i(file_name, "_", 2)
  
  #extract quadrat location
  cone_or_fol <- str_split_i(file_name, "_", 3)
  
  #load the raster
  photo_i <- stack(img_list_dir[i])
  # plotRGB(photo_i)
  
  #rename raster columns 
  names(photo_i) <- c("r", "g", "b")
  
  #convert to data frame
  photo_i_df <- as.data.frame(photo_i, na.rm = FALSE) #keep NA values 
  
  #add columns to df 
  photo_i_df$photo <- file_name
  photo_i_df$site <- site
  photo_i_df$tree <- tree
  photo_i_df$cone_or_fol <- cone_or_fol
  
  #add to data frame of all pixels in each quadrat 
  px_df <- rbind(px_df, photo_i_df)
  
  print(i) 
}

px_df #result is data frame with all pixel values from each photo 

#reformat df and filter for just foliage
fol_df <- px_df |> 
  filter(cone_or_fol == "fol")

#calculate index for each pixel
fol_index_df <- fol_df |> 
  mutate(r_norm = r/(r+g+b),
         g_norm = g/(r+g+b),
         b_norm = b/(r+g+b),
         norm_new_index = (r_norm - g_norm)/(r_norm/g_norm),
         norm_old_index = (r_norm - g_norm)/(r_norm + g_norm),
         site_tree = paste0(substr(site,1,4), tree))

#create boxplot or violin plot to see how much index values vary for foliage pixels
ggplot(fol_index_df) +
  geom_violin(aes(x = site_tree, y = norm_new_index))

fol_index_df |> 
  filter(!site_tree %in% "fisht5") |> 
  ggplot() +
  geom_boxplot(aes(x = site_tree, y = norm_old_index))

fol_index_df |> 
  filter(!site_tree %in% "fisht5") |> 
  ggplot() +
  geom_boxplot(aes(x = site_tree, y = norm_new_index))

#look at how sky conditions impact spectral index

sky_df <- read_csv("C:/Users/hmz25/Desktop/2026 TX drone pics metadata - sky conditions.csv")

sky_df_clean <- sky_df |> 
  mutate(site = substr(site, 1, 4)) |> 
  rename(date_collected = date)

sky_cone_index_df <- cone_index_df |> 
  left_join(sky_df_clean, by = c("site", "date_collected"))

# sum(sky_cone_index_df$condition == "mixed") #12
# sum(sky_cone_index_df$condition == "cloudy") #20
# sum(sky_cone_index_df$condition == "sunny") #32

cloud_cone_index_df <- sky_cone_index_df |> 
  filter(condition == "cloudy")

mod_cloud <- lm(mean_cones_per_g ~ mean_new_norm_index, data = cloud_cone_index_df)
summary(mod_cloud)
r_sq_cloud <- round(summary(mod_cloud)$r.sq, 2)
# p_val <- round(coef(summary(mod_cloud))[2,4], 4)

sun_cone_index_df <- sky_cone_index_df |> 
  filter(condition == "sunny")

mod_sun <- lm(mean_cones_per_g ~ mean_new_norm_index, data = sun_cone_index_df)
summary(mod_sun)

mod_mixed <- lm(mean_cones_per_g ~ mean_new_norm_index, data = sky_cone_index_df)
summary(mod_mixed)

ggplot(sky_cone_index_df, aes(x = mean_new_norm_index, y = mean_cones_per_g, col = condition)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = F) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates based on weather") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  annotate("text", x = -0.175, y = 70, label = paste("R² =",r_sq_cloud)) +
  ggthemes::theme_few()

# Calculate R² for each condition
r2_labels <- sky_cone_index_df |>
  filter(condition != "mixed") |>          # exclude mixed from grouped R²
  group_by(condition) |>
  summarise(
    r2 = summary(lm(mean_cones_per_g ~ mean_new_norm_index))$r.squared
  ) |>
  bind_rows(
    tibble(
      condition = "mixed",
      r2 = summary(lm(mean_cones_per_g ~ mean_new_norm_index, data = sky_cone_index_df))$r.squared
    )
  ) |>
  mutate(
    label = paste0(condition, ": R² = ", round(r2, 2)),
    x = -0.165,
    y = seq(70, 70 - (n() - 1) * 3, by = -3)
  )

# Plot
ggplot(sky_cone_index_df, aes(x = mean_new_norm_index, y = mean_cones_per_g, col = condition)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE) +
  ylim(0, 70) +
  geom_text(
    data = r2_labels,
    aes(x = x, y = y, label = label, col = condition),
    hjust = 1, size = 3.5, show.legend = FALSE
  ) +
  ggtitle("correlation of index values with manual cone density estimates based on weather") +
  xlab(expression(paste("spectral index  (", frac(R - G, R / G), ")"))) +
  ylab("cone density (# cones/g)") + 
  ggthemes::theme_few()

#cloud cover analysis with NOAA RTMA data

#read in cloud cover percentage data (extracted using google earth engine, code here: )
cc_df <- read_csv("cloud_cover_perc_2026.csv")
# head(cc_df)

cc_df_clean <- cc_df |> 
  mutate(site = tolower(substr(site, 1,4)), #match site names with index_df naming convention
         time = hms::as_hms(time_utc - (6*3600))) #convert to DJI timestamp (TX is UTC-6 in winter)
