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
library(ggpubr)

# #lab desktop
# setwd("C:/Users/hmz25/Box/Katz lab/texas")

#hz laptop
setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")


# manual cone data processing ---------------------------------------------

#load in cone count data 
cone_df <- read_csv("01_data/cone processing 26 - counts.csv")

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

# #look at cone density between on-tree v on-ground quadrats
# cone_df_clean |> 
#   group_by(site, tree, quadrat_location) |> 
#   ggplot() + 
#   geom_boxplot(aes(x = factor(tree), y = count/weight, col = quadrat_location)) +
#   facet_wrap(~site)

# cone_density_df |> 
#   filter(quadrat_location == "tree") |> 
#   View()

# quadrat image processing ------------------------------------------------

#load in data frame with pixels values from each quadrat
# created in script "extract_quadrat_DNs"
quadrat_px_df <- read_csv("03_output/quadrat_px_df.csv")

quadrat_px_df #result is data frame with all pixel values from each quadrat 
#rf_mask values = 1 is not cone/fol, 2 is cone/fol 

#apply gamma correction (decoding) to account for non-linearity in JPEG formats
#we are "decoding" gamma so DNs reflect true linear light proportions
#gamma correction: https://en.wikipedia.org/wiki/Gamma_correction 

# test <- read_exif("tx 2026 drone pics/2026 quadrat pics/DJI_20260122121727_0323_V.JPG")
# unique(colnames(test))
# str(test)
# grep("Ga", names(test), value = TRUE, ignore.case = TRUE) #gamma not provided in EXIF info 

#DJI does not provide gamma (proprietary), so use gamma value associated with JPEG

gam <- 2.2

quadrat_px_df_gam <- quadrat_px_df |>
  #linearize DNS by bit-depth scaling and raising to the power of 1/gamma 
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
  mutate(r_norm_gam_8bit = r_norm_gam*255,
         g_norm_gam_8bit = g_norm_gam*255,
         b_norm_gam_8bit = b_norm_gam*255)

#take mean of rgb values, then calculate the index
index_df <- quadrat_px_df_gam |>
  filter(rf_mask == 2,
         adjustment == "unadj") |>
  group_by(photo, site, tree, quadrat_location) |>
  summarize(r_mean = mean(r),
            g_mean = mean(g),
            b_mean = mean(b),
            r_norm_gam_mean = mean(r_norm_gam),
            g_norm_gam_mean = mean(g_norm_gam),
            b_norm_gam_mean = mean(b_norm_gam),
            r_norm_gam_8bit_mean = mean(r_norm_gam_8bit),
            g_norm_gam_8bit_mean = mean(g_norm_gam_8bit),
            b_norm_gam_8bit_mean = mean(b_norm_gam_8bit)) |>
  group_by(photo, site, tree, quadrat_location, across(r_mean:b_norm_gam_8bit_mean)) |>
  summarize(mean_norm_gam_index = (r_norm_gam_mean - g_norm_gam_mean)/(r_norm_gam_mean/g_norm_gam_mean),
            mean_norm_gam_8bit_index = (r_norm_gam_8bit_mean - g_norm_gam_8bit_mean)/(r_norm_gam_8bit_mean/g_norm_gam_8bit_mean)) |>
  mutate(site = substr(site, 1, 4),
         tree = if_else(tree == "female", tree, str_remove(tree, "^t")))


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
  drop_na(mean_norm_gam_index) |> 
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

#model for index using bit-scaled index values 
mod <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = cone_index_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

#trying logistic fit
cone_index_df_logit <- cone_index_df |> 
  #change y var into proportion for logistic fit
  mutate(mean_cones_per_g_prop = mean_cones_per_g/(max(cone_index_df$mean_cones_per_g)),
         mean_norm_gam_index_rescale = (mean_norm_gam_index - min(cone_index_df$mean_norm_gam_index))/
           (max(cone_index_df$mean_norm_gam_index) - min(cone_index_df$mean_norm_gam_index)))

#fit model where Y is continuous proportion and X is continuous
# mod_quasi <- glm(mean_norm_gam_index_rescale ~ mean_cones_per_g, data = cone_index_df_logit, 
#                  family = quasibinomial(link = "logit"))

mod_logit <- glm(mean_norm_gam_index_rescale ~ mean_cones_per_g, data = cone_index_df_logit, 
                 family = "binomial")

summary(mod_logit)

AIC(mod, mod_logit)

# #model for index calculated with 8bit gamma corrected + normalized bands
# #gives same result :-)
# mod <- lm(mean_norm_gam_8bit_index ~ mean_cones_per_g, data = cone_index_df) 
# summary(mod)
# r_sq <- round(summary(mod)$r.sq, 3)
# p_val <- round(coef(summary(mod))[2,4], 4)


ggplot(cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index ((R-G)/(R/G))") + ylab("cones density (cones/g") + 
  annotate("text", x = -0.085, y = 50, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.085, y = 50, label = paste("p =",p_val)) +
  ggthemes::theme_few() 

ggplot(cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  annotate("text", x = -0.05, y = 70, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.051, y = 66, label = paste("p < 0.01")) +
  ggthemes::theme_few()

# #plot to identify quadrats that are outliers on the plt
# ggplot(cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g)) +
#   geom_point(alpha = 0.5) +
#   geom_label(label = paste(cone_index_df$site, cone_index_df$tree, sep = " ")) +
#   ggtitle("tree quadrat images index") +
#   xlab("spectral index") + ylab(cone~density~(cones/g)) +
#   theme_classic()

#trying to fit logistic model 
# # could change data to be bound by max number (divide cones/g by max cones/g then fit logistic)
# mod_logistic <- glm(mean_norm_gam_index ~ mean_cones_per_g, data = cone_index_df, family = "binomial")
# summary(mod_logistic)


#correlation of index values with on-tree quadrat cone density


# other random analyses ---------------------------------------------------

#t test to see if index values between males + females differ
female_index_df <- cone_index_df |> 
  filter(tree == "female")

male_index_df <- cone_index_df |> 
  filter(tree != "female")

t.test(female_index_df$mean_new_index, male_index_df$mean_new_index)

## visualizing index on close up pics of imgs -----------------------------------------------------------------

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

## examine how much variation there is between female spectral index values ---------------------------------------------------

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

## how sky conditions (my obs) impact spectral index ---------------------------------------------------

sky_df <- read_csv("01_data/2026 TX drone pics metadata - sky conditions.csv")

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

mod_cloud <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = cloud_cone_index_df)
summary(mod_cloud)
r_sq_cloud <- round(summary(mod_cloud)$r.sq, 2)
# p_val <- round(coef(summary(mod_cloud))[2,4], 4)

sun_cone_index_df <- sky_cone_index_df |> 
  filter(condition == "sunny")

mod_sun <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = sun_cone_index_df)
summary(mod_sun)

mod_mixed <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = sky_cone_index_df)
summary(mod_mixed)

#plot
ggplot(sky_cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g, col = condition)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = F) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates based on weather") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  annotate("text", x = -0.06, y = 70) +
  ggthemes::theme_few()

#generating plot with R² for all conditions

#calculate R² for each condition
r2_labels <- sky_cone_index_df |>
  filter(condition != "mixed") |>          # exclude mixed from grouped R²
  group_by(condition) |>
  summarise(
    r2 = summary(lm(mean_norm_gam_index ~ mean_cones_per_g))$r.squared
  ) |>
  bind_rows(
    tibble(
      condition = "mixed",
      r2 = summary(lm(mean_norm_gam_index ~ mean_cones_per_g, data = sky_cone_index_df))$r.squared
    )
  ) |>
  mutate(
    label = paste0(condition, ": R² = ", round(r2, 2)),
    x = -0.040,
    y = seq(70, 70 - (n() - 1) * 3, by = -3)
  )

#plot
ggplot(sky_cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g, col = condition)) +
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

#try to add sky conditions as a fixed effect into the model
mod_cc <- lm(mean_norm_gam_index ~ mean_cones_per_g + factor(sky_cone_index_df$condition), data = sky_cone_index_df)
summary(mod_cc)
r_sq_fe <- round(summary(mod_cc)$r.sq, 2)

ggplot(sky_cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g, col = condition)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = F) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates based on sky conditions") + 
  xlab("spectral index") + 
  ylab("cone density (# cones/g)") + 
  annotate("text", x = -0.058, y = 70, label = paste0("R² = ",r_sq_fe,", p < 0.001")) +
  ggthemes::theme_few() + 
  stat_regline_equation(label.y = c(66, 63, 60), label.x = -0.068)  # one value per level of condition, in level order

## cloud cover analysis with NOAA RTMA data ---------------------------------------------------

#read in cloud cover percentage data (extracted using google earth engine, code here: )
cc_df <- read_csv("01_data/cloud_cover_perc_2026.csv")
# head(cc_df)
# str(cc_df)

cc_df_clean <- cc_df |> 
  mutate(site = trimws(tolower(substr(site, 1,4))), #match site names with index_df naming convention
         time = hms::as_hms(time_utc - (6*3600)), #convert to DJI timestamp (TX is UTC-6 in winter)
         date_collected = format(as.Date(date, "%Y-%m-%d"), "%m/%d/%Y")) #reformat date to match other df

# str(cc_df_clean)

#read in metadata for quadrat images to extract time the picture was taken
meta_df <- read_csv("01_data/2026 TX drone pics metadata - focal tree metadata.csv")

meta_df_clean <- meta_df |> 
  mutate(site = substr(site,1,4)) |> 
  rename(date_collected = date,
         tree = focal_tree_n) |> 
  select(site:picture_name) |> 
  mutate(time = substr(str_split_i(picture_name, "_", 2), 9, 14),
         tree = as.character(tree)) |> 
  mutate(time = hms::as_hms(sprintf("%s:%s:%s", 
                                    substr(time, 1, 2), 
                                    substr(time, 3, 4), 
                                    substr(time, 5, 6))))

cone_index_time_df <- cone_index_df |> 
  left_join(meta_df_clean, by = c("site", "date_collected", "tree")) |> 
  mutate(time = hms::round_hms(time, secs = 3600)) #convert time to match cloud cover 

# str(cone_index_time_df)

#create new data frame with sky conditions at time the picture was taken 
cc_cone_index_df <- cone_index_time_df |> 
  left_join(cc_df_clean, 
            by = join_by(site, date_collected, time)) |> 
  drop_na(cloud_cover)

hist(cc_cone_index_df$cloud_cover)

#include as fixed effect in model
mod_cc <- lm(mean_norm_gam_index ~ mean_cones_per_g + cloud_cover, 
             data = cc_cone_index_df)

summary(mod_cc)

#create sunny, mixed, cloudy conditions variable
cc_cone_index_df <- cc_cone_index_df |> 
  mutate(condition = case_when(
    cloud_cover < 25  ~ "sunny",
    cloud_cover > 75  ~ "cloudy",
    TRUE    ~ "mixed"  # acts as the final "else"
  ))

#run model where factor(condition) is fixed effect
mod_cc <- lm(mean_norm_gam_index ~ mean_cones_per_g + factor(cc_cone_index_df$condition), data = cc_cone_index_df)
summary(mod_cc)

# sum(cc_cone_index_df$condition == "mixed") #9
# sum(cc_cone_index_df$condition == "cloudy") #13
# sum(cc_cone_index_df$condition == "sunny") #27

cloud_cone_index_df <- cc_cone_index_df |> 
  filter(condition == "cloudy")

mod_cloud <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = cloud_cone_index_df)
summary(mod_cloud)
r_sq_cloud <- round(summary(mod_cloud)$r.sq, 2)
# p_val <- round(coef(summary(mod_cloud))[2,4], 4)

sun_cone_index_df <- cc_cone_index_df |> 
  filter(condition == "sunny")

mod_sun <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = sun_cone_index_df)
summary(mod_sun)

mixed_cone_index_df <- cc_cone_index_df |> 
  filter(condition == "mixed")

mod_mixed <- lm(mean_norm_gam_index ~ mean_cones_per_g, data = mixed_cone_index_df)
summary(mod_mixed)

ggplot(cc_cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g, col = condition)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = F) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates based on weather") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  ggthemes::theme_few()

#calculate R² for each condition
r2_labels <- cc_cone_index_df |>
  filter(condition != "mixed") |>          # exclude mixed from grouped R²
  group_by(condition) |>
  summarise(
    r2 = summary(lm(mean_norm_gam_index ~ mean_cones_per_g))$r.squared
  ) |>
  bind_rows(
    tibble(
      condition = "mixed",
      r2 = summary(lm(mean_norm_gam_index ~ mean_cones_per_g, data = cc_cone_index_df))$r.squared
    )
  ) |>
  mutate(
    label = paste0(condition, ": R² = ", round(r2, 2)),
    x = -0.04,
    y = seq(70, 70 - (n() - 1) * 3, by = -3)
  )

#plot with R2 values
ggplot(cc_cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g, col = condition)) +
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

#facet wrap by condition
ggplot(cc_cone_index_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g)) + 
  facet_wrap(~condition) +
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = T) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates based on weather") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  ggthemes::theme_few()


# pheno analysis ----------------------------------------------------------

#join index df with df that has info about perc cones open
focal_pheno <- read_csv("01_data/FieldMaps data 2026/focal_trees_2026.csv")

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
  mutate(tree = as.character(tree)) |> 
  left_join(cone_index_df, by = c("site", "tree")) %>% 
  drop_na(mean_norm_gam_index) |> 
  #create column for percent open grouping
  mutate(pheno = case_when(
    percent_cones_open <= 24 ~ "0-24%",
    percent_cones_open <= 75 ~ "25–75%",
    percent_cones_open > 75  ~ "75-100%"
  ))

ggplot(index_pheno_df, aes(x = mean_norm_gam_index, y = mean_cones_per_g, col = pheno)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = F) +
  ylim(0,70) +
  ggtitle("correlation of index values with manual cone density estimates based on weather") + 
  xlab(expression(paste("spectral index  (", frac(R-G, R/G), ")"))) + 
  ylab("cone density (# cones/g)") + 
  ggthemes::theme_few()

#compute R² per group
r2_df_pheno <- index_pheno_df %>%
  group_by(pheno) %>%
  summarise(
    r2 = summary(lm(mean_norm_gam_index ~ mean_cones_per_g))$r.squared,
    x = max(mean_norm_gam_index, na.rm = TRUE),
    y = max(mean_cones_per_g, na.rm = TRUE)
  )

#plot with text labels
ggplot(index_pheno_df, aes(x = mean_norm_gam_index, 
                           y = mean_cones_per_g, 
                           color = pheno)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(data = r2_df_pheno,
            aes(x = -0.04, y = c(70,67,64), 
                label = paste0("R² = ", round(r2, 2))),
            hjust = 1, vjust = 1, size = 4) +
  theme_bw() +
  labs(col = "percent cones open")+ 
  xlab("spectral index") +
  ylab(expression(cone~density~(cones/g))) +
  theme_classic()

#looking at another viz
index_pheno_df |> 
  mutate(percent_cones_open = as.factor(percent_cones_open)) |> 
  ggplot() + 
  geom_boxplot(aes(x = percent_cones_open, y = mean_norm_gam_index)) +
  facet_wrap(~site) +
  theme_classic() +
  ylab("spectral index") + 
  xlab("percent cones open")

index_pheno_df |> 
  ggplot() + 
  geom_boxplot(aes(x = pheno, y = mean_norm_gam_index)) +
  facet_wrap(~site) +
  theme_classic() +
  ylab("spectral index") + 
  xlab("percent cones open")
