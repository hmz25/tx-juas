#script to explore various methods for applying color corrections to quadrat images 

library(raster)
library(terra)
library(dplyr)
library(tidyverse)
library(colorRamps)

img <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t7_tree.tif")
plotRGB(img)
plot(img)
plot((img$wade_t7_tree_1-img$wade_t7_tree_2)/(img$wade_t7_tree_1+img$wade_t7_tree_2), zlim = c(-0.2,0.2))

img_df <- as.data.frame(img) |> 
  rename(r = 1,
         g = 2,
         b = 3,
         rf_mask = 4) |> 
  mutate(r_norm = r/(r+g+b),
         g_norm = g/(r+g+b),
         b_norm = b/(r+g+b),
         r_scale = (r-min(r))/(max(r)-min(r)),
         g_scale = (g-min(g))/(max(g)-min(g)),
         b_scale = (b-min(b))/(max(b)-min(b)))

# Start from original raster template
r_template <- img[[1]]

# Create new raster layers
r_rast <- setValues(r_template, img_df$r_norm)   
g_rast <- setValues(r_template, img_df$g_norm)
b_rast <- setValues(r_template, img_df$b_norm)

# Stack them
norm_stack <- stack(r_rast, g_rast, b_rast)

# Plot
plotRGB(norm_stack, r = 1, g = 2, b = 3, scale = 1)
plot((norm_stack$wade_t7_tree_1.1-norm_stack$wade_t7_tree_1.2)/(norm_stack$wade_t7_tree_1.1+norm_stack$wade_t7_tree_1.2), zlim = c(-0.2,0.2))

# Start from original raster template
r_template <- img[[1]]

# Create new raster layers
r_rast <- setValues(r_template, img_df$r_scale)   
g_rast <- setValues(r_template, img_df$g_scale)
b_rast <- setValues(r_template, img_df$b_scale)

# Stack them
scale_stack <- stack(r_rast, g_rast, b_rast)

# Plot
plotRGB(scale_stack, r = 1, g = 2, b = 3, scale = 1)
plot((scale_stack$wade_t7_tree_1.1-scale_stack$wade_t7_tree_1.2)/(scale_stack$wade_t7_tree_1.1+scale_stack$wade_t7_tree_1.2), zlim = c(-0.2,0.2))


img_index_df <- img_df |> 
  summarize(r_mean = mean(r),
            g_mean = mean(g),
            b_mean = mean(b),
            r_norm_mean = mean(r_norm),
            g_norm_mean = mean(g_norm),
            b_norm_mean = mean(b_norm),
            r_scale_mean = mean(r_scale),
            g_scale_mean = mean(g_scale),
            b_scale_mean = mean(b_scale)) |> 
  mutate(rg_index = (r_mean-g_mean)/(r_mean+g_mean),
         rg_norm_index = (r_norm_mean-g_norm_mean)/(r_norm_mean+g_norm_mean),
         rg_scale_index = (r_scale_mean-g_scale_mean)/(r_scale_mean+g_scale_mean))

plot()


# analyze how grey card images change within and across flights -----------

setwd("C:/Users/hmz25/Box/")

#path to filtered quadrat images
img_folder <- "Katz lab/texas/tx 2026 drone pics/grey card imgs"
img_list <- list.files(img_folder, full.names = FALSE, pattern = ".tif")
img_list_dir <- list.files(img_folder, full.names = TRUE, pattern = ".tif")

grey_card_px_df <- data.frame()

# i = 1

for (i in seq_along(img_list)) {
  #remove the file extension to use as object name
  file_name <- tools::file_path_sans_ext(img_list[i])
  
  #extract site name (everything before "_t")
  # site <- str_extract(file_name, "^[^_]+")
  site <- str_split_i(file_name, "_", 1)
  
  #extract flight date 
  date <- str_split_i(file_name, "_", 2)
  
  #extract tree number (digits after "t")
  tree <- str_split_i(file_name, "_", 3)
  
  #load the raster
  photo_i <- rast(img_list_dir[i])
  # plotRGB(photo_i)
  
  #rename raster columns 
  names(photo_i) <- c("r", "g", "b")
  
  #convert to data frame
  photo_i_df <- as.data.frame(photo_i, na.rm = FALSE) #keep NA values 
  
  #add site, tree, photo ID columns to df 
  photo_i_df$photo <- file_name
  photo_i_df$site <- site
  photo_i_df$date <- date
  photo_i_df$tree <- tree
  
  #add to data frame of all pixels in each quadrat 
  grey_card_px_df <- rbind(grey_card_px_df, photo_i_df)
  
  print(i) 
}

grey_card_px_df

write_csv(grey_card_px_df, "Katz lab/texas/grey_card_px_df.csv")

grey_card_px_mean_df <- grey_card_px_df |> 
  group_by(site, date) |> 
  summarize(mean_r_grey = mean(r),
         mean_g_grey = mean(g),
         mean_b_grey = mean(b))

grey_card_px_df |> 
  pivot_longer(cols = c("r":"b"), names_to = "band", values_to = "dn") |> 
  ggplot() +
  geom_boxplot(aes(x = band, y = dn, col = date)) +
  facet_wrap(~site) + 
  theme_minimal()

# grey_card_px_df_clean <- grey_card_px_df |> 
#   group_by(site, date, tree) |> 
#   summarize(mean_r = mean(r),
#             mean_g = mean(g),
#             mean_b = mean(b)) |> 
#   mutate(mean_index = (mean_r - mean_g)/(mean_r + mean_g))

#analyze how index values change across grey card values (normalized and not)
grey_card_px_df_clean <- grey_card_px_df |>
  mutate(index = (r-g)/(r+g),
         r_norm = r/(r+g+b),
         g_norm = g/(r+g+b),
         b_norm = b/(r+g+b),
         index_norm = (r_norm-g_norm)/(r_norm+g_norm)) |> 
  group_by(site, date, tree) |> 
  summarize(r_mean = mean(r),
            g_mean = mean(g),
            b_mean = mean(b),
            r_norm_mean = mean(r_norm),
            g_norm_mean = mean(g_norm),
            b_norm_mean = mean(b_norm)) |> 
  mutate(index_mean = (r_mean - g_mean)/(r_mean + g_mean),
         index_norm_mean = (r_norm_mean - g_norm_mean)/(r_norm_mean + g_norm_mean))

#visualize how grey card values vary across flights 
grey_card_px_df_viz <- grey_card_px_df |> 
  mutate(index = (r-g)/(r+g)) |> 
  pivot_longer(c("r","g","b","index"), names_to = "band", values_to = "value")

grey_card_px_df_viz |> 
  filter(band != "index") |> 
  mutate(band = factor(band, c("r","g","b"))) |> 
  ggplot() +
  geom_boxplot(aes(x = site, y = value, col = band)) +
  ggtitle("grey card rgb values across flights")

grey_card_px_df_viz |> 
  filter(band == "index") |> 
  ggplot() +
  geom_boxplot(aes(x = site, y = value)) +
  ggtitle("grey card index values across flights")

grey_card_px_df_viz |> 
  filter(band != "index") |> 
  mutate(band = factor(band, c("r","g","b"))) |> 
  ggplot() +
  geom_boxplot(aes(x = tree, y = value, col = band)) +
  facet_wrap(~site) +
  ggtitle("Fig 1: grey card rgb values within flights")

grey_card_px_df_norm_viz <- grey_card_px_df |> 
  mutate(r_norm = r/(r+g+b),
         g_norm = g/(r+g+b),
         b_norm = b/(r+g+b)) |> 
  mutate(index_norm = (r_norm-g_norm)/(r_norm+g_norm)) |> 
  pivot_longer(c("r_norm","g_norm","b_norm","index_norm"), names_to = "band", values_to = "value")

grey_card_px_df_norm_viz |> 
  filter(band != "index_norm") |> 
  mutate(band = factor(band, c("r_norm","g_norm","b_norm"))) |> 
  ggplot() +
  geom_boxplot(aes(x = tree, y = value, col = band)) +
  facet_wrap(~site) +
  ggtitle("Fig 2: normalized grey card rgb values within flights")

grey_card_px_df_norm_viz |> 
  filter(band == "index_norm") |> 
  ggplot() +
  geom_boxplot(aes(x = site, y = value)) +
  ggtitle("normalized grey card index values across flights")

grey_card_px_df_clean |> 
  pivot_longer(cols = c("r_mean":"index_norm_mean"), names_to = "band", values_to = "value") |> 
  filter(band == "index_mean") |> 
  ggplot() +
  geom_boxplot(aes(x = site, y = value))

grey_card_px_df_clean |> 
  pivot_longer(cols = c("r_mean":"index_norm_mean"), names_to = "band", values_to = "value") |> 
  filter(band == "index_norm_mean") |> 
  ggplot() +
  geom_boxplot(aes(x = site, y = value)) +
  ggtitle("normalized index values across flights")

grey_card_px_df_clean |> 
  pivot_longer(cols = c("r_mean":"index_norm_mean"), names_to = "band", values_to = "value") |> 
  filter(band == c("index_mean", "index_norm_mean")) |> 
  ggplot() +
  geom_boxplot(aes(x = site, y = value, col = band)) +
  ggtitle("Fig 3: normalized index values across flights")


# grey card correction for quadrat images ---------------------------------

#join grey card df and quadrat img data frames 

#calculate mean of each band on each flight date in quadrat pictures
quadrat_px_df_clean <- quadrat_px_df |> 
  #limit analysis to cone/foliage pixels and on-tree quadrat images 
  filter(rf_mask == 2,
         quadrat_location == "tree",
         adjustment == "unadj") |>
  group_by(site, tree) |>
  #take mean of RGB bands for analysis
  summarize(r_mean = mean(r),
            g_mean = mean(g),
            b_mean = mean(b))

#take the mean of each RGB band in the grey card images
grey_card_px_mean_df <- grey_card_px_df |> 
  group_by(site, tree) |> 
  summarize(r_grey_mean = mean(r),
            g_grey_mean = mean(g),
            b_grey_mean = mean(b))

#join the grey card pixel values with the quadrat RGB values 
grey_quadrat_px_df_join <- quadrat_px_df_clean |> 
  right_join(grey_card_px_mean_df, by = c("site", "tree"))

#calculate reflectance-like values for each band 
reflectance_df <- grey_quadrat_px_df_join |> 
  mutate(r_reflectance = (0.18/r_grey_mean)*(r_mean),
         g_reflectance = (0.18/g_grey_mean)*(g_mean),
         b_reflectance = (0.18/b_grey_mean)*(b_mean))

#calculate index values
reflectance_index_df <- reflectance_df |> 
  mutate(old_index = (r_reflectance - g_reflectance)/((r_reflectance + g_reflectance)),
         new_index = ((r_reflectance - g_reflectance)/((r_reflectance/g_reflectance))))

#join reflectance index with cone density df 
cone_reflectance_df <- reflectance_index_df |> 
  mutate(tree = as.double(str_extract(tree, "(?<=t).*")),
         site = substr(site, 1, 4)) |> 
  left_join(cone_density_df, by = c("site", "tree")) |> 
  filter(quadrat_location == "tree")


mod <- lm(new_index ~ total_cones, data = cone_reflectance_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(cone_reflectance_df, aes(x = old_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("correlation of index values with cone estimates from quadrats on tree") + 
  xlab("spectral index") + ylab(total~cones~(cones/g)) # + 
  # annotate("text", x = -0.085, y = 17000, label = paste("R² =",r_sq)) +
  # annotate("text", x = -0.085, y = 16400, label = paste("p-val =",p_val)) +
  # ggthemes::theme_few() 

#look at applying correction to quadrat images 
wade_t7 <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t7_tree.tif")
plotRGB(wade_t7)

rf_mask <- wade_t7$wade_t7_tree_4

wade_t7_filt <- mask(wade_t7, rf_mask, maskvalue = 1)

plotRGB(wade_t7_filt)

names(wade_t7_filt) <- c("r", "g", "b", "rf_mask")

wade_t7_filt$r_reflec <- ((0.18/221.6103)*wade_t7_filt$r)
# plot(wade_t7_filt$r_reflec)
# plot(wade_t7_filt$r)

wade_t7_filt$g_reflec <- ((0.18/231.6000)*wade_t7_filt$g)
wade_t7_filt$b_reflec <- ((0.18/236.7846)*wade_t7_filt$b)

# wade_t7_filt$index <- (wade_t7_filt$r_reflec - wade_t7_filt$g_reflec)/(wade_t7_filt$r_reflec/wade_t7_filt$g_reflec)
wade_t7_filt$index <- (wade_t7_filt$r_reflec - wade_t7_filt$g_reflec)/(wade_t7_filt$r_reflec+wade_t7_filt$g_reflec)
plot(wade_t7_filt$index)
cellStats(wade_t7_filt$index, stat = "mean")

plot(wade_t7_filt$index, col = colorRamps::green2red(10))

# wade_t7_filt_index <- wade_t7_filt$r/wade_t7_filt$g
# wade_t7_filt_index <- (wade_t7_filt$r - wade_t7_filt$g - wade_t7_filt$b)/(wade_t7_filt$r / wade_t7_filt$g)
wade_t7_filt_index <- (wade_t7_filt$r - wade_t7_filt$g)/(wade_t7_filt$r/wade_t7_filt$g)

plot(wade_t7_filt_index)