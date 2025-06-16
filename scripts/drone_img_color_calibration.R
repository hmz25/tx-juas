#script for exploring how different lighting conditions/color adjustments impact pixel values


# analyzing white pixels from quadrat in drone flights --------------------

library(dplyr)
library(tidyverse)
library(terra)

#examine pixels in cropped white background of one pic
test_img <- rast("C:/Users/hmz25/Desktop/quadrat_lighting/wade_20250112_white.jpg")

names(test_img) <- c("r", "g", "b")

plotRGB(test_img)
str(test_img)
nrow(test_img)
ncol(test_img)

# test_img_sample <- spatSample(test_img, size = 1, method = "random")
# 
# print(test_img_sample)

hist(test_img)

#load in all pictures of cropped white parts of quadrats
img_dir <- "C:/Users/hmz25/Desktop/quadrat_lighting"
img_files_full <- list.files(img_dir, pattern = "_white.jpg", full.names = TRUE)

img_df_full <- data.frame()

for (i in seq_along(img_files_full)) {
  
  #load in image
  img <- rast(img_files_full[i])
  
  #rename channels to RGB
  names(img) <- c("r", "g", "b")
  
  #convert to data frame
  img_df <- as.data.frame(img)
  
  #add image filename as a column
  img_df$source <- basename(img_files_full[i])
  
  #add to cumulative data frame
  img_df_full <- bind_rows(img_df_full, img_df)
}

img_df_full

#plot to expore how histogram values differ between images
ggplot(img_df_full) +
  geom_histogram(aes(x = r)) +
  facet_wrap(~source) + 
  xlim(230,255) + 
  ylim(0,40) +
  theme_classic()

img_df_full %>% 
  pivot_longer(cols = c("r","g", "b"), names_to = "channel", values_to = "digital_number") %>% 
  ggplot() +
  geom_boxplot(aes(x = channel, y = digital_number)) +
  facet_wrap(~source) + 
  theme_minimal()

img_df_full %>% 
  ggplot() +
  geom_histogram(aes(x = (r-g)/(r+g))) + 
  facet_wrap(~source) + 
  theme_classic()

#visualize as each site histograms overlaying each other, facet wrapped by channel?
df_long <- pivot_longer(img_df_full, cols = c(r, g, b),
                        names_to = "channel", values_to = "digital_number")


#shadow pixel values vs non shadow pixel values from quadrats on the same day

shadow <- rast("C:/Users/hmz25/Desktop/backdrops/shadow quadrats/cath_20241230_t2_white.tif")
no_shadow <- rast("C:/Users/hmz25/Desktop/backdrops/cath_20241230_t4_white.tif")

plotRGB(shadow)
plotRGB(no_shadow)

names(shadow) <- c("r", "g", "b")
names(no_shadow) <- c("r", "g", "b")

shadow_df <- as.data.frame(shadow) %>% 
  mutate(source = "shadow") # %>% 
  # pivot_longer(cols = r:b, values_to = "digital_number", names_to = "channel")

no_shadow_df <- as.data.frame(no_shadow) %>% 
  mutate(source = "no_shadow") # %>% 
  # pivot_longer(cols = r:b, values_to = "digital_number", names_to = "channel")

df <- bind_rows(shadow_df, no_shadow_df)

ggplot(df) +
  geom_histogram(aes(x = digital_number, fill = channel), alpha = 0.5) + 
  facet_wrap(~source) +
  scale_fill_manual(values = c(r = "red",
                    g = "darkgreen",
                    b = "blue")) +
  theme_classic()

ggplot(df_index) +
  geom_histogram(aes(x = (r-g)/(r+g))) +
  facet_wrap(~source) + 
  theme_classic()

# group by dates/sites, look at histograms between dates/sites

dir <- "C:/Users/hmz25/Desktop/backdrops"
files_list <- list.files(dir, pattern = "_white.tif", full.names = TRUE)

df_full <- data.frame()

for (i in seq_along(files_list)) {
  
  #load in image
  img <- rast(files_list[i])
  
  #rename channels to RGB
  names(img) <- c("r", "g", "b")
  
  #convert to data frame
  img_df <- as.data.frame(img)
  
  #add image filename as a column
  img_df$source <- basename(files_list[i])
  
  #add to cumulative data frame
  df_full <- bind_rows(df_full, img_df)
}

df_full

df_full_restr <- df_full %>% 
  separate(source, into = c("site", "date", "tree", "drop"), sep = "_") %>%
  select(-drop) %>% 
  group_by(site, date) 

df_full_restr %>% 
  pivot_longer(cols = r:b, names_to = "channel", values_to = "digital_number") %>%  
  ggplot() +
  geom_histogram(aes(x = digital_number, fill = date), alpha = 0.5) + 
  facet_wrap(~channel)

df_full_restr %>% 
  pivot_longer(cols = r:b, names_to = "channel", values_to = "digital_number") %>%  
  ggplot() +
  geom_jitter(aes(x = date, y = digital_number, color = channel)) + 
  facet_wrap(~channel, ncol = 1) + 
  scale_color_manual(values = c(r = "red", g = "darkgreen", b = "blue")) +
  theme_classic()

#visualize how index value changes on white based on different flights
df_full_restr %>% 
  ggplot() +
  geom_jitter(aes(x = date, y = (r-g)/(r+g), color = date)) + 
  theme_classic()

df_full_restr %>% 
  ggplot() +
  geom_histogram(aes(x = (r-g)/(r+g))) +
  facet_wrap(~date, ncol = 1) +
  theme_classic()


#looking at how white values change between adjusted and non-adjusted images
adj_dir <- "C:/Users/hmz25/Desktop/quadrat_lighting/quadrat_white_ADJUSTED"
adj_files_full <- list.files(adj_dir, pattern = "_white.tif", full.names = TRUE)

adj_df_full <- data.frame()

for (i in seq_along(adj_files_full)) {
  
  #load in image
  adj_img <- rast(adj_files_full[i])
  
  #rename channels to RGB
  names(adj_img) <- c("r", "g", "b")
  
  #convert to data frame
  adj_img_df <- as.data.frame(adj_img)
  
  #add image filename as a column
  adj_img_df$source <- basename(adj_files_full[i])
  
  #add to cumulative data frame
  adj_df_full <- bind_rows(adj_df_full, adj_img_df)
}

adj_df_full

ggplot(adj_df_full) +
  geom_histogram(aes(x = r)) +
  facet_wrap(~source) + 
  xlim(230,255) + 
  ylim(0,40) +
  theme_minimal()

adj_df_full %>% 
  ggplot() +
  geom_histogram(aes(x = (r-g)/(r+g))) + 
  facet_wrap(~source) + 
  theme_classic()


# examine how canopy histograms change based on color calibration --------

#adjusted pic using adobe lightroom to apply drone camera calibration from Sypder Checker
#extracted pixels from one canopy to compare between adjusted and unadjusted drone pic
setwd("C:/Users/hmz25/Desktop/color calibration test")

adj <- rast("adjusted.tif")
# plotRGB(adj)

unadj <- rast("unadjusted.tif")
# plotRGB(unadj)

shp <- st_read("color_test_seg.shp") 

shp_vect <- vect(shp)

shp_reproj <- project(shp_vect, unadj)

# plot(unadj)
# plot(shp_reproj, add = TRUE, col = "red")

adj_canopy <- crop(adj, shp_reproj)
adj_canopy <- mask(adj_canopy, shp_reproj)
plotRGB(adj_canopy)

unadj_canopy <- crop(unadj, shp_reproj)
unadj_canopy <- mask(unadj_canopy, shp_reproj)
plotRGB(unadj_canopy)

adj_df <- as.data.frame(adj_canopy) %>% 
  rename(r = 1,
         g = 2, 
         b = 3) %>% 
  mutate(source = "adj")

unadj_df <- as.data.frame(unadj_canopy) %>% 
  rename(r = 1,
         g = 2, 
         b = 3) %>% 
  mutate(source = "unadj")

px_df <- bind_rows(adj_df, unadj_df)

px_df_long <- px_df %>% 
  pivot_longer(cols = r:b,
               names_to = "channel",
               values_to = "value")

ggplot(px_df_long, aes(x = value, fill = channel)) + 
  geom_histogram(position = "identity",  alpha = 0.5) +
  facet_wrap(~source, scales = "free_x") +
  scale_fill_manual(values = c("r" = "red", "g" = "darkgreen", "b" = "blue")) +
  labs(x = "digital number value", y = "count", title = "rgb histograms for different images") +
  theme_minimal()

ggplot(px_df_long) + 
  geom_boxplot(aes(x = channel, y = value, fill = channel), alpha = 0.5) +
  facet_wrap(~source, scales = "free_x") +
  scale_fill_manual(values = c("r" = "red", "g" = "darkgreen", "b" = "blue")) +
  labs(x = "channel", y = "digital number value", title = "rgb histograms for different images") +
  theme_minimal()

# compare pix4d reflectance output with orthomosaic output ----------------


reflect <- rast("C:/Users/hmz25/Documents/pix4d/wade_20250112/4_index/reflectance/wade_20250112_transparent_reflectance_group1.tif")
print(reflect)
ref_df <- as.data.frame(reflect)

colnames(ref_df) <- c("r", "g", "b")
head(ref_df)
# plot(reflect)
# plotRGB(reflect)

ortho <- rast("C:/Users/hmz25/Documents/pix4d/wade_20250112/3_dsm_ortho/2_mosaic/wade_20250112_transparent_mosaic_group1.tif")


#look at distribution of one pixel for the bands
ggplot(ref_df[1,]) +
  geom_histogram(x = )



