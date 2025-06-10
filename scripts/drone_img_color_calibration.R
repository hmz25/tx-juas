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
  xlim(230,255)


img_df_full %>% 
  pivot_longer(cols = c("r","g", "b"), names_to = "channel", values_to = "digital_number") %>% 
  ggplot() +
  geom_boxplot(aes(x = channel, y = digital_number)) +
  facet_wrap(~source) + 
  theme_minimal()

#visualize as each site histograms overlaying each other, facet wrapped by channel?
df_long <- pivot_longer(img_df_full, cols = c(r, g, b),
                        names_to = "channel", values_to = "digital_number")

ggplot(df_long, aes(x = digital_number, fill = source)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  facet_wrap(~channel, scales = "free_x") +
  labs(x = "digital number value", y = "count", title = "rgb histograms for different images") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) +
  scale_fill_brewer(palette = "Set1")

ggplot(df_long, aes(x = digital_number, fill = channel)) + 
  geom_histogram(position = "identity", , alpha = 0.5) +
  facet_wrap(~source, scales = "free_x") +
  labs(x = "digital number value", y = "count", title = "rgb histograms for different images") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

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



