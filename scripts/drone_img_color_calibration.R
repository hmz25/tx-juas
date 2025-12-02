#script for exploring how different lighting conditions/color adjustments impact pixel values
library(dplyr)
library(tidyverse)
library(terra)
library(ggplot2)
library(sf)
library(exactextractr)

#eventually reformat code to be
#1. load all data in
#2. visualize 

# analyzing white pixels from quadrat in drone flights --------------------

#since same material is used and white, should always have rgb values close to 255
#deviations from 255 indicate different lighting conditions impacting value

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

#visualize how histogram values differ between flights
ggplot(img_df_full) +
  geom_histogram(aes(x = r)) +
  facet_wrap(~source) + 
  xlim(230,255) + 
  ylim(0,40) +
  theme_classic()

#restructure data frame to be in long foramt 
img_df_full %>% 
  pivot_longer(cols = c("r","g", "b"), names_to = "channel", values_to = "digital_number") %>% 
  ggplot() +
  geom_boxplot(aes(x = channel, y = digital_number)) +
  facet_wrap(~source) + 
  theme_minimal()

#visualize how index histogram values differ between flights 
img_df_full %>% 
  ggplot() +
  geom_histogram(aes(x = (r-g)/(r+g))) + 
  xlim(-0.03,0.03) +
  facet_wrap(~source) + 
  theme_classic() +
  ggtitle("no white balance on pics")

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


# shadow pixel values vs non shadow pixel values  --------

#load in picture of white background without shadow vs with shadow
#images taken during same flight

shadow <- rast("C:/Users/hmz25/Desktop/backdrops/shadow quadrats/cath_20241230_t2_white.tif")
no_shadow <- rast("C:/Users/hmz25/Desktop/backdrops/cath_20241230_t4_white.tif")

#inspect images
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

#visualize values of all bands in shadow vs no shadow 
ggplot(df) +
  geom_histogram(aes(x = digital_number, fill = channel), alpha = 0.5) + 
  facet_wrap(~source) +
  scale_fill_manual(values = c(r = "red",
                    g = "darkgreen",
                    b = "blue")) +
  theme_classic()

#visualize index values of shadow vs no shadow 
ggplot(df_index) +
  geom_histogram(aes(x = (r-g)/(r+g))) +
  facet_wrap(~source) + 
  theme_classic()

# white values in adjusted vs non-adjusted imgs --------

#adjusted = using Lightroom Classic to do camera correction from Spyder Checkr

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

#visualize distribution of red value in adjusted, compare to un-adjusted image values in first section of code 
ggplot(adj_df_full) +
  geom_histogram(aes(x = r)) +
  facet_wrap(~source) + 
  xlim(230,255) + 
  ylim(0,40) +
  theme_minimal()

#visualize distribution of index values in adjusted, compare with un-adjusted image values in first section of code 
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

wb <- rast("DJI_20250112123458_0063_V_WB_reproj.tif")
plotRGB(wb)

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

wb_canopy <- crop(wb, shp_reproj)
wb_canopy <- mask(wb_canopy, shp_reproj)
plotRGB(wb_canopy)

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

wb_df <- as.data.frame(wb_canopy) %>% 
  rename(r = 1,
         g = 2, 
         b = 3) %>% 
  mutate(source = "wb")

px_df <- bind_rows(adj_df, unadj_df, wb_df)

#restructure df into long format
px_df_long <- px_df %>% 
  pivot_longer(cols = r:b,
               names_to = "channel",
               values_to = "value")

#examine how histogram of pixel values change based on white balance/adjustment
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

#examine difference in index values based on white balance vs no white balance
px_df %>% 
  filter(source != "adj") %>% 
  ggplot() +
  geom_histogram(aes(x = (r-g)/(r+g))) +
  facet_wrap(~source) +
  theme_classic() +
  ggtitle("canopy pixels in white balance vs no white balance image")

# white quadrat index values change based on white balance --------

#load in all images from quadrats 
#images are cropped white pixels from quadrats on different flights 
#white balanced using auto white balance from Adobe lightroom classic 

wb_dir <- "C:/Users/hmz25/Desktop/quadrat_lighting/white_balance_quadrats"
wb_files_full <- list.files(wb_dir, pattern = "_white.tif", full.names = TRUE) 

wb_df_full <- data.frame()

for (i in seq_along(adj_files_full)) {
  
  #load in image
  wb_img <- rast(wb_files_full[i])
  
  #rename channels to RGB
  names(wb_img) <- c("r", "g", "b")
  
  #convert to data frame
  wb_img_df <- as.data.frame(wb_img)
  
  #add image filename as a column
  wb_img_df$source <- basename(wb_files_full[i])
  
  #add to cumulative data frame
  wb_df_full <- bind_rows(wb_df_full, wb_img_df)
}

wb_df_full

#visualize how histograms of index from white balanced images change in different flights
wb_df_full %>% 
  ggplot() +
  geom_histogram(aes(x = (r-g)/(r+g))) + 
  xlim(-0.03, 0.03) +
  facet_wrap(~source) + 
  theme_classic() + 
  ggtitle("white balance on pics")

# compare pix4d reflectance output with orthomosaic output ----------------

reflect <- rast("C:/Users/hmz25/Documents/pix4d/wade_20250112/4_index/reflectance/wade_20250112_transparent_reflectance_group1.tif")
print(reflect)
#reflectance map values are scaled weirdly, 16 bit (0-65535)?
#if in 16 bit, divide by 256

ref_df <- as.data.frame(reflect)
colnames(ref_df) <- c("r", "g", "b")
head(ref_df)
# plot(reflect)
# plotRGB(reflect)

ortho <- rast("C:/Users/hmz25/Documents/pix4d/wade_20250112/3_dsm_ortho/2_mosaic/wade_20250112_transparent_mosaic_group1.tif")
plotRGB(ortho)

#extract canopy pixels from tree shapefiles in reflectance and ortho
#use exact extract 

canopy_shp <- st_read("C:/Users/hmz25/Box/Katz lab/texas/2025 juas qgis/wade_canopy_seg.shp")
plot(canopy_shp)
canopy_shp_reproj <- st_transform(canopy_shp, crs(reflect))
plot(canopy_shp_reproj, add = T, col = "red")

# crs(canopy_shp_reproj)
# crs(reflect)
# crs(ortho)

reflect_canopies <- exact_extract(reflect, canopy_shp_reproj)

reflect_ttop <- as.data.frame(reflect_canopies[[2]]) %>% 
  rename(r = 1,
         g = 2,
         b = 3) %>% 
  mutate(index = (r-g)/(r+g)) %>% 
  mutate(source = "reflectance")

ggplot(reflect_ttop, aes(x = index)) +
  geom_histogram() +
  xlim(-0.30, 0.30) +
  ylim(0, 41000)

# reflect_ttop_long %>% 
#   mutate(source = "reflectance") %>% 
#   select(index, souce) %>% 
#   pivot_longer(cols = c(index, source), names_to = )

ortho_canopies <- exact_extract(ortho, canopy_shp_reproj)

ortho_ttop <- as.data.frame(ortho_canopies[[2]]) %>% 
  rename(r = 1,
         g = 2,
         b = 3) %>% 
  mutate(index = (r-g)/(r+g)) %>% 
  mutate(source = "ortho")

ggplot(ortho_ttop, aes(x = index)) + 
  geom_histogram() + 
  xlim(-0.30, 0.30) +
  ylim(0, 41000)

ttop_df <- bind_rows(ortho_ttop, reflect_ttop)

ggplot(ttop_df) +
  geom_histogram(aes(x = index, fill = source), alpha = 0.5) +
  ggtitle("histogram of canopy index values for reflectance map vs ortho") +
  theme_classic()

ggplot(ttop_df) +
  geom_point(aes(x = index, y = index, col = source), alpha = 0.5) +
  theme_classic()


# index values in shadow vs not in shadow ---------------------------------

# cones <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/wind_t4.tif")
cones <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/fish_t9.tif")
plotRGB(cones)
names(cones) <- c("r", "g", "b", "mask")

#mask out non foliage/cone pixels
cones_masked <- mask(cones[[1:3]], cones$mask, maskvalues=TRUE)
plotRGB(cones_masked)

#look at how index changes on cones in shadow vs not in shadow
cones_masked$index <- (cones_masked$r-cones_masked$g)/(cones_masked$r+cones_masked$g)
cones_masked$rg <- (cones_masked$r)/(cones_masked$g)
plot(cones_masked$index)
terra::plot(cones_masked$rg, zlim = c(0,1))
terra::plot(cones_masked[["rg"]], zlim = c(0, 3))


cones_masked_df <- as.data.frame(cones_masked, xy = TRUE, na.rm = FALSE)

# Plot with ggplot
ggplot(cones_masked_df, aes(x = x, y = y, fill = rg)) +
  geom_raster() +
  scale_fill_viridis_c(limits = c(0, 3), name = "rg value") +
  coord_equal() +
  theme_classic()


#foliage 
fol <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/cell_t9.tif")
plotRGB(fol)

names(fol) <- c("r", "g", "b", "mask")

fol_index <- (fol$r-fol$g)/(fol$r+fol$g)
plot(fol_index)

#mask out non foliage/cone pixels
fol_masked <- mask(fol[[1:3]], fol$mask, maskvalues=TRUE)
plotRGB(fol_masked)

#look at how index changes on cones in shadow vs not in shadow
fol_masked$index <- (fol_masked$r-fol_masked$g)/(fol_masked$r+fol_masked$g)
fol_masked$rg <- (fol_masked$r)/(fol_masked$g)
plot(fol_masked$index)

#how does white balance help shadow pixels
fol_wb <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/cell_t9.tif")
plotRGB(fol_wb)

names(fol_wb) <- c("r", "g", "b")
fol_wb_index <- (fol_wb$r-fol_wb$g)/(fol_wb$r+fol_wb$g)
plot(fol_wb_index)

#makes low index values slightly higher but not perfect, will still have to set a cutoff value 

#cones from aerial view

# ortho_cones <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/DJI_202501131305_179_gunsiteboundary/DJI_20250113132209_0805_V.JPG")
# plotRGB(ortho_cones)
# 
# ortho_fol <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/DJI_202501131305_179_gunsiteboundary/DJI_20250113132202_0799_V.JPG")
# plotRGB(ortho_fol)

setwd("C:/Users/hmz25/Desktop/shadow_analysis")

ortho_cones <- rast("C:/Users/hmz25/Desktop/shadow_analysis/ortho_cones_img.tif")
plotRGB(ortho_cones)

ortho_fol <- rast("C:/Users/hmz25/Desktop/shadow_analysis/ortho_fol_img.tif")
plotRGB(ortho_fol)

#crop

cones_shp <- st_read("C:/Users/hmz25/Desktop/shadow_analysis/ortho_cones.shp")
plot(cones_shp, add = TRUE, col = "red")

ortho_cones <- crop(ortho_cones, cones_shp)
plotRGB(ortho_cones)

fol_shp <- st_read("C:/Users/hmz25/Desktop/shadow_analysis/ortho_fol.shp")
plot(fol_shp, add = TRUE, col = "red")

ortho_fol <- crop(ortho_fol, fol_shp)
plotRGB(ortho_fol)

#mask

ortho_cones <- mask(ortho_cones, cones_shp)
plotRGB(ortho_cones)

ortho_fol <- mask(ortho_fol, fol_shp)
plotRGB(ortho_fol)

names(ortho_cones) <- c("r", "g", "b")
names(ortho_fol) <- c("r", "g", "b")

# #apply both rfs (trained on handheld vs orthos) to see how they differ?
# ortho_cones_mask <- predict(ortho_cones, rf_mask)
# plot(ortho_cones_mask)
# 
# ortho_cones_masked <- terra::mask(ortho_cones, ortho_cones_mask, maskvalues = TRUE)
# plotRGB(ortho_cones_masked)

#apply rf trained on the ortho images
# ortho_cones_mask2 <- predict(ortho_cones, rf_mask_ortho)
# plot(ortho_cones_mask2)
##this rf removes a LOT more pixels, especially ones in shadow 

#apply index to see how the values change in shadow vs in light 

ortho_cones$index <- (ortho_cones$r-ortho_cones$g)/(ortho_cones$r+ortho_cones$g)
plot(ortho_cones$index)

ortho_fol$index <- (ortho_fol$r-ortho_fol$g)/(ortho_fol$r+ortho_fol$g)
plot(ortho_fol$index)


# cone index values thru season -------------------------------------------

jan3 <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/iphone_quadrat_pics/fish_jan3.tif")
plotRGB(jan3)

jan10 <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/iphone_quadrat_pics/fish_jan10.tif")
plotRGB(jan10)

#try to write a function for renaming and creating the index 
# name_cols <- c("r", "g", "b")
# names(jan3) <- name_cols

# process_img <- function(img, rf){
#   names(img) <- c("r", "g", "b")
#   img_masked <- predict(img, rf)
#   img_masked$index <- (img$r-img$g)/(img$r+img$g)
# }
# test <- process_img(jan3, rf = rf_mask)
# plot(test)

names(jan3) <- c("r", "g", "b")
names(jan10) <- c("r", "g", "b")

jan3$index <- (jan3$r-jan3$g)/(jan3$r+jan3$g)
plot(jan3$index)

jan10$index <- (jan10$r-jan10$g)/(jan10$r+jan10$g)
plot(jan10$index)

jan6 <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/swee_t2.tif")
plotRGB(jan6)

jan15 <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/swee_t9.tif")
plotRGB(jan15)

names(jan6) <- c("r", "g", "b")
names(jan15) <- c("r", "g", "b")

jan6_mask <- predict(jan6, rf_mask)
plot(jan6_mask)

jan6_masked <- mask(jan6, jan6_mask, maskvalues = TRUE)
plotRGB(jan6_masked, main = "jan 6")

jan15_mask <- predict(jan15, rf_mask)
jan15_masked <- mask(jan15, jan15_mask, maskvalues = TRUE)
plotRGB(jan15_masked)

jan6_masked$index <- (jan6_masked$r-jan6_masked$g)/(jan6_masked$r+jan6_masked$g)
plot(jan6_masked$index, main = "jan 6")

jan15_masked$index <- (jan15_masked$r-jan15_masked$g)/(jan15_masked$r+jan15_masked$g)
plot(jan15_masked$index, main = "jan 15")
