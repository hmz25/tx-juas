#script to compare pix4d ortho output with pix4d reflectance map output

library(terra)
library(sf)
library(tidyverse)

reflec <- rast("C:/Users/hmz25/Documents/pix4d/cathedral_20250110/4_index/reflectance/cathedral_20250110_transparent_reflectance_group1.tif")

plotRGB(reflec)

ortho <- rast("C:/Users/hmz25/Documents/pix4d/cathedral_20250110/3_dsm_ortho/2_mosaic/cathedral_20250110_transparent_mosaic_group1.tif")

plotRGB(ortho)

crs(reflec) == crs(ortho)

shp <- st_read("C:/Users/hmz25/Desktop/TX 2025 analysis/test_shp.shp")

shp_reproj <- st_transform(shp, crs = st_crs(ortho))

# st_crs(shp_reproj)
# st_crs(ortho)
# 
# crs(shp_reproj)
# crs(ortho)
# 
# plot(shp_reproj)
# 
# crs(shp_reproj) == crs(ortho)
# 
# plot(shp_reproj, add = TRUE, col = "red")

shp_vect <- vect(shp_reproj)

ortho_ttops <- extract(ortho, shp_vect)

reflect_ttops <- extract(reflec, shp_vect)

ortho_ttops_df <- as.data.frame(ortho_ttops) %>% 
  dplyr::select(1:4) %>% 
  rename(r = 2,
         g = 3,
         b = 4)

reflect_ttops_df <- as.data.frame(reflect_ttops) %>% 
  dplyr::select(1:4) %>% 
  rename(r = 2,
         g = 3,
         b = 4)

ortho_ttops_df$source <- "ortho"
reflect_ttops_df$source <- "reflectance"

combined_df <- bind_rows(ortho_ttops_df, reflect_ttops_df)

df_long <- combined_df %>%
  pivot_longer(cols = c("r", "g", "b"), names_to = "color", values_to = "value")

df_long$color <- factor(df_long$color, levels = c("r", "g", "b"))

p <- ggplot(df_long, aes(x = source, y = value, fill = color)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  
  facet_wrap(~ID) +                         #facet by IDs for diff trees
  scale_fill_manual(values = c("r" = "red", "g" = "green", "b" = "blue")) +  # Set RGB colors
  theme_minimal() +
  labs(title = "distribution of RGB values for each tree",
       x = "img type",
       y = "RGB value")

img_cal <- rast("C:/Users/hmz25/Desktop/DJI_202501121533_176_celltowersiteboundary_ADJUSTED/DJI_20250112153630_0031_V.tif")
plotRGB(img_cal)

img_uncal <- rast("C:/Users/hmz25/Desktop/tx 2025 tagged drone images/tagged_DJI_202501121533_176_celltowersiteboundary/DJI_20250112153630_0031_V.JPG")
plotRGB(img_uncal)

img_cal_df <- as.data.frame(img_cal) %>% 
  rename(r = 1,
         g = 2,
         b = 3)

img_uncal_df <- as.data.frame(img_uncal) %>% 
  rename(r = 1,
         g = 2,
         b = 3)

img_cal_df$source = "cal"
img_uncal_df$source = "uncal"

combined_df <- bind_rows(img_cal_df, img_uncal_df)

df_long <- combined_df %>%
  pivot_longer(cols = c("r", "g", "b"), names_to = "color", values_to = "value")

df_long$color <- factor(df_long$color, levels = c("r", "g", "b"))

p1 <- ggplot(df_long, aes(x = source, y = value, fill = color)) +
  geom_boxplot() +  
  scale_fill_manual(values = c("r" = "red", "g" = "green", "b" = "blue")) +  # Set RGB colors
  theme_minimal() +
  labs(title = "distribution of RGB values for calibrated vs uncalibrated img",
       x = "img type",
       y = "RGB value")

adj <- rast("C:/Users/hmz25/Desktop/cath_quadrats_ADJUSTED/cath_t3_filt2.tif")
plotRGB(adj)

unadj <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/cropped_quadrat_pics/cath_t3_filt2.tif")
plotRGB(unadj)

adj_df <- as.data.frame(adj) %>% 
  rename(r = 1,
         g = 2,
         b = 3) %>% 
  filter((r < 255))
unadj_df <- as.data.frame(unadj) %>% 
  rename(r = 1,
         g = 2,
         b = 3) %>% 
  filter((r < 255))

adj_df$source = "cal"
unadj_df$source = "uncal"

combined_df <- bind_rows(adj_df, unadj_df)

df_long <- combined_df %>%
  pivot_longer(cols = c("r", "g", "b"), names_to = "color", values_to = "value")

df_long$color <- factor(df_long$color, levels = c("r", "g", "b"))

p2 <- ggplot(df_long, aes(x = source, y = value, fill = color)) +
  geom_boxplot() +  
  scale_fill_manual(values = c("r" = "red", "g" = "green", "b" = "blue")) +  # Set RGB colors
  theme_minimal() +
  labs(title = "distribution of RGB values for calibrated vs uncalibrated img",
       x = "img type",
       y = "RGB value")
