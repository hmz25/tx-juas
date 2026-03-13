#script to analyze cone pixels vs foliage pixels

library(dplyr)
library(tidyverse)
library(raster)

setwd("C:/Users/hmz25/Box/Katz lab/texas/")

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

px_df_long <- px_df |> 
  pivot_longer(cols = c("r","g","b"), names_to = "band", values_to = "dn") |> 
  mutate(band = factor(band, c("r","g","b")))


#visualize where the max separation is between cone and foliage

ggplot(px_df_long) +
  geom_histogram(aes(x = dn, y = after_stat(density), fill = band), alpha = 0.5) +
  facet_wrap(~cone_or_fol) +
  theme_classic()

ggplot(px_df_long) +
  geom_point(aes(x = dn, y = dn, col = band), alpha = 0.5) +
  facet_wrap(~cone_or_fol)

ggplot(px_df_long) +
  geom_density(aes(x = dn, col = band)) +
  facet_wrap(~cone_or_fol) + 
  theme_classic()

#visualize new indices on quadrat imgs
wade_t7 <- stack("tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics/wade_t7_tree.tif")
plotRGB(wade_t7)

rf_mask <- wade_t7$wade_t7_tree_4

wade_t7_filt <- mask(wade_t7, rf_mask, maskvalue = 1)

plotRGB(wade_t7_filt)

names(wade_t7_filt) <- c("r", "g", "b", "rf_mask")

# wade_t7_filt_index <- wade_t7_filt$r/wade_t7_filt$g
# wade_t7_filt_index <- (wade_t7_filt$r - wade_t7_filt$g - wade_t7_filt$b)/(wade_t7_filt$r / wade_t7_filt$g)
wade_t7_filt_index <- (wade_t7_filt$r - wade_t7_filt$g)/(wade_t7_filt$r/wade_t7_filt$g)

plot(wade_t7_filt_index)

fisher_t6 <- 

