#script for analyzing the spectral index of each quadrat on each tree

library(terra)
library(dplyr)
library(tidyverse)
library(stringr)
library(tools)

#load in quadrat pics
setwd("C:/Users/hmz25/")

photo_dir <- "Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level"
#list.files(photo_dir)

quadrat_files <- list.files(photo_dir, full.names = TRUE)

# test <- rast("Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/cathedral_t1_q4.tif")
# 
# plot(test)
# plotRGB(test)

#load in pictures, these pics have already been filtered using the random forest pixel classifier
for (i in seq_along(quadrat_files)) {

  file_name <- file_path_sans_ext(basename(quadrat_files[i]))

  assign(file_name, rast(quadrat_files[i]))

  print(i)
}

# plot(fisher_t2_q5) 
# plotRGB(fisher_t2_q5) # 1 = not foliage, 2 = yes foliage 
# 
# plotRGB(cathedral_t1_q1)
# plot(cathedral_t1_q1)

# file_name <- file_path_sans_ext(basename(quadrat_files[1]))
# 
# cathedral_t1_q1_df <- as.data.frame(cathedral_t1_q1) %>%
#   rename(r = 1, b = 2, g = 3, foliage = 4) %>%
#   filter(foliage == 2) %>%
#   mutate(quadrat = file_name) %>% 
#   mutate(orange_index = ((r-g)/(r+g)))

quadrat_list <- vector("list", length(quadrat_files))

#load in each masked quadrat picture as a raster file and create a df with its pixel values

for (i in seq_along(quadrat_files)){
  
  quadrat_i <- rast(quadrat_files[i])
  
  file_name <- file_path_sans_ext(basename(quadrat_files[i]))
  
  quadrat_df <- as.data.frame(quadrat_i) %>% 
    rename(r = 1, b = 2, g = 3, foliage = 4) %>% 
    filter(foliage == 2) %>% #this filters out pixels that are not cones or foliage 
    mutate(quadrat = file_name) %>% 
    mutate(orange_index = ((r-g)/(r+g)))
  
  quadrat_list[[i]] <- quadrat_df
  
  print(i)
}
#the difference between this pixel filter and the other one is that the other one counted the number of pixel values that were cone, vs the number of pixel values that were not cone based on a threshold defined by the spectral index 
#this code directly relates the spectral index reading to the number of cones

#join quadrat pixel values
full_df <- dplyr::bind_rows(quadrat_list)

# str(full_df)                                    
# tail(full_df)
# unique(full_df$quadrat)


# add in cone count data

quadrat_cones <- read_csv("Box/texas/pollen_production/TX jan 24/data analysis/2024_quadratcones.csv")

#add cones per weight columns
quadrat_cones <- quadrat_cones %>%
  dplyr::select(date_collected,
                site,
                tree,
                quadrat,
                total_mass,
                s1_count, s1_weight,
                s2_count, s2_weight,
                s3_count, s3_weight,
                s4_count, s4_weight,
                s5_count, s5_weight) %>%
  mutate(s1 = (quadrat_cones$s1_count/quadrat_cones$s1_weight),
         s2 = (quadrat_cones$s2_count/quadrat_cones$s2_weight),
         s3 = (quadrat_cones$s3_count/quadrat_cones$s3_weight),
         s4 = (quadrat_cones$s4_count/quadrat_cones$s4_weight),
         s5 = (quadrat_cones$s5_count/quadrat_cones$s5_weight))  

#take mean of cone per weight for each quadrat
quadrat_cones <- quadrat_cones %>%
  dplyr::select(date_collected,
                site,
                tree,
                quadrat,
                total_mass,
                s1,
                s2,
                s3,
                s4,
                s5)

quadrat_cones$cone_mean <- rowMeans(subset(quadrat_cones, select = c("s1", "s2", "s3", "s4", "s5")),
                                    na.rm = TRUE)

#take standard deviation of quadrat cones
quadrat_cones <- quadrat_cones %>%
  rowwise %>%
  mutate(cone_sd = sd(c_across(s1:s5))) %>%
  ungroup()

#calculate total number of cones in each quadrat
quadrat_cones <- quadrat_cones %>%
  mutate(total_cones = cone_mean*total_mass) #total cones = cones per 25cmx25cm (625cm2)

#rename date column and ensure dates are in the right format
quadrat_cones <- rename(quadrat_cones, date = date_collected)
quadrat_cones$date <- mdy(quadrat_cones$date)

#reformat cones df
cones_per_quadrat_df <- quadrat_cones %>% 
  dplyr::select(date, site, tree, quadrat, total_cones) %>% 
  mutate(tree = as.character(tree)) %>% 
  mutate(site = str_replace(site, "cell tower", "cell"))

#reformat quadrat df 
quadrat_px_df <- full_df %>% 
  separate(quadrat, into = c("site", "tree", "quadrat"), sep = "_") %>% 
  mutate(tree = str_remove(tree, "t"), quadrat = str_remove(quadrat, "q")) %>% 
  dplyr::select(r, g, b, site, tree, quadrat, orange_index)

quadrat_px_df_test <- quadrat_px_df %>% 
  mutate(tree = str_remove(tree, "t"), quadrat = str_remove(quadrat, "q")) %>% 
  dplyr::select(r, g, b, site, tree, quadrat, orange_index) %>% 
  group_by(site, tree, quadrat) %>% 
  summarize(sum_index = sum(orange_index))
  
  

# quadrat_px_df_sub <- subset(quadrat_px_df)[3,]
# 
# quadrat_px_df_sub %>%
#   mutate(tree = str_remove(tree, "t"), quadrat = str_remove(quadrat, "q"))
  
#join px and cones df
cones_px_df <- quadrat_px_df_test %>% 
  left_join(cones_per_quadrat_df, by = c("site", "tree", "quadrat"))

#visualize
ggplot(cones_px_df) +
  aes(x = total_cones, y = sum_index, col = site) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  ggthemes::theme_few()
