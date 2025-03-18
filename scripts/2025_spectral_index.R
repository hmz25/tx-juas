library(tidyverse)
library(dplyr)
library(terra)
# library(magick) #install.packages("magick")
library(stringr)

setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/")


# manual cone data processing ---------------------------------------------

#load in cone count data 
cone_df <- read_csv("Katz lab/texas/cone processing 25 - counts.csv")

cone_df_clean <- cone_df |> 
  rename(name_site = site,
         total_subsample_weight = subsample_weight) |>  
  pivot_longer(
           cols = starts_with("s"),
           names_to = c("sample_n", ".value"),
           names_pattern = "s(\\d+)_(.*)") |> 
  mutate(site = substr(name_site, 1, 4)) |> 
  dplyr::select(date_collected, site, tree, total_mass, total_subsample_weight, sample_n, weight, count, notes)

ggplot(cone_df_clean, aes(x = as.factor(tree), y = count)) +
  geom_boxplot() +
  facet_wrap( ~ site)

# cone_var_df <- cone_df_clean |> 
#   group_by(name_site) |> 
#   summarise(cone_var = sd(count))

# quadrat image processing ------------------------------------------------

#pre-processing, ignore 

# img_folder <- "/Users/hannahzonnevylle/Desktop/quadrat pics 2025/backdrops"
# quadrat_pics <- list.files(img_folder, pattern = "\\.tif$", full.names = FALSE)
# 
# new_folder <- "/Users/hannahzonnevylle/Desktop/quadrat pics 2025/cropped_quadrat_pics"
# 
# #create new folder 
# if (!dir.exists(new_folder)) {
#   dir.create(new_folder)
# }
# 
# #process img files
# for (file in quadrat_pics) {
#   #extract parts before the first "_" and after the last "_"
#   parts <- unlist(strsplit(file, "_"))
#   new_name <- paste0(parts[1], "_", tail(parts, 1))
#   
#   # Define old and new file paths
#   old_path <- file.path(img_folder, file)
#   new_path <- file.path(new_folder, new_name)
#   
#   # Rename and move the file
#   file.rename(old_path, new_path)
# }

#load in quadrat pics
img_folder <- "/Users/hannahzonnevylle/Desktop/quadrat pics 2025/extra_filtered" #this is the folder that only contains pixels that we are sure are cones/foliage 
img_list <- list.files(img_folder, full.names = FALSE)
img_list_dir <- list.files(img_folder, full.names = TRUE)

# test <- rast("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas/tx 2025 drone pics/cropped_quadrat_pics/cath_t2.tif")
# plotRGB(test)

# test <- rast(img_list_dir[4])
# plotRGB(test)
# 
# test_df <- as.data.frame(test) |> 
#   rename(r = 1,
#          g = 2, 
#          b = 3)
# 
# # ggplot(test_df) +
# #   geom_histogram(aes(x = r))
# 
# test_df_filt <- test_df |> 
#   mutate(across(c(r, g, b), ~ ifelse(r >= 240 | g >= 240 | b >= 240, NA, .)), #filter out white pixels 
#          across(c(r, g, b), ~ ifelse(r <= 10 & g <= 10 & b <= 10, NA, .))) #filter out black pixels 

# test <- rast(img_list_dir[8])
# plotRGB(test)
# 
# df <- as.data.frame(test)
# 
# names(test) <- c("r", "g", "b")
# 
# test_masked <- test

# test_masked[test >= 240] <- NA
# plotRGB(test_masked)
# test_masked[test$cath_t3_1 <= 10 & test$cath_t3_2 <= 10 & test$cath_t3_3 <= 10] <- NA
# plotRGB(test_masked)

# threshFunction <- function(test){
#   test_masked <- test
#   test_masked[test >= 240] <- NA
#   test_masked[test$r <= 10 & test$g <= 10 & test$b <= 10] <- NA
#   return(test_masked)
# }
# 
# test_function <- threshFunction(test)
# plotRGB(test_function)

# test_df <- as.data.frame(test_function, na.rm = FALSE)

# i = 9
# 
# img_list <- img_list[1:3]

quadrat_px_df <- data.frame()

i = 19

for (i in seq_along(img_list)) {
  #remove the file extension to use as object name
  file_name <- tools::file_path_sans_ext(img_list[i])
  
  #extract site name (everything before "_t")
  site <- str_extract(file_name, "^[^_]+")
  
  #extract tree number (digits after "t")
  tree <- str_extract(file_name, "(?<=t)\\d+")
  
  #load the raster
  photo_i <- rast(img_list_dir[i])
  
  #rename raster columns to r, g, b
  names(photo_i) <- c("r", "g", "b")
  
  #filter out non-foliage and cone pixels
  photo_i[photo_i$r >= 240 | photo_i$g >= 240 | photo_i$b >= 240] <- NA
  # plotRGB(photo_i)
  # plotRGB(photo_i, colNA = "red")
  
  #convert to data frame
  photo_i_df <- as.data.frame(photo_i, na.rm = FALSE) #keep NA values 
  
  #add site and tree columns to df 
  photo_i_df$site <- site
  photo_i_df$tree <- tree
  
  #add to data frame of all pixels in each quadrat 
  quadrat_px_df <- rbind(quadrat_px_df, photo_i_df)
  
  print(i) #88 instead of 89 because not using tree 6 from cath-- moved in wind
}

quadrat_px_df


index_df <- quadrat_px_df |> 
  mutate(orange_index = (r - g) / (r + g)) |> 
  group_by(site, tree) |> 
  summarize(
    mean_index = mean(orange_index, na.rm = TRUE),
    prop_valid = sum(!is.na(orange_index)) / n()  #proportion of non-NA values
  ) |> 
  mutate(
    site = substr(site, 1, 4),
    adjusted_mean_index = mean_index * prop_valid #adjusted mean accounting for NA proportion
  )


# combine quadrat pixel data with cone count data -------------------------

# str(index_df)
# str(cone_df_clean)
# 
# index_df <- index_df |> 
#   mutate(tree = as.numeric(tree))

total_cone_df <- cone_df_clean |> 
  mutate(cones_per_g = count/weight) |> 
  group_by(tree, site, total_mass) |> 
  summarize(mean_cones_per_g = mean(cones_per_g)) |> 
  mutate(total_cones = mean_cones_per_g*total_mass) |> 
  mutate(tree = as.character(tree))

cone_px_df <- left_join(index_df, total_cone_df)

#visualize data
ggplot(cone_px_df, aes(x = mean_index, y = total_cones, col = site)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

ggplot(cone_px_df, aes(x = adjusted_mean_index, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()

# quadrat_cones_rgb_test_treeID <- quadrat_cones_rgb_test %>% 
#   mutate(tree_id = paste(site, tree, sep = "_")) %>% 
#   as.data.frame()
# 
# ggplot(quadrat_cones_rgb_test_treeID, aes(x = photo_i_index_rg_thresh_sum, y = total_cones/(0.25*0.25), col = tree_id)) +
#   geom_point(alpha = 0.5) + 
#   theme_bw() + 
#   geom_smooth(method = "lm", se = FALSE) +
#   xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()

fit <- lm(cone_px_df$total_cones  ~ cone_px_df$mean_index)
summary(fit)

