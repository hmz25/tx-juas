library(tidyverse)
library(dplyr)
library(terra)
library(magick) #install.packages("magick")

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
  dplyr::select(date_collected, name_site, tree, total_mass, total_subsample_weight, sample_n, weight, count, notes)

ggplot(cone_df_clean, aes(x = as.factor(tree), y = count)) +
  geom_boxplot() +
  facet_wrap( ~ name_site)

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
img_folder <- "Katz lab/texas/tx 2025 drone pics/cropped_quadrat_pics"
img_list <- list.files(img_folder, full.names = FALSE)
img_list_dir <- list.files(img_folder, full.names = TRUE)

# test <- rast("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas/tx 2025 drone pics/cropped_quadrat_pics/cath_t2.tif")
# plotRGB(test)

test <- rast(img_list_dir[4])
plotRGB(test)

test2 <- image_read(img_list_dir[6])

for (i in seq_along(img_list)) {
  #remove the file extension to use as object name
  file_name <- tools::file_path_sans_ext(img_list[i])
  
  #load the raster
  photo_i <- rast(img_list_dir[i])
  
  # Assign the raster to an object named after the file
  assign(file_name, photo_i, envir = .GlobalEnv)
  
  print(i)
}

# plotRGB(wade_t5)
# plotRGB(wind_t4)
# plotRGB(sonora_t1)
