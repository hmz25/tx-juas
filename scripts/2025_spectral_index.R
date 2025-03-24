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
# img_folder <- "/Users/hannahzonnevylle/Desktop/quadrat pics 2025/manually_filtered"  
# img_list <- list.files(img_folder, pattern = "_filt2\\.tif$", full.names = FALSE) #only load imgs containing pixels that we are sure are cones/foliage
# img_list_dir <- list.files(img_folder, pattern = "_filt2\\.tif$", full.names = TRUE)

img_folder <- "/Users/hannahzonnevylle/Desktop/quadrat pics 2025/best_quadrats"
img_list <- list.files(img_folder, full.names = FALSE)
img_list_dir <- list.files(img_folder, full.names = TRUE)

quadrat_px_df <- data.frame()

# i = 19

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

# index_df <- quadrat_px_df |>
#   group_by(site, tree) |> 
#   summarize(mean_r = mean(r, na.rm = T),
#             mean_g = mean(g, na.rm = T),
#             mean_b = mean(b, na.rm = T))|>
#   mutate(site = substr(site, 1, 4))

index_df <- quadrat_px_df |>
  mutate(orange_index = (r - g) / (r + g)) |>
  group_by(site, tree) |>
  summarize(mean_index = mean(orange_index, na.rm = TRUE)) |>
  mutate(site = substr(site, 1, 4))

# index_df <- quadrat_px_df |> 
#   mutate(orange_index = (r - g) / (r + g)) |> 
#   group_by(site, tree) |> 
#   summarize(
#     mean_index = mean(orange_index, na.rm = TRUE),
#     prop_valid = sum(!is.na(orange_index)) / n()  #proportion of non-NA values
#   ) |> 
#   mutate(
#     site = substr(site, 1, 4),
#     adjusted_mean_index = mean_index * prop_valid #adjusted mean accounting for NA proportion
#   )


# combine quadrat pixel data with cone count data -------------------------

# str(index_df)
# str(cone_df_clean)
# 
# index_df <- index_df |> 
#   mutate(tree = as.numeric(tree))

total_cone_df <- cone_df_clean |> 
  mutate(cones_per_g = count/weight) |> 
  group_by(date_collected, tree, site, total_mass) |> 
  summarize(mean_cones_per_g = mean(cones_per_g)) |> 
  mutate(total_cones = mean_cones_per_g*total_mass) |> 
  mutate(tree = as.character(tree))

cone_index_df <- left_join(index_df, total_cone_df)

cone_px_df <- left_join(quadrat_px_df, total_cone_df)

# ggplot(cone_px_df, aes(x = mean_r, y = mean_cones_per_g, col = site)) + 
#   geom_point(alpha = 0.5)

#join df with manual pheno data

fieldmaps_df <- read_csv('/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas/focal_trees_2025.csv')\

fieldmaps_df_clean <- fieldmaps_df |> 
  mutate(parsed_datetime = mdy_hms(date_time),  # Convert to datetime format
         date_collected = format(parsed_datetime, "%m/%d/%Y"),  # Extract and format date
         time = format(parsed_datetime, "%I:%M:%S %p")) |>   # Extract and format time
  select(site,
         focal_tree_number,
         pollen_cone_density,
         date_collected) |> 
  mutate(tree = as.character(focal_tree_number),
         site = tolower(substr(site, 1, 4))) |> 
  select(-focal_tree_number) 

# cone_px_df <- merge(cone_px_df, fieldmaps_df_clean)

cone_index_pcd_df <- cone_index_df |> 
  left_join(fieldmaps_df_clean, join_by(site, tree, closest(date_collected >= date_collected)))

cone_px_pcd_df <- cone_px_df |> 
  left_join(fieldmaps_df_clean, join_by(site, tree)) |> 
  select(-date_collected.x, -date_collected.y) |> 
  drop_na()


# visualize data ----------------------------------------------------------


ggplot(cone_index_df, aes(x = mean_index, y = total_cones, col = site)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

ggplot(cone_index_df, aes(x = mean_index, y = total_cones)) + 
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

fit <- lm(cone_index_df$total_cones  ~ cone_index_df$mean_index)
summary(fit)

df_long <- cone_px_pcd_df %>%
  pivot_longer(cols = c(r, g, b), names_to = "color_channel", values_to = "value") |> 
  mutate(pollen_cone_density = factor(pollen_cone_density, levels = c("low", "low-medium", "medium", "medium-high", "high", "very high")))

#plot faceted histograms
ggplot(df_long, aes(x = value, fill = color_channel)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ pollen_cone_density, scales = "free_y") +  # Facets ordered
  scale_fill_manual(values = c("r" = "red", "g" = "forestgreen", "b" = "darkblue")) +
  theme_classic() +
  labs(title = "distribution of R, G, B values across PCD",
       x = "RGB value",
       y = "freq") 



