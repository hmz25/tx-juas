#code to apply spectral index to orbital reconstructions of trees

# load libraries ----------------------------------------------------------
library(lidR)
library(terra)
library(sf)
library(raster)
library(dplyr)
library(randomForest)

#browseVignettes(package = "lidR")



# LAS data ----------------------------------------------------------------
 

#read in LAS data from orbital reconstruction
wade_t3_las <- readLAS("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/point_cloud/20240103_Wade_tree3_orbital_group1_densified_point_cloud.las",
                   select = "RGB")

#explore the data
print(wade_t3_las)
str(wade_t3_las)
plot(wade_t3_las)
#plot(wade_t3, color = RGB)

#clip LAS to extent of focal tree
##load in shape file of manually segmented focal tree
wade_t3_shp <- st_read("C:/Users/hmz25/Desktop/TX 2024 analysis/qgis/wade_t3_extent.shp")

##reproject to same crs
#projection(wade_t3) <- crs(wade_t3_ext)
#wade_t3_las_reproj <- st_transform(wade_t3_las, projection(wade_t3_ext))
wade_t3_shp_reproj <- st_transform(wade_t3_shp, st_crs(wade_t3_las))
compareCRS(wade_t3_las, wade_t3_shp_reproj) #false but it's okay

#clip las point cloud to focal tree
wade_t3_las_clip <- clip_roi(wade_t3_las, wade_t3_shp_reproj)
plot(wade_t3_las_clip) #yay!
##can we apply index to the las plot? 
#str(wade_t3_las_clip)

#define las bands
R <- wade_t3_las@data$R
#print(R)
G <- wade_t3_las@data$G
B <- wade_t3_las@data$B


# pixel classifier --------------------------------------------------------

#run pixel classifier to filter out non cone and foliage pixels from images

#load pictures to create training dataset for non-foliage and non-cones
sup_not_twig <- stack("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/not_2.tiff")
#plotRGB(sup_not_twig) 
#sup_not_twig$not_1_1 
#sup_not_twig$not_1_1[1:100]

#dataframe for non-foliage and non-cone pixels
sup_not_twig_df <- as.data.frame(sup_not_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  filter(r != 255) %>% 
  mutate(class = "not") 
#head(sup_not_twig_df)

#load pictures to create training dataset for foliage and cones
sup_yes_twig <- stack("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/yes_1.tif")
#plotRGB(sup_yes_twig) 
#sup_yes_twig$yes_1_1 
#sup_yes_twig$yes_1_1[1:100]

#dataframe for foliage and cone pixels
sup_yes_twig_df <- as.data.frame(sup_yes_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  filter(r != 255) %>% 
  mutate(class = "yes")  
#head(sup_yes_twig_df)

#combine training datasets and randomly select pixels from theme
training_df <- bind_rows(sup_not_twig_df, sup_yes_twig_df) %>%
  dplyr::select(-max) %>%
  sample_n(100000) %>%
  mutate(class = as.factor(class))
#RENAME SO THAT LAYER YES = 1, NO = 0 INSTEAD OF DEFAULT 1 AND 2 
#head(training_df)
#str(training_df)

#run a pixel-based classifier
rf_mask <- randomForest(class ~ ., data = training_df, na.action=na.omit)
#rf_mask


# apply pixel classifier to LAS orbital image --------------------------

#create df of las pixel values
wade_t3_las_clip_df <- as.data.frame(wade_t3_las_clip@data) %>%
  rename(c("r" = 4 , "g" = 5, "b" = 6)) %>% 
  select("r", "g", "b") %>% 
  mutate(r8 = r/256,
         g8 = g/256,
         b8 = b/256) %>% #convert 16 bit color in las to 8 bit color in raster  
  select("r8", "g8", "b8") %>% 
  rename(c("r" = 1, "g" = 2, "b" = 3))
#head(wade_t3_las_clip_df)

#create df with pixel values assigned to cone/foliage vs not cone/foliage 
wade_t3_las_clip_mask_df <- predict(rf_mask, wade_t3_las_clip_df) %>% 
  as.data.frame()
# head(photo_i_mask_df)
# photo_i_df <- photo_i_df %>%
#   mutate(is_foreground = photo_i_mask_df,
#          is_foreground_numeric = case_when( is_foreground == "yes" ~ 1,
#                                             is_foreground == "not" ~ 0))
# #head(photo_i_df)

wade_t3_las_mask_df <- wade_t3_las_clip_mask_df %>% 
  cbind(wade_t3_las_clip_df, wade_t3_las_clip_mask_df) %>% 
  select(c(1:4)) %>% 
  rename("rf_mask" = 1)

#add pixel classification values back to las data 
wade_t3_las_clip@data$rf_mask <- wade_t3_las_clip_mask_df # photo_i_df$is_foreground
#str(wade_t3_las_clip)
#print(wade_t3_las_clip@data$rf_mask)

# ortho data --------------------------------------------------------------

#load in ortho from wade
wade_ortho <- stack("C:/Users/hmz25/Documents/pix4d/20240103_Wade/3_dsm_ortho/2_mosaic/20240103_Wade_transparent_mosaic_group1.tif")
#plotRGB(wade_ortho)

#crop ortho down to smaller size 
wade_ortho_clip <- crop(wade_ortho, extent(wade_t3_shp_reproj))
#plotRGB(wade_ortho_clip)

#extract t3 from ortho
wade_t3_rgb <- mask(wade_ortho_clip, wade_t3_shp_reproj)
plotRGB(wade_t3_rgb)

#apply pixel classifier to ortho image
wade_t3_rgb_sub <- subset(wade_t3_rgb, 1:3) 
names(wade_t3_rgb_sub) <- colnames(training_df)[1:3]
wade_t3_rgb_masked <- predict(wade_t3_rgb_sub, rf_mask)
#plot(wade_t3_rgb_masked)

wade_t3_rgb <- stack(wade_t3_rgb_sub, wade_t3_rgb_masked)

#create dataframe for rgb image
wade_t3_rgb_df <- as.data.frame(wade_t3_rgb) %>% 
  na.omit() %>% 
  rename("rf_mask" = "layer_value")



# orange index -----------------------------------------------------


#calculate index values for las orbital

## add index column for las masked df 
wade_t3_las_mask_df <- wade_t3_las_mask_df %>% 
  filter(rf_mask == "yes") %>% 
  mutate(orange_index = (r-g)/(r+g))

#take sum index value of orbital (to compare to sum index value of same tree from ortho) 
sum_index_las <- sum(wade_t3_las_mask_df$orange_index)


#calculate index values for rgb ortho image 

wade_t3_rgb_mask_df <- wade_t3_rgb_df %>% 
  filter(rf_mask == "yes") %>% 
  mutate(orange_index = (r-g)/(r+g))
#head(wade_t3_rgb_mask_df)  

#take sum index value for t3 from ortho image
sum_index_rgb <- sum(wade_t3_rgb_mask_df$orange_index) 


# visualize index on las and rgb images -----------------------------------


#visualize index on wade t3

#define index 
ortho_index <- orange_index <- function(r, g) {
  index <- ((r-g)/(r+g))
  return(index)
}

red_band <- wade_ortho_clip[[1]]
green_band <- wade_ortho_clip[[2]]

index_map <- overlay(red_band, green_band, fun = ortho_index)

plot(index_map)

#mask out everything from ortho image except for tree crown polygon 
t3_raster <- raster::rasterize(wade_t3_shp_reproj, index_map, field = 1)
#plot(t3_raster)

wade_t3_ortho_index <- mask(index_map, t3_raster)
plot(wade_t3_ortho_index)

cone_palette <- colorRampPalette(c("green", "orangered"))

plot(wade_t3_ortho_index, col = cone_palette(100), alpha = 1, axes = FALSE, box = FALSE)

#NEXT STEPS
## use equation to get number of cones per pixel 
## figure out pixel size in las file
## create loops to do this for all orbital images rather than just one 

