#script to apply index to coarsened handheld drone images

setwd("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/")


# examine distribution of index values in handheld image ------------------


# cath10 <- rast("cath_t10.tif")
cath10 <- raster::stack("cath_t10.tif")
plotRGB(cath10)

mask_layer <- cath10[[4]] == 2 

# cath10_masked <- mask(cath10[[1:3]], mask_layer, maskvalues=TRUE)
cath10_masked <- raster::mask(cath10[[1:3]], mask_layer, maskvalue = FALSE)
plot(cath10_masked)
plotRGB(cath10_masked)

#examining distribution of index values 
cath10_df <- as.data.frame(cath10_masked) %>% 
  rename(r = 1,
         g = 2,
         b = 3) %>% 
  mutate(index = (r-g)/(r+g))

ggplot(cath10_df) +
  geom_histogram(aes(x = index))
#could plot other indices to see the most bimodal 


# coarsen resolution of handheld images to resolution of orthos -----------


#load in ortho to see resolution
# cath10_ortho <- rast("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/quadrat pics 2025/backdrops/cath_20250110_t10.JPG")
# plotRGB(cath10_ortho)

cath10_ortho <- stack("C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/quadrat pics 2025/cropped_quadrat_pics/cath_t10.tif")
plotRGB(cath10_ortho)

names(cath10_ortho) <- c("r", "g", "b")
cath10_ortho$index <- (cath10_ortho$r-cath10_ortho$g)/(cath10_ortho$r+cath10_ortho$g)
plot(cath10_ortho$index)

#compare resolution of ortho with resolution of handheld 
dim(cath10) #876 966
dim(cath10_ortho) #9 10

#scale by factor of 100 x and 90 y 
# cath10_agg <- aggregate(cath10, fact=c(100,90))
cath10_agg <- aggregate(cath10_masked, fact=c(100,90))
# raster(cath10_agg)
# plotRGB(raster::stack(cath10_agg))
plotRGB(cath10_agg)
dim(cath10_agg)
dim(cath10_ortho)

#calculate index for handheld and re-scaled index and examine how they change based on resolution
names(cath10) <- c("r", "g", "b", "mask")
# names(cath10_agg) <- c("r", "g", "b", "mask")
names(cath10_agg) <- c("r", "g", "b")

cath10$index <- (cath10$r-cath10$g)/(cath10$r+cath10$g)
plot(cath10$index)
hist(cath10$index, xlim = c(-0.4,0.4))

cath10_agg$index <- (cath10_agg$r-cath10_agg$g)/(cath10_agg$r+cath10_agg$g)
plot(cath10_agg$index)
hist(cath10_agg$index, xlim = c(-0.4,0.4))

hist(cath10_ortho$index, xlim = c(-0.4,0.4))

#look at same info for different image

#terra 
# sono1 <- rast("sono_t1.tif")
# plotRGB(sono1)
# 
# mask_layer <- sono1[[4]]
# 
# sono1_masked <- mask(sono1[[1:3]], mask_layer, maskvalues=TRUE)
# plot(sono1_masked)
# plotRGB(sono1_masked)
# 
# names(sono1_masked) <- c("r", "g", "b")
# 
# sono1_masked$index <- (sono1_masked$r-sono1_masked$g)/(sono1_masked$r+sono1_masked$g)
# plot(sono1_masked$index)
# 
# sono1_masked_agg <- aggregate(sono1_masked, fact=c(100,90), fun = "mean")
# plotRGB(sono1_masked_agg)
# 
# sono1_agg <- aggregate(sono1, fact=c(100,90), fun = "mean")
# plotRGB(sono1_agg)

#raster
sono1 <- raster::stack("sono_t1.tif")
plotRGB(sono1)

mask_layer <- sono1[[4]] == 2

sono1_masked <- raster::mask(sono1[[1:3]], mask_layer, maskvalue=FALSE)
plot(sono1_masked)
plotRGB(sono1_masked)

names(sono1_masked) <- c("r", "g", "b")

sono1_masked$index <- (sono1_masked$r-sono1_masked$g)/(sono1_masked$r+sono1_masked$g)
plot(sono1_masked$index)

sono1_masked_agg <- aggregate(sono1_masked, fact=c(100,90), fun = "mean")
plotRGB(sono1_masked_agg)
plot(sono1_masked_agg$index)


#testing how index performs on aggregated photos

# create new folder with white balanced masked photos


# apply index to all masked drone photos -----------------------------------------------
setwd("C:/Users/hmz25/Box/")

# photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/"
photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_white_balanced"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)

photo_df <- data.frame()

# i = 1

#try with and without masking out cone vs not cone

#without masking 
#CHANGE TO BE LOADED IN AS RASTER STACK
for(i in seq_along(photo_list)){
  #load in image
  photo_i <- rast(photo_list_full_dir[i])
  # plotRGB(photo_i)
  
  #aggregate pixels to coarser resolution
  photo_i_agg <- aggregate(photo_i, fact=c(100,100), fun = "mean")
  # plotRGB(photo_i_agg)
  
  # #mask out non foliage/cone pixels 
  # photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  # photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA

  #calculate index 
  photo_i_index_rg_dif <- (photo_i_agg[[1]] - photo_i_agg[[2]])/(photo_i_agg[[1]] + photo_i_agg[[2]]) 
  # plot(photo_i_index_rg_dif)
  
  # plotRGB(photo_i)
  # #color_ramp <- colorRampPalette(c("darkgreen", "orange"))
  # plot(photo_i_index_rg_dif, col = color_ramp(25))
  
  photo_i_rg_dif <- global(photo_i_index_rg_dif, "mean", na.rm = T) #take mean index value 
  
  #create threshold value so that all values below threshold below that value (not cones) are dropped 
  photo_i_index_rg_thresh <- photo_i_index_rg_dif #plot(photo_i)
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] < 0.05] <- 0 #not cone
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] > 0.05] <- 1 #cone
  photo_i_index_rg_thresh_mean <- global(photo_i_index_rg_thresh, "mean", na.rm = T) #calculate mean number of cells that are cone  #plot(photo_i_index_rg_thresh)
  photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean*
    ((ncell(photo_i_index_rg_thresh) - global(is.na(photo_i_index_rg_thresh), "sum", na.rm = FALSE)[1, 1]) / 
       ncell(photo_i_index_rg_thresh))
  #threshold sum is mean number of pixels that are cones
  #multiplied by the number of thresholded cells minus the sum of non-cone pixels divided by the number of thresholded cells
  #AKA the mean index * the proportion of pixels that are cone  
  #'photo_i_index_rg_thresh_mean' is the proportion of cone-classified pixels among valid (non-NA) cells.
  #'photo_i_index_rg_thresh_sum' adjusts that mean by valid cell coverage, giving a normalized cone score.
  
  #add image pixel and index values to a data frame 
  photo_i_r <- global(photo_i[[1]], "mean")
  photo_i_g <- global(photo_i[[2]], "mean")
  photo_i_b <- global(photo_i[[3]], "mean")
  photo_i_mask <- global(photo_i[[4]], "mean") - 1 #1 = background, 2 = foreground
  
  photo_i_rgb <- data.frame(photo_list[i], photo_i_r, photo_i_g, photo_i_b, photo_i_mask, photo_i_rg_dif, 
                            photo_i_index_rg_thresh_mean, photo_i_index_rg_thresh_sum)
  
  photo_df <- bind_rows(photo_df, photo_i_rgb)
  
  print(i)
}

head(photo_df)

#with masking

i = 2

library(officer)
library(rvg)
library(gridExtra)

masked_photo_df <- data.frame()

ppt <- read_pptx()

for(i in seq_along(photo_list)){
  #load in image
  photo_i <- raster::stack(photo_list_full_dir[i])
  # plotRGB(photo_i)
  
  # #mask out non foliage/cone pixels
  # photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  # photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
  
  #aggregate pixels to coarser resolution
  photo_i_agg <- aggregate(photo_i, fact=c(100,100), fun = "mean")
  # plot(photo_i_agg)
  # plotRGB(photo_i_agg)
  
  #mask out non foliage/cone pixels
  # plot(photo_i_agg[[4]])
  photo_i_agg[[1]][photo_i_agg[[4]] < 1.4] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i_agg[[2]][photo_i_agg[[4]] < 1.4] <- NA #set all green pixel values that are not foliage/cones to NA
  # plot(photo_i_agg)
  # plotRGB(photo_i_agg)
  
  #calculate index 
  photo_i_index_rg_dif <- (photo_i_agg[[1]] - photo_i_agg[[2]])/(photo_i_agg[[1]] + photo_i_agg[[2]]) 
  # plot(photo_i_index_rg_dif)
  
  # plotRGB(photo_i)
  # #color_ramp <- colorRampPalette(c("darkgreen", "orange"))
  # plot(photo_i_index_rg_dif, col = color_ramp(25))
  
  photo_i_rg_dif <- cellStats(photo_i_index_rg_dif, "mean") #take mean index value
  
  #create threshold value so that all values below threshold below that value (not cones) are dropped 
  photo_i_index_rg_thresh <- photo_i_index_rg_dif #plot(photo_i)
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] < 0.05] <- 0
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] > 0.05] <- 1
  photo_i_index_rg_thresh_mean <- cellStats(photo_i_index_rg_thresh, "mean") #plot(photo_i_index_rg_thresh)
  photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean* 
    ((ncell(photo_i_index_rg_thresh) - summary(photo_i_index_rg_thresh)[6]) / ncell(photo_i_index_rg_thresh)) #multiply by portion of cells not NA
  #(ncell(photo_i_index_rg_thresh) - ncell(photo_i_index_rg_thresh, na.omit = TRUE)) #plotRGB(photo_i) #ncell(photo_i) #cellStats(photo_i_index_rg_thresh, "mean")
  # #threshold sum is mean number of pixels that are cones
  # #multiplied by the number of thresholded cells minus the sum of non-cone pixels divided by the number of thresholded cells
  # #AKA the mean index * the proportion of pixels that are cone  
  # #'photo_i_index_rg_thresh_mean' is the proportion of cone-classified pixels among valid (non-NA) cells
  # #'photo_i_index_rg_thresh_sum' adjusts that mean by valid cell coverage, giving a normalized cone score
 
  #add image pixel and index values to a data frame
  photo_i_r <- cellStats(photo_i[[1]], "mean")
  photo_i_g <- cellStats(photo_i[[2]], "mean")
  photo_i_b <- cellStats(photo_i[[3]], "mean")
  photo_i_mask <- cellStats(photo_i[[4]], "mean") - 1 #1 = background, 2 = foreground
  
  photo_i_rgb <- data.frame(photo_list[i], photo_i_r, photo_i_g, photo_i_b, photo_i_mask, photo_i_rg_dif, 
                            photo_i_index_rg_thresh_mean, photo_i_index_rg_thresh_sum)
  
  masked_photo_df <- bind_rows(masked_photo_df, photo_i_rgb)
  
  # # Create a slide and set title
  # ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  # slide_title <- tools::file_path_sans_ext(basename(photo_list[i]))
  # ppt <- ph_with(ppt, value = slide_title, location = ph_location_type(type = "title"))
  # 
  # # Create individual plot objects as vector graphics
  # plot_orig <- dml(code = { plotRGB(photo_i, main = "original handheld") })
  # plot_agg  <- dml(code = { plotRGB(photo_i_agg, main = "aggregated") })
  # plot_idx  <- dml(code = { plot(photo_i_index_rg_dif, main = "index") })
  # 
  # # Place each plot on the slide in a column layout
  # ppt <- ph_with(ppt, plot_orig, location = ph_location(left = 0.1, top = 1.2, width = 3, height = 3))
  # ppt <- ph_with(ppt, plot_agg,  location = ph_location(left = 3.6, top = 1.2, width = 3, height = 3))
  # ppt <- ph_with(ppt, plot_idx,  location = ph_location(left = 7.1, top = 1.2, width = 3, height = 3))
  
  print(i)
}

head(masked_photo_df)

print(ppt, target = "aggregated_index_vis.pptx", overwrite = T)

#how does threshold look w cone density data?

#how does raw index value look w cone density data? 


# combine cone count data and rgb data ------------------------------------



#calculate total number of cones in each quadrat
cone_density_df <- cone_df_clean %>% 
  mutate(cone_per_g = count*weight,
         std = sd(cone_per_g)) %>% 
  group_by(site, tree, total_mass) %>% 
  summarize(mean_cone_dens = mean(cone_per_g)) %>% 
  summarize(total_cones = mean_cone_dens*total_mass) 


#join rgb data frame with cone density data frame
# photo_df_join <- photo_df %>%
#   mutate(site = str_extract(photo_list.i., "^[^_]+"),
#          tree = as.character(str_extract(photo_list.i., "(?<=_t)\\d+"))) %>% 
#   filter(!photo_list.i. %in% c("swee_t4.tif",
#                                "wade_t8.tif",
#                                "rock_t1.tif",
#                                "sono_t10.tif",
#                                "fish_t8.tif",
#                                "fish_t10.tif",
#                                "swee_t10.tif"))

masked_photo_df_join <- masked_photo_df %>%
  mutate(site = str_extract(photo_list.i., "^[^_]+"),
         tree = as.character(str_extract(photo_list.i., "(?<=_t)\\d+"))) %>% 
  filter(!photo_list.i. %in% c("swee_t4.tif",
                               "wade_t8.tif",
                               "rock_t1.tif",
                               "sono_t10.tif",
                               "fish_t8.tif",
                               "fish_t10.tif",
                               "swee_t10.tif"))

#imgs we could exclude 
#wade_10.tif
#fish_t12.tif
#swee_t3.tif
#sono_t1.tif
#cath_t10.tif
#swee_t8.tif
#swee_t5.tif

quadrat_cones <- left_join(cone_density_df, photo_df_join) %>%
  filter(!is.na(total_cones)) %>%
  filter(!is.na(mean.4)) #%>%
  # mutate(photo_i_mask = photo_i_mask - 1)

# quadrat_cones %>%
#   dplyr::select(site, tree, total_cones, mean.4) %>%
#   View()

masked_quadrat_cones <- left_join(cone_density_df, masked_photo_df_join) %>% 
  filter(!is.na(total_cones),
         !is.na(photo_i_rg_dif))

#raw index values

#no mask
ggplot(quadrat_cones, aes(x=mean.4, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)

#mask
ggplot(masked_quadrat_cones, aes(x=photo_i_rg_dif, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()

#with threshold 

#no mask 
ggplot(quadrat_cones, aes(x=mean.5, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few()

#mask
masked_quadrat_cones %>% 
  filter(photo_i_index_rg_thresh_sum > 0 ) %>% 
  ggplot(aes(x=photo_i_index_rg_thresh_sum, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few()


mod <- lm(photo_i_rg_dif ~ total_cones, data = quadrat_cones_rgb)
summary(mod)

quadrat_cones_rgb_test <- quadrat_cones_rgb %>%  
  filter((photo_i_r + photo_i_g + photo_i_b) > 0)

#quadrat_cones_rgb_test <- quadrat_cones_rgb_test %>% filter(photo_i_mask > 0.5) #applying this filter results in no values 

ggplot(quadrat_cones_rgb_test, aes(x=photo_i_index_rg_thresh_sum, y = total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)


fit <- lm(quadrat_cones_rgb_test$total_cones  ~ quadrat_cones_rgb_test$photo_i_index_rg_thresh_sum   )
summary(fit)
r_sq <- summary(fit)$r.squared

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = total_cones, label = paste(site, tree))) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few() +
  annotate("text", 
           x = min(quadrat_cones_rgb$photo_i_index_rg_thresh_sum), 
           y = max(quadrat_cones_rgb$total_cones), 
           label = paste("R² =", round(r_sq, 3)), 
           hjust = 0, size = 4, color = "black")

ggplot(quadrat_cones_rgb, aes(x=photo_i_index_rg_thresh_sum, y = total_cones, label = paste(site, tree))) + 
  geom_point(alpha = 0.5) + 
  geom_label() +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + 
  ggthemes::theme_few() +
  annotate("text", 
           x = min(quadrat_cones_rgb$photo_i_index_rg_thresh_sum), 
           y = max(quadrat_cones_rgb$total_cones), 
           label = paste("R² =", round(r_sq, 3)), 
           hjust = 0, size = 4, color = "black")


#imgs we could exclude 
#wade_10.tif
#fish_t12.tif
#swee_t3.tif
#sono_t1.tif
#cath_t10.tif
#swee_t8.tif
#swee_t5.tif

