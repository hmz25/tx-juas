#script to apply index to coarsened handheld drone images

library(officer)
library(rvg)
library(gridExtra)
library(raster)
library(terra)
library(tidyverse)
library(officer)
library(magrittr)

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

# apply index to aggregated photos -----------------------------------------------

setwd("C:/Users/hmz25/Box/")

# photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/masked_handheld_quadrat_pics/"
photo_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_white_balanced"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)

# i = 1

#note: should not be using threshold because these are coarsened

photo_df <- data.frame()

for(i in seq_along(photo_list)){
  #load in image
  photo_i <- raster::stack(photo_list_full_dir[i])
  # plotRGB(photo_i)
  # plot(photo_i)
  
  #mask out non foliage/cone pixels
  photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
  
  #aggregate pixels to coarser resolution
  # photo_i_agg <- aggregate(photo_i, fact=c(100,100), fun = "mean")
  # # plot(photo_i_agg)
  # # plotRGB(photo_i_agg)
  
  #check if '_female' is in the file name
  #females have been selected from aerial imgs, so already at correct res
  if (grepl("_female", basename(photo_list[i]))) {
    photo_i_agg <- photo_i  #do not aggregate, just assign
  } else {
    #aggregate pixels to coarser resolution
    photo_i_agg <- aggregate(photo_i, fact = c(100, 100), fun = "mean")
  }
  
  #mask out non foliage/cone pixels after aggregation 
  # plot(photo_i_agg[[4]])
  photo_i_agg[[1]][photo_i_agg[[4]] < 1.5] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i_agg[[2]][photo_i_agg[[4]] < 1.5] <- NA #set all green pixel values that are not foliage/cones to NA
  # plot(photo_i_agg)
  # plotRGB(photo_i_agg)
  
  #calculate index 
  photo_i_index_rg_dif <- (photo_i_agg[[1]] - photo_i_agg[[2]])/(photo_i_agg[[1]] + photo_i_agg[[2]]) 
  # plot(photo_i_index_rg_dif)
  
  #for cool viz
  # plotRGB(photo_i)
  # #color_ramp <- colorRampPalette(c("darkgreen", "orange"))
  # plot(photo_i_index_rg_dif, col = color_ramp(25))
  
  #calculate mean index 
  photo_i_rg_dif <- cellStats(photo_i_index_rg_dif, "mean") #take mean index value
  
  #create threshold value so that all values below threshold below that value (not cones) are dropped 
  photo_i_index_rg_thresh <- photo_i_index_rg_dif #plot(photo_i)
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] < 0.05] <- 0 #not cone
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] > 0.05] <- 1 #cone plot(photo_i_index_rg_thresh)
  photo_i_index_rg_thresh_mean <- cellStats(photo_i_index_rg_thresh, "mean") #plot(photo_i_index_rg_thresh)
  photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean* 
    ((ncell(photo_i_index_rg_thresh) - summary(photo_i_index_rg_thresh)[6]) / ncell(photo_i_index_rg_thresh)) #multiply by portion of cells not NA
  #(ncell(photo_i_index_rg_thresh) - ncell(photo_i_index_rg_thresh, na.omit = TRUE)) #plotRGB(photo_i) #ncell(photo_i) #cellStats(photo_i_index_rg_thresh, "mean")
  # #threshold sum is mean number of pixels that are cones
  # #multiplied by the number of threshold cells minus the sum of non-cone pixels divided by the number of thresholded cells
  # #AKA the mean index * the proportion of pixels that are cone  
  # #'photo_i_index_rg_thresh_mean' is the proportion of cone-classified pixels among valid (non-NA) cells
  # #'photo_i_index_rg_thresh_sum' adjusts that mean by valid cell coverage, giving a normalized cone score
 
  #calculate mean pixel and index values 
  photo_i_r <- cellStats(photo_i[[1]], "mean")
  photo_i_g <- cellStats(photo_i[[2]], "mean")
  photo_i_b <- cellStats(photo_i[[3]], "mean")
  photo_i_mask <- cellStats(photo_i[[4]], "mean") - 1 #1 = background, 2 = foreground
  
  #add values to a data frame
  photo_i_rgb <- data.frame(photo_list[i], photo_i_r, photo_i_g, photo_i_b, photo_i_mask, photo_i_rg_dif,
                            photo_i_index_rg_thresh_mean, photo_i_index_rg_thresh_sum)
  
  photo_df <- bind_rows(photo_df, photo_i_rgb)
  
  print(i)
}

head(photo_df)

#how does threshold look w cone density data?
#how does raw index value look w cone density data? 

# calculate cone counts for all quadrats ------------------------------------

cone_df <- read_csv("Katz lab/texas/cone processing 25 - counts.csv")

cone_df_clean <- cone_df |> 
  rename(name_site = site,
         total_subsample_weight = subsample_weight) |>  
  pivot_longer(
    cols = starts_with("s"),
    names_to = c("sample_n", ".value"),
    names_pattern = "s(\\d+)_(.*)") |> 
  mutate(site = substr(name_site, 1, 4)) |> 
  dplyr::select(date_collected, site, tree, total_mass, total_subsample_weight, sample_n, weight, count, notes) %>% 
  mutate(tree = as.character(tree)) %>% 
  mutate(tree = if_else(tree == "female", 
                        "female", 
                        paste0("t", as.character(tree))))

#visualize error bars on cone count data
cone_df_clean %>% 
  group_by(site, tree) %>% 
  mutate(cone_per_g = count/weight) %>% 
  ggplot() +
  geom_boxplot(aes(x = tree, y = cone_per_g))+
  facet_wrap(~site)

#calculate cone per g in each quadrat

#find avg weight of cones
g_cone_df <- read_csv("Katz lab/texas/cone processing 25 - cone_g.csv")

mean_g_cone_df <- g_cone_df %>% 
  mutate(g_cone = cone_weight/cone_count) %>% 
  summarize(mean_g_cone = mean(g_cone))

print(mean_g_cone_df)

#from analysis above, mean g of cone = 0.002314149

cone_g_df <- cone_df_clean %>% 
  dplyr::select(-notes) %>% 
  mutate(g_cones = count*0.002314149, #multiply cone count by weight of each cone
         g_fol = weight-g_cones, #calculate g fol + twig in sample by subtracting total weight of cones
         cones_per_fol = g_cones/g_fol) %>% #calculate ratio of g cone/g twig + fol 
  group_by(site, tree, total_mass) %>% 
  summarize(mean_gcones_gfol = mean(cones_per_fol),
            mean_cones_per_g = mean(count/weight)) %>% #calculate mean ratio of g cone/g twig + fol and mean # cones per g sample
  summarize(total_gcones_gfol = mean_gcones_gfol*total_mass,
            total_cones_per_g = mean_cones_per_g*total_mass) #calculate total g cone/g twig + fol for sample and total #cones per sample 

# join rgb data frame with cone density data frame ------------------------

#edit photo df to be able to join columns with cone density df 
photo_df_join <- photo_df %>%
  mutate(site = str_extract(photo_list.i., "^[^_]+"),
         tree = as.character(str_extract(photo_list.i., "(?<=_)[^\\.]+"))) # %>% #exclude pics that have lots of shadows
  # filter(!photo_list.i. %in% c("swee_t4.tif", 
  #                              "wade_t8.tif",
  #                              "rock_t1.tif",
  #                              "sono_t10.tif",
  #                              "fish_t8.tif",
  #                              "fish_t10.tif",
  #                              "swee_t10.tif"))


#imgs we could exclude 
#wade_10.tif
#fish_t12.tif
#swee_t3.tif
#sono_t1.tif
#cath_t10.tif
#swee_t8.tif
#swee_t5.tif

quadrat_cones <- left_join(photo_df_join, cone_g_df, by = c("site", "tree")) %>% 
  mutate(total_gcones_gfol = replace_na(total_gcones_gfol, 0),
         total_cones_per_g = replace_na(total_cones_per_g, 0)) #replace NAs with zeros for analysis


# create slideshow to visualize quadrat imgs with PCD ---------------------

#process images and save plots
output_dir <- "pptx_imgs"
dir.create(output_dir, showWarnings = FALSE)

# i = 1

slide_data <- data.frame()

for (i in seq_along(photo_list)) {
  photo_i <- raster::stack(photo_list_full_dir[i])
  photo_name <- tools::file_path_sans_ext(basename(photo_list_full_dir[i]))
  
  # Process image as before (masking, aggregation, index)
  
  #mask out non foliage/cone pixels
  photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
  
  
  #check if '_female' is in the file name
  #females have been selected fron aerial imgs, so already at correct res
  if (grepl("_female", basename(photo_list[i]))) {
    photo_i_agg <- photo_i  #do not aggregate, just assign
  } else {
    #aggregate pixels to coarser resolution
    photo_i_agg <- aggregate(photo_i, fact = c(100, 100), fun = "mean")
  }
  
  #mask out non foliage/cone pixels after aggregation 
  # plot(photo_i_agg[[4]])
  photo_i_agg[[1]][photo_i_agg[[4]] < 1.5] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i_agg[[2]][photo_i_agg[[4]] < 1.5] <- NA #set all green pixel values that are not foliage/cones to NA
  # plot(photo_i_agg)
  # plotRGB(photo_i_agg)
  
  photo_i_index_rg_dif <- (photo_i_agg[[1]] - photo_i_agg[[2]]) / 
    (photo_i_agg[[1]] + photo_i_agg[[2]])
  photo_i_rg_dif <- cellStats(photo_i_index_rg_dif, "mean")
  
  # Save plots with consistent naming
  orig_path <- file.path(output_dir, paste0(photo_name, "_orig.png"))
  agg_path <- file.path(output_dir, paste0(photo_name, "_agg.png"))
  index_path <- file.path(output_dir, paste0(photo_name, "_index.png"))
  
  # Original
  png(orig_path, width = 800, height = 800)
  plotRGB(photo_i, main = "original")
  dev.off()
  
  # Aggregated
  png(agg_path, width = 800, height = 800)
  plotRGB(photo_i_agg, main = "coarsened")
  dev.off()
  
  # Index with fixed zlim
  png(index_path, width = 5, height = 5, units = "in", res = 300)
  plot(
    photo_i_index_rg_dif,
    zlim = c(-0.2, 0.2),
    main = "index",
    cex.main = 2,  # Bigger title
    legend.width = 2.5,    # Make the scale bar thicker
    legend.shrink = 0.8,
    legend.args = list(
      text = "index value"
    )
  )

  dev.off()
  
  # Save metadata
  slide_data <- bind_rows(slide_data, data.frame(
    name = photo_name,
    orig_path, agg_path, index_path,
    mean_index = photo_i_rg_dif
  ))
}

#join with cone data
slide_data <- slide_data %>%
  mutate(site = str_extract(name, "^[^_]+"),
         tree = str_extract(name, "(?<=_)[^\\.]+"))

slide_data <- left_join(slide_data, cone_g_df, by = c("site", "tree")) %>%
  mutate(
    total_gcones_gfol = replace_na(total_gcones_gfol, 0),
    total_cones_per_g = replace_na(total_cones_per_g, 0)
  )

#generate power point
ppt <- read_pptx()

for (i in seq_len(nrow(slide_data))) {
  row <- as.list(slide_data[i, ])  
  
  text_block <- fpar(
    ftext(sprintf(
    "mean index: %.3f\ng cone/g fol: %.2f g\ntotal cones: %.0f",
    row$mean_index,
    row$total_gcones_gfol,
    row$total_cones_per_g
  ),
  fp_text(font.size = 14, font.family = "Arial")
    ),
  fp_p = fp_par(text.align = "left")
  )
  
  ppt <- ppt %>%
    #add slide with default layout
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    
    #title at the top
    ph_with(
      value = row$name,
      location = ph_location(left = 0.5, top = 0, width = 9, height = 1)
    ) %>%
    
    #original image: top-left under title
    ph_with(
      external_img(row$orig_path, height = 3, width = 3.5),
      location = ph_location(left = 0.5, top = 1.0)
    ) %>%
    
    #text block: right of original image
    ph_with(
      value = text_block,
      location = ph_location(left = 5, top = 0.7, width = 5, height = 2.5)
    ) %>%
    
    #coarsened image: directly below original image
    ph_with(
      external_img(row$agg_path, height = 3, width = 3.5),
      location = ph_location(left = 0.5, top = 4.2)
    ) %>%
    
    #index plot: right of coarsened image (slightly larger)
    ph_with(
      external_img(row$index_path, height = 5, width = 5, unit = "in"),
      location = ph_location(left = 5, top = 2.5, width = 5, height = 5)
    )
}

#save presentation
print(ppt, target = "cone_slides.pptx", overwrite = T)

# analyze cone density ~ index values -------------------------------------

#raw index values with g cones/g fol
ggplot(quadrat_cones, aes(x = photo_i_rg_dif, y = total_gcones_gfol)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm") + 
  xlab("spectral index") 

#threshold index values with g cones/g fol
ggplot(quadrat_cones, aes(x = photo_i_index_rg_thresh_sum, y = total_gcones_gfol)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm") + 
  xlab("spectral index") 

#raw index values with total cones
ggplot(quadrat_cones, aes(x = photo_i_rg_dif, y = total_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm") + 
  xlab("spectral index")

#threshold index values with total cones
ggplot(quadrat_cones, aes(x = photo_i_index_rg_thresh_sum, y = total_cones_per_g)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm") + 
  xlab("spectral index")


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




