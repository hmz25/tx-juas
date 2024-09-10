library(raster)
library(readr)
#library(tidyverse)
library(terra)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
#library(matrixStats)
#install.packages("randomForest")
library(randomForest)
library(lubridate)
library(ggplot2)

#add in cone density per quadrat data

quadrat_cones <- read_csv("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/2024_quadratcones.csv")

#add cones per weight columns
quadrat_cones <- quadrat_cones %>%
  select(date_collected,
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
  select(date_collected,
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
  mutate(total_cones = cone_mean*total_mass)

#rename date column and ensure dates are in the right format
quadrat_cones <- rename(quadrat_cones, date = date_collected)
quadrat_cones$date <- mdy(quadrat_cones$date)

#rename site column to match the site column for the pixels
quadrat_cones$site <- substr(quadrat_cones$site, 1, 4)

setwd("C:/Users/dsk273")
#create a function to load image files
list.files("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels") #83 files

eyelevel_files <- list.files("~/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels")#[1]
image_directory <- "~/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels/"


test <- stack("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels/kimble_t3_q3.tif")
plotRGB(test)


test2 <- test
test2$kimble_t3_q3_3[test2$kimble_t3_q3_1 > 100 & test2$kimble_t3_q3_2 > 100 & test2$kimble_t3_q3_3 > 100] <- 255
test2$kimble_t3_q3_1[test2$kimble_t3_q3_1/test2$kimble_t3_q3_2 > 1.5] <- 255
plotRGB(test2)

test3 <- test$kimble_t3_q3_1/test$kimble_t3_q3_3
plot(test3)



### create a model to mask non foliage and non cones
#training dataset for non-foliage and non-cones
sup_not_twig <- stack("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/not_2.tiff")
#plotRGB(sup_not_twig) #sup_not_twig$not_1_1 #sup_not_twig$not_1_1[1:100]

sup_not_twig_df <- as.data.frame(sup_not_twig) %>%
  filter(not_1_1 != 255) %>%  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  mutate(class = "not") #head(sup_not_twig_df)

#training dataset for foliage and cones
sup_yes_twig <- stack("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/yes_1.tif")
#plotRGB(sup_yes_twig) #sup_yes_twig$yes_1_1 #sup_yes_twig$yes_1_1[1:100]

sup_yes_twig_df <- as.data.frame(sup_yes_twig) %>%
  filter(yes_1_1 != 255) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3, "max" = 4)) %>%
  mutate(class = "yes")  #head(sup_yes_twig_df)

#combine training datasets and randomly select pixels from theme
training_df <- bind_rows(sup_not_twig_df, sup_yes_twig_df) %>%
  dplyr::select(-max) %>%
  sample_n(100000) %>%
  mutate(class = as.factor(class))
# head(training_df)
# str(training_df)

#run a pixel-based classifier
rf_mask <- randomForest(class ~ ., data = training_df, na.action=na.omit)
rf_mask





### apply model to a single photo
photo_i <- stack("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels/sonora_t6_q8.tif")
#plotRGB(photo_i)

photo_i_df <- as.data.frame(photo_i) %>%
  rename(c("r" = 1 , "g" = 2, "b" = 3))  #head(test_df)

photo_i_mask_df <- predict(rf_mask, photo_i_df)
# head(photo_i_mask_df)
# photo_i_df <- photo_i_df %>%
#   mutate(is_foreground = photo_i_mask_df,
#          is_foreground_numeric = case_when( is_foreground == "yes" ~ 1,
#                                             is_foreground == "not" ~ 0))
# #head(photo_i_df)

photo_i <- addLayer(photo_i, photo_i[[3]])
photo_i[[4]] <- photo_i_mask_df # photo_i_df$is_forground
plot(photo_i)
# raster::writeRaster(photo_i, "C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/photo_i_test.tif")
# test <- raster::stack("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/sonora_t6_q8.tif")
# plot(test)



### apply model to all cropped handheld photos and save in a new folder
photo_dir <- "C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)


for(i in 1:length(photo_list)){
  photo_i <- stack(photo_list_full_dir[i])  #plotRGB(test)
  
  photo_i_df <- as.data.frame(photo_i) %>%
    rename(c("r" = 1 , "g" = 2, "b" = 3))  #head(test_df)
  
  photo_i_mask_df <- predict(rf_mask, photo_i_df)
  # head(photo_i_mask_df)
  # photo_i_df <- photo_i_df %>%
  #   mutate(is_foreground = photo_i_mask_df,
  #          is_foreground_numeric = case_when( is_foreground == "yes" ~ 1,
  #                                             is_foreground == "not" ~ 0))
  # #head(photo_i_df)
  
  photo_i <- addLayer(photo_i, photo_i[[3]])
  photo_i[[4]] <- photo_i_mask_df # photo_i_df$is_forground
  
  save_file_name = paste0("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/",photo_list[i])
  raster::writeRaster(photo_i, save_file_name, overwrite = TRUE)
  print(i)
}



### calculate rgb values and an index for each photo ####
photo_dir <- "C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/"
photo_list <- list.files(photo_dir, full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, full.names = TRUE)

photo_masked_df <- data.frame()

for(i in 1:length(photo_list)){
  photo_i <- stack(photo_list_full_dir[i]) #plot(photo_i) #photo_i <- stack(photo_list_full_dir[2])
  
  photo_i[[1]][photo_i[[4]] == 1] <- NA
  photo_i[[2]][photo_i[[4]] == 1] <- NA
  photo_i_index_rg_dif <- (photo_i[[1]] - photo_i[[2]])/(photo_i[[1]] + photo_i[[2]])
   
  photo_i_rg_dif <- cellStats(photo_i_index_rg_dif, "mean")
  
  photo_i_index_rg_thresh <- photo_i_index_rg_dif #plot(photo_i)
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] < 0.05] <- 0
  photo_i_index_rg_thresh[photo_i_index_rg_dif[] > 0.05] <- 1
  photo_i_index_rg_thresh_mean <- cellStats(photo_i_index_rg_thresh, "mean") #calculate  #plot(photo_i_index_rg_thresh)
  photo_i_index_rg_thresh_sum <- photo_i_index_rg_thresh_mean* 
    ((ncell(photo_i_index_rg_thresh) - summary(photo_i_index_rg_thresh)[6]) / ncell(photo_i_index_rg_thresh)) #multiply by portion of cells not NA
    #(ncell(photo_i_index_rg_thresh) - ncell(photo_i_index_rg_thresh, na.omit = TRUE)) #plotRGB(photo_i) #ncell(photo_i) #cellStats(photo_i_index_rg_thresh, "mean")

  
  photo_i_r <- cellStats(photo_i[[1]], "mean")
  photo_i_g <- cellStats(photo_i[[2]], "mean")
  photo_i_b <- cellStats(photo_i[[3]], "mean")
  photo_i_mask <- cellStats(photo_i[[4]], "mean") - 1 #1 == background, 2 = foreground
  
  photo_i_rgb <- data.frame(photo_list[i], photo_i_r, photo_i_g, photo_i_b, photo_i_mask, photo_i_rg_dif, 
                            photo_i_index_rg_thresh_mean, photo_i_index_rg_thresh_sum)
  photo_masked_df <- bind_rows(photo_masked_df, photo_i_rgb)
  print(i)
}



#add values to dataframe that has the number of cones per quadrat
photo_masked_df_join <- photo_masked_df %>%
  mutate(site = substr(photo_list.i., 1, 4),
         tree = as.numeric(stringr::str_sub(photo_list.i., -8, -8)),
         quadrat = stringr::str_sub(photo_list.i., -5, -5))

quadrat_cones_rgb <- left_join(quadrat_cones, photo_masked_df_join) %>%
  filter(!is.na(total_cones)) %>%
  filter(!is.na(photo_i_r)) %>% mutate(photo_i_mask = photo_i_mask - 1)

quadrat_cones_rgb_test <- quadrat_cones_rgb %>%  
  filter((photo_i_r + photo_i_g + photo_i_b) > 0)

quadrat_cones_rgb_test <- quadrat_cones_rgb_test %>% filter(photo_i_mask > 0.5)

ggplot(quadrat_cones_rgb_test, aes(x=photo_i_index_rg_thresh_sum, y = total_cones/(0.25*0.25))) + geom_point(alpha = 0.5) + theme_bw() + geom_smooth(method = "lm", se = FALSE) +
  xlab("spectral index") + ylab(cone~density~(cones/m^2)) + ggthemes::theme_few()#scale_color_viridis_c()  # + facet_wrap(~site)


fit <- lm(quadrat_cones_rgb_test$total_cones  ~ quadrat_cones_rgb_test$photo_i_index_rg_thresh_sum   )
summary(fit)

#fit$residuals
quadrat_cones_rgb_test  <- quadrat_cones_rgb_test %>%
  mutate(photo_i_rg_dif_resid = fit$residuals,
         photo_i_rg_dif_pred = fit$fitted.values)



cor(quadrat_cones_rgb_test)
quadrat_cones_rgb_test %>% dplyr::select(10:23, - photo_list.i.) %>% 
PerformanceAnalytics::chart.Correlation(.)


test <- raster::stack("C:/Users/dsk273/Box/texas/pollen_production/TX jan 24/data analysis/cropped masked eye level/kimble_t6_q7.tif")
plot(test)

test2 <- test
test2[[1]][test2[[4]] == 1] <- NA
test2[[2]][test2[[4]] == 1] <- NA
test2_index_rg_dif <- (test2[[1]] - test2[[2]])/(test2[[1]] + test2[[2]])

plot(test2_index_rg_dif)


##############################################################################################################
#hz old stuff





#define bright and dark thresholds for RGB
##no bright threshold for R, cone R values are too high
bright_g <- 215
bright_b <- 150

dark_r <- 35
dark_g <- 40
dark_b <- 20

process_eyelevel_images <- function(eyelevel_files){
  #use regex to extract site, tree number and quadrat number
  site_name <- substr(eyelevel_files, start = 1, stop = 4) #taking the first four characters of the site names
  tree_number <- str_extract(eyelevel_files, "(?<=_t)\\d+")
  quadrat_number <- str_extract(eyelevel_files, "(?<=_q)\\d+")
  #create a raster file
  image_file_path <- paste0(image_directory, eyelevel_files)
  raster_stack_i <- raster::stack(image_file_path)
  #disaggregate raster stack by factor of 30
  raster_stack_i_disag <- raster::aggregate(raster_stack_i, fact = c(29,33))
  #create a data frame from the raster
  raster_stack_df <- as.data.frame(values(raster_stack_i_disag)) %>%
    mutate(site = site_name,
           tree = tree_number,
           quadrat = quadrat_number) %>%
    rename("r" = 1, "g" = 2, "b" = 3)
  #filter out bright and dark pixels
  raster_stack_df <- raster_stack_df %>%
    filter(!(g >= bright_g & b >= bright_b) &
             !(r <= dark_r & g <= dark_g & b <= dark_b))
  #plot raster image to check if correct image has been loaded
  # plotRGB(raster_stack_i, r = 1, g = 2, b = 3, main = paste("Site:", site_name, "Tree:", tree_number, "Quadrat:", quadrat_number))
  #combine into a data frame
  return(raster_stack_df)
}

#checking the code
process_eyelevel_images(list.files("~/Box/texas/pollen_production/TX jan 24/data analysis/cropped eye levels")[15]) #pulling up file numbers named in the function to check if it works

#create data frame with the pixel values from the images
quadrat_pixels <- map_dfr(eyelevel_files, process_eyelevel_images)

# #normalize rgb values in df
# norm_quadrat_pixels <- quadrat_pixels %>%
#   mutate(
#     r = r / 255,
#     g = g / 255,
#     b = b / 255
#   )

#take average red, green, and blue pixel values per quadrat
avg_pixel_quadrat <- quadrat_pixels %>%
  group_by(site, quadrat, tree) %>%
  summarize(
    r = mean(r), #r is now mean of r values
    g = mean(g), #g is now mean of g values
    b = mean(b) #b is now mean of b values
  )  

#join pixel values per quadrat with cone numbers
quadrat_cones <- mutate(quadrat_cones, tree = as.character(tree))
avg_pixel_quadrat <- mutate(avg_pixel_quadrat, tree = as.character(tree))
avg_pixel_cone_density <- left_join(avg_pixel_quadrat, quadrat_cones,
                                    by = c('site', 'quadrat', 'tree')) %>%  
  select('site', 'tree', 'quadrat', 'date', 'r', 'g', 'b', 'cone_mean', 'cone_sd', 'total_cones')

# #export avg_pixel_cone_density df
# library(openxlsx)
# write.xlsx(avg_pixel_cone_density, "~/Box/texas/pollen_production/TX jan 24/data analysis")

#looking at avg pixel values based on cone density
avg_pixel_cone_density %>%
  ggplot(aes(x = r, y = total_cones)) +
  geom_point() +
  theme_bw()

avg_pixel_cone_density %>%
  ggplot(aes(x = g, y = total_cones)) +
  geom_point() +
  theme_bw()

avg_pixel_cone_density %>%
  ggplot(aes(x = b, y = total_cones)) +
  geom_point() +
  theme_bw()

hist(avg_pixel_cone_density$r, breaks = 20)
hist(avg_pixel_cone_density$g, breaks = 20)
hist(avg_pixel_cone_density$b, breaks = 20)

#exploring parameter space of empirically-derived indices
rgbIndex <- function(c1, c2, c3, r, g, b){
  x1 <- ((c1*r)+(c2*g)+(c3*b))/(r+g+b)
  return(x1)
}

rgIndex <- function(c1, c2, r, g, b){
  x2 <- ((c1*r)-(c2*g)/(r+g+b))
  return(x2)
}

occIndex <- function(c1, c2, r, g, b){
  x3 <- ((((c1*r)+(c2*g))/(r+g+b)))
  return(x3)
}

ratioIndex <- function(c1, c2, r, g){
  x4 <- ((c1*r) / (c2*g))
  return(x4)
}

difIndex <- function(c1, c2, r, g){
  x5 <- (c1*r-c2*g)
  return(x5)
}

normRatioDifIndex <- function(c1, c2, r, g){
  x6 <- ((c1*r-c2*g)/(c1*r+c2*g))*(c1*r/c2*g)
  return(x6)
}

normDifIndex <- function(c1, c2, r, g){
  x7 <- ((c1*r-c2*g)/(c1*r+c2*g))
  return(x7)
}

difRatioIndex <- function(c1, c2, r, g){
  x8 <- ((c1*r-c2*g)*(c1*r/c2*g))
  return(x8)
}

normSumIndex <- function(c1, c2, r, g, b){
  x9 <- ((((c1*r)+(c2*g))/(r+g+b))*(r+g))
  return(x9)
}

#create loop to populate new df with r-sq, rmse, p-val, slope, and intercept values for possible spectral indices

#initialize an empty data frame to store results
indices <- data.frame()

#generate coefficient combinations and evaluate indices
for (i in 1:length(seq(-1.01, 1, by = 0.1))) {
  for (j in 1:length(seq(-1.01, 1, by = 0.1))) {
    # for (c1 in seq(-1, 1, by = 0.1)) {
    #   for (c2 in seq(-1, 1, by = 0.1)) {
    
    c1 <- seq(-1.01, 1, by = 0.1)[i] # c1 <- 0.1 #c1 <- 0.8
    c2 <- seq(-1.01, 1, by = 0.1)[j] # c2 <- 0.1 #c2 <- 0.2
    
    #calculate indices for current coefficients
    avg_pixel_cone_density <- avg_pixel_cone_density %>%
      mutate(
        ratio_index = ratioIndex(c1, c2, r, g),
        dif_index = difIndex(c1, c2, r, g),
        normRatio_index = normRatioDifIndex(c1, c2, r, g),
        normDif_index = normDifIndex(c1, c2, r, g),
        difRatio_index = difRatioIndex(c1, c2, r, g))
    
    #calculate r-sq and rmse for ratio_index
    model_ratio <- lm(total_cones ~ ratio_index, data = avg_pixel_cone_density)
    ggplot(avg_pixel_cone_density, aes(x = ratio_index, y = cone_mean)) + geom_point()
    r_squared_ratio <- summary(model_ratio)$r.squared
    rmse_ratio <- sqrt(mean(model_ratio$residuals^2))
    
    #calculate r-sq and rmse for dif_index
    model_dif <- lm(total_cones ~ dif_index, data = avg_pixel_cone_density)
    r_squared_dif <- summary(model_dif)$r.squared
    rmse_dif <- sqrt(mean(model_dif$residuals^2))
    
    #calcualte r-sq and rmse for normRatio_index
    model_normRatio <- lm(total_cones ~ normRatio_index, data = avg_pixel_cone_density)
    r_squared_normRatio <- summary(model_normRatio)$r.squared
    rmse_normRatio <- sqrt(mean(model_normRatio$residuals^2))
    
    #calcualte r-sq and rmse for normDif_index
    model_normDif <- lm(total_cones ~ normDif_index, data = avg_pixel_cone_density)
    r_squared_normDif <- summary(model_normDif)$r.squared
    rmse_normDif <- sqrt(mean(model_normDif$residuals^2))
    
    #calcualte r-sq and rmse for difRatio_index
    model_difRatio <- lm(total_cones ~ difRatio_index, data = avg_pixel_cone_density)
    r_squared_difRatio <- summary(model_difRatio)$r.squared
    rmse_difRatio <- sqrt(mean(model_difRatio$residuals^2))
    
    #add results to the data frame
    indices <- rbind(indices, data.frame(
      index_type = "ratioIndex",
      c1 = c1, c2 = c2,
      r_squared = r_squared_ratio,
      rmse = rmse_ratio
    ))
    
    indices <- rbind(indices, data.frame(
      index_type = "difIndex",
      c1 = c1, c2 = c2,
      r_squared = r_squared_dif,
      rmse = rmse_dif
    ))
    
    indices <- rbind(indices, data.frame(
      index_type = "normRatio_index",
      c1 = c1, c2 = c2,
      r_squared = r_squared_normRatio,
      rmse = rmse_normRatio
    ))
    
    indices <- rbind(indices, data.frame(
      index_type = "normDif_index",
      c1 = c1, c2 = c2,
      r_squared = r_squared_normDif,
      rmse = rmse_normDif
    ))
    
    indices <- rbind(indices, data.frame(
      index_type = "difRatio_index",
      c1 = c1, c2 = c2,
      r_squared = r_squared_difRatio,
      rmse = rmse_difRatio
    ))
  }
}

#now do same for indices with indices with 3 bands
#initialize an empty data frame to store results for 3 bands
indices_three <- data.frame()

#generate coefficient combinations and evaluate indices
for (i in 1:length(seq(-1.01, 1, by = 0.1))) {
  for (j in 1:length(seq(-1.01, 1, by = 0.1))) {
    for(k in 1:length(seq(-1.01, 1, by = 0.1))){
      
      c1 <- seq(-1.01, 1, by = 0.1)[i] # c1 <- 0.1 #c1 <- 0.8
      c2 <- seq(-1.01, 1, by = 0.1)[j] # c2 <- 0.1 #c2 <- 0.7
      c3 <- seq(-1.01, 1, by = 0.1)[k]
      
      #calculate indices for current coefficients
      avg_pixel_cone_density <- avg_pixel_cone_density %>%
        mutate(
          rgb_index = rgbIndex(c1, c2, c3, r, g, b),
          rg_index = rgIndex(c1, c2, r, g, b),
          occ_index = occIndex(c1, c2, r, g, b),
          normSum_index = normSumIndex(c1, c2, r, g, b)
        )
      
      #calculate r-sq and rmse for rgb_index
      model_rgb <- lm(total_cones ~ rgb_index, data = avg_pixel_cone_density)
      r_squared_rgb <- summary(model_rgb)$r.squared
      rmse_rgb <- sqrt(mean(model_rgb$residuals^2))
      
      #calculate r-sq and rmse for rg_index
      model_rg <- lm(total_cones ~ rg_index, data = avg_pixel_cone_density)
      r_squared_rg <- summary(model_rg)$r.squared
      rmse_rg <- sqrt(mean(model_rg$residuals^2))
      
      #calculate r-sq and rmse for occ_index
      model_occ <- lm(total_cones ~ occ_index, data = avg_pixel_cone_density)
      r_squared_occ <- summary(model_occ)$r.squared
      rmse_occ <- sqrt(mean(model_occ$residuals^2))
      
      #calculate r-sq and rmse for normSum_index
      model_normSum <- lm(total_cones ~ normSum_index, data = avg_pixel_cone_density)
      r_squared_normSum <- summary(model_normSum)$r.squared
      rmse_normSum <- sqrt(mean(model_normSum$residuals^2))
      
      #add results to the data frame
      indices_three <- rbind(indices_three, data.frame(
        index_type = "rgb_index",
        c1 = c1, c2 = c2, c3 = c3,
        r_squared = r_squared_rgb,
        rmse = rmse_rgb
      ))
      
      indices_three <- rbind(indices_three, data.frame(
        index_type = "rg_index",
        c1 = c1, c2 = c2, c3 = NA,
        r_squared = r_squared_rg,
        rmse = rmse_rg
      ))
      
      indices_three <- rbind(indices_three, data.frame(
        index_type = "occ_index",
        c1 = c1, c2 = c2, c3 = c3,
        r_squared = r_squared_occ,
        rmse = rmse_occ
      ))
      
      indices_three <- rbind(indices_three, data.frame(
        index_type = "normSum_index",
        c1 = c1, c2 = c2, c3 = NA,
        r_squared = r_squared_normSum,
        rmse = rmse_normSum
      ))
    }
  }
}

#looking at fit of indices
hist(indices$r_squared)
ggplot(indices, aes(x = c1, y = c2, color = r_squared)) + geom_point(size = 3) + facet_wrap(~index_type) +
  theme_bw() + scale_color_viridis_c()

#define best-fit orange index from red and green bands based on indices df
avg_pixel_cone_density <- avg_pixel_cone_density %>%
  mutate(orange_index_rg = (((-0.41*r)-(0.59*g))/((-0.41*r)+(0.59*g))))

#extract the R-squared and p-value from the best-fit rg model
lm_model_rg <- lm(total_cones ~ orange_index_rg, data = avg_pixel_cone_density)
rsq_rg <- summary(lm_model_rg)$r.squared
p_value_rg <- summary(lm_model_rg)$coefficients[2, 4]

annotation_rg <- sprintf("R² = %.2f\np-value = %.2e", rsq_rg, p_value_rg)

#plot the rg model and index
avg_pixel_cone_density %>%
  ggplot(aes(x = orange_index_rg, y = total_cones)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  geom_text(x = Inf, y = Inf, label = annotation_rg, hjust = 1, vjust = 1.1, size = 5, color = "black")

#define best-fit orange index for three bands based on indices_three df
avg_pixel_cone_density <- avg_pixel_cone_density %>%
  mutate(orange_index_rgb = (((0.59*r)+(-0.61*g))/(r+g+b))*(r+g))

#extract the R-squared and p-value from the best-fit rgb model
lm_model_rgb <- lm(total_cones ~ orange_index_rgb, data = avg_pixel_cone_density)
rsq_rgb <- summary(lm_model_rgb)$r.squared
p_value_rgb <- summary(lm_model_rgb)$coefficients[2, 4]

annotation_rgb <- sprintf("R² = %.2f\np-value = %.2e", rsq_rgb, p_value_rgb)

#plot the rgb model and index
avg_pixel_cone_density %>%
  ggplot(aes(x = orange_index_rgb, y = total_cones)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  geom_text(x = Inf, y = Inf, label = annotation_rgb, hjust = 1, vjust = 1.1, size = 5, color = "black")



