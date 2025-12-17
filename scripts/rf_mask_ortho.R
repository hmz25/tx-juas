#code to run random forest pixel classifier to filter out pixels that are not foliage or cone

library(randomForest)

setwd("C:/Users/hmz25/Box/")

# orthomosaic rf pixel classifier -----------------------------------------

#load pictures to create training dataset for non-foliage and non-cones
not_twig <- stack("texas/pollen_production/TX jan 24/data analysis/not_ortho.tif")
#plotRGB(not_twig) 
#not_twig$not_ortho_1 
#not_twig$not_ortho_1[1:100] #all 255

#dataframe for non-foliage and non-cone pixels
not_twig_df <- as.data.frame(not_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>%
  filter(r != 255) %>% 
  mutate(class = "not") 
#head(not_twig_df)

#load pictures to create training dataset for foliage and cones
yes_twig <- stack("texas/pollen_production/TX jan 24/data analysis/yes_ortho.tif")
#plotRGB(yes_twig) 
#yes_twig$yes_ortho_1 
#yes_twig$yes_ortho_1[1:100] #also all 255

#dataframe for foliage and cone pixels
yes_twig_df <- as.data.frame(yes_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>%
  filter(r != 255) %>% 
  mutate(class = "yes")  
#head(yes_twig_df)

#combine training datasets and randomly select pixels from theme
training_df_ortho <- bind_rows(not_twig_df, yes_twig_df) %>%
  sample_n(100000, replace = TRUE) %>%
  mutate(class = as.factor(class))
#head(training_df_ortho)
#str(training_df_ortho)

#run a pixel-based classifier
set.seed(100)
rf_mask_ortho <- randomForest(class ~ ., data = training_df_ortho, na.action=na.omit)
#rf_mask_ortho

#save rf object
save(rf_mask_ortho, file = "rf_mask_ortho.RData")


# handheld image pixel classifier -----------------------------------------

#examining foliage values
list.files("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025") 

image_dir <- "Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/"
quadrat_files <- list.files(image_dir)#[1]

test <- rast("Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/cath_t6.tif")
plotRGB(test)


test2 <- test
test2$cath_t6_3[test2$cath_t6_1 > 100 & test2$cath_t6_2 > 100 & test2$cath_t6_3 > 100] <- 255
test2$cath_t6_1[test2$cath_t6_1/test2$cath_t6_2 > 1.5] <- 255
plotRGB(test2)

test3 <- test$cath_t6_1/test$cath_t6_3
plot(test3)

#training dataset for non-foliage and non-cones
sup_not_twig <- rast("Katz lab/texas/tx 2025 drone pics/not_fol.tif")
#plotRGB(sup_not_twig) #sup_not_twig$not_fol_1 #sup_not_twig$not_fol_1[1:100]

sup_not_twig_df <- as.data.frame(sup_not_twig) %>% 
  filter(not_fol_1 != 255) %>%  
  rename(c("r" = 1 , "g" = 2, "b" = 3)) %>% 
  mutate(class = "not") 
#head(sup_not_twig_df)

#training dataset for foliage and cones
sup_yes_twig <- rast("Katz lab/texas/tx 2025 drone pics/yes_fol.tif")
#plotRGB(sup_yes_twig) #sup_yes_twig$yes_fol_1 #sup_yes_twig$yes_fol_1[1:100]

sup_yes_twig_df <- as.data.frame(sup_yes_twig) %>%
  filter(yes_fol_1 != 255) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>% 
  mutate(class = "yes")  #head(sup_yes_twig_df)

#combine training datasets and randomly select pixels from them
training_df <- bind_rows(sup_not_twig_df, sup_yes_twig_df) %>%
  sample_n(100000) %>%
  mutate(class = as.factor(class))
# head(training_df)
# str(training_df)

#run a pixel-based classifier
set.seed(100)
rf_mask <- randomForest(class ~ ., data = training_df, na.action=na.omit)
rf_mask

#save rf mask for handheld imgs
save(rf_mask, file = "rf_mask_handheld.RData")


# iphone pixel classifier -------------------------------------------------


