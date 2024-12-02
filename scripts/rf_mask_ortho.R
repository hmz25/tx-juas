
library(randomForest)

#load pictures to create training dataset for non-foliage and non-cones
not_twig <- stack("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/not_ortho.tif")
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
yes_twig <- stack("C:/Users/hmz25/Box/texas/pollen_production/TX jan 24/data analysis/yes_ortho.tif")
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