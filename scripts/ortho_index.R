##script to apply index to segmented ortho images

library(terra)
library(sf)
library(tidyverse)
library(dplyr)
library(randomForest)
# install.packages("exactextractr")
library(exactextractr)

setwd("C:/Users/hmz25/Box/Katz lab/texas/")

# train rf model to filter foliage vs non foliage pixels --------------------

#load pictures to create training dataset for non-foliage and non-cones
not_twig <- rast("not_fol_ortho.tif")
#plotRGB(not_twig) 
#not_twig$not_fol_ortho_1 
#not_twig$not_fol_ortho_1[1:100] 

#dataframe for non-foliage and non-cone pixels
not_twig_df <- as.data.frame(not_twig) %>%
  rename(c("r" =1 , "g" = 2, "b" = 3)) %>%
  filter(r != 255) %>% 
  mutate(class = "not") 
#head(not_twig_df)

#load pictures to create training dataset for foliage and cones
yes_twig <- rast("yes_fol_ortho.tif")
#plotRGB(yes_twig) 
#yes_twig$yes_fol_ortho_1 
#yes_twig$yes_fol_ortho_1[1:100] 

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

# set dir for ortho images ----------------------------------------------------

ortho_dir <- "2025 orthos"
ortho_list <- list.files(ortho_dir, pattern = ".tif$", full.names = FALSE)
ortho_list_full_dir <- list.files(ortho_dir, pattern = ".tif$", full.names = TRUE)

# ortho <- rast("2025 orthos/sonora_20250115_transparent_mosaic_group1.tif")
# plotRGB(ortho)

# set dir for segmented canopy shape files --------------------------------------------

shp_dir <- "2025 juas qgis"
shp_list <- list.files(shp_dir, pattern = ".shp$", full.names = FALSE)
shp_list_full_dir <- list.files(shp_dir, pattern = ".shp$", full.names = TRUE)

# shp <- st_read("2025 juas qgis/sono_canopy_seg.shp")


# loop through each site and shape file to calculate mean index --------


site_mean_index_df <- data.frame()

site_index_df <- data.frame()

i = 1

for (i in seq_along(ortho_list)){
  
  #extract site name from filename (before the first "_")
  site_name <- str_extract(basename(ortho_list[i]), "^[^_]+")
  
  #load in ortho for the site
  ortho <- rast(ortho_list_full_dir[i])
  # plotRGB(ortho)
  
  #rename ortho columns 
  names(ortho) <- c("r", "g", "b", "transparant")
  
  #load in the shapefile for the site
  shp <- st_read(shp_list_full_dir[i])
  
  #align the crs of ortho and shape file
  shp_reproj <- st_transform(shp, crs(ortho))
  # crs(ortho) == crs(shp_reproj)
  # plot(shp_reproj, add = TRUE, col = "red")
  
  #extract values from ortho that are within the canopy shapefiles
  extracted_values <- exact_extract(ortho, shp_reproj, include_xy = TRUE)
  
  # #testing visually to see if it's doing the right thing
  # str(extracted_values[[1]])
  # test_df <- extracted_values[[1]] %>%
  #   select(x, y, everything())
  # 
  # test_rast <- rast(test_df, type = "xyz")
  # 
  # plotRGB(test_rast)
  
  #convert extracted values to df
  extracted_df <- bind_rows(extracted_values)
  
  #apply rf classifier to extracted pixels
  extracted_df$class <- predict(rf_mask_ortho, extracted_df)
  # extracted_df$class == "yes"
  
  #filter df for foliage pixels 
  fol_pixels <- extracted_df %>% 
    filter(class == "yes")
  
  # #visual check
  # fol_pixels_test <- fol_pixels %>% 
  #   select(x, y, everything())
  # 
  # test_rast2 <- rast(fol_pixels_test, type = "xyz")
  # 
  # plotRGB(test_rast2)
  
  #to get all pixels rather than mean
  index_df <- fol_pixels %>%
    mutate(rg_index = (r - g) / (r + g)) %>% 
    pull(rg_index) 
  
  site_index_df <- rbind(site_index_df, data.frame(site = site_name, rg_index = index_df))
  
  # #calculate mean index value for the site 
  # site_mean_index <- fol_pixels %>%
  #   mutate(rg_index = (r - g) / (r + g)) %>%
  #   summarize(mean_index = mean(rg_index, na.rm = TRUE)) %>%
  #   pull(mean_index)
  # 
  # #add the mean site values and site name to the df
  # site_mean_index_df <- rbind(site_mean_index_df, 
  #                             data.frame(site = site_name, mean_index = site_mean_index))
  # 
  print(n)
  
}

site_index_df

write_csv(site_index_df, file = "C:/Users/hmz25/Box/Katz lab/texas/site_index.csv", append = FALSE)

site_mean_index_df

ggplot(data = site_mean_index_df, aes(x = site, y = mean_index)) +
  geom_point() +
  theme_classic() +
  labs(y = 'mean site-level index')


# compare this year index with 2024 smap data for each site ---------------

#read in smap data
smap_df <- read_csv("tx_SMAP_2024.csv")

#add in county data to site mean index df 
tx_counties <- read_csv("tx_site_metadata - Sheet1.csv") %>% 
  rename(site = site_name)

site_mean_index_join_df <- site_mean_index_df %>% 
  left_join(tx_counties, by = "site") %>% 
  select(-c(county.x, county.y, coords.x, coords.y, county.x.x, coords)) %>% 
  rename(county = county.y.y) %>% 
  as.data.frame()

write_csv(site_mean_index_join_df, "site_index_df.csv")

#join smap df with index df 
site_index_smap <- smap_df %>% 
  rename(county = NAME) %>% 
  left_join(site_mean_index_df, by = "county") %>% 
  rename(mean_smap = mean)

ggplot(site_index_smap, aes(x = mean_smap, y = mean_index, col = site)) +
  geom_point() +
  scale_x_continuous(limits = c(0.2, 0.25)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() 

ggplot(site_index_smap) +
  geom_point(aes(x = mean_smap, y = mean_index, col = site)) +
  geom_smooth(aes(x = mean_smap, y = mean_index), method = "lm", se = FALSE, col = "grey", alpha = 0.5) +
  coord_cartesian(xlim = c(0.2, 0.25)) +
  theme_minimal()


#run linear model 
index_smap_lm <- lm(data = site_index_smap, mean_index ~ mean_smap)
summary(index_smap_lm)

index_rsq <- summary(index_smap_lm)$r.sq

index_pval <- summary(index_smap_lm)$coefficients[2,4]    # p-value for the slope (mean_smap)

#plot
ggplot(site_index_smap) +
  geom_point(aes(x = mean_smap, y = mean_index)) +
  geom_smooth(aes(x = mean_smap, y = mean_index), method = "lm", se = T) +
  coord_cartesian(xlim = c(0.2, 0.25)) +
  xlab(expression("mean soil moisture ("*m^3*"/"*m^3*")")) +
  ylab("mean index value") +
  annotate("text", 
           x = 0.245, y = -0.015,
           label = paste("R² =", round(index_rsq, 3), "\np =", signif(index_pval, 3)), 
           hjust = 0, size = 4, color = "black") +
  ggthemes::theme_few()


# compare site-level reproduction with other environmental variables --------
library(janitor)

precip_df <- read_csv("site_mean_precip2024.csv") 

precip_df_clean <- precip_df %>% 
  select(Name, `_sum`, `_mean`, `_stdev`, `_variance`) %>% 
  clean_names() %>% 
  mutate(name = tolower(name),
         site = sub(" .*", "", name),
         site = case_when(
           site == "kerr" ~ "gun",
           site == "real" ~ "windmill",
           TRUE ~ site  # keep all others unchanged
         )) %>% 
  select(site, mean) %>% 
  rename(mean_precip = mean)

temp_df <- read_csv("site_mean_temp2024.csv") 

clean_data <- function(df) {
  df %>%
    select(Name, `_mean`, `_stdev`, `_variance`) %>%
    clean_names() %>%
    mutate(name = tolower(name),
           site = sub(" .*", "", name),
           site = case_when(
             site == "kerr" ~ "gun",
             site == "real" ~ "windmill",
             TRUE ~ site
           )) %>%
    select(site, mean)
}

temp_df_clean <- clean_data(temp_df) %>% 
  rename(mean_temp = mean)

aridity_df <- read_csv("site_mean_AI2024.csv")
aridity_df_clean <- clean_data(aridity_df) %>% 
  rename(mean_aridity = mean)

smap_df <- read_csv("site_mean_smap2024.csv") 
smap_df_clean <- clean_data(smap_df) %>% 
  rename(mean_smap = mean)

#combine all dfs into one!
combined_df <- site_mean_index_df %>%
  merge(smap_df_clean, by = "site") %>%
  merge(temp_df_clean, by = "site") %>%
  merge(precip_df_clean, by = "site") %>%
  merge(aridity_df_clean, by = "site")

#combine environmental vars df
abiotic_df <- smap_df_clean %>%
  merge(temp_df_clean, by = "site") %>%
  merge(precip_df_clean, by = "site") %>%
  merge(aridity_df_clean, by = "site")

write_csv(abiotic_df, file = "C:/Users/hmz25/Desktop/plsci 5200/final project/site_environ_vars.csv")

p_smap <- ggplot(combined_df) +
  geom_point(aes(x = mean_smap, y = mean_index, col = site)) +
  geom_smooth(aes(x = mean_smap, y = mean_index), method = "lm", se = T) +
  labs(x = expression("mean soil moisture ("*m^3*"/"*m^3*")"), y = "mean index") +
  theme_classic()

p_temp <- ggplot(combined_df) +
  geom_point(aes(x = (mean_temp-273.15), y = mean_index, col = site)) +
  geom_smooth(aes(x = (mean_temp-273.15), y = mean_index), method = "lm", se = T) +
  labs(x = "mean max temperature (C)", y = "mean index") +
  theme_classic()

p_precip <- ggplot(combined_df) +
  geom_point(aes(x = mean_precip, y = mean_index, col = site)) +
  geom_smooth(aes(x = mean_precip, y = mean_index), method = "lm", se = T) +
  labs(x = "mean precipitation (mm)", y = "mean index") +
  theme_classic()

p_aridity <- ggplot(combined_df) +
  geom_point(aes(x = mean_aridity, y = mean_index, col = site)) +
  geom_smooth(aes(x = mean_aridity, y = mean_index), method = "lm", se = T) +
  labs(x = "mean aridity index (unitless)", y = "mean index") +
  theme_classic()


m_smap <- lm(mean_index ~ mean_smap, data = combined_df)
summary(m_smap)

m_temp <- lm(mean_index ~ mean_temp, data = combined_df)
summary(m_temp)

m_precip <- lm(mean_index ~ mean_precip, data = combined_df)
summary(m_precip)

m_aridity <- lm(mean_index ~ mean_aridity, data = combined_df)
summary(m_aridity)

combined_m <- list(m_smap, m_temp, m_precip, m_aridity)

model_stats <- combined_m %>%
  map(broom::glance) %>%
  bind_rows(.id = "model_name") %>% 
  mutate(model_name = case_when(
    model_name == "1" ~ "smap",
    model_name == "2" ~ "temp",
    model_name == "3" ~ "precip",
    model_name == "4" ~ "aridity"
  ))

model_stats_clean <- model_stats %>% 
  select(model_name, r.squared, adj.r.squared, p.value, logLik, AIC, BIC)

# --- Visualize the results ---
# Bar plot of R-squared
ggplot(model_stats, aes(x = model_name, y = r.squared)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "R-squared values across models",
       x = "Model", y = "R-squared") +
  theme_bw()

# Bar plot of p-value
ggplot(model_stats, aes(x = model_name, y = p.value)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "P-values across models",
       x = "Model", y = "p-value") +
  theme_bw()
