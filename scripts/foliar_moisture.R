library(dplyr)
library(tidyverse)
library(googlesheets4)

setwd("C:/Users/hmz25/Box/")

#load in foliar moisture data 
fm_df <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/cone processing 25 - foliar moisture.csv")

# fm_df_clean <- fm_df |> 
#   filter(!(is.na(twig_dry_weight_bag) & 
#              is.na(twig_dry_weight_nobag) & 
#              is.na(foliage_dry_weight_bag) & 
#              is.na(foliage_dry_weight_nobag) ))

gs4_auth()

fm_df <- read_sheet("https://docs.google.com/spreadsheets/d/1SoBhfMm27Z1QscAtZ-nNaT_XGT5W7JowVIIPHangN4c/edit?gid=1685363826#gid=1685363826",
                    sheet = "foliar moisture")

#create data frame for twig weights
twig_df <- fm_df |> 
  dplyr::select(-c(cones_wet_weight, 
            foliage_wet_weight,
            date_oven, 
            foliage_dry_weight_bag, 
            foliage_dry_weight_nobag, 
            cone_dry_weight, 
            ...15)) 

twig_df <- twig_df |> 
  mutate(twig_dry_weight = ifelse(is.na(twig_dry_weight_nobag), 
                                           twig_dry_weight_bag - 7.26, 
                                           twig_dry_weight_nobag)) |> 
  dplyr::select(-c(twig_dry_weight_bag, twig_dry_weight_nobag)) |> 
  mutate(perc_water = (twig_wet_weight - twig_dry_weight)/twig_wet_weight * 100)

#red zipper = 6.97
#ziploc = 9.76
#texas tough = 8.93
  
#create data frame for foliage weights 
fol_df <- fm_df |> 
  dplyr::select(-c(cones_wet_weight, 
            twig_wet_weight,
            date_oven, 
            twig_dry_weight_bag, 
            twig_dry_weight_nobag, 
            cone_dry_weight, 
            ...15)) 

fol_df <- fol_df |> 
  mutate(fol_dry_weight = ifelse(is.na(foliage_dry_weight_nobag), 
                                  foliage_dry_weight_bag - 7.26, 
                                  foliage_dry_weight_nobag)) |> 
  dplyr::select(-c(foliage_dry_weight_bag, foliage_dry_weight_nobag)) |> 
  mutate(perc_water = (foliage_wet_weight - fol_dry_weight)/foliage_wet_weight * 100)

mean_fol_perc_water <- mean(fol_df$perc_water, na.rm = T)
mean_twig_perc_water <- mean(twig_df$perc_water, na.rm = T)

mean_combined_perc_water <- mean(0.70*mean_fol_perc_water + 0.30*mean_twig_perc_water)

ggplot(fol_df) +
  geom_boxplot(aes(x = site, y = perc_water, col = site)) + 
  theme_classic() + 
  labs(title = "foliage % moisture")

ggplot(twig_df) +
  geom_boxplot(aes(x = site, y = perc_water, col = site)) +
  theme_classic() +
  labs(title = "twig % moisture")

ggplot(twig_df) +
  geom_histogram(aes(x = perc_water), alpha = 0.5) + 
  facet_wrap(~site) +
  theme_classic() + 
  labs(title = "histogram of twig % moisture values")

ggplot(fol_df) +
  geom_histogram(aes(x = perc_water), alpha = 0.5) + 
  facet_wrap(~site) +
  theme_classic() + 
  labs(title = "histogram of twig % moisture values")
