#script to calculate live fuel moisture content for juas


# set up work environment -------------------------------------------------

library(dplyr)
library(tidyverse)
library(googlesheets4)

#set wd
#lab desktop
# setwd("C:/Users/hmz25/Box/")

#hz laptop
setwd('/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas')

# 2026 foliar moisture ----------------------------------------------------

#load in data
gs4_auth()

fm_df_26 <- read_sheet("https://docs.google.com/spreadsheets/d/111VY46CZsyJ1Mzai52_bK9yqDHDfj9DcAJxjz4t_JII/edit?gid=1685363826#gid=1685363826",
                    sheet = "foliar moisture")

#calculate foliar moisture
#using "live fuel moisture content" formula as per https://www.sciencedirect.com/science/article/pii/S003442572030167X#s0010
#((wet weight - dry weight)/dry weight)*100

#changed the above to divide by wet weight, which gives the % water weight

site_fm_df_26 <- fm_df_26 |> 
  group_by(site) |> 
  summarize(mean_fm = mean((((total_wet_weight - bag_weight_wet)-(total_dry_weight - bag_weight_dry))/
                             (total_wet_weight - bag_weight_wet))*100)) |> 
  mutate(date_collected = "2026")


# mean_fm <- mean((site_fm_df$mean_fm)/100)

#this gives the % water in the sample
#to convert wet samples to dry, do (wet weight)*(1-(% perc water/100)))

# 2025 foliar moisture ----------------------------------------------------

#load in foliar moisture data 
# fm_df <- read_csv("01_data/cone processing 25 - foliar moisture.csv")

# fm_df_clean <- fm_df |> 
#   filter(!(is.na(twig_dry_weight_bag) & 
#              is.na(twig_dry_weight_nobag) & 
#              is.na(foliage_dry_weight_bag) & 
#              is.na(foliage_dry_weight_nobag) ))

gs4_auth()

fm_df_25 <- read_sheet("https://docs.google.com/spreadsheets/d/1SoBhfMm27Z1QscAtZ-nNaT_XGT5W7JowVIIPHangN4c/edit?gid=1685363826#gid=1685363826",
                    sheet = "foliar moisture")

#create data frame for twig weights
twig_df <- fm_df_25 |> 
  dplyr::select(-c(cones_wet_weight, 
            foliage_wet_weight,
            date_oven, 
            foliage_dry_weight_bag, 
            foliage_dry_weight_nobag, 
            cone_dry_weight, 
            ...15)) 

site_twig_df <- twig_df |> 
  mutate(twig_dry_weight = ifelse(is.na(twig_dry_weight_nobag), 
                                           twig_dry_weight_bag - 7.26, 
                                           twig_dry_weight_nobag)) |> 
  dplyr::select(-c(twig_dry_weight_bag, twig_dry_weight_nobag)) |> 
  mutate(perc_water = (twig_wet_weight - twig_dry_weight)/twig_wet_weight * 100) |> 
  group_by(date_collected, site) |> 
  summarize(twig_mean_perc_water = mean(perc_water))

#red zipper = 6.97
#ziploc = 9.76
#texas tough = 8.93
  
#create data frame for foliage weights 
fol_df <- fm_df_25 |> 
  dplyr::select(-c(cones_wet_weight, 
            twig_wet_weight,
            date_oven, 
            twig_dry_weight_bag, 
            twig_dry_weight_nobag, 
            cone_dry_weight, 
            ...15)) 

site_fol_df <- fol_df |> 
  mutate(fol_dry_weight = ifelse(is.na(foliage_dry_weight_nobag), 
                                  foliage_dry_weight_bag - 7.26, 
                                  foliage_dry_weight_nobag)) |> 
  dplyr::select(-c(foliage_dry_weight_bag, foliage_dry_weight_nobag)) |> 
  mutate(perc_water = (foliage_wet_weight - fol_dry_weight)/foliage_wet_weight * 100) |> 
  group_by(date_collected, site) |> 
  summarize(fol_mean_perc_water = mean(perc_water, na.rm = T))

# mean_fol_perc_water <- mean(fol_df$perc_water, na.rm = T)
# mean_twig_perc_water <- mean(twig_df$perc_water, na.rm = T)
# 
# mean_combined_perc_water <- mean(0.70*mean_fol_perc_water + 0.30*mean_twig_perc_water)
# 
# ggplot(fol_df) +
#   geom_boxplot(aes(x = site, y = perc_water, col = site)) + 
#   theme_classic() + 
#   labs(title = "foliage % moisture")
# 
# ggplot(twig_df) +
#   geom_boxplot(aes(x = site, y = perc_water, col = site)) +
#   theme_classic() +
#   labs(title = "twig % moisture")
# 
# ggplot(twig_df) +
#   geom_histogram(aes(x = perc_water), alpha = 0.5) + 
#   facet_wrap(~site) +
#   theme_classic() + 
#   labs(title = "histogram of twig % moisture values")
# 
# ggplot(fol_df) +
#   geom_histogram(aes(x = perc_water), alpha = 0.5) + 
#   facet_wrap(~site) +
#   theme_classic() + 
#   labs(title = "histogram of twig % moisture values")

#combine twig and foliage moisture data as column in df 
# mean_combined_perc_water <- mean(0.70*mean_fol_perc_water + 0.30*mean_twig_perc_water)
site_fm_df_25 <- left_join(site_fol_df, site_twig_df, by = c("site", "date_collected")) |> 
  mutate(mean_perc_water = (0.70*fol_mean_perc_water) + (0.30*twig_mean_perc_water))

#estimate 2024 site foliar moisture based on mean values from sites in 2025 + 2026

#combine both year dfs and export for downstream analysis 
site_fm_combined <- site_fm_df_25 |> 
  select(-c("fol_mean_perc_water", "twig_mean_perc_water")) |> 
  mutate(date_collected = as.character(date_collected),
         date_collected = substr(date_collected,1,4)) |> 
  rename(mean_fm = mean_perc_water) |> 
  bind_rows(site_fm_df_26) |> 
  rename(year = date_collected)

write_csv(site_fm_combined, "03_output/sample_perc_moisture_df.csv")


