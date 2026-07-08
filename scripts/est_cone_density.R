#script to calculate cone density on trees based on spectral index and allometric equations
library(tidyverse)
library(ggpubr)
library(rstatix)
library(multcompView)


setwd('/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas')

# read in ortho pixel values ----------------------------------------------------

#load summary data frame with mean pixel values from each tree from each site and flight

df <- read_csv("03_output/summary_ortho_px_df.csv")
length(unique(df$poly_id))
unique(df$site)

df2 <- read_csv("~/Downloads/oneflight_ortho_px_df.csv") |> 
  mutate(poly_st = site)

df <- df |> 
  mutate(poly_id = as.character(poly_id)) |> 
  bind_rows(df2)

#quick check of female index values to decide cut off value
female_df <- read_csv("03_output/median_female_index_df.csv")
max(female_df$median_female_index_val)
#set cut off value at -0.01


# calculate index value for each tree -------------------------------------
df_index <- df |> 
  mutate(rg_index_mean = (mean_r_norm_gam - mean_g_norm_gam)/(mean_r_norm_gam + mean_g_norm_gam)) 

# join with field maps data -----------------------------------------------

#read in field maps data
fieldmaps_df <- read_csv("01_data/all_trees_shp_clean.csv")

fieldmaps_df <- fieldmaps_df |> 
  mutate(poly_id = as.character(poly_id))

#join by poly_id
df_index_clean <- df_index |> 
  left_join(fieldmaps_df, by = c("poly_id","site")) |> 
  #remove empty id column automatically imported from qgis
  select(-id) 

# estimate cone/g for each tree based on index ----------------------------

#bring in sky condition data 
sky_df_26 <- read_csv("01_data/2026 TX drone pics metadata - sky conditions.csv")

sky_df_26_clean <- sky_df_26 |> 
  mutate(site = substr(site, 1, 4)) |> 
  rename(date_collected = date)

sky_df_25 <- read_csv("01_data/2025 TX drone pics metadata - sky conditions.csv")

sky_df_25_clean <- sky_df_25 |> 
  mutate(site = substr(site, 1, 4)) |> 
  rename(date_collected = date)

sky_df_24 <- read_csv("01_data/2024 TX drone pics metadata - sky conditions.csv")

sky_df_24_clean <- sky_df_24 |> 
  mutate(site = substr(site, 1, 4)) |> 
  rename(date_collected = date)

sky_df <- bind_rows(sky_df_26_clean, sky_df_25_clean, sky_df_24_clean) |> 
  select(-notes)

#correct format of date column 
sky_df <- sky_df |> 
  mutate(flight_date = format(as.Date(sky_df$date_collected, format = "%m/%d/%Y"), "%Y%m%d")) |> 
  mutate(flight_date = as.double(flight_date))

#join sky condition info with index info 
df_clean <- df_index_clean |> 
  left_join(sky_df, by = c("site", "flight_date")) |> 
  select(-date_collected)

#apply correct regression for sky conditions
#from index regressions... 
#cloudy: y = 1000x + 68
#mixed: y = 1500x + 61
#sunny: y = 830x + 40

cone_density_est_df <- df_clean |> 
  #calculate cone density 
  mutate(cones_per_g = case_when(
    condition == "cloudy" ~ (1000 * rg_index_mean) + 68,
    condition == "mixed" ~ (1500 * rg_index_mean) + 61,
    condition == "sunny" ~ (830 * rg_index_mean) + 40
  )) |> 
  #apply allometric equation to get total cones
  mutate(total_crown_biomass_kg = 1.55*area^1.09,
         total_crown_biomass_g = total_crown_biomass_kg*1000,
         total_cone_density = total_crown_biomass_g * cones_per_g) |> 
  #fixing site naming 
  mutate(year = substr(flight_date,1,4),
         site = if_else(is.na(site), poly_st, site),
         site = case_when(site == "fosh" ~ "fish",
                          .default = site)) |>
  #reformat dates for flights taken in 2025 field season but technically in year 2024
  mutate(year = case_when(
    flight_date == "20241230" ~ "2025",
    flight_date == "20241231" ~ "2025",
    .default = as.character(year)
  ))

# cone_density_est_df <- df_clean |> 
#   #calculate cone density 
#   mutate(cones_per_g = case_when(
#     #set cut-off value for index where below this index value, cones = 0 
#     rg_index_mean <= -0.01 ~ 0,
#     #calculate cones/g for when mean index is above cut-off 
#     condition == "cloudy" ~ (1000 * rg_index_mean) + 68,
#     condition == "mixed" ~ (1500 * rg_index_mean) + 61,
#     condition == "sunny" ~ (830 * rg_index_mean) + 40
#   )) |> 
#   #apply allometric equation to get total cones
#   mutate(total_crown_biomass_kg = 1.55*area^1.09,
#          total_crown_biomass_g = total_crown_biomass_kg*1000,
#          total_cone_density = total_crown_biomass_g * cones_per_g) |> 
#   #fixing site naming 
#   mutate(year = substr(flight_date,1,4),
#          site = if_else(is.na(site), poly_st, site),
#          site = case_when(site == "fosh" ~ "fish",
#                           .default = site)) |>
#   #reformat dates for flights taken in 2025 field season but technically in year 2024
#   mutate(year = case_when(
#     flight_date == "20241230" ~ "2025",
#     flight_date == "20241231" ~ "2025",
#     .default = as.character(year)
#   ))

# cone_density_est_df |> 
#   filter(is.na(site)) |> 
#   View()


#prelim data viz
cone_density_est_df |> 
  ggplot(aes(x = year, y = total_cone_density, group = poly_id, colour = site)) +
  geom_line() +
  geom_point() + 
  theme_classic()

summary_df <- cone_density_est_df %>%
  group_by(site,year) %>%
  summarise(
    mean_density = mean(cones_per_g, na.rm = TRUE),
    se_density = sd(cones_per_g, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot() +
  # individual trajectories - thin, transparent, no legend
  geom_line(data = cone_density_est_df,
            aes(x = year, y = cones_per_g, group = poly_id),
            colour = "grey70", alpha = 0.4, linewidth = 0.4) +
  # ribbon showing +/- 1 SE around the mean
  geom_ribbon(data = summary_df,
              aes(x = year, ymin = mean_density - se_density, ymax = mean_density + se_density),
              alpha = 0.2, fill = "steelblue") +
  # mean trend line on top
  geom_line(data = summary_df, aes(x = year, y = mean_density),
            colour = "steelblue", linewidth = 1.2) +
  geom_point(data = summary_df, aes(x = year, y = mean_density),
             colour = "steelblue", size = 2) +
  facet_wrap(~site) + 
  labs(x = "year", y = "cone density (cones/g)",
       title = "cone density over time",
       subtitle = "grey lines = individual trees; blue = total mean cone density") +
  theme_classic()

#spaghetti plot of individuals over time 
cone_density_est_df |> 
  filter(!site%in%c("good","gun"),
         cones_per_g >= 0) |> 
  ggplot(aes(x = year, y = cones_per_g, group = poly_id)) +
  geom_line(alpha = 0.15, color = "steelblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1) +
  facet_wrap(~site) +
  theme_classic() +
  ylab("cone density (cones/g)")

cone_density_est_df |> 
  filter(!site %in% c("good", "gun"),
         cones_per_g >= 0) |> 
  ggplot(aes(x = year, y = cones_per_g, group = poly_id)) +
  geom_line(alpha = 0.15, color = "steelblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1) +
  stat_summary(aes(group = 1), fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", color = "black", width = 0.3, linewidth = 0.8) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black", size = 2.5) +
  facet_wrap(~site) +
  theme_classic() +
  ylab("cone density (cones/g)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14)
  )

# #heat map of individuals over time
# cone_density_est_df |> 
#   filter(!site == "good",
#          cones_per_g >= 0) |> 
#   ggplot(aes(x = year, y = reorder(poly_id, cones_per_g), fill = cones_per_g)) +
#   geom_tile() +
#   facet_grid(site ~ ., scales = "free_y", space = "free_y") +
#   scale_fill_viridis_c() +
#   theme_minimal() +
#   theme(axis.text.y = element_blank())

# filter data once so it's used consistently for both the model and the plot
plot_df <- cone_density_est_df |> 
  filter(!site %in% c("good", "gun"),
         cones_per_g >= 0) 

plot_df <- cone_density_est_df |> 
  filter(!site %in% c("good", "gun"),
         cones_per_g >= 0) |> 
  group_by(site, poly_id, year) |> 
  summarize(mean_cones_per_g = mean(cones_per_g),
            mean_total_cones = mean(total_cone_density))

# plot_df <- cone_density_est_df |> 
#   group_by(site, poly_id, year) |> 
#   summarize(mean_cones_per_g = mean(cones_per_g, na.rm = TRUE),
#             mean_total_cones = mean(total_cone_density, na.rm = TRUE),
#             .groups = "drop") |> 
#   filter(!is.nan(mean_cones_per_g), !is.nan(mean_total_cones))

# run Tukey HSD per site, comparing years
tukey_letters <- plot_df |> 
  mutate(year = factor(year)) |> 
  group_by(site) |> 
  group_modify(~ {
    mod <- aov(mean_cones_per_g ~ year, data = .x)
    tuk <- TukeyHSD(mod)
    letters <- multcompLetters4(mod, tuk)$year$Letters
    tibble(year = names(letters), letter = letters)
  }) |> 
  ungroup() |> 
  mutate(year = (as.character(year)))

# get y position for each label (a bit above the max value per site/year)
label_pos <- plot_df |> 
  group_by(site, year) |> 
  summarise(y_pos = max(mean_cones_per_g, na.rm = TRUE) * 1.05, .groups = "drop") |> 
  left_join(tukey_letters, by = c("site", "year"))

# plot
plot_df |> 
  ggplot(aes(x = year, y = mean_cones_per_g, group = poly_id)) +
  geom_line(alpha = 0.15, color = "steelblue") +
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "black", linewidth = 1) +
  stat_summary(aes(group = 1), fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", color = "black", width = 0.3, linewidth = 0.8) +
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "black", size = 2.5) +
  geom_text(data = label_pos, aes(x = year, y = y_pos, label = letter),
            inherit.aes = FALSE, size = 4) +
  facet_wrap(~site) +
  theme_classic() +
  ylab("cone density (cones/g)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14)
  )


#individual-level analyses of how cone density varies as a function of 
#1. canopy area
#2. cone production in t-1

#canopy area
canopy_area_df <- read_csv("03_output/summary_ortho_px_df.csv")



