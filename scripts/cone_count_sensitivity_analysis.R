#script to examine how well cone count subsampling works
library(tidyverse)
library(dplyr)

#read in cone count data 
cone_subsample <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/cone processing 26 - counts.csv")

#calculate estimated cone/g and total cones

cone_subsample_clean <- cone_subsample |> 
  pivot_longer(
    cols = "s1_weight":"s10_count",
    names_to = c("sample_n", ".value"),
    names_pattern = "s(\\d+)_(.*)")

cone_subsample_cone_density <- cone_subsample_clean |> 
  filter(quadrat_location == "tree") |> 
  mutate(cones_per_g = count/weight) |> 
  group_by(site, tree, total_mass, subsample_weight) |> 
  summarize(mean_cone_per_g = mean(cones_per_g)) |> 
  mutate(total_sample_cones = mean_cone_per_g*subsample_weight,
         total_est_cones = mean_cone_per_g*total_mass)

#read in sensitivity analysis cone count data
cone_sensitivity <- read_csv("C:/Users/hmz25/Box/Katz lab/texas/cone processing 26 - subsample sensitivity analysis.csv")
  

#calculate estimated cone/g and total cones
cone_sensitivity_clean <- cone_sensitivity |> 
  drop_na(s1_weight) |> 
  pivot_longer(
    cols = "s1_weight":"s16_count",
    names_to = c("sample_n", ".value"),
    names_pattern = "s(\\d+)_(.*)")

cone_sensitivity_cone_density <- cone_sensitivity_clean |> 
  mutate(cones_per_g_sens = count/weight) |> 
  drop_na(cones_per_g_sens) |> 
  group_by(site, tree) |> 
  summarize(mean_cone_per_g_sens = mean(cones_per_g_sens))

#compare 
cone_subs_sens_df <- cone_sensitivity_cone_density |> 
  left_join(cone_subsample_cone_density, by = c("site", "tree")) |> 
  mutate(total_sample_cones_sens = mean_cone_per_g_sens*subsample_weight,
         total_est_cones_sens = mean_cone_per_g_sens*total_mass,
         site_tree = paste(site, tree, sep = "_")) 

cone_subs_sens_df |> 
  ggplot() +
  geom_point(aes(x = mean_cone_per_g, y = mean_cone_per_g_sens))

#trying different join + visualization
cone_subs_sens_df <- cone_sensitivity_clean |> 
  drop_na(count) |> 
  rename(sample_n_sens = sample_n,
         weight_sens = weight,
         count_sens = count) |> 
  select(-notes) |> 
  left_join(cone_subsample_clean, by = c("date_collected", "site", "tree", "quadrat", "quadrat_location")) |> 
  select(-notes) |> 
  pivot_longer(
    cols = c("sample_n_sens", "weight_sens", "count_sens", "sample_n", "weight", "count"),
    names_to = c(".value", "type"),
    names_pattern = "(sample_n|weight|count)_?(sens)?"
  ) %>%
  mutate(
    type = if_else(type == "", "subsample", "sens")
  ) |> 
  mutate(site_tree = paste(site, tree, sep = "_"))

cone_subs_sens_df |> 
  ggplot() + 
  geom_boxplot(aes(x = site_tree, y = (count/weight), fill = type)) +
  ylab("cone density (cones/g)") + 
  xlab("focal tree") + 
  labs(fill = "sampling strategy") +
  scale_fill_discrete(labels = c("total count", "subsample count")) + 
  theme_minimal()

# cone_subs_sens_df |> 
#   ggplot() + 
#   geom_boxplot(aes(x = site_tree, y = (count*total_mass), fill = type)) +
#   ylab("cone density (cones/g)") + 
#   xlab("focal tree") + 
#   labs(fill = "sampling strategy") +
#   scale_fill_discrete(labels = c("total count", "subsample count")) + 
#   theme_minimal()

test_cone_subs_sens_df <- cone_subs_sens_df |> 
  group_by(date_collected, site, tree, type, total_mass) |> 
  summarize(mean_cone_per_g = mean(count/weight)) |> 
  mutate(total_cones_corrected = mean_cone_per_g*total_mass) |> 
  select(-mean_cone_per_g) |> 
  pivot_wider(names_from = type, values_from = total_cones_corrected) |> 
  rename(subsample_total_cones = subsample,
         sensitivity_total_cones = sens) |> 
  mutate(tree = as.character(tree),
         site = substr(site, 1, 4))


#see how index values compare to the total count samples
# index_sens_df <- cone_sensitivity_cone_density |>
#   mutate(site = substr(site, 1, 4),
#          tree = as.character(tree)) |>
#   full_join(cone_index_df, by = c("site", "tree")) |>
#   mutate(
#     mean_cone_per_g_sens = if_else(
#       tree == "female",
#       0,
#       mean_cone_per_g_sens)
#   ) |>
#   drop_na(mean_cone_per_g_sens) |>
#   mutate(total_cones_sens = mean_cone_per_g_sens*total_cones)

index_sens_df <- cone_sensitivity_cone_density |>
  mutate(site = substr(site, 1, 4),
         tree = as.character(tree)) |>
  full_join(cone_index_df, by = c("site", "tree")) |>
  mutate(
    mean_cone_per_g_sens = if_else(
      tree == "female",
      0,
      mean_cone_per_g_sens)
  ) |>
  drop_na(mean_cone_per_g_sens) |> 
  left_join(test_cone_subs_sens_df, by = c("date_collected", "site", "tree", "total_mass")) |> 
  mutate(sensitivity_total_cones = if_else(tree == "female", 0, sensitivity_total_cones))

index_sens_df_no_female <- index_sens_df |> 
  filter(!tree %in% "female")

mod <- lm(mean_new_norm_index ~ mean_cone_per_g_sens, data = index_sens_df) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(index_sens_df, aes(x = mean_new_norm_index, y = mean_cone_per_g_sens)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  xlab("spectral index ((R-G)/(R/G))") + ylab("cone density (cones/g)") + 
  annotate("text", x = -0.10, y = 100, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.10, y = 95, label = paste("p-val =",p_val)) +
  ggthemes::theme_few() 

mod <- lm(mean_new_norm_index ~ sensitivity_total_cones, data = index_sens_df_no_female) 
summary(mod)
r_sq <- round(summary(mod)$r.sq, 3)
p_val <- round(coef(summary(mod))[2,4], 4)

ggplot(index_sens_df_no_female, aes(x = mean_new_norm_index, y = sensitivity_total_cones)) + 
  geom_point(alpha = 0.5) + 
  theme_bw() + 
  geom_smooth(method = "lm", se = TRUE) +
  xlab("spectral index ((R-G)/(R/G))") + ylab("total cones") + 
  annotate("text", x = -0.10, y = 35000, label = paste("R² =",r_sq)) +
  annotate("text", x = -0.10, y = 32000, label = paste("p-val =",p_val)) +
  ggthemes::theme_few() 
