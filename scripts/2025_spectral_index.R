setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/")

#load in cone count data 
cone_df <- read_csv("Katz lab/texas/cone processing 25 - counts.csv")

cone_df_clean <- cone_df |> 
  rename(name_site = site,
         total_subsample_weight = subsample_weight) |>  
  pivot_longer(
           cols = starts_with("s"),
           names_to = c("sample_n", ".value"),
           names_pattern = "s(\\d+)_(.*)") |> 
  select(date_collected, name_site, tree, total_mass, total_subsample_weight, sample_n, weight, count, notes)

ggplot(cone_df_clean, aes(x = as.factor(tree), y = count)) +
  geom_boxplot() +
  facet_wrap( ~ name_site)

cone_var_df <- cone_df_clean |> 
  group_by(name_site) |> 
  summarise(cone_var = sd(count))

# quadrat image processing ------------------------------------------------


