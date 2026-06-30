#script to calculate cone density on trees based on spectral index and allometric equations

setwd('/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas')

# read in index values ----------------------------------------------------

#load summary dataframe with mean index value from each tree from each site and flight
#note -- this has been filtered so that index values were calculated after filtering out index values from each tree 
#that fell below the median female index value for that site/flight
summary_index_df <- read_csv("03_output/filtered_summary_index_df.csv")

#estimate cone/g for each tree based on spectral index

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
summary_index_df_clean <- summary_index_df |> 
  left_join(sky_df, by = c("site", "flight_date")) |> 
  select(-date_collected)

#apply correct regression for sky conditions
#from index regressions... 
#cloudy: y = 1100x + 66
#mixed: y = 1800x + 61
#sunny: y = 1100x + 41

cone_density_est_df <- summary_index_df_clean |> 
  mutate(cones_per_g = case_when( 
    condition == "cloudy" ~ (1100 * rg_index_mean) + 66,
    condition == "mixed" ~ (1800 * rg_index_mean) + 61,
    condition == "sunny" ~ (1100 * rg_index_mean) + 41
  )) |> 
  mutate(total_crown_biomass_kg = 1.55*area^1.09,
         total_crown_biomass_g = total_crown_biomass_kg*1000,
         total_cones_density = total_crown_biomass_g * cones_per_g)


