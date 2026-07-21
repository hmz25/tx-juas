#script to clean + analyze index values from ortho images

library(tidyverse)
library(data.table)

output_dir

output_dir <- "F:/ortho_px_output"

#read in index values and create df 
rds_files <- list.files(output_dir, pattern = "*.rds", full.names = TRUE)
rds_files <- rds_files[c(11,17,36)] #subset to new files
ortho_px_df <- map_df(rds_files, readRDS)
# str(ortho_px_df)
# head(ortho_px_df)
# length(unique(ortho_px_df$poly_id))

# write_csv(ortho_px_df, "F:/ortho_px_df.csv")

#apply same DN corrections as done when creating index 
gam <- 2.2

ortho_px_df_clean <- ortho_px_df %>% 
  #gama decode to linearize 
  mutate(r_lin = (r / 255)^(1/gam), 
         g_lin = (g / 255)^(1/gam),
         b_lin = (b / 255)^(1/gam)) %>% 
  #make brightness invariant 
  mutate(r_norm_gam = r_lin / (r_lin + g_lin + b_lin),
         g_norm_gam = g_lin / (r_lin + g_lin + b_lin),
         b_norm_gam = b_lin / (r_lin + g_lin + b_lin)) 
  
#create summary df by taking mean r, g, b values to calculate the index 
summary_px_df <- ortho_px_df_clean %>%
  group_by(site, poly_id, poly_st, flight_date) %>%
  summarize(mean_r = mean(r),
            mean_g = mean(g),
            mean_b = mean(b),
            mean_r_lin = mean(r_lin),
            mean_g_lin = mean(g_lin),
            mean_b_lin = mean(b_lin),
            mean_r_norm_gam = mean(r_norm_gam),
            mean_g_norm_gam = mean(g_norm_gam),
            mean_b_norm_gam = mean(b_norm_gam)
  )

#for new additions
summary_px_df <- ortho_px_df_clean %>%
  mutate(site = substr(polygon_id,1,4)) %>% 
  group_by(site, polygon_id, flight_date) %>%
  summarize(mean_r = mean(r),
            mean_g = mean(g),
            mean_b = mean(b),
            mean_r_lin = mean(r_lin),
            mean_g_lin = mean(g_lin),
            mean_b_lin = mean(b_lin),
            mean_r_norm_gam = mean(r_norm_gam),
            mean_g_norm_gam = mean(g_norm_gam),
            mean_b_norm_gam = mean(b_norm_gam)
  ) %>% 
  rename(poly_id = polygon_id)

str(summary_px_df)
colnames(summary_px_df)
length(unique(summary_px_df$poly_id))
unique(summary_px_df$flight_date)

#write_csv(summary_px_df, "03_output/summary_ortho_px_df.csv")

#for new flights
write_csv(summary_px_df, "03_output/oneflight_ortho_px_df.csv")

#subset df for some exploratory analyses
sample_ids <- index_df %>%
  distinct(poly_id) %>%
  slice_sample(n = 10) %>%
  pull(poly_id)

# keep all rows matching those poly_id values
index_df_sub <- index_df %>%
  filter(poly_id %in% sample_ids)

length(unique(index_df_sub$poly_id))

index_df_sub %>% 
  mutate(year = substr(flight_date,1,4)) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(poly_id), y = rg_index, col = year)) +
  theme_classic()


# filter index values based on female index values ------------------------

#calculate median female index value for each site/flight
# female_df <- summary_index_df %>%
#   filter(clssfct == "female")
# 
# ggplot(female_df, aes(x = factor(poly_id), y = rg_index_mean, col = flight_date)) +
#   geom_boxplot() +
#   facet_wrap(~site, scales = "free_x")

female_df <- ortho_px_df %>%
  filter(clssfct == "female")
length(unique(female_df$poly_id))

ggplot(female_df, aes(x = factor(poly_id), y = rg_index, col = flight_date)) +
  geom_boxplot() +
  theme_classic()

#calculate median value for female trees per site per flight
#to determine cutoff value for spectral index values

median_female_index_df <- female_df |>
  group_by(site, flight_date) |>
  summarize(median_female_index_val = median(rg_index))

# # write_csv(median_female_index_df, "03_output/median_female_index_df.csv")

# ggplot(female_df, aes(x = factor(poly_id), y = rg_index_mean, col = flight_date)) + 
#   geom_boxplot() + 
#   facet_wrap(~site, scales = "free_x")
# 
# female_df <- index_df %>% 
#   filter(clssfct == "female")
# 
# ggplot(female_df, aes(x = factor(poly_id), y = rg_index, col = flight_date)) + 
#   geom_boxplot() + 
#   facet_wrap(~site, scales = "free_x")

#filter out index values below median female index value from each tree based on flight date and site

#load in median female index vals per flight/sight
median_female_index_vals <- read_csv("03_output/median_female_index_df.csv")

median_female_index_vals <- median_female_index_vals %>% 
  mutate(flight_date = as.character(flight_date))

#filter out rows from index df that are below median female val
index_df_filt <- index_df %>%
  left_join(median_female_index_vals, by = c("site", "flight_date")) %>%
  filter(rg_index >= median_female_index_val)

length(unique(index_df_filt$poly_id))
length(unique(index_df$poly_id))

#convert to data.table
median_female_index_vals <- as.data.table(median_female_index_vals)
index_df <- as.data.table(index_df)

#set keys for joining
setkey(median_female_index_vals, site, flight_date)
setkey(index_df, site, flight_date)

#join and filter
index_df_filtered <- index_df[median_female_index_vals, on = .(site, flight_date), 
                    nomatch = 0L][rg_index >= median_female_index_val]

write_csv(index_df_filtered, "F:/filtered_index_df_2026.csv")

#create summary df from filtered df 
filtered_summary_index_df <- index_df_filtered %>% 
  group_by(area, site, poly_id, clssfct, date, year, fcl_tr_, flight_date,
           prcnt__, notes, flg_dns, height, ttl_stms, bsl_dmt,
           source, crp_frc, crp_cnt, tree_id,
           cnpy_cl, stm_dmt, dbh, totl_stm,
           dbh_blw, ttl_s_1) %>%
  summarize(mean_r = mean(r),
            mean_g = mean(g),
            mean_b = mean(b),
            mean_r_lin = mean(r_lin),
            mean_g_lin = mean(g_lin),
            mean_b_lin = mean(b_lin),
            mean_r_norm_gam = mean(r_norm_gam),
            mean_g_norm_gam = mean(g_norm_gam),
            mean_b_norm_gam = mean(b_norm_gam),
            mean_rg_index = mean(rg_index)
  )

filtered_summary_mean_index_df <- filtered_summary_index_df %>%
  mutate(rg_index_mean = (mean_r_norm_gam - mean_g_norm_gam)/
           (mean_r_norm_gam/mean_g_norm_gam)) %>% 
  filter(!"female" %in% clssfct)


#write_csv(filtered_summary_mean_index_df, "03_output/filtered_summary_index_df.csv")

#pheno analysis

pheno_df <- index_df %>% 
  filter(year == 2025)
