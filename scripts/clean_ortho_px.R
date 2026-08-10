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
colnames(ortho_px_df)
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


