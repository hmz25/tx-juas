#script to download environmental data for the years + sites

library(daymetr)
library(tidyverse)
library(sf)
library(purrr)
library(lubridate)
library(terra)
library(lme4)
library(lmerTest)
library(patchwork)
library(MuMIn)

setwd("~/Library/CloudStorage/Box-Box/Katz lab/texas/")

# load in kml of sites ----------------------------------------------------

#load in sites
all_sites <- st_read("01_data/all_sites.kml")
plot(all_sites)
all_sites$Name


#rename sites to match other data sources

# #the site names stored in the KML file that was used to spatially join the data are the formal names from the landowners,
# #so they are different than the naming scheme I use for memory, which I'll fix now
match_site_name <- function(site_name) {
  case_when(
    site_name == "Cathedral Oaks (Hays 2021-54)" ~ "cathedral",
    site_name == "Fisher (Comal 2022-24)" ~ "fisher",
    site_name == "Cell Tower (Hays-Travis 2022-02)" ~ "cell tower",
    site_name == "Bandera 2023-37" ~ "bandera",
    site_name == "Sonora" ~ "sonora",
    site_name == "Wade" ~ "wade",
    site_name == "Sweeten" ~ "sweeten",
    site_name == "Kimble 2023-03" ~ "kimble",
    site_name == "Creek (Real 2024-14)" ~ "creek",
    site_name == "Glimmer (Travis 2023-84)" ~ "glimmer",
    site_name == "Williamson 2022-13" ~ "williamson",
    site_name == "Kerr (Kerr 2025-42)" ~ "kerr",
    site_name == "Bentley (Burnet 2024-20)" ~ "bentley",
    site_name == "Good guys (Bandera 2021-32)" ~ "good guys",
    site_name == "Windmill (Real 2024-15)" ~ "windmill",
    site_name == "Rocky (Kerr 2022-38)" ~ "rocky",
    site_name == "Gun (Kerr 2024-40)" ~ "gun",
    site_name == "Christmas (Kendall 2024-10)" ~ "christmas",
    TRUE ~ site_name #keep unchanged if no match
  )
}
# 
# df_clean <- df %>%
#   mutate(site = match_site_name(site))
# head(df)

all_sites$Name <- match_site_name(all_sites$Name)

#using daymet to extract precip (prcp) and max temp (tmax)
#https://www.earthdata.nasa.gov/data/projects/daymet
#need years 2023-2026

#daymet requires WGS84 (EPSG:4326) decimal degree lat/lon
all_sites_ll <- st_transform(all_sites, crs = 4326)

# table(st_geometry_type(all_sites_ll))

#extract centroid point of sites for daymet download 
all_sites_pts <- st_centroid(all_sites_ll)

coords <- st_coordinates(all_sites_pts)

site_df <- data.frame(
  site = all_sites_pts$Name,
  lat  = coords[, "Y"],
  lon  = coords[, "X"],
  stringsAsFactors = FALSE
)


#guard against duplicate site names/coords issues
stopifnot(!anyNA(site_df$lat), !anyNA(site_df$lon))


# download daymet data for sites ------------------------------------------

start_year <- 2023
end_year   <- 2025

failed_downloads <- character(0)

daymet_list <- pmap(site_df, function(site, lat, lon) {
  message("Downloading Daymet data for: ", site)
  tryCatch(
    {
      df <- download_daymet(
        site      = site,
        lat       = lat,
        lon       = lon,
        start     = start_year,
        end       = end_year,
        internal  = TRUE,
        simplify  = TRUE   # returns tidy long-format data frame
      )
      df$site <- site
      df
    },
    error = function(e) {
      message("  -> failed for ", site, ": ", conditionMessage(e))
      failed_downloads <<- c(failed_downloads, site)
      NULL
    }
  )
})

daymet_all <- bind_rows(daymet_list)

if (length(failed_downloads) > 0) {
  message("\nSites that failed to download (check names/coords or year range): ")
  print(failed_downloads)
}


# clean daymet data -------------------------------------------------------

#keep only precip and tmax
#convert yday to calendar date 

# Daymet variable names when simplify = TRUE:
#   "prcp..mm.day."  -> daily total precipitation (mm)
#   "tmax..deg.c."   -> daily maximum temperature (deg C)
# Daymet uses a 365-day calendar (Dec 31 is dropped in leap years), so
# yday - 1 days after Jan 1 of that year gives the correct calendar date
# for all 365 "Daymet days".

daymet_clean <- daymet_all %>%
  filter(measurement %in% c("prcp..mm.day.", "tmax..deg.c.")) %>%
  mutate(
    date  = as.Date(yday - 1, origin = paste0(year, "-01-01")),
    month = month(date)
  )

#aggregate to monthly vals

#precip = monthly total (sum of daily precip)
#tmax = monthly mean of daily max temp

precip_monthly <- daymet_clean %>%
  filter(measurement == "prcp..mm.day.") %>%
  group_by(site, year, month) %>%
  summarize(precip_mm_total = sum(value, na.rm = TRUE), .groups = "drop")

tmax_monthly <- daymet_clean %>%
  filter(measurement == "tmax..deg.c.") %>%
  group_by(site, year, month) %>%
  summarize(tmax_C_mean = mean(value, na.rm = TRUE), .groups = "drop")

monthly_summary <- precip_monthly %>%
  left_join(tmax_monthly, by = c("site", "year", "month")) %>%
  arrange(site, year, month)


# # save daymet df ----------------------------------------------------------
#
# 
# dir.create("01_data/daymet", showWarnings = FALSE, recursive = TRUE)
# 
# write.csv(
#   monthly_summary,
#   "01_data/daymet/all_sites_daymet_monthly_2023_2026.csv",
#   row.names = FALSE
# )
# 
# # Also keep the raw daily pull in case you need finer resolution later
# write.csv(
#   daymet_clean,
#   "01_data/daymet/all_sites_daymet_daily_2023_2026.csv",
#   row.names = FALSE
# )
# 
# message("\nDone. Monthly summary written to:")
# message(" 01_data/daymet/all_sites_daymet_monthly_2023_2026.csv")
# 
# monthly_summary


# load in smap data -------------------------------------------------------

#smap data downloaded using google earth engine
#root zone soil moisture (0-100cm) from smap L8

smap_df <- read_csv("01_data/smap_monthly_2023_2025.csv")

smap_df$Name <- match_site_name(smap_df$Name)

smap_df_clean <- smap_df |> 
  select(Name, mean, month, year) |> 
  rename(site = Name,
         smap_mean = mean)


# create df for all environ vars + cone density ------------------------------------------

#combine environ vars into one df 
environ_vars_df <- smap_df_clean |> 
  left_join(monthly_summary, by = c("site", "month", "year")) |> 
  mutate(site = substr(site,1,4))

#create lagged df to link with cone density 
environ_vars_lag_df <- environ_vars_df %>% 
  mutate(year = year + 1) %>% 
  mutate(year = as.character(year))

#read in cone density estimates
cone_density_est_df_summary <- cone_density_est_df |> 
  group_by(site, year) |> 
  summarize(mean_cones_per_g = mean(cones_per_g, na.rm = T),
            sd_cones_per_g = sd(cones_per_g, na.rm = T),
            mean_total_cones = mean(total_cone_density, na.rm = T),
            sd_total_cones = sd(total_cone_density, na.rm = T),
            .groups = "drop") |> 
  filter(site != "good")

cone_density_est_df_summary <- cone_density_est_df |> 
  group_by(site, year) |> 
  summarize(mean_cones_per_g = mean(cones_per_g, na.rm = T),
            mean_total_cones = mean(total_cone_density, na.rm = T)) |> 
  mutate(sd_cones_per_g = sd(mean_cones_per_g, na.rm = T),
         sd_total_cones = sd(mean_total_cones, na.rm = T)) |> 
  filter(site != "good")
  
#create df for repro in t-1
cone_lag_df <- cone_density_est_df_summary |> 
  mutate(year = as.numeric(year)) |> 
  mutate(year = year + 1) %>% 
  mutate(year = as.character(year)) |> 
  rename(mean_cones_per_g_lag = mean_cones_per_g,
         sd_cones_per_g_lag = sd_cones_per_g,
         mean_total_cones_lag = mean_total_cones,
         sd_total_cones_lag = sd_total_cones)

#link with cone density data 
mod_df <- cone_density_est_df_summary |> 
  left_join(environ_vars_lag_df, by = c("site", "year")) |> 
  left_join(cone_lag_df, by = c("site", "year"))
  
# mod_df_clean <- mod_df |> 
#   filter(smap_mean != "-9999") |> 
#   drop_na(smap_mean)

#not including smap in analysis right now
mod_df_clean <- mod_df |> 
  select(-smap_mean)

# mod_df_sub <- mod_df_clean |> 
#   filter(month %in% 3:6) |> 
#   group_by(site, year, mean_cones_per_g, sd_cones_per_g, mean_total_cones, sd_total_cones, 
#            mean_cones_per_g_lag, sd_cones_per_g_lag, mean_total_cones_lag, sd_total_cones_lag) |> 
#   summarize(mean_smap = mean(smap_mean, na.rm = T),
#             mean_precip_mm_total = mean(precip_mm_total, na.rm = T),
#             mean_tmax_C_mean = mean(tmax_C_mean, na.rm = T),
#             .groups = "drop")

mod_df_sub <- mod_df_clean |> 
  filter(month %in% 3:6) |> 
  group_by(site, year, mean_cones_per_g, sd_cones_per_g, mean_total_cones, sd_total_cones, 
           mean_cones_per_g_lag, sd_cones_per_g_lag, mean_total_cones_lag, sd_total_cones_lag) |> 
  summarize(mean_precip_mm_total = mean(precip_mm_total, na.rm = T),
            mean_tmax_C_mean = mean(tmax_C_mean, na.rm = T),
            sum_precip_mm_total = sum(precip_mm_total, na.rm = T),
            .groups = "drop")

mod_df_year <- mod_df_clean |> 
  group_by(site, year, mean_cones_per_g, sd_cones_per_g, mean_total_cones, sd_total_cones, 
           mean_cones_per_g_lag, sd_cones_per_g_lag, mean_total_cones_lag, sd_total_cones_lag) |> 
  summarize(mean_precip_mm_total = mean(precip_mm_total, na.rm = T),
            mean_tmax_C_mean = mean(tmax_C_mean, na.rm = T),
            sum_precip_mm_total = sum(precip_mm_total, na.rm = T),
            .groups = "drop")

#model + plot each var
mod_cone_lag <- lm(mean_cones_per_g ~ mean_cones_per_g_lag + year + site,
             data = mod_df_sub)
summary(mod_cone_lag)

# mod_cone_lag <- lm(mean_cones_per_g ~ mean_cones_per_g_lag,
#                    data = mod_df_sub)
# summary(mod_cone_lag)

# #trying to build plot with lm()
# mod_summary <- summary(mod_cone_lag)
# intercept <- coef(mod_cone_lag)[1]
# slope <- coef(mod_cone_lag)[2]
# r2 <- mod_summary$r.squared
# pval <- mod_summary$coefficients[2, 4]  # p-value for the slope
# 
# # Build a readable label
# eq_label <- paste0(
#   "y = ", round(slope, 3), "x + ", round(intercept, 3),
#   "\nR² = ", round(r2, 3),
#   "\np = ", formatC(pval, format = "e", digits = 2)
# )
# 
# eq_label <- paste0(
#   "y = ", round(slope, 3), "x + ", round(intercept, 3),
#   "\nR² = ", round(r2, 3),
#   "\np < 0.01"
# )
# 
# lag_cone_p <- ggplot(mod_df_lag_sub_summary, aes(x = mean_cones_per_g_lag, y = mean_cones_per_g, col = site)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = mean_cones_per_g - sd_cones_per_g,
#                     ymax = mean_cones_per_g + sd_cones_per_g)) +
#   theme_classic() +
#   geom_abline(intercept = intercept, slope = slope, color = "black", linewidth = 1) +
#   annotate("text", x = -Inf, y = Inf, label = eq_label,
#            hjust = -0.1, vjust = 1.2, size = 4) +
#   xlab("mean cone density in prior year") +
#   ylab("mean cone density (cones/g)") +
#   theme(axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12),
#         strip.text = element_text(size = 12))

#modeling + building plot with lmer
mod_cone_lag <- lmer(mean_cones_per_g ~ mean_cones_per_g_lag + year + (1 | site),
                     data = mod_df_sub)

mod_cone_lag <- lmer(mean_cones_per_g ~ mean_cones_per_g_lag + (1 | site),
                     data = mod_df_sub)
summary(mod_cone_lag)

mod_summary <- summary(mod_cone_lag) #plot(mod_cone_lag)
mod_slope <- round(mod_summary$coefficients[2,1], 5)
mod_intercept <- round(mod_summary$coefficients[1,1], 5)

cone_pred_plot <- sjPlot::plot_model(mod_cone_lag, type = "pred", terms = "mean_cones_per_g_lag", margin = "empirical")

cone_plot <- cone_pred_plot +
  geom_point(aes(x = mean_cones_per_g_lag, y = mean_cones_per_g, col = site), alpha = 0.3, data = mod_df) +
  xlab("cone production in year prior") +
  ylab("cone density (cones/g)") +
  ggthemes::theme_few() +
  scale_color_discrete() +
  ggtitle("cone production in year-1") +
  annotate("text", x = 30, y = 50,
           label = paste0("y = ", mod_slope, " * x + ", mod_intercept),
           hjust = 1) +
  theme(plot.title = element_text(size = 20))

# #re-do plot below to be mean + sd  
# cone_plot <- cone_pred_plot +
#   geom_point(aes(x = mean_cones_per_g_lag, y = mean_cones_per_g, col = site), alpha = 0.3, data = mod_df) +
#   xlab("cone production in year prior") +
#   ylab("cone density (cones/g)") +
#   ggthemes::theme_few() +
#   scale_color_discrete() +
#   ggtitle("cone production in year-1") +
#   annotate("text", x = 35, y = 50,
#            label = paste0("y = ", mod_slope, " * x + ", mod_intercept),
#            hjust = 1) +
#   theme(plot.title = element_text(size = 20))

#create plots 

lag_cone_p <- ggplot(mod_df_lag_sub_summary, aes(x = mean_cones_per_g_lag, y = mean_cones_per_g, col = site)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean_cones_per_g - sd_cones_per_g, 
                    ymax = mean_cones_per_g + sd_cones_per_g)) +
  theme_classic() + 
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE) +
  stat_regline_equation(aes(group = 1), label.x.npc = "left", label.y.npc = 1) +
  stat_cor(aes(group = 1), label.x.npc = "left", label.y.npc = 0.95) +
  xlab("mean cone density in prior year") + 
  ylab("mean cone density (cones/g)") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))

precip_p <- ggplot(mod_df_sub, aes(x = mean_precip_mm_total, y = mean_cones_per_g, col = site)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean_cones_per_g - sd_cones_per_g, 
                    ymax = mean_cones_per_g + sd_cones_per_g)) +
  theme_classic() + 
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE) +
  stat_regline_equation(aes(group = 1), label.x.npc = "left", label.y.npc = 1) +
  stat_cor(aes(group = 1), label.x.npc = "left", label.y.npc = 0.95) +
  xlab("mean total spring precipitation (mm)") + 
  ylab("mean cone density (cones/g)") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))

temp_p <- ggplot(mod_df_sub, aes(x = mean_tmax_C_mean, y = mean_cones_per_g, col = site)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean_cones_per_g - sd(mean_cones_per_g), 
                    ymax = mean_cones_per_g + sd(mean_cones_per_g))) +
  theme_classic() + 
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE) +
  stat_regline_equation(aes(group = 1), label.x.npc = "left", label.y.npc = 1) +
  stat_cor(aes(group = 1), label.x.npc = "left", label.y.npc = 0.95) +
  xlab("mean total spring temperature (C)") + 
  ylab("mean cone density (cones/g)") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))

smap_p <- ggplot(mod_df_lag_sub_summary, aes(x = mean_smap_mean, y = mean_cones_per_g, col = site)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean_cones_per_g - sd_cones_per_g, 
                    ymax = mean_cones_per_g + sd_cones_per_g)) +
  theme_classic() + 
  geom_smooth(aes(group = 1), method = "lm", color = "black", se = TRUE) +
  stat_regline_equation(aes(group = 1), label.x.npc = "left", label.y.npc = 1) +
  stat_cor(aes(group = 1), label.x.npc = "left", label.y.npc = 0.95) +
  xlab("mean spring soil moisture (m3/m3)") + 
  ylab("mean cone density (cones/g)") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))

lag_cone_p + precip_p + temp_p + smap_p




#this is the code I want to use 
mod_df_long_clean <- mod_df_long |> 
  select(c(site, poly_id, flight_date.x, area, height, basal_diameter,
           date, cones_per_g, total_cone_density, total_crown_biomass_g,
           year, month, total_cone_density_lag, var, val))

mod_df_long_summary <- mod_df_long_clean |> 
  group_by(site, year, var) |> 
  summarize(mean_cones_per_g = mean(cones_per_g, na.rm = T),
            mean_val = mean(val, na.rm = T))

ggplot(mod_df_long_summary) + 
  geom_point(aes(x = mean_val, y = mean_cones_per_g, col = site)) + 
  facet_wrap(~var, scales = "free") + 
  theme_minimal()
  
mod_df_long_summary <- mod_df_long_clean |> 
  group_by(site, year, var) |> 
  summarize(mean_cones_per_g = mean(cones_per_g, na.rm = T),
            sd_cones_per_g = sd(cones_per_g, na.rm = T),
            mean_val = mean(val, na.rm = T),
            sd_val = sd(val, na.rm = T),
            .groups = "drop")

ggplot(mod_df_long_summary, aes(x = mean_val, y = mean_cones_per_g, col = site)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean_cones_per_g - sd_cones_per_g, 
                    ymax = mean_cones_per_g + sd_cones_per_g)) +
  facet_wrap(~var, scales = "free") + 
  theme_classic() + 
  geom_smooth(aes(group = 1), method = "lm", color = "black") +
  xlab("mean value") + 
  ylab("mean cone density (cones/g)") + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))


mod_precip <- lm(mean_cones_per_g ~ mean_precip_mm_total, data = mod_df_year)
summary(mod_precip)

mod_precip <- lm(mean_cones_per_g ~ mean_precip_mm_total + site, data = mod_df_year)
summary(mod_precip)

mod_precip <- lmer(mean_cones_per_g ~ mean_precip_mm_total + (1 | site),
                     data = mod_df_sub)
summary(mod_precip)

mod_precip <- lmer(mean_cones_per_g ~ sum_precip_mm_total + (1 | site),
                   data = mod_df_sub)
summary(mod_precip)

mod_precip <- lmer(mean_cones_per_g ~ mean_precip_mm_total + (1 | site), 
                   data = mod_df_year)

mod_precip <- lmer(mean_cones_per_g ~ mean_precip_mm_total + (1 | site), 
                   data = mod_df_year)

r.squaredGLMM(mod_precip)

#marginal R²
marginal_r2 <- r.squaredGLMM(mod_precip)[1, "R2m"]
marginal_r2

#p-value for the fixed effect
precip_pvalue <- summary(mod_precip)$coefficients["mean_precip_mm_total", "Pr(>|t|)"]
precip_pvalue

mod_summary <- summary(mod_precip) #plot(mod_cone_lag)
mod_slope <- round(mod_summary$coefficients[2,1], 3)
mod_intercept <- round(mod_summary$coefficients[1,1], 3)

precip_pred_plot <- sjPlot::plot_model(mod_precip, type = "pred", terms = "mean_precip_mm_total", margin = "empirical")


precip_plot <- precip_pred_plot +
  geom_point(aes(x = sum_precip_mm_total, y = mean_cones_per_g, col = site), data = mod_df_sub) +
  xlab("sum spring precipitation (mm)") +
  ylab("cone density (cones/g)") +
  ggthemes::theme_few() +
  scale_color_discrete() +
  ggtitle("cone production ~ spring precipitation") +
  annotate("text", x = 250, y = 50,
           label = paste0("y = ", mod_slope, " * x + ", mod_intercept),
           hjust = 1) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16))

precip_plot <- precip_pred_plot +
  geom_point(aes(x = mean_precip_mm_total, y = mean_cones_per_g, col = site), data = mod_df_year) +
  xlab("mean yearly precipitation (mm)") +
  ylab("cone density (cones/g)") +
  ggthemes::theme_few() +
  scale_color_discrete() +
  ggtitle("cone production ~ mean precipitation") +
  annotate("text", x = 40, y = 47,
           label = paste0("y = ", mod_slope, " * x + ", mod_intercept,
                          "\nR² = ", round(marginal_r2, 3),
                          "\np = ", round(precip_pvalue, 3)),
           hjust = 0) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"))

mod_temp <- lmer(mean_cones_per_g ~ mean_tmax_C_mean + (1 | site),
                   data = mod_df_sub)
summary(mod_temp)

mod_summary <- summary(mod_temp) #plot(mod_cone_lag)
mod_slope <- round(mod_summary$coefficients[2,1], 3)
mod_intercept <- round(mod_summary$coefficients[1,1], 3)

temp_pred_plot <- sjPlot::plot_model(mod_temp, type = "pred", terms = "mean_tmax_C_mean", margin = "empirical")


temp_plot <- temp_pred_plot +
  geom_point(aes(x = mean_tmax_C_mean, y = mean_cones_per_g, col = site), data = mod_df_sub) +
  xlab("mean spring temperature (°C)") +
  ylab("cone density (cones/g)") +
  ggthemes::theme_few() +
  scale_color_discrete() +
  ggtitle("cone production ~ spring temperature") +
  annotate("text", x = 29, y = 50,
           label = paste0("y = ", mod_slope, " * x + ", mod_intercept),
           hjust = 1) +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 16))

temp_plot + precip_plot



