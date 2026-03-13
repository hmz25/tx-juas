# library(smapr)
library(terra)
library(dplyr)
library(tidyverse)
library(ggthemes) #install.packages("ggthemes")
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(sf)
library(ggmap) #install.packages("ggmap")
library(googlesheets4)
library(ggspatial) #install.packages("ggspatial")
library(patchwork)

setwd("C:/Users/hmz25/Box/Katz lab/texas/field_site_map")

# load in TX shape files --------------------------------------------------

tx_counties_shp <- st_read("tx_counties.shp")
# plot(tx_counties_shp)

tx_state_shp <- st_read("tx_state_boundary.shp") 
# plot(tx_state_shp)


# load in environmental data ----------------------------------------------


# tx_smap_2024 <- read_csv("/Users/hannahzonnevylle/Desktop/juas/2025 analysis/tx_SMAP_2024.csv")

tx_smap_2019_2024 <- read_csv("tx_SMAP_20192024.csv")

tx_counties_smap <- tx_counties_shp |> 
  rename(NAME = CNTY_NM) |> 
  left_join(tx_smap_2019_2024, by = "NAME") |> 
  select(NAME, mean, geometry) |> 
  st_as_sf()

# ggplot(tx_counties_smap) +
#   geom_sf(aes(fill = mean), color = "black", size = 0.1) + # color for borders and width
#   scale_fill_viridis_c(option = "plasma", name = "soil moisture", direction = -1) + 
#   theme_minimal() + 
#   labs(
#     title = "soil moisture by county",
#     caption = "data source: NASA SMAP L4"
#   ) + 
#   theme_void()

#reproject environmental data to be in same CRS 

# st_crs(tx_counties_smap_reproj)
# st_crs(tx_counties_shp)
# st_crs(tx_state_shp)

tx_counties_shp_reproj <- st_transform(tx_counties_shp, crs = st_crs(tx_state_shp))


# ggplot() +
#   geom_sf(data = tx_state_shp, fill = NA, color = "black") + # Plot state boundaries
#   coord_sf() +
#   theme_classic() +
#   geom_sf(data = tx_counties_smap_reproj, aes(fill = mean)) + 
#   scale_fill_viridis(option = "inferno", 
#                      expression("soil moisture (m"^3*"/m"^3*")"), 
#                      #breaks = seq(0, 0.5, by = 0.025),
#                      direction = -1) + 
#   geom_point(data = sites_df, aes(x = lon, y = lat), 
#              size = 2, 
#              shape = 21, 
#              color = "white", 
#              fill = "black",
#              stroke = 0.5) 

#create bounding box to limit map to site area
bbox <- c(xmin = -100.98, ymin = 29.6, xmax = -97.56, ymax = 31.26)

# load in site data -------------------------------------------------------

#load in shape file for sites 
sites_shp <- st_read("C:/Users/hmz25/Box/Katz lab/texas/all_sites.kml")
# plot(sites_shp$geometry)

#reformat to match with site metadata 
sites_shp_clean <- sites_shp |> 
  mutate(coords = st_centroid(tx_sites$geometry), #add column for coordinates
         #match naming convention with site metadata 
         site_name = str_split_i(tx_sites$Name, "\\(", 1)) |> 
  mutate(site_name = recode(site_name,
                            "Williamson 2022-13" = "Williamson",
                            "Kimble 2023-03" = "Kimble")) |> 
  mutate(site_name = tolower(site_name)) |> 
  mutate(site_name = trimws(site_name))


#load in google sheet with metadata on number of visits
gs4_auth()

sites_df <- read_sheet("https://docs.google.com/spreadsheets/d/1jB-oP0xmGaZrooFFFfhpuTJfUZ3AcrEKUljGjbsFyZs/edit?gid=0#gid=0")

sites_df_clean <- sites_df |> 
  select(site_name, ecolab_name, county, visit_year) |> 
  group_by(site_name, ecolab_name, county) |> 
  summarize(n_visits = n()) |> #add data on number of visits to the site 
  ungroup()

# length(unique(sites_df_clean$site_name))

#join site_df with shape file to get data for map 
sites_sf_df <- sites_shp_clean |> 
  left_join(sites_df_clean, by = "site_name") |> 
  mutate(x_coord = st_coordinates(sites_sf_df$coords)[,1],
         y_coord = st_coordinates(sites_sf_df$coords)[,2]) |> 
  mutate(n_visits = as.factor(n_visits))



# load in tiff with juas basal area abundance -----------------------------


juas_ba <- rast("Ashe_juniper_basal_area_300m.tif")
plot(juas_ba)  
# crs(juas_ba)
# crs(juas_ba) == crs(tx_state_shp)

tx_crs <- st_crs(tx_state_shp)$wkt  #extract as WKT for compatibility

#reproject raster to match sf object's CRS
juas_ba_reproj <- project(juas_ba, st_crs(tx_state_shp)$wkt)
crs(juas_ba_reproj) == crs(tx_state_shp)

#convert raster to a data frame for ggplot
juas_ba_df <- as.data.frame(juas_ba_reproj, xy = TRUE)

#rename value column
colnames(juas_ba_df)[3] <- "value"  

juas_ba_df_filt <- juas_ba_df |>
  filter(value != -2147483648)


# create maps! ------------------------------------------------------------

#outline of TX with basal area and bounding box

p_outline <- ggplot() +
  geom_sf(data = tx_state_shp, fill = NA, color = "black") +
  geom_rect(
    aes(xmin = bbox["xmin"], xmax = bbox["xmax"],
        ymin = bbox["ymin"], ymax = bbox["ymax"]),
    color = "red", fill = NA, linewidth = 1
  ) +
  geom_raster(data = juas_ba_df_filt, aes(x = x, y = y), alpha = 0.5) +
  scale_fill_viridis_c() +  
  coord_sf(xlim = st_bbox(tx_state_shp)[c("xmin", "xmax")],
           ylim = st_bbox(tx_state_shp)[c("ymin", "ymax")],
           expand = FALSE) +
  theme_void()


p_main <- ggplot() +
  geom_sf(data = tx_counties_smap_reproj, aes(fill = mean)) +
  geom_point(data = sites_sf_df, 
             aes(x = x_coord, y = y_coord, shape = n_visits), 
             size = 3,
             stroke = 1.5,
             col = "lightgrey") + 
  scale_shape_manual(values = c(1, 3, 8)) + 
  labs(shape = "years in study")  +
  scale_fill_viridis(option = "inferno",
                     expression("soil moisture (m"^3*"/m"^3*")"),
                     direction = -1) +
  coord_sf(xlim = c(-100.5, -97.75), ylim = c(29.6, 31.26)) + #limit to bounding box
  annotation_scale(location = "bl", width_hint = 0.3) +  # Add scale bar at bottom left
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) + # Add north arrow
  theme_void()

p_final <- (p_outline + theme(plot.margin = unit(c(0,30,0,0), "pt"))) + p_main

ggsave("site_map.png",
       p_final, 
       width = 10,
       height = 8,
       units = "in")

