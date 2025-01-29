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

#loading in site coords
sites <- list(
  kerr = st_point(c(-99.5005102, 30.0034628)),
  real = st_point(c(-99.9541053, 29.9027972)),
  medina = st_point(c(-99.3545039, 29.8690432)),
  creek = st_point(c(-99.9796525, 29.9175)),
  gun = st_point(c(-99.4759973, 30.035774)),
  sonora = st_point(c(-100.6438711, 30.5729456)),
  sweeten = st_point(c(-100.2233128, 30.0168492)),
  wade = st_point(c(-98.05948508225009, 30.82497215848099)),
  williamson = st_point(c(-97.872075, 30.611831)),
  glimmer = st_point(c(-97.909797, 30.397233)),
  kimble = st_point(c(-100.065006,30.704469)),
  cell_tower = st_point(c(-98.095108, 30.301833)),
  cathedral = st_point(c(-98.1696527, 29.9587435)),
  fisher = st_point(c(-98.3387971, 29.9638185))
)

#create a data frame from the list of points
sites_df <- data.frame(
  name = names(sites),
  lon = sapply(sites, function(point) point[1]),
  lat = sapply(sites, function(point) point[2])
) |> 
  mutate(name = recode(name, cell_tower = "cell")) 

#note for HZ - make a google sheet of site coords and yearly rain averages

# register_stadiamaps("f8355a41-21b7-4615-b280-99f79131c6e1", write = FALSE) #didn't work

# install.packages('osmdata')
library(osmdata)

register_stadiamaps(key = "f8355a41-21b7-4615-b280-99f79131c6e1")

stadiamaps_key(key = "f8355a41-21b7-4615-b280-99f79131c6e1")

#coords for TX bounding box
tx_bb <- c(-106.64719063660635,
           25.840437651866516,
           -93.5175532104321,
           36.50050935248352)

#coords for hill country bounding box
hill_country_bb <- c(-100.98,29.6,-97.56,31.26)

tx_map <- get_stadiamap(bbox = c(-106.64719063660635,
                                 25.840437651866516,
                                 -93.5175532104321,
                                 36.50050935248352), 
                        zoom = 6,
                        maptype = "stamen_toner_lite",
                        color = c("bw")) 

hillcountry_map <- get_stadiamap(bbox = hill_country_bb, 
                      zoom = 6,
                      maptype = "stamen_toner_lite",
                      color = c("bw"))

# ggmap::ggmap(map2) + 
#   geom_point(data = sites_df, aes(x = lon, y = lat, col = name), size = 2) +
#   labs(color = "site names") 

ggmap::ggmap(hillcountry_map) + 
  geom_point(data = sites_df, aes(x = lon, y = lat), size = 4, shape = 3) +
  labs(color = "site names")

ggmap::ggmap(tx_map) +
  geom_point(data = sites_df, aes(x = lon, y = lat), size = 3, shape = 3) 

# ggmap::ggmap(tx_map) +
#   geom_point(data = sites_df, aes(x = lon, y = lat), size = 4, shape = 3) +
#   labs(color = "site names") + 
#   scale_fill_hue()


tx_counties_shp <- st_read("/Users/hannahzonnevylle/Desktop/juas/GIS/tx_counties.shp")

tx_state_shp <- st_read("/Users/hannahzonnevylle/Desktop/juas/GIS/tx_state_boundary.shp") 

# tx_smap_2024 <- read_csv("/Users/hannahzonnevylle/Desktop/juas/2025 analysis/tx_SMAP_2024.csv")

tx_smap_2019_2024 <- read_csv("/Users/hannahzonnevylle/Desktop/juas/2025 analysis/tx_SMAP_20192024.csv")

#plot(tx_counties_shp)

tx_counties_smap <- tx_counties_shp |> 
  rename(NAME = CNTY_NM) |> 
  left_join(tx_smap_2019_2024, by = "NAME") |> 
  select(NAME, mean, geometry) |> 
  st_as_sf()

ggplot(tx_counties_smap) +
  geom_sf(aes(fill = mean), color = "black", size = 0.1) + # color for borders and width
  scale_fill_viridis_c(option = "plasma", name = "soil moisture", direction = -1) + 
  theme_minimal() + # Clean theme
  labs(
    title = "Soil Moisture by County",
    caption = "Data source: NASA SMAP L4"
  )

tx_counties_smap_reproj <- st_transform(tx_counties_smap, crs = st_crs(tx_state_shp))

# st_crs(tx_counties_smap_reproj)
# st_crs(tx_counties_shp)
# st_crs(tx_state_shp)

tx_counties_shp_reproj <- st_transform(tx_counties_shp, crs = st_crs(tx_state_shp))

# ggplot() +
#   geom_sf(data = tx_state_shp, fill = NA, color = "black") + # Plot state boundaries
#   coord_sf() +
#   theme_classic() +
#   geom_sf(data = tx_counties_smap_reproj, aes(fill = mean)) + 
#   scale_fill_gradient(
#     low = "red", 
#     high = "darkblue", 
#     na.value = "gray90", #color for NA values
#     name = "soil moisture") +
#   geom_point(data = sites_df, aes(x = lon, y = lat), 
#              size = 2, 
#              shape = 21, 
#              color = "white", 
#              fill = "black",
#              stroke = 0.5) 
# 
# ggplot() +
#   geom_sf(data = tx_state_shp, fill = NA, color = "black") + # Plot state boundaries
#   coord_sf() +
#   theme_classic() +
#   geom_sf(data = tx_counties_smap_reproj, aes(fill = mean)) + 
#   scale_fill_gradient(
#     low = "red", 
#     high = "darkblue", 
#     na.value = "gray90", #color for NA values
#     name = "soil moisture") +
#   geom_point(data = sites_df, aes(x = lon, y = lat), 
#              size = 2, 
#              shape = 4, 
#              color = "white")

ggplot() +
  geom_sf(data = tx_state_shp, fill = NA, color = "black") + # Plot state boundaries
  coord_sf() +
  theme_classic() +
  geom_sf(data = tx_counties_smap_reproj, aes(fill = mean)) + 
  scale_fill_viridis(option = "inferno", 
                     name = "soil moisture", 
                     #breaks = seq(0, 0.5, by = 0.025),
                     direction = -1) + 
  geom_point(data = sites_df, aes(x = lon, y = lat), 
             size = 2, 
             shape = 21, 
             color = "white", 
             fill = "black",
             stroke = 0.5) 
  



