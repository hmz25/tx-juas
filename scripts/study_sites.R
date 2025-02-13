library(sf)

setwd("C:/Users/hmz25/Box/")

#load in kml files for site boundaries 
kml_layers <- st_layers("Katz lab/texas/ecolab sites 2025.kml")

drone_sites <- st_read("Katz lab/texas/ecolab sites 2025.kml", layer = "drone flights") 

drone_sites$site_type <- "drone"

data_sites <- st_read("Katz lab/texas/ecolab sites 2025.kml", layer = "2 visits")

data_sites$site_type <- "data"

#combine drone sites and data sites 
sites_df <- rbind(drone_sites, data_sites) 

#calculate area in m2 for each sites 
sites_df$area <- st_area(sites_df)

#calculate total area for all study sites in km2
sites_total_area <- sites_df %>% 
  summarize(total_area = sum(area)/1000000)

print(sites_total_area) #in km2, not m2
