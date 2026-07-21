#script to clean field maps data and merge with canopy shp files

#set up work environment
library(sf)
library(dplyr)
library(tidyverse)
library(janitor)
library(lubridate)

# #wd for lab desktop
# setwd("C:/Users/hmz25/Box/Katz lab/texas/")

# #wd for hz macbook
# setwd("/Users/hannahzonnevylle/Library/CloudStorage/Box-Box/Katz lab/texas")

#mo comp 
setwd("C:/Users/HMZ/Box/texas")

#load in field maps data sheets and combine into one df

#2026 data 
focal_26 <- read_csv("01_data/FieldMaps data 2026/focal_trees_2026.csv") %>% 
  clean_names()
# colnames(focal_26)

female_26 <- read_csv("01_data/FieldMaps data 2026/female_trees.csv") %>% 
  clean_names()
# colnames(female_26)

misc_26 <- read_csv("01_data/FieldMaps data 2026/misc_trees.csv") %>% 
  clean_names()
# colnames(misc_26)

#2025 data 
focal_25 <- read_csv("01_data/qgis/focal_trees_2025.csv") %>% 
  clean_names()
# colnames(focal_25)

misc_25 <- read_csv("01_data/qgis/misc_trees.csv") %>% 
  clean_names()
# colnames(misc_25)

trees_24 <- read_csv("01_data/field_data_Jan24_HZ_DK/trees.csv") %>% 
  clean_names()
# colnames(trees_24)

misc_24 <- read_csv("01_data/field_data_Jan24_HZ_DK/misc_points.csv") %>% 
  clean_names()
# colnames(misc_24)

#standardize data frames

#define the desired columns in final output
output_cols <- c("x", "y", "date_time", "site", "percent_cones_open", "notes", 
                  "focal_tree_number", "foliage_density", "height", "total_stems", 
                  "basal_diameter", "crop_fraction", "crop_count", "tree_id", 
                  "canopy_class", "stem_diameter", "dbh", "total_stem", 
                  "dbh_below", "total_st_1", "source")


#2026 data
focal_26_std <- focal_26 %>%
  mutate(source = "focal_26") %>%
  dplyr::select(all_of(output_cols[output_cols %in% names(.)]))

female_26_std <- female_26 %>%
  mutate(source = "female_26") %>%
  dplyr::select(all_of(output_cols[output_cols %in% names(.)]))

misc_26_std <- misc_26 %>%
  mutate(source = "misc_26") %>%
  dplyr::select(all_of(output_cols[output_cols %in% names(.)]))

#2025 data
focal_25_std <- focal_25 %>%
  rename(
    focal_tree_number = focal_tree,
    percent_cones_open = percent_co,
    foliage_density = foliage_de
  ) %>%
  mutate(source = "focal_25") %>%
  dplyr::select(all_of(output_cols[output_cols %in% names(.)]))

misc_25_std <- misc_25 %>%
  rename(
    percent_cones_open = percent_co,
    foliage_density = foliage_de
  ) %>%
  mutate(source = "misc_25",
         date_time = as.character(date_time)) %>%
  dplyr::select(all_of(output_cols[output_cols %in% names(.)]))

#2024 data
trees_24_std <- trees_24 %>%
  rename(
    focal_tree_number = tree_n,
    dbh = dbh_cm,
    date_time = date_and_t
  ) %>%
  mutate(source = "trees_24",
         date_time = as.character(date_time),
         focal_tree_number = as.double(focal_tree_number)) %>%
  dplyr::select(all_of(output_cols[output_cols %in% names(.)]))

misc_24_std <- misc_24 %>%
  mutate(source = "misc_24",
         date_time = as.character(date_time)) %>%
  dplyr::select(all_of(output_cols[output_cols %in% names(.)]))

#combine

all_fieldmaps_df <- bind_rows(
  focal_26_std, female_26_std, misc_26_std,
  focal_25_std, misc_25_std,
  trees_24_std, misc_24_std
)

#clean df 
all_fieldmaps_df_clean <- all_fieldmaps_df %>%
  #add date column
  mutate(
    date = sapply(date_time, function(dt) {
      if (grepl("-", dt)) {
        #already in YYYY-MM-DD format, use as-is
        dt
      } else {
        #in M/D/YYYY H:MM:SS AM/PM format
        #extract just the date part (before the space)
        date_str <- sub(" .*", "", dt)
        #split by "/" to get month, day, year
        parts <- as.integer(strsplit(date_str, "/")[[1]])
        #reformat as YYYY-MM-DD
        sprintf("%04d-%02d-%02d", parts[3], parts[1], parts[2])
      }
    }),
    site = tolower(substr(site, 1, 4))
  ) %>% 
  #add columns to check focal tree number across years
  mutate(
    year_fieldmaps = as.numeric(substr(date, 1, 4)),
    focal_tree_n_26 = case_when(year_fieldmaps == 2026 ~ focal_tree_number),
    focal_tree_n_25 = case_when(year_fieldmaps == 2025 ~ focal_tree_number),
    focal_tree_n_24 = case_when(year_fieldmaps == 2024 ~ focal_tree_number)
  )

# write_csv(all_fieldmaps_df_clean, "01_data/all_years_fieldmaps.csv")

#joining canopy files and shape files

#join field maps and shape files
all_trees <- all_fieldmaps_df_clean

#set directory for shifted canopy shape files
shp_dir <- "03_output/corrected_canopy_shp"
shp_list_full <- list.files(shp_dir, full.names = TRUE, pattern = ".shp")

#read shape files and combine
shapefiles <- map_df(shp_list_full, st_read, .id = "shp_source")

#each row is a different polygon, add poly_id column
shapefiles$poly_id <- seq.int(nrow((shapefiles)))

#extract site codes from file names
site_codes <- as.data.frame(substr(str_split_i(basename(shp_list_full), "_", 1), 1, 4))
site_codes$shp_source <- seq.int(nrow(site_codes)) #add matching column 

site_codes <- site_codes %>% 
  rename(poly_site = `substr(str_split_i(basename(shp_list_full), "_", 1), 1, 4)`) %>% 
  mutate(shp_source = as.character(shp_source))

shapefiles <- shapefiles %>% 
  left_join(site_codes, by = "shp_source")

#prep field maps data to join
#convert field maps obs to sf spatial points
trees_sf <- all_trees %>%
  st_as_sf(
    coords = c("x", "y"),
    crs = st_crs(shapefiles)  #match the shapefile CRS
  )

# #spatial join by finding nearest point to each polygon per shape file 
# #nearest neighbor join (one tree per polygon)
# shapefiles_with_trees <- st_join(
#   shapefiles,
#   trees_sf %>% select(site, focal_tree_number, focal_tree_n_26, focal_tree_n_25, focal_tree_n_24, source, date),
#   join = st_nearest_feature,
#   suffix = c("", "_tree")
# )

# #intersection join (all trees within each polygon)
# shapefiles_with_trees <- st_join(
#   shapefiles,
#   trees_sf %>% select(site, year, focal_tree_number, focal_tree_n_26, focal_tree_n_25, focal_tree_n_24, source, date),
#   join = st_intersects
# )

#intersection join (all trees within each polygon)
shapefiles_with_trees <- st_join(
  shapefiles,
  trees_sf,
  join = st_intersects
)

length(unique(shapefiles_with_trees$poly_id))

#looks good, no empty geoms

#subsetting to do some data cleaning on focal trees
#need to make sure same focal trees have consistent naming across each year for comparison 

#select focal trees
shapefiles_with_trees_focal <- shapefiles_with_trees %>% 
  drop_na(focal_tree_number)

#compare focal tree numbers across the years 

# focal_trees_summary <- shapefiles_with_trees_focal %>% 
#   pivot_wider(
#     id_cols = c(site, geometry),
#     names_from = year,
#     values_from = focal_tree_number,
#     names_prefix = "n_"
#   )

focal_trees_summary <- shapefiles_with_trees_focal %>% 
  group_by(site, poly_id, geometry, year_fieldmaps) %>%
  #create column names with suffixes for duplicates
  mutate(focal_tree_number = as.character(focal_tree_number),
    col_name = paste0("n_", year_fieldmaps, if_else(row_number() == 1, "", paste0("_", row_number())))
  ) %>%
  ungroup() %>%
  #pivot on the pre-made column names
  pivot_wider(
    id_cols = c(site, geometry, poly_id),
    names_from = col_name,
    values_from = focal_tree_number
  ) %>%
  #create the priority focal_tree_n column
  mutate(
    focal_tree_n = coalesce(n_2026, n_2025, n_2024)
  ) %>% 
  mutate(row_id = row_number())

#examine which focal trees have different numbers for the same trees across years 
duplicates <- focal_trees_summary %>% 
  group_by(site, focal_tree_n) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  arrange(site, focal_tree_n)

#trees that had different classifications for years before 2026 can be dropped and not considered focal
#add focal classification
shp_focal_final <- shapefiles_with_trees_focal %>%
  group_by(site, focal_tree_number) %>%
  slice_max(order_by = year_fieldmaps, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(classification = "focal")

length(unique(shp_focal_final$poly_id))

#still duplicates, going in to manually remove
shp_focal_final <- shp_focal_final %>% 
  slice(-c(14, 17, 64))

#other trees
misc_tree_df <- shapefiles_with_trees %>% 
  filter(is.na(focal_tree_number)) %>% #select trees that aren't focal trees
  #create classification column
  mutate(
    classification = case_when(
      grepl("Female", notes, ignore.case = TRUE) | source == "female_26" ~ "female",
      TRUE ~ "misc"
    )
  )

#bind the focal and misc trees together
all_trees_final <- bind_rows(shp_focal_final, misc_tree_df)

length(unique(all_trees_final$poly_id))


# write_csv(all_trees_final, "01_data/all_trees_shp_clean.csv") 

st_write(all_trees_final, "01_data/all_trees_canopy_seg_shp.shp", delete_dsn = TRUE)
