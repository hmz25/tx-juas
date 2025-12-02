library(raster)
library(terra)
library(dplyr)

# create slideshow to visualize quadrat imgs with PCD ---------------------

getwd()

#process images and save plots
output_dir <- "pptx_img_viz"
dir.create(output_dir, showWarnings = FALSE)

photo_dir <- "C:/Users/hmz25/Box/Katz lab/texas/tx 2025 drone pics/handheld quadrat pics 2025/white_balanced/masked_white_balanced"
photo_list <- list.files(photo_dir, pattern = ".tif", full.names = FALSE)
photo_list_full_dir <- list.files(photo_dir, pattern = ".tif", full.names = TRUE)

# i = 1

slide_data <- data.frame()

for (i in seq_along(photo_list)) {
  photo_i <- raster::stack(photo_list_full_dir[i])
  photo_name <- tools::file_path_sans_ext(basename(photo_list_full_dir[i]))
  
  # Process image as before (masking, aggregation, index)
  
  #mask out non foliage/cone pixels
  photo_i[[1]][photo_i[[4]] == 1] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i[[2]][photo_i[[4]] == 1] <- NA #set all green pixel values that are not foliage/cones to NA
  
  
  #check if '_female' is in the file name
  #females have been selected fron aerial imgs, so already at correct res
  if (grepl("_female", basename(photo_list[i]))) {
    photo_i_agg <- photo_i  #do not aggregate, just assign
  } else {
    #aggregate pixels to coarser resolution
    photo_i_agg <- aggregate(photo_i, fact = c(100, 100), fun = "mean")
  }
  
  #mask out non foliage/cone pixels after aggregation 
  # plot(photo_i_agg[[4]])
  photo_i_agg[[1]][photo_i_agg[[4]] < 1.5] <- NA #set all red pixel values that are not foliage/cones to NA
  photo_i_agg[[2]][photo_i_agg[[4]] < 1.5] <- NA #set all green pixel values that are not foliage/cones to NA
  # plot(photo_i_agg)
  # plotRGB(photo_i_agg)
  
  photo_i_index_rg_dif <- (photo_i_agg[[1]] - photo_i_agg[[2]]) / 
    (photo_i_agg[[1]] + photo_i_agg[[2]])
  photo_i_rg_dif <- cellStats(photo_i_index_rg_dif, "mean")
  
  # Save plots with consistent naming
  orig_path <- file.path(output_dir, paste0(photo_name, "_orig.png"))
  agg_path <- file.path(output_dir, paste0(photo_name, "_agg.png"))
  index_path <- file.path(output_dir, paste0(photo_name, "_index.png"))
  
  # Original
  png(orig_path, width = 800, height = 800)
  plotRGB(photo_i, main = "original")
  dev.off()
  
  # Aggregated
  png(agg_path, width = 800, height = 800)
  plotRGB(photo_i_agg, main = "coarsened")
  dev.off()
  
  # Index with fixed zlim
  png(index_path, width = 5, height = 5, units = "in", res = 300)
  plot(
    photo_i_index_rg_dif,
    zlim = c(-0.2, 0.2),
    main = "index",
    cex.main = 2,  # Bigger title
    legend.width = 2.5,    # Make the scale bar thicker
    legend.shrink = 0.8,
    legend.args = list(
      text = "index value"
    )
  )
  
  dev.off()
  
  # Save metadata
  slide_data <- bind_rows(slide_data, data.frame(
    name = photo_name,
    orig_path, agg_path, index_path,
    mean_index = photo_i_rg_dif
  ))
}

#join with cone data
slide_data <- slide_data %>%
  mutate(site = str_extract(name, "^[^_]+"),
         tree = str_extract(name, "(?<=_)[^\\.]+"))

slide_data <- left_join(slide_data, cone_g_df, by = c("site", "tree")) %>%
  mutate(
    total_gcones_gfol = replace_na(total_gcones_gfol, 0),
    total_cones_per_g = replace_na(total_cones_per_g, 0)
  )

#generate power point
ppt <- read_pptx()

for (i in seq_len(nrow(slide_data))) {
  row <- as.list(slide_data[i, ])  
  
  text_block <- fpar(
    ftext(sprintf(
      "mean index: %.3f\ng cone/g fol: %.2f g\ntotal cones: %.0f",
      row$mean_index,
      row$total_gcones_gfol,
      row$total_cones_per_g
    ),
    fp_text(font.size = 14, font.family = "Arial")
    ),
    fp_p = fp_par(text.align = "left")
  )
  
  ppt <- ppt %>%
    #add slide with default layout
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    
    #title at the top
    ph_with(
      value = row$name,
      location = ph_location(left = 0.5, top = 0, width = 9, height = 1)
    ) %>%
    
    #original image: top-left under title
    ph_with(
      external_img(row$orig_path, height = 3, width = 3.5),
      location = ph_location(left = 0.5, top = 1.0)
    ) %>%
    
    #text block: right of original image
    ph_with(
      value = text_block,
      location = ph_location(left = 5, top = 0.7, width = 5, height = 2.5)
    ) %>%
    
    #coarsened image: directly below original image
    ph_with(
      external_img(row$agg_path, height = 3, width = 3.5),
      location = ph_location(left = 0.5, top = 4.2)
    ) %>%
    
    #index plot: right of coarsened image (slightly larger)
    ph_with(
      external_img(row$index_path, height = 5, width = 5, unit = "in"),
      location = ph_location(left = 5, top = 2.5, width = 5, height = 5)
    )
}

#save presentation
print(ppt, target = "cone_slides.pptx", overwrite = T)