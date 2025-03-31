library(magick)

# Set your image folder
img_folder <- "/Users/hannahzonnevylle/Desktop/quadrat pics 2025/cropped_quadrat_pics"
output_folder <- file.path(img_folder, "jpeg_images")
dir.create(output_folder)

# List all TIFF files
tiff_files <- list.files(img_folder, pattern = "\\.tiff?$", full.names = TRUE)

# Convert each TIFF to JPEG
for (file in tiff_files) {
  img <- image_read(file)
  new_name <- file.path(output_folder, paste0(tools::file_path_sans_ext(basename(file)), ".jpg"))
  image_write(img, new_name, format = "jpeg")
  
  print(file)
}

library(officer)

ppt <- read_pptx()

# List converted JPEG images
jpeg_files <- list.files(output_folder, pattern = "\\.jpg$", full.names = TRUE)

for (file in jpeg_files) {
  # Extract site and number
  filename <- basename(file)
  matches <- regmatches(filename, regexec("^(.*?)_t(\\d+)_filt", filename))
  
  if (length(matches[[1]]) == 3) {
    site <- matches[[1]][2]
    number <- matches[[1]][3]
    title <- paste(site, "T", number)
  } else {
    title <- filename # Fallback
  }
  
  # Add a new slide with title and image
  ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
  ppt <- ph_with(ppt, title, location = ph_location_type(type = "title"))  # Use "title" instead of "ctrTitle"
  ppt <- ph_with(ppt, external_img(file, width = 6, height = 4), location = ph_location_type(type = "body"))
}

# Save PowerPoint
pptx_file <- file.path(img_folder, "slideshow_unfilt.pptx")
print(ppt, target = pptx_file)
cat("Slideshow created:", pptx_file, "\n")
