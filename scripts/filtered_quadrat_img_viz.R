library(magick)
library(raster)
library(terra)
library(jpeg)

# ---- set input/output folder for images ----
input_folder  <- "C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics"
output_folder <- "C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/jpeg_cropped_quadrat_pics"

# ---- create output folder ----
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---- get list of .tif files ----
tif_files <- list.files(
  input_folder,
  pattern = ".tif",
  full.names = TRUE,
  ignore.case = TRUE
)

# ---- convert each file from tif to jpeg ----
for (i in seq_along(tif_files)) {
  
  file <- tif_files[i]   # Get actual file path
  
  #get image
  img <- image_read(file)
  
  #create output file name
  base_name <- tools::file_path_sans_ext(basename(file))
  output_file <- file.path(output_folder, paste0(base_name, ".jpeg"))
  
  #write as JPEG
  image_write(img, path = output_file, format = "jpeg")
  
  print(file)
}

#create folder for filtered jpegs for visualization

# ---- set input/output folder for images ----
input_folder  <- "C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/cropped_quadrat_pics/masked_cropped_quadrat_pics"
output_folder <- "C:/Users/hmz25/Box/Katz lab/texas/tx 2026 drone pics/2026 quadrat pics/jpeg_cropped_filtered_quadrat_pics"

# ---- create output folder ----
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ---- get list of files ----
tif_files <- list.files(
  input_folder,
  pattern = ".tif",
  full.names = TRUE,
  ignore.case = TRUE
)

# ---- convert each filtered picture from tif to jpeg ----

# i = 4

for (i in seq_along(tif_files)) {
  
  file <- tif_files[i]
  
  #get image
  img <- stack(file)
  
  #rename bands
  names(img) <- c("r", "g", "b", "rf_mask")
  
  #filter out pixels that aren't foliage or cones 
  img_filt <- mask(img, img$rf_mask, maskvalue = 2, inverse = TRUE)
  
  #create output file name
  base_name <- tools::file_path_sans_ext(basename(file))
  output_file <- file.path(output_folder, paste0(base_name, "_filt.jpg"))
  
  # ---- OPEN JPEG DEVICE ----
  jpeg(filename = output_file,
       width = 480, 
       height = 480, 
       units = "px", 
       pointsize = 12,
       quality = 75)
  
  #plot to file
  plotRGB(img_filt)
  
  # ---- CLOSE DEVICE ----
  dev.off()
  
  print(file)
}
