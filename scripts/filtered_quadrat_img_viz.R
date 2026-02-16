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

# ---- Get list of .tif and .tiff files ----
tif_files <- list.files(
  input_folder,
  pattern = ".tif",
  full.names = TRUE,
  ignore.case = TRUE
)

# ---- Convert each file ----
for (i in seq_along(tif_files)) {
  
  file <- tif_files[i]   # Get actual file path
  
  # Read image
  img <- image_read(file)
  
  # Create output filename
  base_name <- tools::file_path_sans_ext(basename(file))
  output_file <- file.path(output_folder, paste0(base_name, ".jpeg"))
  
  # Write as JPEG
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

# ---- Get list of files ----
tif_files <- list.files(
  input_folder,
  pattern = ".tif",
  full.names = TRUE,
  ignore.case = TRUE
)

# ---- Convert each file ----

# i = 4

for (i in seq_along(tif_files)) {
  
  file <- tif_files[i]
  
  # Load image
  img <- stack(file)
  
  # Rename bands
  names(img) <- c("r", "g", "b", "rf_mask")
  
  # Filter mask
  img_filt <- mask(img, img$rf_mask, maskvalue = 2, inverse = TRUE)
  
  # Create output filename
  base_name <- tools::file_path_sans_ext(basename(file))
  output_file <- file.path(output_folder, paste0(base_name, "_filt.jpg"))
  
  # ---- OPEN JPEG DEVICE ----
  jpeg(filename = output_file,
       width = 480, 
       height = 480, 
       units = "px", 
       pointsize = 12,
       quality = 75)
  
  # Plot to file
  plotRGB(img_filt)
  
  # ---- CLOSE DEVICE ----
  dev.off()
  
  print(file)
}

# 
# #see which quadrat pictures are missing
# 
# #create list of quadrats from df 
# 
# img_names <- tools::file_path_sans_ext(basename(tif_files))
# 
# img_names |> as.data.frame() |> View()
# 
# img_df <- img_names |> 
#   as.data.frame() |> 
#   separate(
#     col = img_names,
#     into = c("site", "tree", "quadrat_location", "adjustment"), 
#     sep = "_"          
#   )
# 
# img_df_clean <- img_df |> 
#   mutate(tree = str_extract(tree, "(?<=t).*"),
#          tree = as.double(tree))
# 
# quadrat_imgs <- read_csv("C:/Users/hmz25/Desktop/cone processing 26 - counts.csv")
# 
# missing_quadrat_imgs <- anti_join(quadrat_imgs, img_df_clean, by = c("site", "tree", "quadrat_location"))
# 
# # anti_join() returns all rows from x without a match in y
