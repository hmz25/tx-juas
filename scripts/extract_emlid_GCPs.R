# convert llh emlid data to use for gcps in pix4d -------------------------


# llh <- read.table("C:/Users/hmz25/Desktop/KatzLab_20240109183445 (1)/KatzLab_solution_20240109183445.LLH", 
#                   header = FALSE, sep = "", stringsAsFactors = FALSE)
# colnames(llh) <- c("Date", "Time", "Latitude", "Longitude", "Height", 
#                 "Q", "ns", "sdn", "sde", "sdu", "sdne", "sdeu", "sdun", "Age", "Ratio")

# lines <- readLines("C:/Users/hmz25/Desktop/KatzLab_20240109183445 (1)/KatzLab_solution_20240109183445.LLH", n = 1)
# cat(lines)
# charToRaw(lines)


# llh_gcp <- llh %>% 
#   select(Latitude, Longitude, Height) %>% 
#   summarise(x = mean(Latitude),
#             y = mean(Longitude),
#             z = mean(Height))

# dir <- "C:/Users/hmz25/Desktop/Emlid logs"
# emlid_list <- list.files(dir, pattern = ".LLH", full.names = FALSE)
# emlid_list_full_dir <- list.files(dir, pattern = ".LLH", full.names = TRUE)
# 
# for (i in seq_along(dir)) {
#   
#   # load in each file
#   llh <- read.table(emlid_list[i], 
#                     header = FALSE, sep = "", stringsAsFactors = FALSE)
#   colnames(llh) <- c("Date", "Time", "Latitude", "Longitude", "Height", 
#                      "Q", "ns", "sdn", "sde", "sdu", "sdne", "sdeu", "sdun", "Age", "Ratio")
#   
#   # create csv output for the x, y, z coordiante
#   llh_gcp <- llh %>% 
#     select(Latitude, Longitude, Height) %>% 
#     summarise(x = mean(Latitude),
#               y = mean(Longitude),
#               z = mean(Height))
#   
#   # merge csv files that have the same date into one csv
#   if 
#   # and add a column for number (needed for pix4d)
# }

# Set the directory
dir <- "C:/Users/hmz25/Desktop/Emlid logs"

# Get list of LLH files with full paths
emlid_list <- list.files(dir, pattern = "\\.LLH$", full.names = TRUE)

# Helper function to extract date (YYYYMMDD) and recording number
extract_datetime <- function(filepath) {
  filename <- basename(filepath)
  # Extract 14-digit timestamp string after last underscore
  timestamp <- str_extract(filename, "\\d{14}")
  date <- substr(timestamp, 1, 8)
  time <- substr(timestamp, 9, 14)
  list(date = date, time = time)
}

# Initialize a list to store emlid data for each date
data_by_date <- list()

# Loop through files by index
for (i in seq_along(emlid_list)) {
  
  filepath <- emlid_list[i]
  filename <- basename(filepath)
  
  # Extract date and number from filename
  date_time <- extract_datetime(filename)
  
  # Read the LLH file
  llh <- read.table(filepath, header = FALSE, sep = "", stringsAsFactors = FALSE)
  colnames(llh) <- c("Date", "Time", "Latitude", "Longitude", "Height", 
                     "Q", "ns", "sdn", "sde", "sdu", "sdne", "sdeu", "sdun", "Age", "Ratio")
  
  # create csv output for the x, y, z coordiante
  llh_gcp <- llh %>% 
    select(Latitude, Longitude, Height) %>% 
    summarise(x = mean(Latitude),
              y = mean(Longitude),
              z = mean(Height))
  output_file_name <- file.path(dir, paste0(filename, ".csv"))
  write_csv(llh_gcp, output_file_name)
}
  
#   # Store by date
#   if (!date_time$date %in% names(data_by_date)) {
#     data_by_date[[info$date]] <- llh
#   } else {
#     data_by_date[[date_time$date]] <- bind_rows(data_by_date[[info$date]], llh)
#   }
# }
# 
# # Write one CSV per date
# for (date in names(data_by_date)) {
#   output_file <- file.path(dir, paste0("combined_", date, ".csv"))
#   write_csv(data_by_date[[date]], output_file)
# }
  
# Set the base directory
dir <- "C:/Users/hmz25/Desktop/Emlid logs"

# Get list of LLH file names and full paths
emlid_list <- list.files(dir, pattern = "\\.LLH$", full.names = FALSE)
emlid_list_full_dir <- list.files(dir, pattern = "\\.LLH$", full.names = TRUE)

# Helper function to extract date and number from filename
extract_datetime <- function(filename) {
  # This assumes format like "KatzLab_solution_20240118204325.LLH"
  timestamp <- str_extract(filename, "\\d{14}")
  date <- substr(timestamp, 1, 8)
  number <- substr(timestamp, 9, 14)
  list(date = date, number = number)
}

# PART 1: Process each LLH file individually and save CSV summaries
for (i in seq_along(emlid_list)) {
  
  filename <- emlid_list[i]
  filepath <- emlid_list_full_dir[i]
  
  # Extract date and number from filename
  info <- extract_datetime(filename)
  
  # Read the LLH file
  llh <- read.table(filepath, header = FALSE, sep = "", stringsAsFactors = FALSE)
  colnames(llh) <- c("Date", "Time", "Latitude", "Longitude", "Height", 
                     "Q", "ns", "sdn", "sde", "sdu", "sdne", "sdeu", "sdun", "Age", "Ratio")
  
  # Create summarized output
  llh_gcp <- llh %>% 
    summarise(x = mean(Latitude),
              y = mean(Longitude),
              z = mean(Height)) %>%
    mutate(Number = info$number)
  
  # Save summary CSV for this LLH file
  output_file_name <- file.path(dir, paste0(filename, ".csv"))
  write_csv(llh_gcp, output_file_name)
}

# PART 2: Combine all summary CSVs by date
summary_csvs <- list.files(dir, pattern = "\\.LLH\\.csv$", full.names = TRUE)

# Initialize a list to store by date
data_by_date <- list()

for (i in seq_along(summary_csvs)) {
  filepath <- summary_csvs[i]
  filename <- basename(filepath)
  
  info <- extract_datetime(filename)
  date <- info$date
  
  df <- read_csv(filepath, show_col_types = FALSE)
  
  if (!date %in% names(data_by_date)) {
    data_by_date[[date]] <- df
  } else {
    data_by_date[[date]] <- bind_rows(data_by_date[[date]], df)
  }
}

# Write one merged CSV per date
for (date in names(data_by_date)) {
  output_path <- file.path(dir, paste0("combined_", date, ".csv"))
  write_csv(data_by_date[[date]], output_path)
}

