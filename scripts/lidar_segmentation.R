library(lidR)
library(sf)
library(terra)
library(mosaic)
library(tictoc)
library(geometry)
#library(lidRplugins)

# preparing .las files---------------------------------------------
setwd("C:/Users/hmz25/Desktop/TX 2024 analysis")
folders <- list.dirs(path="./TX_final_250624/rs_data", recursive = F) #folders with each dataset
sites <- st_read("./Ecolab sites 2024.kml") #loading sites
sites <- st_buffer(sites,20) #20m buffer to avoid cutting trees on on the sites
files_for_crs <- list.files(path = "./TX_final_250624/rs_data/north_central", pattern = ".laz$", full.names = T, recursive = TRUE) #loading one of datasets for correct crs
ctg_for_crs <- catalog(files_for_crs)

for (j in seq_along(1:length(folders))){          #each folder/dataset is processed separately
  dataset <- folders[j]
  files <- list.files(path = dataset, pattern = ".laz$", full.names = T, recursive = TRUE)
  ctg <- catalog(files) #loading catalog
  geometries <-  as.data.frame(ctg$geometry) #reading geometries of each tile in the catalog
  list <- list() #empty list
  index <- c()   #empty vector
  for(i in seq_along(1:nrow(geometries))){ #checking each geometry if it appears for the first time in catalog (indexing 1), or not (indexing 2)
    geo <- geometries[i,]
    res <- geo %in% list
    if(res == FALSE){
      list <- c(list,geo)
      index <- c(index, 1)}else{index <- c(index, 2)}
  }
  df <- as.data.frame(cbind(files,index)) # adding index to listed files
  df <- df[df$index == 1,] # selecting only these with index 1
  files <- df$files # returning to first form of indexed files and re-reading it to catalog
  ctg <- catalog(files)
  sites <- st_transform(sites, st_crs(ctg))# crs transforming
  for (k in seq_along(1:nrow(sites))){# cutting to sites
    site1 <- sites[k,]
    name <- site1$Name
    site1 <- st_zm(site1, drop = TRUE, what = "ZM")# removing ZM dimension  
    las <- clip_roi(ctg, site1)
    if(length(las$X) == 0){next}# if .las is empty, site is not covered with dataset -> next
    las <- st_transform(las, st_crs(ctg_for_crs)) #crs transformation
    writeLAS(las, file = paste(dataset, name, ".las", sep = ""))
  }
}
### result should be 9 .las files ready for segmentation





# looking for a function for searching windows ----------------------------------
#measurements of 9 J.ashei from Tolleson et al.
heights <- c(0.89, 0.63, 0.80, 1.22, 1.83,  1.44, 3.31, 2.76, 2.95)
diameter <- c(0.90, 0.47, 0.48,  0.94,  2.24, 0.96,  4.50, 3.83, 3.71)
df <- data.frame(heights,diameter)
fit2 <- fitModel(diameter~A*heights+B+C*sqrt(heights), data=df)# functions to fit 
fit3 <- fitModel(diameter~heights^2*B+C, data=df)
fit4 <- fitModel(diameter~heights*A+B, data=df)
fit5 <- fitModel(diameter~heights^2*A+heights*B+C, data=df)
#its better to plot it in desmos calculator

fun2 <- function(x) {sqrt(x)*-3.519325+x*2.891521+1.409334}# Tolleson et al
fun1 <- function(x) {x^2 * 0.00901 + 2.51503}# Popescu and Wynne 2004
f <- function(x) {x*0.02 + 4}# Katz et al. 2020

#FIA data approach
setwd("C:/Users/sobie/Desktop/traineeship/sprawy badawcze/TX_lidar")
id_tree <- read.csv2("ID_TREE.csv")
head(id_tree)

length(id_tree$SPCD[id_tree$SPCD == 61])# checking  how many of these are J.ashei = 2342
#id_tree <- id_tree[id_tree$SPCD == 61,]

id_tree$CROWN_DIA_WIDE <- as.numeric(id_tree$CROWN_DIA_WIDE)/3.28084#feets to meters
id_tree$CROWN_DIA_90 <- as.numeric(id_tree$CROWN_DIA_90)/3.28084
id_tree$TOTAL_LENGTH <- id_tree$TOTAL_LENGTH/3.28084
id_tree$crown_mean <- (id_tree$CROWN_DIA_WIDE+id_tree$CROWN_DIA_90)/2 #longest diameter and perpendicular diameter mean (not used)

f4_model <- fitModel(CROWN_DIA_WIDE~B*TOTAL_LENGTH+C+A*sqrt(TOTAL_LENGTH), data=id_tree)
f4_model
f4 <- function(x) {y <- (sqrt(x)*4.0896154+x*-0.1877362-4.6446776)*2 #the original crown height/width relationship was multiplied by 2
y[x < 3.2] <- 4 # min. searching window = 4 (x read from the function chart)
return(y)}

plot(id_tree$TOTAL_LENGTH, id_tree$CROWN_DIA_WIDE)

# CHM, segmentation --------------------------------------------------
setwd("C:/Users/sobie/Desktop/traineeship/sprawy badawcze/TX_lidar")
las_files <- list.files(path="./rs_data", pattern = ".las$", full.names = T) #reading .las fies names

#f4 <- function(x) {y <- (sqrt(x)*4.0896154+x*-0.1877362-4.6446776)*2 
#y[x < 3.2] <- 4 # min. searching window = 4 (x read from the function chart)
#return(y)}

f5 <- function(x) {y <- (sqrt(x)*4.0896154+x*-0.1877362-4.6446776)*1.5             #alternative function with smaller searching window 
y[x < 3.9] <- 4 # min. searching window = 4 (x read from the function chart)
return(y)}

######## loop for each .las file
for(i in seq_along(1:length(las_files))){
  las <- readLAS(las_files[[i]]) 
  las <- normalize_height(las, tin()) 
  las <- filter_poi(las, Classification == 5 | Classification == 4) #high and medium vegetation filtering
  ttops <- locate_trees(las, lmf(f5, hmin = 1, shape="circular", ws_args = "Z"))# ttops detection (change function)
  name <- gsub(".*./rs_data/|.las.*", "", las_files[[i]]) # reading .las file name
  
  chm <- rasterize_canopy(las, res = 0.5, algorithm = p2r()) #creating canopy height model
  kernel <- matrix(1,3,3)
  chm_smoothed <- terra::focal(chm, w = kernel, fun = median, na.rm = TRUE)# smoothing
  
  dalp <- segment_trees(las, dalponte2016(chm_smoothed, ttops, max_cr=30, th_tree = 1, th_seed = 0.35, th_cr = 0.5)) # segment point cloud
  #li <- segment_trees(las, li2012(hmin = 2, speed_up = 15)) #second method
  #watershed  <- segment_trees(las, watershed(chm_smoothed)) #third method
  
  df <- delineate_crowns(dalp, type="concave", concavity=3 ) #creating 2D polygons
  #df_concave <- delineate_crowns(dalp, type="concave", concavity = 2) #2nd method for polygon 
  
  #surface and volume calculation (may be useful later)
  data <- dalp@data
  data <-data[,c(1:3,20)]
  names(data) <- c("x", "y", "z", "treeID")
  data2 <- split(data , f = data$treeID )
  data2 <- lapply(data2, function(x) x[,c(-4)])
  data2 <- lapply(data2, function(x) as.matrix(x))
  
  wynik <- c(0,0,0) 
  for (i in seq_along(data2)){
    tryCatch({
      tree <- data2[[i]]
      rows <- nrow(tree)
      params <- convhulln(tree, options="FA")
      volume <- params[[3]]
      surface <- params[[2]]
      res <- cbind(i, volume, surface)
      wynik <- rbind (wynik, res)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  wynik <- wynik[-1,]
  
  #assigning VOLUME and SURFACE to polygons and exporting as .gpkg
  wynik <- as.data.frame(wynik)
  colnames(wynik) <- c("treeID", "volume", "surface")
  merged <- merge(df, wynik, by="treeID")
  
  #saving results
  st_write(st_as_sf(merged), dsn = paste("./qgis/F7_results_concave/", name, "_merged.gpkg"))
  writeRaster(chm_smoothed, filename = paste("./qgis/F7_results_concave/", name, "_chm_smoothed.gpkg"), overwrite=F)
  st_write(ttops, dsn = paste("./qgis/F7_results/", name, "_ttops_concave.gpkg"))
}

plot(dalp, bg = "white", size = 4, color = "treeID")



# naip_download -----------------------------------------------------------

#one function(year from 2022,2020,2018,2016)
tx_download <- function(sites,year,dsn){
  dest <- paste(dsn,"/tile_index.zip", sep="")
  
  if(year == 2022){
    utils::download.file("https://data.tnris.org/49450905-d993-438f-8d8b-50129030744d/assets/texas-naip-imagery-tile-index.zip", destfile = dest)
    utils::unzip(dest, exdir = paste(dirname(dest),"/tile_index", sep=""))
    tiles <- st_read(paste(dirname(dest),"/tile_index", "/naip-2022-nc-cir-60cm_index.shp", sep=""))
    file.remove(dest)
  }else if(year == 2020){
    utils::download.file("https://data.tnris.org/aa5183ca-a1bd-4b5f-9b63-4ba48d01b83d/assets/texas-naip-imagery-tile-index.zip", destfile = dest)
    utils::unzip(dest, exdir = paste(dirname(dest),"/tile_index", sep=""))
    tiles <- st_read(paste(dirname(dest),"/tile_index", "/naip20_nc_cir_60cm_index.shp", sep="")) 
    file.remove(dest)
  }else if(year == 2018){
    utils::download.file("https://data.tnris.org/f1d66250-4021-47df-9fe9-9fca286b0f50/assets/texas-naip-imagery-tile-index.zip", destfile = dest)
    utils::unzip(dest, exdir = paste(dirname(dest),"/tile_index", sep=""))
    tiles <- st_read(paste(dirname(dest),"/tile_index", "/naip-2018-2019-nc-cir-60cm-index.shp", sep="")) 
    file.remove(dest)
  }else if(year == 2016){
    utils::download.file("https://data.tnris.org/a40c2ff9-ccac-4c76-99a1-2382c09cf716/assets/texas-naip-imagery-tile-index.zip", destfile = dest)
    utils::unzip(dest, exdir = paste(dirname(dest),"/tile_index", sep=""))
    tiles <- st_read(paste(dirname(dest),"/tile_index", "/naip_2016_nc_cir_1m_tileindex.shp", sep="")) 
    file.remove(dest)
  }else (stop("Choose year from : 2022, 2020, 2018, 2016"))
  
  sites <- st_buffer(sites,20)# 20m buffer to avoid cutting trees on on the sites
  sites <- st_transform(sites, st_crs(tiles))
  #choosing intersecting ortho tiles
  test <- st_intersects(tiles, sites, sparse = F)
  idx <- which(apply(test, 1, any))
  tiles <- tiles[idx,]
  names <- tiles[[1]]
  
  for(i in seq_along(1:length(names))){
    name <- names[i]
    if(year == 2022){url <- paste("https://data.tnris.org/49450905-d993-438f-8d8b-50129030744d/resources/naip22-60cm_",
                                  substr(name, start = 20, stop = 26), "_nccir-doqq.zip", sep="")
    }else if(year == 2020){url <- paste("https://data.tnris.org/aa5183ca-a1bd-4b5f-9b63-4ba48d01b83d/resources/naip20-60cm_",
                                        substr(name, start = 20, stop = 26), "_nccir-doqq.zip", sep="")
    }else if(year == 2018){url <- paste("https://data.tnris.org/f1d66250-4021-47df-9fe9-9fca286b0f50/resources/naip18-60cm_",
                                        substr(name, start = 20, stop = 26), "_nccir-doqq.zip", sep="")
    }else if(year == 2016){url <- paste("https://data.tnris.org/a40c2ff9-ccac-4c76-99a1-2382c09cf716/resources/naip16-1m_",
                                        substr(name, start = 18, stop = 24), "_nccir-doqq.zip", sep="")}
    dest <- paste(dsn, substr(name, start = 1, stop = 35), ".zip", sep="") 
    utils::download.file(url, destfile = dest)
    utils::unzip(dest, exdir = paste(dsn,"/",year,sep=""))
    file.remove(dest)
  }
  unlink(paste(dsn, "/tile_index", sep=""), recursive = TRUE)
}


setwd("C:/Users/sobie/Desktop/traineeship/sprawy badawcze/TX_lidar")
sites <- st_read("./qgis/TX_sites.gpkg")# loading sites

tx_download(sites, 2016, "./rs_data/naip_ortho")
