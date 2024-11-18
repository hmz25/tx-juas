
library(RStoolbox)
library(readobj)
#install.packages('svgViewR', dependencies=TRUE)
library(svgViewR)
library(rgl)
library(r3js)
library(raster)
library(terra)


# upload data and explore data structure ----------------------------------


# obj <- read.obj("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/3d_mesh/20240103_Wade_tree3_orbital_simplified_3d_mesh.obj",
#                 convert.rgl = TRUE, triangulate = FALSE)

#str(obj)

obj1 <- readOBJ("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/3d_mesh/20240103_Wade_tree3_orbital_simplified_3d_mesh.obj")
shade3d(obj1, color = "green")
 
str(obj1) #$vb component refers to the vertex buffer of the 3D object, which is a matrix where each column represents the coordinates of a vertex in 3D space
#The matrix has four rows
#The first row contains x-coordinates of vertices.
#The second row contains y-coordinates of vertices.
#The third row contains z-coordinates of vertices.
#The fourth row is typically filled with 1s as a homogeneous coordinate, used in 3D graphics for transformations (like scaling and rotation).

#$texcoords refers to the texture coordinates associated with the mode
#These coordinates are used to map 2D textures (like images) onto the 3D surface of the model, aligning specific parts of the texture image with corresponding areas of the mesh.
#Texture coordinates define how a 2D image wraps around the 3D model, determining which parts of the image correspond to each triangle or face on the model's surface.
#Structure: $texcoords is typically a matrix with two rows, where:
#The first row contains U-coordinates (horizontal axis on the texture).
#The second row contains V-coordinates (vertical axis on the texture).
#Each column represents a texture coordinate for a specific vertex, usually ranging from 0 to 1, where (0,0) is the bottom-left corner of the texture and (1,1) is the top-right.
#These coordinates allow the model to "unwrap" the 3D surface onto a 2D plane, making it possible to apply colors, patterns, or detailed imagery that follow the contours of the object.

#index triangles of the model, provides the connectivity information for the mesh by defining which vertices form each triangle
#a matrix with three rows, where each column corresponds to a triangle. The values in each column are indices pointing to specific vertices in the $vb (vertex buffer) matrix
#For example, if a column in $it has the values (1, 5, 7), it means that the triangle is formed by connecting the vertices found in the 1st, 5th, and 7th columns of the $vb matrix



# calculating total surface area of mesh by summing SA of triangles in obj ----------------------


# vertices <- obj1$vb  #vertex buffer matrix (4 rows: x, y, z, 1 for homogeneous coordinates)
# triangles <- obj1$it  #index triangles (3 rows, each column represents a triangle)
# 
# #calculate areas of each triangle
# triangle_areas <- apply(triangles, 2, function(tri_indices) {
#   # get coordinates of the vertices for each triangle
#   v1 <- vertices[1:3, tri_indices[1]]  # first vertex (x, y, z)
#   v2 <- vertices[1:3, tri_indices[2]]  # second vertex (x, y, z)
#   v3 <- vertices[1:3, tri_indices[3]]  # third vertex (x, y, z)
#   
#   # calculate edge vectors
#   edge1 <- v2 - v1
#   edge2 <- v3 - v1
#   
#   # cross product of edge1 and edge2
#   cross_prod <- c(
#     edge1[2] * edge2[3] - edge1[3] * edge2[2],
#     edge1[3] * edge2[1] - edge1[1] * edge2[3],
#     edge1[1] * edge2[2] - edge1[2] * edge2[1]
#   )
#   
#   # area of the triangle
#   area <- 0.5 * sqrt(sum(cross_prod^2))
#   return(area)
# })
# 
# triangle_areas
# 
# #initialize a vector to store the area of each triangle
# n_triangles <- ncol(triangles)
# triangle_areas <- numeric(n_triangles)
# 
# # Loop through each triangle
# for (i in 1:n_triangles) {
#   # Get indices of the vertices for the triangle
#   v1_index <- triangles[1, i]
#   v2_index <- triangles[2, i]
#   v3_index <- triangles[3, i]
#   
#   # Get coordinates of the vertices
#   v1 <- vertices[1:3, v1_index]  # First vertex (x, y, z)
#   v2 <- vertices[1:3, v2_index]  # Second vertex (x, y, z)
#   v3 <- vertices[1:3, v3_index]  # Third vertex (x, y, z)
#   
#   # Calculate edge vectors
#   edge1 <- v2 - v1
#   edge2 <- v3 - v1
#   
#   # Cross product of edge1 and edge2
#   cross_prod <- c(
#     edge1[2] * edge2[3] - edge1[3] * edge2[2],
#     edge1[3] * edge2[1] - edge1[1] * edge2[3],
#     edge1[1] * edge2[2] - edge1[2] * edge2[1]
#   )
#   
#   # Area of the triangle
#   triangle_areas[i] <- 0.5 * sqrt(sum(cross_prod^2))
# }
# 
# #areas of each triangle
# triangle_areas
# triangle_sa <- sum(triangle_areas) #believe this is in m, need to check



# exploring texturized mesh jpg -------------------------------------------

#load in jpg of 3d mesh to see # of pixels 
wade_t3_texture <- stack("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/3d_mesh/20240103_Wade_tree3_orbital_texture.jpg")
plotRGB(wade_t3_texture)
dim(wade_t3_texture) #8192 pixels
px_per_triangle <- triangle_sa/(dim(wade_t3_texture))

##avg ground sampling distance = 1.49 cm (from pix4d quality report)
pix_per_cm <- (1/1.49)

# #trying to calculate voxels from las data
# wade_t3_las <- readLAS("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/point_cloud/20240103_Wade_tree3_orbital_group1_densified_point_cloud.las",
#                        select = "RGB")
# #plot(wade_t3_las)
# wade_t3_voxel <- voxelize_points(wade_t3_las, 1)

##look at color differences between original pic and 3d mesh pic 

##create df of all triangles 
### in $it, says which columns from the $vb correspond to the vertices of each triangle
### in $texcoords, says where on the texturized mesh each triangle is
### need to create an x, y, z, column for each triangle #, the associated tex coord, and pull the RGB value from the jpg using the associated txcoord

#create rgb df from texture mesh
wade_r <- wade_t3_texture[[1]]
#str(wade_r)
wade_g <- wade_t3_texture[[2]]
wade_b <- wade_t3_texture[[3]]

wade_mesh_rgb <- as.data.frame(stack(wade_r, wade_g, wade_b))

px_per_triangle2 <- nrow(wade_mesh_rgb)/n_triangles

#how many triangles per 25cm2? 

#how to apply pixel classifier? 



# extract rgb value from each triangle + calculate spectral index ---------


#extract the vertex coordinates and texture coordinates
vb <- obj1$vb[1:3, ]  #extract x, y, z coordinates from vb (omit homogeneous coordinate)
it <- obj1$it         #triangle indices (each column is a triangle)
texcoords <- obj1$texcoords  #x and y coordinates on texture mesh jpeg

#create a data frame of triangles with associated texture coordinates
n_triangles <- ncol(it)
triangle_df <- data.frame(matrix(ncol = 15, nrow = n_triangles))

colnames(triangle_df) <- c("x1", "y1", "z1", 
                           "x2", "y2", "z2", 
                           "x3", "y3", "z3",
                           "u1", "v1", 
                           "u2", "v2", 
                           "u3", "v3")

for (i in 1:n_triangles) { #this loop takes a very long time... is there an easier way to do this?
  #vertex indices for each triangle
  v1_index <- it[1, i] #create index for vertex 1 by pulling from first row in $it (gives x coord for i triangle)
  v2_index <- it[2, i] #create index for vertex 2 by pulling from second row in $it (gives y coord for i triangle)
  v3_index <- it[3, i] #create index for vertex 3 by pulling from third row in $it (z coord for i triangle)
  
  #vertex coordinates
  triangle_df[i, c("x1", "y1", "z1")] <- vb[, v1_index] #gives you x, y, and z values for the first vertex of i triangle
  triangle_df[i, c("x2", "y2", "z2")] <- vb[, v2_index] #gives you x, y, and z values for the second vertex of i triangle
  triangle_df[i, c("x3", "y3", "z3")] <- vb[, v3_index] #gives you x, y, and z values for the third vertex of i triangle
  
  #texture coordinates
  triangle_df[i, c("u1", "v1")] <- texcoords[, v1_index] #gives you u and v coordinates for first vertex of i triangle
  triangle_df[i, c("u2", "v2")] <- texcoords[, v2_index] #gives you u and v coordinates for second vertex of i triangle
  triangle_df[i, c("u3", "v3")] <- texcoords[, v3_index] #gives you u and v coordinates for third vertex of i triangle 
  
  print(i)
}

#map texture coordinates to image pixels
#texture coordinates are normalized [0, 1] so need to be scaled to pixel indices
img_width <- ncol(wade_t3_texture)
img_height <- nrow(wade_t3_texture)

triangle_df <- within(triangle_df, { 
  #multiply u by the image width (in pixels) to get the x coord (horizontal) for each vertex in each triangle
  #multiply v by the image height (in pixels) to get the y coord (vertical) for each vertex in each triangle
  #adding 1 ensures the coordinates map correctly to the raster indexing which starts at 1 instead of 0
  u1 <- round(u1 * (img_width - 1)) + 1 
  v1 <- round(v1 * (img_height - 1)) + 1 
  u2 <- round(u2 * (img_width - 1)) + 1
  v2 <- round(v2 * (img_height - 1)) + 1
  u3 <- round(u3 * (img_width - 1)) + 1
  v3 <- round(v3 * (img_height - 1)) + 1
})

#extract RGB values for each texture coordinate
get_rgb <- function(u, v, wade_t3_texture) {
  r <- raster::extract(wade_t3_texture[[1]], cbind(u, v))
  g <- raster::extract(wade_t3_texture[[2]], cbind(u, v))
  b <- raster::extract(wade_t3_texture[[3]], cbind(u, v))
  return(c(R = r, G = g, B = b))
}

triangle_df$r1 <- NA
triangle_df$g1 <- NA
triangle_df$b1 <- NA
triangle_df$r2 <- NA
triangle_df$g2 <- NA
triangle_df$b2 <- NA
triangle_df$r3 <- NA
triangle_df$g3 <- NA
triangle_df$b3 <- NA

# Loop through each row and assign the RGB values from the texture for each vertex
for (i in 1:3) {  # Use 1:nrow(triangle_df) for the entire dataset
  
  # Extract RGB values for each vertex from the texture image
  rgb1 <- get_rgb(triangle_df$u1[i], triangle_df$v1[i], wade_t3_texture)
  rgb2 <- get_rgb(triangle_df$u2[i], triangle_df$v2[i], wade_t3_texture)
  rgb3 <- get_rgb(triangle_df$u3[i], triangle_df$v3[i], wade_t3_texture)
  
  # Assign r, g, b values separately for each vertex
  triangle_df$r1[i] <- rgb1[1]
  triangle_df$g1[i] <- rgb1[2]
  triangle_df$b1[i] <- rgb1[3]
  
  triangle_df$r2[i] <- rgb2[1]
  triangle_df$g2[i] <- rgb2[2]
  triangle_df$b2[i] <- rgb2[3]
  
  triangle_df$r3[i] <- rgb3[1]
  triangle_df$g3[i] <- rgb3[2]
  triangle_df$b3[i] <- rgb3[3]
  
  print(i)
}


#df with RGB values for each triangle
triangle_df

#visualizing triangle rgb values to check if loop is assigning correct values to locations 
library(dplyr)
library(tidyverse)
triangle_df_sub <- triangle_df %>% 
  select("u1":"b3") %>% 
  mutate(triangle_n = row_number()) %>% 
  select(triangle_n, everything()) %>% 
  filter(triangle_n %in% 1:3) %>% 
  pivot_longer(cols = c("u1", "u2", "u3", "v1", "v2", "v3"), 
               names_to = "coord", 
               values_to = "coord_val") %>%   
  mutate(axis = substr(coord, 1, 1), number = substr(coord, 2, 2)) %>%
  group_by(triangle_n, number, axis) %>% 
  summarise(coord_val) %>% 
  pivot_wider(names_from = axis, values_from = coord_val) %>% 
  mutate(triangle_n = as.character(triangle_n))

ggplot(data = as.data.frame(triangle_df_sub),
       mapping = aes(x = u, y = v, col = triangle_n)) +
  geom_point()

#visualizing triangle rgb values to check that it's working
library(ggplot2)
library(jpeg)

plotRGB(wade_t3_texture)

points(x = 999, y = 415, pch = 16, cex = 1, col = "red")
points(x = 1009, y = 418, pch = 16, cex = 1, col = "blue")

#overlay polygons on plotRGB (no geographic coordinates)
#make shape file out u1:u3 add u1 
#look at RGB 


##sum spectral index

##conversion for # triangles per 25cm vs total # triangles 
