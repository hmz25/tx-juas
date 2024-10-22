
library("RStoolbox")
library("readobj")

obj1 <- read.obj("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/3d_mesh/20240103_Wade_tree3_orbital_simplified_3d_mesh.obj",
                convert.rgl = FALSE, triangulate = TRUE)

obj2 <- read.obj("C:/Users/hmz25/Documents/pix4d/20240103_Wade_tree3_orbital/2_densification/3d_mesh/20240103_Wade_tree3_orbital_simplified_3d_mesh.obj",
                 convert.rgl = TRUE, triangulate = TRUE)
