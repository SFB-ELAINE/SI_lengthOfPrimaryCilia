# Function for creating a 3D TIF with a known cilia                 ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/02/02
# Last changed: 2024/02/02

createTestCiliumImage <- function(output_dir,
                                  number_of_pixels_x_y = 100,
                                  number_of_layers_z = 20,
                                  cilium_length_in_pixels = 10,
                                  cilium_width_in_pixels = 3,
                                  cilium_height_in_pixels = 3,
                                  relative_start_position = c(0.5,0.5,0.5),
                                  rotatation_angles_degree = c(0,0,0),
                                  gblur_sigma = 0){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }

  # Load packages
  library(groundhog)
  pkgs <- c("BiocManager", "devtools", "dplyr", "reticulate")
  groundhog.library(pkgs, groundhog.day)
  
  if(!("EBImage" %in% utils::installed.packages())){
    print("Installing EBImage.")
    BiocManager::install("EBImage")
  }
  
  require(EBImage)
  
  # Initialize straight cilium
  cilium_coordinates <- expand.grid(x=0:(cilium_length_in_pixels-1),
                                    y=0:(cilium_width_in_pixels-1),
                                    z=0:(cilium_height_in_pixels-1))
  
  # Rotate cilium
  rotatation_angles <- rotatation_angles_degree * 2 * pi / 360
  a <- rotatation_angles[1]
  b <- rotatation_angles[2]
  c <- rotatation_angles[3]
  rotation_matrix <- round(x = matrix(data = c(
    cos(a)*cos(b),
    cos(a)*sin(b)*sin(c)-sin(a)*cos(c),
    cos(a)*sin(b)*cos(c)+sin(a)*sin(c),
    sin(a)*cos(b),
    sin(a)*sin(b)*sin(c)+cos(a)*cos(c),
    sin(a)*sin(b)*cos(c)-cos(a)*sin(c),
    -sin(b),
    cos(b)*sin(c),
    cos(b)*cos(c)),
    nrow = 3, ncol = 3, byrow = TRUE),
    digits = 10)
  
  
  
  new_cilium_cordinates <- matrix(data = NA,
                                  nrow = nrow(cilium_coordinates),
                                  ncol = ncol(cilium_coordinates))
  
  for(i in 1:nrow(cilium_coordinates)){
    new_cilium_cordinates[i, ] <- t(rotation_matrix %*% t(cilium_coordinates[i,]))
  }
  
  new_cilium_cordinates <- round(new_cilium_cordinates)
  
  

  
  
  # pixel_indices <- which(image_array == 1, arr.ind = TRUE)
  
  
  
  
  # # Initialize 3D array
  # image_array <- array(data = 0, dim = c(number_of_pixels_x_y,
  #                                        number_of_pixels_x_y,
  #                                        number_of_layers_z))
  
  # # Include cilium
  x_pixel_start <- max(0, floor(relative_start_position[1] * number_of_pixels_x_y - 0.5*(max(new_cilium_cordinates[,1])+min(new_cilium_cordinates[,1])) ))
  y_pixel_start <- max(0, floor(relative_start_position[2] * number_of_pixels_x_y - 0.5*(max(new_cilium_cordinates[,2])+min(new_cilium_cordinates[,2])) ))
  z_layer_start <- max(1, floor(relative_start_position[3] * number_of_layers_z   - 0.5*(max(new_cilium_cordinates[,3])+min(new_cilium_cordinates[,3])) ))
  # 
  # 
  # image_array[(x_pixel_start):(x_pixel_start+cilium_length_in_pixels-1),
  #             (y_pixel_start):(y_pixel_start+cilium_width_in_pixels-1),
  #             (z_layer_start):(z_layer_start+cilium_height_in_pixels-1)] <- 1
  # 
  
  
  # Add cilium coordinates to image
  
  image_array_rotated <- array(data = 0, dim = c(number_of_pixels_x_y,
                                                 number_of_pixels_x_y,
                                                 number_of_layers_z))
  
  image_array_rotated[cbind(x_pixel_start + new_cilium_cordinates[,1],
                            y_pixel_start + new_cilium_cordinates[,2],
                            z_layer_start + new_cilium_cordinates[,3] )] <- 1
  
  
  
  # image_array_rotated_xz <- array(data = 0, dim = c(number_of_pixels_x_y,
  #                                                   number_of_layers_z,
  #                                                   number_of_pixels_x_y))
  # 
  # image_array_rotated_xz[cbind(x_pixel_start + new_cilium_cordinates[,1],
  #                           z_layer_start + new_cilium_cordinates[,3],
  #                           y_pixel_start + new_cilium_cordinates[,2])] <- 1
  
  
  
  # Fill holes in x-y-plane
  image_array_rotated <- EBImage::fillHull(x = image_array_rotated)
  
  # Fill holes in x-z-plane
  image_array_rotated_xz <- aperm(image_array_rotated, c(1,3,2))
  image_array_rotated_xz <- EBImage::fillHull(x = image_array_rotated_xz)
  image_array_rotated_xz <- aperm(image_array_rotated_xz, c(1,3,2))
  
  # Fill holes in y-z-plane
  image_array_rotated_yz <- aperm(image_array_rotated, c(2,3,1))
  image_array_rotated_yz <- EBImage::fillHull(x = image_array_rotated_yz)
  image_array_rotated_yz <- aperm(image_array_rotated_yz, c(3,1,2))
  
  # Combine all three images
  image_array_rotated <- 0+(image_array_rotated | image_array_rotated_xz |
    image_array_rotated_yz)
  
  # Add Gaussian blur
  # image_array_rotated_blur <- image_array_rotated/5
  
  # EBImage::image(x = image_array_rotated_blur)
  if(gblur_sigma > 0){
    image_array_rotated <- EBImage::gblur(image_array_rotated, sigma = gblur_sigma)
  }
  
  # display(image_array_rotated_blur)
  
  # pixel_indices <- which(image_array == 1, arr.ind = TRUE)
  # 
  # for(i in 1:nrow(pixel_indices)){
  #   image_array_rotated[t(rotation_matrix %*% pixel_indices[i,])] <- 1
  # }
  # rm(i)
  # 
  
  dir_name <- paste0("ArtificialCilium_",
                     "l_", cilium_length_in_pixels,
                     "w_", cilium_width_in_pixels,
                     "h_", cilium_height_in_pixels,
                     "_rotated_",
                     paste0(rotatation_angles_degree, collapse="_"),
                     "_gblur_",
                     gblur_sigma)
  dir.create(file.path(output_dir, dir_name), showWarnings = FALSE)
  
  # image_name <- paste0("ArtificialCilium_",
  #                      "l_", cilium_length_in_pixels,
  #                      "w_", cilium_width_in_pixels,
  #                      "h_", cilium_height_in_pixels,
  #                      ".tif")
  # EBImage::writeImage(x = image_array, files = file.path(output_dir, image_name))
  
  for(i in 1:number_of_layers_z){
    image_name <- paste0(dir_name,
                         "_z",
                         i,
                         ".tif")
    EBImage::writeImage(x = image_array_rotated[,,i],
                        bits.per.sample = 16,
                        files = file.path(output_dir, dir_name, image_name))
    
  }
  
  
  # EBImage::writeImage(x = image_array_rotated, files = file.path(output_dir, image_name))
  
  # image_name <- paste0("ArtificialCilium_",
  #                      "l_", cilium_length_in_pixels,
  #                      "w_", cilium_width_in_pixels,
  #                      "h_", cilium_height_in_pixels,
  #                      "_rotated_",
  #                      paste0(rotatation_angles_degree, collapse="_"),
  #                      "_combined.tif")
  # EBImage::writeImage(x = image_array_rotated2, files = file.path(output_dir, image_name))
  
}
