# Function for creating a 3D TIF with a known cilia                 ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/02/02
# Last changed: 2024/04/15

createTestCiliumImage <- function(output_dir,
                                  cilium_shape = "horizontal",
                                  number_of_pixels_x_y = 100,
                                  number_of_layers_z = 20,
                                  cilium_length_in_pixels = 10,
                                  cilium_width_in_pixels = 3,
                                  cilium_height_in_pixels = 3,
                                  relative_start_position = c(0.5,0.5,0.5),
                                  rotatation_angles_degree = c(0,0,0),
                                  gblur_sigma = 0,
                                  cilium_color = "green"){
  
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
  if(cilium_shape == "horizontal"){
    cilium_coordinates <- expand.grid(x=0:(cilium_length_in_pixels-1),
                                      y=0:(cilium_width_in_pixels-1),
                                      z=0:(cilium_height_in_pixels-1))
  }else if(cilium_shape == "c"){
    cilium_coordinates <- data.frame(x=rep(c(0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8,8,9,9,9), 3),
                                     y=rep(c(3,4,5,2,3,4,1,2,3,1,2,3,0,1,2,0,1,2,1,2,3,1,2,3,2,3,4,3,4,5), 3),
                                     z=c(rep(0, 30), rep(1, 30), rep(2, 30)))
  }else{
    print("Please call the function with a correct cilium shape.")
    return()
  }
  
  
  # Rotate cilium
  rotatation_angles <- rotatation_angles_degree * 2 * pi / 360
  a <- rotatation_angles[1] # rotation around z-axis
  b <- rotatation_angles[2] # rotation around y-axis
  c <- rotatation_angles[3] # rotation around x-axis
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
  
  # display(image_array_rotated)
  
  # Color the image
  cilium_color <- tolower(cilium_color)
  gray_cilium <- FALSE
  
  if(cilium_color == "grey" || cilium_color == "gray"){
    gray_cilium <- TRUE
    print("The image stays as a grayscale image.")
  }else if(cilium_color == "red" || cilium_color == "r"){
    image_array_rotated <- EBImage::rgbImage(red = image_array_rotated)
  }else if(cilium_color == "green" || cilium_color == "g"){
    image_array_rotated <- EBImage::rgbImage(green = image_array_rotated)
  }else if(cilium_color == "blue" || cilium_color == "b"){
    image_array_rotated <- EBImage::rgbImage(blue = image_array_rotated)
  }else{
    print("Please call the function with a correct color name.")
  }
  
  
  # pixel_indices <- which(image_array == 1, arr.ind = TRUE)
  # 
  # for(i in 1:nrow(pixel_indices)){
  #   image_array_rotated[t(rotation_matrix %*% pixel_indices[i,])] <- 1
  # }
  # rm(i)
  # 
  
  dir_name <- paste0("SimulatedCilium_",
                     "l_", cilium_length_in_pixels,
                     "w_", cilium_width_in_pixels,
                     "h_", cilium_height_in_pixels,
                     "_rotated_",
                     paste0(rotatation_angles_degree, collapse="_"),
                     "_gblur_",
                     gblur_sigma)
  dir.create(file.path(output_dir, dir_name), showWarnings = FALSE, recursive = TRUE)
  
  # image_name <- paste0("SimulatedCilium_",
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
    if(gray_cilium){
      EBImage::writeImage(x = image_array_rotated[,,i],
                          bits.per.sample = 16,
                          files = file.path(output_dir, dir_name, image_name))
    }else{
      EBImage::writeImage(x = image_array_rotated[,,,i],
                          bits.per.sample = 16,
                          files = file.path(output_dir, dir_name, image_name))
    }
    
    
  }
  
  
  # EBImage::writeImage(x = image_array_rotated, files = file.path(output_dir, image_name))
  
  # image_name <- paste0("SimulatedCilium_",
  #                      "l_", cilium_length_in_pixels,
  #                      "w_", cilium_width_in_pixels,
  #                      "h_", cilium_height_in_pixels,
  #                      "_rotated_",
  #                      paste0(rotatation_angles_degree, collapse="_"),
  #                      "_combined.tif")
  # EBImage::writeImage(x = image_array_rotated2, files = file.path(output_dir, image_name))
  
  return(image_array_rotated)
}
