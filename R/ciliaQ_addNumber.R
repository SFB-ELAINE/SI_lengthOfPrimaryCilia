# Function for adding a number to an image                          ++++++++
# (Originally used in R package detectCilia)
# Author: Kai Budde
# Created: 2022/06/17
# Last changed: 2022/06/17


#' @title ciliaQ_addNumber
#' @description Adds a number to the image layer
#' @details Adds an integer to a x-y-3(rgb)-representation of an image.
#' @aliases addnumbertoimage addNumbertoimage addNumberToimage 
#' addnumberToImage addnumbertoImage
#' @author Kai Budde
#' @export addNumberToImage
#' @param image An three-dimensional array of numbers between 0 and 1
#' @param number A number (integer to be drawn/copied)
#' @param pos_x A number (x-value for starting pixel (upper left corner))
#' @param pos_y A number (y-value for starting pixel (upper left corner))
#' @param number_size_factor A number (factor for resizing the number)
#' @param image_layer A number (layer to be used)

ciliaQ_addNumber <- function(image = NULL,
                             number = NULL,
                             pos_x = NULL,
                             pos_y = NULL,
                             number_size_factor = NULL,
                             image_layer = 3){
  
  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }
  if(is.null(number)){
    print(paste("Please call the function with a number", sep=""))
    return()
  }
  
  if(is.null(pos_x)){
    pos_x <- 1
  }
  if(is.null(pos_y)){
    pos_y <- 1
  }
  
  # Read in correct integers -----------------------------------------------
  list_of_digits <- unlist(strsplit(x = toString(number), split = ""))
  number_of_digits <- length(list_of_digits)
  
  # Add the digits to the image --------------------------------------------
  image_with_numbers <- image
  
  # Go through all digit images and save the matrices in a list ------------
  for(i in 1:number_of_digits){
    digit <- list_of_digits[i]
    
    digit_path <- paste(digit, ".tiff", sep="")
    digit_path <- paste0("data/digits/", digit_path)
    
    digit_image <- tiff::readTIFF(source = digit_path, convert = TRUE,
                           info = FALSE)
    # digit_image <- EBImage::readImage(files = digit_path, type = "tiff")
    digit_image <- as.array(digit_image)
    
    # Only keep the black layer
    digit_image <- digit_image[,,4]
    if(i == 1){
      digit_images <- list()
      
      
      if(is.null(number_size_factor)){
        # Resizing factor for priting numbers if not given
        # The heigth should be 14px
        
        
        # height of the image (number of rowss)
        number_size_factor <- 14 / dim(digit_image)[2]
      }
      
      # if(is.null(number_size_factor)){
      #   min_repeating_number <-
      #     min(dim(image_with_numbers)[1]/dim(digit_image)[1],
      #         dim(image_with_numbers)[2]/dim(digit_image)[2])
      #   
      #   # It should be possible to have every number at least 30 times
      #   # in every direction
      #   number_size_factor <- min_repeating_number / 30
      # }
      
      digits_size_x <- 0
      digits_size_y <- 0
    }
    
    # Resize image
    digit_image <- resizeImage(digit_image, number_size_factor)
    digit_images[[i]] <- digit_image
    
    # Save dimensions of all digits
    digits_size_y <- max(digits_size_y, dim(digit_image)[1])
    digits_size_x <- digits_size_x + dim(digit_image)[2]
    
  }
  
  # # Choose layer of image where the number should be added to
  # if(tolower(number_color) == "red"){
  #   image_layer <- 1
  # }
  # if(tolower(number_color) == "green"){
  #   image_layer <- 2
  # }
  # if(tolower(number_color) == "blue"){
  #   image_layer <- 3
  # }
  
  # Adapt the starting position so all digits will be completely seen ---
  
  # Vertical side
  if( (pos_y + digits_size_y) >= dim(image_with_numbers)[1] ){
    pos_y <- dim(image_with_numbers)[1] - digits_size_y
  }
  
  # Horizontal side
  if( (pos_x + digits_size_x) >= dim(image_with_numbers)[2]){
    pos_x <- dim(image_with_numbers)[2] - digits_size_x
  }
  
  
  # Add all digit images to bigger one ---
  for(i in 1:number_of_digits){
    digit_image_size_x <- dim(digit_images[[i]])[2]
    digit_image_size_y <- dim(digit_images[[i]])[1]
    
    
    for(col in 1:digit_image_size_x){
      for(row in 1:digit_image_size_y){
        image_with_numbers[pos_y + row, pos_x + col, image_layer] <-
          min(digit_images[[i]][row, col] +
                image_with_numbers[pos_y + row, pos_x + col, image_layer], 1)
      }
    }
    
    # Adapt starting position for the next digit
    # pos_y <- pos_y + digit_image_size_y
    pos_x <- pos_x + digit_image_size_x
  }
  
  return(image_with_numbers)
}

# Another helper's function

#' @title resizeImage
#' @description Resizes an image
#' @details Resizes an image with a given scaling factor
#' @aliases resizeimage
#' @author Kai Budde
#' @export addNumberToImage
#' @param image An one to three-dimensional array of numbers between 0 and 1
#' @param number_size_factor A number (factor for resizing the number)

resizeImage <- function(image = NULL,
                        number_size_factor = 1){
  
  # Default values for missing arguments -----------------------------------
  if(is.null(image)){
    print(paste("Please call the function with an image.", sep=""))
    return()
  }
  if(is.null(number_size_factor)){
    print(paste("Please call the function with a number", sep=""))
    return()
  }
  
  if(number_size_factor < 0){
    print(paste("The factor may only be a number > 0", sep=""))
    return()
  }
  
  # Leave image as is if factor == 1 ---------------------------------------
  if(number_size_factor == 1){
    return(image)
  }
  
  # Shrink image if number_size_factor </> 1 ---------------------------------
  if(number_size_factor > 0){
    
    new_height <-  round(number_size_factor * dim(image)[1])
    new_width  <-  round(number_size_factor * dim(image)[2])
    
    new_img = apply(image, 2, function(y){return (spline(y, n = new_height)$y)})
    new_img = t(apply(new_img, 1, function(y){return (spline(y, n = new_width)$y)}))
    
    new_img[new_img < 0] = 0
    new_img = round(new_img)
    
    
    return(new_img)
  }
  
  print("Something went wrong.")
  return(0)
}

