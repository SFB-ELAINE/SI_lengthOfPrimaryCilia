# Function for copying all tifs as z stack images                   ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/04/15
# Last changed: 2024/04/15


copyAllTifsAsZstack <- function(input_dir){
  
  if(!("EBImage" %in% utils::installed.packages())){
    print("Installing EBImage.")
    BiocManager::install("EBImage")
  }
  
  require(EBImage)
  
  # Create output directory
  output_dir <- paste0(input_dir,"_zstacks")
  dir.create(output_dir, showWarnings = FALSE)
  
  # Filter results directories #############################################
  tif_dirs <- list.dirs(input_dir, recursive = FALSE, full.names = TRUE)
  
  # Go through every output folder and copy z-stack projection image #######
  
  for(i in 1:length(tif_dirs)){
    tif_files <- list.files(path = tif_dirs[i], pattern = "tif", full.names = TRUE)
    order_of_files <- as.numeric(gsub(pattern = ".+z([[:digit:]]{1,2})\\.tif", replacement = "\\1", x = tif_files))
    order_of_files <- match(sort(order_of_files), order_of_files)
    
    for(j in 1:length(tif_files)){
      if(j == 1){
        image <- EBImage::readImage(files = tif_files[order_of_files[j]])
      }else{
        dummy_image <- EBImage::readImage(files = tif_files[order_of_files[j]])
        image <- EBImage::combine(image, dummy_image)
      }
    }
    
    EBImage::writeImage(x = image, files = file.path(output_dir, paste0(basename(tif_dirs[i]), ".tif")))
    
  }
  rm(i)
  rm(j)
  
}
