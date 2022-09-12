# Script for saving tif files with z projection                     ++++++++
# Author: Kai Budde
# Created: 2022/06/17
# Last changed: 2022/06/17


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
library(groundhog)
pkgs <- c("tiff", "tidyverse")
groundhog.library(pkgs, groundhog.day)

source("R/ciliaq_addNumber.R")

# Please adapt the following parameters ####################################

input_dir <- "CiliaQ/"
input_file_ciliaq <- "CiliaQ/output/ciliaq_data.csv"
input_file_metadata <- "data/automaticDetection/cultivation/summary_metadata.csv"

# Output directory
output_dir <- "CiliaQ/zprojections/"

# Z projection of image ####################################################

tif_files <- list.files(path = input_dir, pattern = "CQP_CQ_RP\\.tif", full.names = TRUE)
tif_files_base <- basename(tif_files)
tif_files_base <- gsub(pattern = "_CQP_CQ_RP\\.tif", replacement = "_zprojection", x = tif_files_base)

df_results_ciliaq <- readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
df_metadata <- readr::read_csv(file = input_file_metadata, name_repair = "universal")

for(i in 1:length(tif_files)){
  current_image <- tif_files[i]
  
  image_loaded <- tiff::readTIFF(source = current_image, info = TRUE,
                                 all = TRUE,
                                 convert = TRUE, as.is = FALSE)
  
  image_description  <- attributes(image_loaded[[1]])$description
  number_of_channels <- as.numeric(gsub(pattern = ".+\nchannels=([0-9]{1,2})\nslices.+", replacement = "\\1", x = image_description))
  number_of_images   <- as.numeric(gsub(pattern = ".+\nslices=([0-9]{1,2})\nhyperstack.+", replacement = "\\1", x = image_description))
  
  image_dimension  <- attributes(image_loaded[[1]])$dim
  image_dimension <- c(image_dimension, number_of_channels)
  
  # Move all layers and channels into one big array
  image_data <- array(0, dim = c(image_dimension, number_of_images))
  
  for(j in 1:number_of_images){
    for(k in 1:number_of_channels){
      image_data[,,k,j] <- image_loaded[[(j-1)*number_of_channels + k]]
    }
  }
  rm(list = c("j", "k"))
  
  
  # Create empty stack image
  image_stack <- array(0, dim = image_dimension)
  
  for(channelid in 1:image_dimension[3]){
    image_stack[,,channelid] = apply(image_data[,,channelid,], c(1,2), max)
  }

  
  # Channel 3 is the mask, channel 4 the original channel
    # Save mask as binary channel
  
  image_stack[,,3] <- ifelse(test = image_stack[,,3] > 0, yes = 1, no = 0)
  
  # normalize channel
  # image_stack[,,4] <- image_stack[,,4]/max(image_stack[,,4], na.rm = TRUE)
  # image_stack[,,3] <- image_stack[,,3]/max(image_stack[,,3], na.rm = TRUE)
  
  # Save tiff (without the third channel)
  dir.create(output_dir, showWarnings = FALSE)
  tiff::writeTIFF(what = image_stack[,,c(3,2,1)],
                  where = paste0(output_dir, tif_files_base[i],
                                 "_mask.tif"),
                  bits.per.sample = 8L,
                  compression = "none",
                  reduce = TRUE)
  
  # Add cilia numbers ######################################################
  image_name <- gsub(pattern = "_RP\\.tif", replacement = "", x = basename(tif_files)[i])
  df_dummy <- df_results_ciliaq %>% 
    filter(file_name == image_name)
  
  image_name <- gsub(pattern = "_CQP_CQ_RP\\.tif", replacement = ".czi", x = basename(tif_files)[i])
  xy_scaling <- df_metadata$scaling_x_in_um[df_metadata$fileName == image_name]
  if(xy_scaling != df_metadata$scaling_y_in_um[df_metadata$fileName == image_name]){
    print("We have differnt x and y scaling.")
  }
  
  df_dummy$center_x_pixels <- round(x = df_dummy$x.center..micron./xy_scaling, digits = 0)
  df_dummy$center_y_pixels <- round(x = df_dummy$y.center..micron./xy_scaling, digits = 0)
  
  cilia_numbers <- unique(df_dummy$ID)
  image_stack_number <- image_stack
  
  for(cilia_number in cilia_numbers){
    pos_x <- df_dummy$center_x_pixels[df_dummy$ID == cilia_number]
    pos_y <- df_dummy$center_y_pixels[df_dummy$ID == cilia_number]
    
    image_stack_number <- ciliaQ_addNumber(image = image_stack_number,
                                           number = cilia_number,
                                           pos_x = pos_x, pos_y = pos_y,
                                           image_layer = c(3,2),
                                           number_size_factor = 0.2)
  }
  rm(cilia_number)
  
  tiff::writeTIFF(what = image_stack_number[,,c(3,2,1)],
                  where = paste0(output_dir, tif_files_base[i],
                                 "_mask_numbers.tif"),
                  bits.per.sample = 8L,
                  compression = "none",
                  reduce = TRUE)
  
}

