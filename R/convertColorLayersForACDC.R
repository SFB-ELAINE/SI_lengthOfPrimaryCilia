# Script for converting color layers for use with ACDC detection        ++++
# Author: Kai Budde-Sagert
# Created: 2022/07/22
# Last changed: 2023/12/27

convertColorLayersForACDC <- function(input_dir, output_dir, projection_method = "max"){
  
  # Load packages #############################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("devtools")
  groundhog.library(pkgs, groundhog.day)
  
  
  # Install the R package for reading czi images
  devtools::install_github("SFB-ELAINE/readCzi", ref = "v0.4.0")
  require(readCzi)
  

  # Please adapt the following parameters ####################################
  czi_files <- list.files(path = input_dir, pattern = "czi", full.names = TRUE)
  dir.create(path = output_dir, showWarnings = FALSE)
  
  for(i in 1:length(czi_files)){
    input_file <- czi_files[i]
    readCzi::convertCziToTif(input_file = input_file,
                             output_dir = output_dir,
                             convert_all_slices = FALSE,
                             zstack_projection = TRUE,
                             projection_method = projection_method,
                             higher_contrast_projection = FALSE,
                             normalize_projection = FALSE,
                             change_color_layers = "red<->green",
                             add_scale_bar = FALSE)
  }
  
  
  
}
