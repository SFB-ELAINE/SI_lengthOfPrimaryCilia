# Function for automatic cilia detection using detectCilia +++++++++++++++++
# using the cultivation images
# Author: Kai Budde-Sagert
# Created: 2022/05/04
# Last changed: 2023/12/08


automaticCiliaDetection <- function(input_dir, output_dir){
  
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
  
  # Read in Python package for reading czi files
  # (Users will be asked to install miniconda
  # when starting for the first time)
  if(! "czifile" %in% reticulate::py_list_packages()$package){
    reticulate::py_install("czifile")
  }
  
  # Install the R package for reading czi images
  devtools::install_github("SFB-ELAINE/readCzi", ref = "v0.4.0")
  require(readCzi)
  
  
  # Install the R package for detecting cilia in microscopy images
  devtools::install_github("SFB-ELAINE/detectCilia", ref = "v0.8.1")
  require(detectCilia)
  
  # Detect cilia and get meta data #########################################
  
  # Data frame with certain parameter values
  file_names <- list.files(path = input_dir)
  file_names_czi <- file_names[grepl("czi", file_names)]
  file_names_czi <- file.path(input_dir, file_names_czi)
  
  number_of_czi_files <- length(file_names_czi)
  
  # Detect cilia
  for(i in 1:number_of_czi_files){

    print(paste0("Detecting cilia in ", file_names_czi[i], ". The current ",
                 "time is ", Sys.time(),"."))
    detectCilia::detectCilia(input_file_czi = file_names_czi[i],
                             cilium_color = "red",
                             nucleus_color = "blue",
                             projection_method_for_threshold_calculation = "max",
                             threshold_by_density_of_cilium_pixels = TRUE,
                             use_histogram_equalization_for_threshold_calculation = FALSE)
    gc()
  }
  rm(i)
  
  # Get and save metadata as csv
  for(i in 1:number_of_czi_files){
    if(i==1){
      df_metadata <- readCzi::readCziMetadata(input_file = file_names_czi[i],
                                              save_metadata = FALSE)
    }else{
      df_dummy <- readCzi::readCziMetadata(input_file = file_names_czi[i],
                                           save_metadata = FALSE)
      df_metadata <- rbind(df_metadata, df_dummy)
      rm(df_dummy)
    }
  }
  rm(i)
  
  dir.create(path = output_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(x = df_metadata,
            file = file.path(output_dir, "summary_metadata.csv"),
            row.names = FALSE)
  write.csv2(x = df_metadata,
             file = file.path(output_dir, "summary_metadata_de.csv"),
             row.names = FALSE)
  
}
