# Function for automatic cilia detection using detectCilia +++++++++++++++++
# using the cultivation images
# Author: Kai Budde-Sagert
# Created: 2022/05/04
# Last changed: 2024/02/19


automaticCiliaDetection <- function(input_dir,
                                    output_dir = NULL,
                                    number_of_expected_nulcei = NA){
  
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
  devtools::install_github("SFB-ELAINE/readCzi", ref = "v0.4.1")
  require(readCzi)
  
  
  # Install the R package for detecting cilia in microscopy images
  devtools::install_github("SFB-ELAINE/detectCilia", ref = "v0.8.4")
  # devtools::install_local("../detectCilia")
  require(detectCilia)
  
  
  # Detect cilia and get meta data #########################################
  
  # Data frame with certain parameter values
  file_names <- list.files(path = input_dir)
  file_names_czi <- file_names[grepl("czi", file_names)]
  
  if(length(file_names_czi) > 0){
    # Directory contains czi files
    file_names_czi <- file.path(input_dir, file_names_czi)
    number_of_czi_files <- length(file_names_czi)
    
    # Detect cilia
    for(i in 1:number_of_czi_files){

      print(paste0("Detecting cilia in ", file_names_czi[i], ". The current ",
                   "time is ", Sys.time(),"."))
      dummy <- detectCilia::detectCilia(input_file_czi = file_names_czi[i],
                                        cilium_color = "red",
                                        nucleus_color = "blue",
                                        projection_method_for_threshold_calculation = "max",
                                        threshold_by_density_of_cilium_pixels = TRUE,
                                        use_histogram_equalization_for_threshold_calculation = FALSE)
    }
    rm(i)
    rm(dummy)
    
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
    
    print("Writing metadata file.")
    dir.create(path = output_dir, showWarnings = FALSE, recursive = TRUE)
    write.csv(x = df_metadata,
              file = file.path(output_dir, "summary_metadata.csv"),
              row.names = FALSE)
    write.csv2(x = df_metadata,
               file = file.path(output_dir, "summary_metadata_de.csv"),
               row.names = FALSE)
    
  }else{
    # Directory contains other directories with tifs
    tif_dirs <- list.files(path = input_dir, full.names = TRUE)
    grepl(pattern = input_dir, x = tif_dirs)
    number_of_images <- length(tif_dirs)
    
    # Detect cilia
    for(i in 1:number_of_images){
      
      print(paste0("Detecting cilia in ", tif_dirs[i], ". The current ",
                   "time is ", Sys.time(),"."))
      if(is.na(number_of_expected_nulcei)){
        number_of_expected_nulcei <- 1
      }
      dummy <- detectCilia::detectCilia(input_dir_tif = tif_dirs[i],
                                        export_normalized_images = FALSE,
                                        pixel_size = 1,
                                        slice_distance = 1,
                                        number_of_expected_nuclei = number_of_expected_nulcei,
                                        min_cilium_area_in_pixels = 5,
                                        max_cilium_area_in_pixels = 100,
                                        nucleus_color = NULL,
                                        cilium_color = "green",
                                        number_size_factor = 0.2)
      gc()
    }
    rm(i)
    rm(dummy)
  }
  
  
}
