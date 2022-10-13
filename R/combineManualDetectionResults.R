# Script for combining all results from manual detection +++++++++++++++++++
# Author: Kai Budde
# Created: 2022/09/13
# Last changed: 2022/09/13


combineManualDetectionResults <- function(input_dir, output_dir,
                                          metadata_file){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2022-03-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse")#, "rquery", "rqdatatable")
  groundhog.library(pkgs, groundhog.day)
  
  # Load data ##############################################################
  
  # Get results (csv files)
  input_files <- list.files(path = input_dir, pattern = "csv", full.names = TRUE)
  
  file_names <- gsub(pattern = ".+results_(.+)\\.csv", replacement = "\\1", x = input_files)
  file_names <- paste0(file_names, ".czi")
  
  # Meta data
  df_metadata <- readr::read_csv(file = metadata_file,
                                 name_repair = "universal")
  
  # name_of_files <- gsub(pattern = ".+results_(.+)\\.csv", replacement = "\\1", x = input_files)
  # name_of_files <- paste0(name_of_files, ".czi")
  # 
  # # Keep only relevant rows
  # df_metadata <- df_metadata[df_metadata$fileName %in% name_of_files
  #   grepl(pattern = name_of_files,
  #                                  x = df_metadata$fileName, fixed = TRUE),]
  
  
  # Aggregate all needed information of manual detection ###################
  
  # Go through every result and build a large tibble with all of them
  for(i in 1:length(input_files)){
    
    # Read files
    df_dummy <- readr::read_csv(file = input_files[i],
                                name_repair = "universal")
    
    # Append information of file, researcher who's done the manual detection
    # and the pixel size
    
    if(dim(df_dummy)[1] > 0){
      df_dummy$researcher <- researcher
      df_dummy$fileName <- file_names[i]
      df_dummy$horizontal_scaling_in_um <- df_metadata$scaling_x_in_um[
        grepl(pattern = file_names[i], x = df_metadata$fileName, fixed = TRUE)]
      df_dummy$vertical_scaling_in_um   <- df_metadata$scaling_z_in_um[
        grepl(pattern = file_names[i], x = df_metadata$fileName, fixed = TRUE)]
      
      if(i == 1){
        df_detection_results <- df_dummy
      }else{
        df_detection_results <- dplyr::bind_rows(df_detection_results, df_dummy)
      }
    }
  }
  
  rm(df_dummy)
  
  # Relocate column
  df_detection_results <- df_detection_results %>% 
    dplyr::relocate(fileName, researcher)
  
  # Calculate horizontal and vertical lengths ##############################
  
  # Horizontal length in um
  df_detection_results$horizontal_length_in_um <-
    df_detection_results$horizontal_length_in_pixels *
    df_detection_results$horizontal_scaling_in_um
  
  # Number of zstack layers
  df_detection_results$vertical_length_in_layers <- 
    df_detection_results$z_upper -
    df_detection_results$z_lower + 1
  
  # Add vertical length in um
  df_detection_results$vertical_length_in_um <- 
    df_detection_results$vertical_length_in_layers *
    df_detection_results$vertical_scaling_in_um
  
  # Add total length in um (using the Pythagorean theorem)
  df_detection_results$total_length_in_um <- 
    sqrt(df_detection_results$vertical_length_in_um^2 +
           df_detection_results$horizontal_length_in_um^2)
  
  # Save resulting data frame ##############################################
  
  dir.create(output_dir, showWarnings = FALSE)
  readr::write_csv(x = df_detection_results,
                   file = file.path(output_dir, "df_manual_results.csv"))
  readr::write_csv2(x = df_detection_results,file = file.path(output_dir, "df_manual_results_de.csv"))
  
}
