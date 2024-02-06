# Script for combining all results from manual detection +++++++++++++++++++
# Author: Kai Budde-Sagert
# Created: 2022/09/13
# Last changed: 2023/12/08


combineManualDetectionResults <- function(input_dir, output_dir,
                                          metadata_file,
                                          cilia_mapping_file = NULL,
                                          manual_result_files = NULL){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse", "rquery")#, "rqdatatable")
  groundhog.library(pkgs, groundhog.day)
  
  # Load data ##############################################################
  
  # Get results (csv files)
  input_files <- list.files(path = input_dir, pattern = "csv", full.names = TRUE)
  
  if(!is.null(manual_result_files)){
    input_files <- input_files[
      grepl(pattern = paste(manual_result_files, collapse = "|"), x = input_files, ignore.case = TRUE)]
  }
  
  file_names <- gsub(pattern = ".+results_(.+)\\.csv", replacement = "\\1", x = input_files)
  file_names <- paste0(file_names, ".czi")
  
  # Meta data
  df_metadata <- readr::read_csv(file = metadata_file,
                                 name_repair = "universal")
  
  # Keep only relevant rows of metadata
  df_metadata <- df_metadata[df_metadata$fileName %in% file_names,]
  
  # Read mapping file
  if(!is.null(cilia_mapping_file)){
    df_cilia_mapping <- readr::read_csv(file = file.path(input_dir, cilia_mapping_file),
                                        name_repair = "universal")
  }
  
  
  # Aggregate all needed information of manual detection ###################
  
  # Go through every result and build a large tibble with all of them
  for(i in 1:length(input_files)){
    
    # Read files
    df_dummy <- readr::read_csv(file = input_files[i],
                                name_repair = "universal")
    
    # Remove columns with the name "old" in it
    df_dummy <- df_dummy %>% 
      dplyr::select(-grep(pattern = "old", x = names(df_dummy), ignore.case = TRUE))
    
    
    # Append information of file, researcher who's done the manual detection
    # and the pixel size
    
    if(dim(df_dummy)[1] > 0){
      researcher <- gsub(
        pattern = "(.+)_results.+", replacement = "\\1", x = basename(path = input_files[i]), ignore.case = TRUE)
      df_dummy$researcher <- researcher
      
      df_dummy$fileName <- file_names[i]
      df_dummy$horizontal_scaling_in_um <- df_metadata$scaling_x_in_um[
        grepl(pattern = file_names[i], x = df_metadata$fileName, fixed = TRUE)]
      df_dummy$vertical_scaling_in_um   <- df_metadata$scaling_z_in_um[
        grepl(pattern = file_names[i], x = df_metadata$fileName, fixed = TRUE)]
      
      # Rename cilium_number column (Depending whether Clemens' or Kai's
      # enumeration was used)
      if(!is.null(cilia_mapping_file)){
        if(researcher == "nadja"){
          researcher = "kai"
        }else if(researcher == "simone"){
          researcher = "clemens"
        }
        
        names(df_dummy)[names(df_dummy) == "cilium_number"] <- paste("cilium_number_",
                                                                     researcher, sep="")
      }
      
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
  
  if(!is.null(cilia_mapping_file)){
    df_detection_results <- dplyr::relocate(.data = df_detection_results,
                                            cilium_number_kai,
                                            .after = cilium_number_clemens)
  }
  
  
  # Add mapping of cilium numbers ##########################################
  
  if(!is.null(cilia_mapping_file)){
    # Join result and cilia mapping data frames
    df_detection_results_clemens <- df_detection_results[df_detection_results$researcher == "clemens", ]
    df_detection_results_clemens <- rquery::natural_join(a = df_detection_results_clemens, b = df_cilia_mapping, by = c("fileName", "cilium_number_clemens"), jointype= "FULL")
    
    df_detection_results_kai <- df_detection_results[df_detection_results$researcher == "kai", ]
    df_detection_results_kai <- rquery::natural_join(a = df_detection_results_kai, b = df_cilia_mapping, by = c("fileName", "cilium_number_kai"))
    
    df_detection_results_nadja <- df_detection_results[df_detection_results$researcher == "nadja", ]
    df_detection_results_nadja <- rquery::natural_join(a = df_detection_results_nadja, b = df_cilia_mapping, by = c("fileName", "cilium_number_kai"))
    
    df_detection_results <- dplyr::bind_rows(df_detection_results_clemens,
                                             df_detection_results_kai,
                                             df_detection_results_nadja)
    
    rm(list = c("df_detection_results_clemens", "df_detection_results_kai", "df_detection_results_nadja"))
    
  }

  # Fill empty cells of "df_detection_results" ###############################
  
  # Horizontal length in um
  if(! ("horizontal_length_in_um" %in% names(df_detection_results))){
    df_detection_results$horizontal_length_in_um <- NA
  }
  fill_these_cells <- is.na(df_detection_results$horizontal_length_in_um)
  df_detection_results$horizontal_length_in_um[fill_these_cells] <- 
    df_detection_results$horizontal_length_in_pixels[fill_these_cells] *
    df_detection_results$horizontal_scaling_in_um[fill_these_cells]
  
  # Horizontal length in pixels
  if(! ("horizontal_length_in_pixels" %in% names(df_detection_results))){
    df_detection_results$horizontal_length_in_pixels <- NA
  }
  fill_these_cells <- is.na(df_detection_results$horizontal_length_in_pixels)
  df_detection_results$horizontal_length_in_pixels[fill_these_cells] <- 
    df_detection_results$horizontal_length_in_um[fill_these_cells] /
    df_detection_results$horizontal_scaling_in_um[fill_these_cells]
  
  # # Number of zstack layers
  # fill_these_cells <- is.na(df_detection_results$zstack_layers)
  # df_detection_results$zstack_layers[fill_these_cells] <- 
  #   df_detection_results$z_upper[fill_these_cells] -
  #   df_detection_results$z_lower[fill_these_cells] + 1
  
  # Calculate horizontal and vertical lengths ##############################
  
  # # Horizontal length in um
  # df_detection_results$horizontal_length_in_um <-
  #   df_detection_results$horizontal_length_in_pixels *
  #   df_detection_results$horizontal_scaling_in_um
  # 
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
