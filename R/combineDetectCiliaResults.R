# Function for combining the results (csv data) from  the R package ++++++++
# detectCilia using the cultivation images                          ++++++++
# Author: Kai Budde-Sagert
# Created: 2021/06/24
# Last changed: 2024/02/19


combineDetectCiliaResults <- function(input_dir,
                                      output_dir,
                                      use_directory_filter = "none",
                                      remove_directory_filter = "none"){
  
  # Load packages ############################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse")
  groundhog.library(pkgs, groundhog.day)
  
  # Filter results directories #############################################
  
  output_dirs <- list.dirs(input_dir, recursive = TRUE, full.names = TRUE)
  # Remove input dir from list
  output_dirs <- output_dirs[!(output_dirs == input_dir)]
  # Check for real detectCilia output
  output_dirs <- output_dirs[grepl(pattern = "output$", x = output_dirs, ignore.case = TRUE)]
  
  if(use_directory_filter != "none"){
    output_dirs <- output_dirs[grepl(pattern = use_directory_filter, x = output_dirs)]
  }
  if(remove_directory_filter != "none"){
    output_dirs <- output_dirs[!grepl(pattern = remove_directory_filter, x = output_dirs)]
  }
  
  # Read csv files and combine them into one data frame ####################
  
  for(i in 1:length(output_dirs)){
    
    if(i == 1){
      
      # Information of found cilia
      df_cilia <- readr::read_csv(file.path(output_dirs[i],"cilium_summary.csv"), name_repair = "universal")
      df_cilia$to_be_removed <- "yes_or_no"
      # df_cilia$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(output_dirs[i]))
      # df_cilia <- df_cilia %>% 
      #   dplyr::relocate(fileName)
      # df_cilia <- df_cilia %>% 
      #   select("fileName", "cilium", "vertical_length_in_um", "horizontal_length_in_um", "total_length_in_um", "to_be_removed")
      
      # Information of nuclei
      if(file.exists(file.path(output_dirs[i],"nuclei_number.csv"))){
        df_nuclei <- readr::read_csv(file.path(output_dirs[i],"nuclei_number.csv"), name_repair = "universal")
        df_nuclei$corrected_number <- NA
        # df_nuclei$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(output_dirs[i]))
        # df_nuclei <- df_nuclei %>% 
        #   dplyr::relocate(fileName)
        # # df_nuclei <- df_nuclei[,c(3,1,2)]
      }
      
      # Information of parameter values
      df_parameters <- readr::read_csv(file.path(output_dirs[i],"parameter_list.csv"), name_repair = "universal")
      # df_parameters$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(output_dirs[i]))
      # df_parameters <- df_parameters %>% 
      #   dplyr::relocate(fileName)
      
    }else{
      
      # Information of found cilia
      df_cilia_dummy <- readr::read_csv(file.path(output_dirs[i],"cilium_summary.csv"), name_repair = "universal")
      df_cilia_dummy$to_be_removed <- "yes_or_no"
      # df_cilia_dummy$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(output_dirs[i]))
      # df_cilia_dummy <- df_cilia_dummy %>% 
      #   dplyr::relocate(fileName)
      # df_cilia_dummy <- df_cilia_dummy %>% 
      #   select("fileName", "cilium", "vertical_length_in_um", "horizontal_length_in_um", "total_length_in_um", "to_be_removed")
      df_cilia <- dplyr::bind_rows(df_cilia, df_cilia_dummy)
      
      # Information of nuclei
      if(file.exists(file.path(output_dirs[i],"nuclei_number.csv"))){
        df_nuclei_dummy <- readr::read_csv(file.path(output_dirs[i],"nuclei_number.csv"), name_repair = "universal")
        df_nuclei_dummy$corrected_number <- NA
        # df_nuclei_dummy$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(output_dirs[i]))
        # df_nuclei_dummy <- df_nuclei_dummy %>% 
        #   dplyr::relocate(fileName)
        df_nuclei <- dplyr::bind_rows(df_nuclei, df_nuclei_dummy)
      }
     
      
      # Information of parameter values
      df_parameters_dummy <- readr::read_csv(file.path(output_dirs[i],"parameter_list.csv"), name_repair = "universal")
      # df_parameters_dummy$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(output_dirs[i]))
      # df_parameters_dummy <- df_parameters_dummy %>%
      #   dplyr::relocate(fileName)
      df_parameters <- dplyr::bind_rows(df_parameters, df_parameters_dummy)
      
      if(exists("df_nuclei_dummy")){
        rm(list = c("df_cilia_dummy", "df_nuclei_dummy",
                    "df_parameters_dummy"))
      }else{
        rm(list = c("df_cilia_dummy", "df_parameters_dummy"))
      }
      
      
    }
    
  }

  # Save resulting csv files #################################################
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  readr::write_csv(x = df_cilia,
                   file = file.path(output_dir, "summary_cilia.csv"))
  readr::write_csv2(x = df_cilia,
                    file = file.path(output_dir, "summary_cilia_de.csv"))
  
  if(exists("df_nuclei")){
    readr::write_csv(x = df_nuclei,
                     file = file.path(output_dir, "summary_nuclei.csv"))
    readr::write_csv2(x = df_nuclei,
                      file = file.path(output_dir, "summary_nuclei_de.csv"))
  }
  
  readr::write_csv(x = df_parameters,
                   file = file.path(output_dir, "summary_parameters.csv"))
  readr::write_csv2(x = df_parameters,
                    file = file.path(output_dir, "summary_parameters_de.csv"))
  
  
}
