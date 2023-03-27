# Function for reading ACDC results from xlsx file                  ++++++++
# Author: Kai Budde
# Created: 2022/12/19
# Last changed: 2022/12/19

ACDC_readXLSX <- function(input_file, output_dir){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse")
  groundhog.library(pkgs, groundhog.day)
  
  # Read data
  df_data <- suppressMessages(
    readxl::read_xlsx(path = input_file, sheet = 2))
  
  number_of_images <- df_data[[1]][max(which(df_data[[1]]=="Image ID"))+1]
  number_of_images <- as.numeric(number_of_images)
  na_lines <- which(is.na(df_data[[1]]))
  number_of_lines <- nrow(df_data)
  column_names <- tidy_names(as.character(df_data[4,]), syntactic = TRUE, quiet = TRUE)
  
  column_names <- gsub(pattern = "\\.nm\\.", replacement = ".pixel.", x = column_names)
  
  # Get all tables that are part of the big one.
  for(i in 1:number_of_images){
    if(i == 1){
      file_name <- basename(df_data[[2]][na_lines[i]+2])
      line_end <- ifelse(i == number_of_images, yes = number_of_lines, no = (na_lines[i+1]-1))
      df_results <- df_data %>% 
        dplyr::slice((na_lines[i]+4):line_end)
      names(df_results) <- column_names
      df_results$fileName <- file_name
      suppressMessages(df_results <- readr::type_convert(df_results))
    }else{
      file_name <- basename(df_data[[2]][na_lines[i]+2])
      line_end <- ifelse(i == number_of_images, yes = number_of_lines, no = (na_lines[i+1]-1))
      df_results_dummy <- df_data %>% 
        dplyr::slice((na_lines[i]+4):line_end)
      names(df_results_dummy) <- column_names
      df_results_dummy$fileName <- file_name
      suppressMessages(df_results_dummy <- readr::type_convert(df_results_dummy))
      
      df_results <- dplyr::bind_rows(df_results, df_results_dummy)
    }
  }
  
  readr::write_csv(x = df_results, file = file.path(output_dir, "ciliaData.csv"))
  
}