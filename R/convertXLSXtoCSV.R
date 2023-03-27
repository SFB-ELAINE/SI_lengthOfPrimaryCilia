# Function for converting data from xslx to csv files ++++++++++++++++++++++
# Author: Kai Budde
# Created: 2022/03/30
# Last changed: 2023/03/25


convertXLSXtoCSV <- function(input_dir, output_dir, language = "en"){
  
  # Load packages ############################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("readxl", "tidyverse")
  groundhog.library(pkgs, groundhog.day)
  
  # Convert xlsx to csv files ##############################################
  input_files <- list.files(path = input_dir, pattern = "xlsx", full.names = TRUE)
  dir.create(path = output_dir, showWarnings = FALSE)
  
  for(i in 1:length(input_files)){
    
    # Read xslx file
    df_data <- readxl::read_xlsx(path = input_files[i], na = "NA")
    filename <- basename(input_files[i])
    filename <- gsub(pattern = "xlsx", replacement = "csv", x = filename)
    
    # Write csv file
    if(language == "de"){
      readr::write_csv2(x = df_data,file = file.path(output_dir, filename))
    }else{
      readr::write_csv(x = df_data,file = file.path(output_dir, filename))
    }
    
  }
  
  
}

