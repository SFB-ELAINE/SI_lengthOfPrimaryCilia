# Script for converting data from to csv files +++++++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2022/03/30
# Last changed: 2022/06/09

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()
# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")
library(groundhog)
pkgs <- c("readxl")
groundhog.library(pkgs, groundhog.day)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Directory with xlsx files
dir_with_xlsx_files <- "data/manualDetection/cultivation/originalFiles"
language <- "en"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Convert xlsx to csv files ################################################
input_files <- list.files(path = dir_with_xlsx_files, pattern = "xlsx", full.names = TRUE)
output_files <- gsub(pattern = "originalFiles/", replacement = "originalFiles_csv/", x = input_files, ignore.case = TRUE)

dir.create(path = gsub(pattern = "originalFiles", replacement = "originalFiles_csv", x = dir_with_xlsx_files, ignore.case = TRUE), showWarnings = FALSE)

for(i in 1:length(input_files)){
  
  # Read xslx file
  df_data <- readxl::read_xlsx(path = input_files[i], na = "NA")
  
  # Write csv file
  if(language == "deu"){
    write.csv2(x = df_data, file = gsub(pattern = "xlsx", replacement = "csv",
                                        x = output_files[i]), row.names = FALSE)
  }else{
    write.csv(x = df_data, file = gsub(pattern = "xlsx", replacement = "csv",
                                       x = output_files[i]), row.names = FALSE)
  }
  
  
}

