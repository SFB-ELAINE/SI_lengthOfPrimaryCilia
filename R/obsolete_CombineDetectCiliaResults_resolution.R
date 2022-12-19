# Script for combining the results (csv data) from  the R package ++++++++++
# detectCilia using the resolution images                         ++++++++++
# using the resolution images
# Author: Kai Budde
# Created: 2022/10/11
# Last changed: 2022/10/11

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# input_dir <-"C:/Users/Kai/Documents/git/gitHub/detectCilia/tests/movedHere"
input_dir <- "E:/PhD/Daten/Cilia/resolutionImages"
output_dir <- "data/automaticDetection/resolution"

# analyzeCziImages TRUE if the czi images have not yet been analyzed
analyzeCziImages <- FALSE

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load packages #############################################################

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"
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
# TODO: Check the latest version
if(!("readCzi" %in% installed.packages()[,"Package"])){
  # TODO: Add the latest version used
  # devtools::install_github("SFB-ELAINE/readCzi", ref = "v0.1.13")
  devtools::install_github("SFB-ELAINE/readCzi")
}
require(readCzi)


# Install the R package for detecting cilia in microscopy images
# TODO: Check the latest version
if(!("detectCilia" %in% installed.packages()[,"Package"])){
  # TODO: Add the latest version used
  devtools::install_github("SFB-ELAINE/detectCilia")
  # devtools::install_github("SFB-ELAINE/detectCilia", ref = "v0.1.13")
}
require(detectCilia)

# Read csv files and combine them into one data frame ######################

# Import cilium data from results
input_directories <- list.dirs(path = input_dir, full.names = TRUE)
input_directories <- input_directories[grepl(pattern = "output", x = input_directories, ignore.case = TRUE)]

for(i in 1:length(input_directories)){
  if(i == 1){
    df_cilium_summary <- readr::read_csv(file = paste0(input_directories[i], "/cilium_summary.csv"), name_repair = "universal")
    df_cilium_summary$to_be_removed <- "yes_or_no"
    df_cilium_summary$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(input_directories[i]))
    df_cilium_summary <- df_cilium_summary %>% 
      dplyr::relocate(fileName)
    
    df_nuclei_number <- readr::read_csv(file = paste0(input_directories[i], "/nuclei_number.csv"), name_repair = "universal")
    df_nuclei_number$corrected_number <- NA
    df_nuclei_number$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(input_directories[i]))
    df_nuclei_number <- df_nuclei_number %>% 
      dplyr::relocate(fileName)
    
    df_parameter_list <- readr::read_csv(file = paste0(input_directories[i], "/parameter_list.csv"), name_repair = "universal")
    df_parameter_list$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(input_directories[i]))
    df_parameter_list <- df_parameter_list %>% 
      dplyr::relocate(fileName)
  }else{
    df_cilium_summary_dummy <- readr::read_csv(file = paste0(input_directories[i], "/cilium_summary.csv"), name_repair = "universal")
    df_cilium_summary_dummy$to_be_removed <- "yes_or_no"
    df_cilium_summary_dummy$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(input_directories[i]))
    df_cilium_summary_dummy <- df_cilium_summary_dummy %>% 
      dplyr::relocate(fileName)
    
    df_nuclei_number_dummy <- readr::read_csv(file = paste0(input_directories[i], "/nuclei_number.csv"), name_repair = "universal")
    df_nuclei_number_dummy$corrected_number <- NA
    df_nuclei_number_dummy$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(input_directories[i]))
    df_nuclei_number_dummy <- df_nuclei_number_dummy %>% 
      dplyr::relocate(fileName)
    
    df_parameter_list_dummy <- readr::read_csv(file = paste0(input_directories[i], "/parameter_list.csv"), name_repair = "universal")
    df_parameter_list_dummy$fileName <- gsub(pattern = "_output", replacement = ".czi", x = basename(input_directories[i]))
    df_parameter_list_dummy <- df_parameter_list_dummy %>% 
      dplyr::relocate(fileName)
    
    df_cilium_summary <- dplyr::bind_rows(df_cilium_summary, df_cilium_summary_dummy)
    df_nuclei_number <- dplyr::bind_rows(df_nuclei_number, df_nuclei_number_dummy)
    df_parameter_list <- dplyr::bind_rows(df_parameter_list, df_parameter_list_dummy)
  }
  
}
rm(list = c("df_cilium_summary_dummy", "df_nuclei_number_dummy", "df_parameter_list_dummy", "i"))

write.csv(x = df_cilium_summary,
          file = paste(output_dir,"/","summary_cilia.csv", sep=""),
          row.names = FALSE)
write.csv2(x = df_cilium_summary,
           file = paste(output_dir,"/","summary_cilia_de.csv", sep=""),
           row.names = FALSE)

write.csv(x = df_nuclei_number,
          file = paste(output_dir,"/","summary_nuclei.csv", sep=""),
          row.names = FALSE)
write.csv2(x = df_nuclei_number,
           file = paste(output_dir,"/","summary_nuclei_de.csv", sep=""),
           row.names = FALSE)

write.csv(x = df_parameter_list,
          file = paste(output_dir,"/","summary_parameters.csv", sep=""),
          row.names = FALSE)
write.csv2(x = df_parameter_list,
           file = paste(output_dir,"/","summary_parameters_de.csv", sep=""),
           row.names = FALSE)

