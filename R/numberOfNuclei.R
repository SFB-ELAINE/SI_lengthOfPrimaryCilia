# Script for calculating the number of nuclei per group             ++++++++
# Author: Kai Budde
# Created: 2023/01/15
# Last changed: 2023/01/15


numberOfNuclei <- function(input_file){
  
  # # Delete everything in the environment
  # rm(list = ls())
  # # close all open plots in RStudio
  # graphics.off()
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2022-03-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse")
  groundhog.library(pkgs, groundhog.day)
  
  # Import and clean data ####################################################
  df_results_automatic <- readr::read_csv(file = input_file, name_repair = "universal")
  
  
  # Add information of cultivation
  
  # cultivations <- df_results$fileName
  # cultivations <- gsub(pattern = "(.+)_zstack.+", replacement = "\\1", x = cultivations)
  # cultivations <- unique(cultivations)
  
  df_results_automatic$cultivation <- NA
  
  names_of_experiments <- c("ITS", "ITS w/ Dexa",
                            "ITS w/ Dexa + IGF + TGF",
                            "FBS")
  
  df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[1]
  df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[2]
  df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[3]
  df_results_automatic$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[4]
  
  
  nuclei_detection_results <- df_results_automatic %>%
    dplyr::group_by(cultivation) %>%
    dplyr::summarise(number_of_images = n(), numer_of_nuclei = sum(numberOfNuclei))
  
  print(paste("The number of found nuclei are in all images are:"))
  print(nuclei_detection_results)
  
}

