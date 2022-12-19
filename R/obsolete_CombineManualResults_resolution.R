# Script for combining all results from manual detection +++++++++++++++++++
# Author: Kai Budde
# Created: 2022/09/13
# Last changed: 2022/09/13

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()
# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")

# Load packages
library(groundhog)
pkgs <- c("tidyverse", "rquery", "rqdatatable")
groundhog.library(pkgs, groundhog.day)


# new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# require(dplyr)
# require(rquery)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Directory with results
dir_with_csv_files <- "data/manualDetection/resolution/originalFiles_csv"

# File name of metadata from the execution of readCzi
metadata_file <- "data/automaticDetection/resolution/summary_metadata.csv"

# Results names
researcher <- "kai"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Output directory
dir_output <- gsub(pattern = "/originalFiles_csv", replacement = "", x = dir_with_csv_files, ignore.case = TRUE)


# Get results (csv files)
input_files <- list.files(path = dir_with_csv_files, pattern = "csv", full.names = TRUE)

# Load data ###############################################################

# Meta data
df_metadata <- readr::read_csv(file = metadata_file,
                               name_repair = "universal")

name_of_files <- gsub(pattern = ".+results_(.+TGF).+", replacement = "\\1", x = input_files[1])

# Keep only relevant rows
df_metadata <- df_metadata[grepl(pattern = name_of_files, x = df_metadata$fileName, fixed = TRUE),]


# Aggregate all needed information of manual detection #####################

# Go through every result and build a large tibble with all of them
for(i in 1:length(input_files)){
  
  # Read files
  df_dummy <- readr::read_csv(file = input_files[i],
                              name_repair = "universal")
  
  # Append information of file, researcher who's done the manual detection
  # and the pixel size
  
  if(dim(df_dummy)[1] > 0){
    df_dummy$researcher <- researcher
    fileName <- gsub(pattern = "kai_results_", replacement = "", x = basename(path = input_files[i]))
    fileName <- gsub(pattern = "csv", replacement = "czi", x = fileName)
    df_dummy$fileName <- fileName
    
    df_dummy$horizontal_scaling_in_um <- df_metadata$scaling_x_in_um[grepl(pattern = fileName, x = df_metadata$fileName, fixed = TRUE)]
    df_dummy$vertical_scaling_in_um   <- df_metadata$scaling_z_in_um[grepl(pattern = fileName, x = df_metadata$fileName, fixed = TRUE)]
    
    if(i == 1){
      df_detection_results <- df_dummy
    }else{
      df_detection_results <- dplyr::bind_rows(df_detection_results, df_dummy)
    }
  }
}

rm(df_dummy)

# Map the cilium numbers of Clemens to the other researchers ###############

# Relocate column
df_detection_results <- df_detection_results %>% 
  dplyr::relocate(fileName, researcher)

# Calculate horizontal and vertical lengths ################################

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

# # Delete all rows that contain cilia at image borders ######################
# df_detection_results <- df_detection_results %>%
#   dplyr::filter(comments != "at border" | is.na(comments))


# Save resulting data frame ################################################

dir.create(dir_output, showWarnings = FALSE)
readr::write_csv(x = df_detection_results, file = paste(dir_output, "/df_manual_results.csv", sep=""))
readr::write_csv2(x = df_detection_results, file = paste(dir_output, "/df_manual_results_de.csv", sep=""))
