# Script for getting meta information of czi files +++++++++++++++++++++++++
# Author: Kai Budde
# Created: 2021/12/10
# Last changed: 2021/12/10

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

list.of.packages <- c("BiocManager", "reticulate", "XML", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(!("EBImage" %in% utils::installed.packages())){
  print("Installing EBImage.")
  BiocManager::install("EBImage")
  #BiocManager::install("MaxContrastProjection")
}

require(devtools)
require(EBImage)
require(reticulate)
require(XML)

# Read in Python package for reading czi files
# (Users will be asked to install miniconda
# when starting for the first time)
reticulate::py_install("czifile")

# Install readCzi from GitHub ##############################################
devtools::install_github(repo = "https://github.com/SFB-ELAINE/readCzi")
require(readCzi)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Directory with czi files
dir_with_czi_files <- "Y:/Paper DetectCilia/Data/LSM-Aufnahmen/190815/"
filter_string <- "asc"
output_dir <- "data/manualDetection/190815_AscDexa"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Read and write metadata of czi files #####################################

input_files <- list.files(path = dir_with_czi_files, pattern = "czi", full.names = TRUE)
input_files <- input_files[
  grepl(pattern = filter_string, x = input_files, ignore.case = TRUE)]


for(i in 1:length(input_files)){
  
  # Read metadata
  df_dummy <- readCzi::readCziMetadata(input_file = input_files[i], save_metadata = FALSE)
  
  if(i == 1){
    df_metadata <- df_dummy
  }else{
    df_metadata <- rbind(df_metadata, df_dummy)
  }
  
}

write.csv2(x = df_metadata, file = paste(output_dir, "/df_metadata_de.csv", sep=""),
           row.names=FALSE)
write.csv(x = df_metadata, file = paste(output_dir, "/df_metadata_en.csv", sep=""),
          row.names=FALSE)
