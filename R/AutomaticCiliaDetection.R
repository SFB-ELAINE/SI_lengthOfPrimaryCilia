# Script for automatic cilia detection using detectCilia +++++++++++++++++++
# Author: Kai Budde
# Created: 2022/05/04
# Last changed: 2022/06/14

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# input_dir <-"C:/Users/Kai/Documents/git/gitHub/detectCilia/tests/movedHere"
input_dir <- "E:/PhD/Daten/Cilia/allImages"
output_dir <- "data/automaticDetection/cultivation"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load packages #############################################################

list.of.packages <- c("BiocManager", "devtools", "dplyr", "reticulate")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(!("EBImage" %in% utils::installed.packages())){
  print("Installing EBImage.")
  BiocManager::install("EBImage")
}

require(dplyr)
require(devtools)
require(EBImage)
require(reticulate)

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


## CHECK ALL FILES IN GIVEN DIRECTORY --------------------------------------

# Data frame with certain parameter values
file_names <- list.files(path = input_dir)
file_names_czi <- file_names[grepl("czi", file_names)]
file_names_czi <- paste(input_dir, file_names_czi, sep="/")
number_of_czi_files <- length(file_names_czi)

# Get and save metadata as csv
for(i in 1:number_of_czi_files){
  if(i==1){
    df_metadata <- readCzi::readCziMetadata(input_file = file_names_czi[i])
  }else{
    df_dummy <- readCzi::readCziMetadata(input_file = file_names_czi[i])
    df_metadata <- rbind(df_metadata, df_dummy)
    rm(df_dummy)
  }
}
rm(i)

write.csv(x = df_metadata,
          file = paste(output_dir,"/","summary_metadata.csv", sep=""),
          row.names = FALSE)
write.csv2(x = df_metadata,
           file = paste(output_dir,"/","summary_metadata_de.csv", sep=""),
           row.names = FALSE)

# Detect cilia
for(i in 1:number_of_czi_files){

  output_list <- detectCilia::detectCilia(input_file_czi = file_names_czi[i])
  
}
rm(i)
