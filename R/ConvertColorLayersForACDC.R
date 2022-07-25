# Script for converting color layers for use with ACDC detection        ++++
# Author: Kai Budde
# Created: 2022/07/22
# Last changed: 2022/07/22


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
directory_with_czi_files <- "ACDC" 

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load packages #############################################################

list.of.packages <- c("devtools")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(devtools)

# Install readCzi if not already installed (used version: 0.1.??)
devtools::install_github(repo = "https://github.com/SFB-ELAINE/readCzi")#, ref = "v0.1.??")
require(readCzi)

# Please adapt the following parameters ####################################
czi_files <- list.files(path = directory_with_czi_files, pattern = "czi", full.names = TRUE)
for(i in 1:length(czi_files)){
  input_file <- czi_files[i]
  readCzi::convertCziToTif(input_file = input_file, change_layers = "red<->green", add_scale_bar = FALSE)
}
