# Main script for reproducing all results                +++++++++++++++++++
# Author: Kai Budde
# Created: 2022/10/11
# Last changed: 2022/10/11

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# 1. Automatic detection of cilia from resolution images ###################

# Data input
input_dir <- "E:/PhD/Daten/Cilia/resolutionImages"
output_dir <- "data/automaticDetection/resolution"

# Calling the function
source("R/automaticCiliaDetection.R")
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 2. Combine results of automatic detection from resolution images #########

# Data input
input_dir <- "E:/PhD/Daten/Cilia/resolutionImages"
output_dir <- "data/automaticDetection/resolution"
use_directory_filter <- "none"
remove_directory_filter <- "none"

# Calling the function
source("R/combineDetectCiliaResults.R")
automaticCiliaDetection(input_dir, output_dir, directory_filter, directory_remove_filter)

# Removing objects
rm(list = c("input_dir", "output_dir", "directory_filter", "directory_remove_filter"))








# x. Automatic detection of cilia from cultivation images ###################

# Data input
input_dir <- "E:/PhD/Daten/Cilia/allImages"
output_dir <- "data/automaticDetection/cultivation"

# Calling the function
source("R/automaticCiliaDetection.R")
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# x + 1. Combine results of automatic detection from cultivation images ####

# Data input
input_dir <- "E:/PhD/Daten/Cilia/allImages"
output_dir <- "data/automaticDetection/cultivation"
use_directory_filter <- "none" #none #190815
remove_directory_filter <- "none" #compare

# Calling the function
source("R/combineDetectCiliaResults.R")
automaticCiliaDetection(input_dir, output_dir, directory_filter, directory_remove_filter)

# Removing objects
rm(list = c("input_dir", "output_dir", "directory_filter", "directory_remove_filter"))
