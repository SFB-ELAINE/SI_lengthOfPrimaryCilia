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





# x. Automatic detection of cilia from cultivation images ###################

# Data input
input_dir <- "E:/PhD/Daten/Cilia/allImages"
output_dir <- "data/automaticDetection/cultivation"

# Calling the function
source("R/automaticCiliaDetection.R")
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))
