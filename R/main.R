# Main script for reproducing all results                +++++++++++++++++++
# Author: Kai Budde
# Created: 2022/10/11
# Last changed: 2022/10/13

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# 1. Automatic detection of cilia from resolution images ###################

# Data input
input_dir <- "E:/PhD/Daten/Cilia/resolutionImages"
output_dir <- "data/automaticDetection/resolution/test"

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
combineDetectCiliaResults(input_dir, output_dir,
                          use_directory_filter, remove_directory_filter)

# Removing objects
rm(list = c("input_dir", "output_dir",
            "use_directory_filter", "remove_directory_filter"))

# 3. Convert original results from manual detection to csv #################

# Data input
input_dir <- "data/manualDetection/resolution/originalFiles"
output_dir <- "data/manualDetection/resolution/originalFiles_csv"
language <- "en"

# Calling the function
source("R/convertXLSXtoCSV.R")
convertXLSXtoCSV(input_dir, output_dir, language)

# Removing objects
rm(list = c("input_dir", "output_dir", "language"))


# 4. Combine results of manual detection from resolution images ############

# Data input
input_dir <- "data/manualDetection/resolution/originalFiles_csv"
output_dir <- "data/manualDetection/resolution"
# File name of metadata from the execution of readCzi
metadata_file <- "data/automaticDetection/resolution/summary_metadata.csv"

# Results names
researcher <- "kai"

# Calling the function
source("R/combineManualDetectionResults.R")
combineManualDetectionResults(input_dir, output_dir, metadata_file)

# Removing objects
rm(list = c("input_dir", "output_dir", "metadata_file"))






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
combineDetectCiliaResults(input_dir, output_dir,
                          use_directory_filter,
                          remove_directory_filter)

# Removing objects
rm(list = c("input_dir", "output_dir",
            "use_directory_filter", "remove_directory_filter"))

# x + 2. Convert original results from manual detection to csv #############

# Data input
input_dir <- "data/manualDetection/cultivation/originalFiles"
output_dir <- "data/manualDetection/cultivation/originalFiles_csv"
language <- "en"

# Calling the function
source("R/convertXLSXtoCSV.R")
convertXLSXtoCSV(input_dir, output_dir, language)

# Removing objects
rm(list = c("input_dir", "output_dir", "language"))


# x + 3. Combine results of manual detection from cultivation images #######

# Data input
input_dir <- "data/manualDetection/cultivation/originalFiles_csv"
output_dir <- "data/manualDetection/cultivation"
# File name of metadata from the execution of readCzi
metadata_file <- "data/automaticDetection/cultivation/summary_metadata.csv"

# Results names
researcher <- "kai"

# Calling the function
source("R/combineManualDetectionResults.R")
combineManualDetectionResults(input_dir, output_dir, metadata_file)

# Removing objects
rm(list = c("input_dir", "output_dir", "metadata_file"))

