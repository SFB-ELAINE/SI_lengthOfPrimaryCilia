# Main script for reproducing all results from the publication +++++++++++++
# "detectCilia: An R package for automated detection and       +++++++++++++
# 3D-measurement of primary cilia - Studying the influence of  +++++++++++++
# cultivation methods on the lengths of primary cilia"         +++++++++++++
# Author: Kai Budde
# Created: 2022/10/11
# Last changed: 2023/01/15

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# 1. Manual detection of cilia in resolution and cultivation images ########

# See appendix of the publication.

# 2. Automatic detection of cilia                         ##################
# 2.1 Automatic detection of cilia from resolution images ##################

# Data input
input_dir <- "E:/PhD/Daten/Cilia/resolutionImages"
output_dir <- "data/automaticDetection/resolution"

# Calling the function
source("R/automaticCiliaDetection.R")
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 2.2 Combine results of automatic detection from resolution images ########

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


# 2.3 Convert original results from manual detection of resolution #########
#    images to csv

# Data input
input_dir <- "data/manualDetection/resolution/originalFiles"
output_dir <- "data/manualDetection/resolution/originalFiles_csv"
language <- "en"

# Calling the function
source("R/convertXLSXtoCSV.R")
convertXLSXtoCSV(input_dir, output_dir, language)

# Removing objects
rm(list = c("input_dir", "output_dir", "language"))

# 2.4 Combine results of manual detection from resolution images ############

# Data input
input_dir <- "data/manualDetection/resolution/originalFiles_csv"
output_dir <- "data/manualDetection/resolution"
# File name of metadata from the execution of readCzi
metadata_file <- "data/automaticDetection/resolution/summary_metadata.csv"

# Calling the function
source("R/combineManualDetectionResults.R")
combineManualDetectionResults(input_dir, output_dir, metadata_file)

# Removing objects
rm(list = c("input_dir", "output_dir", "metadata_file"))


# 2.5 Automatic detection of cilia from cultivation images #################

# Data input
input_dir <- "E:/PhD/Daten/Cilia/allImages"
output_dir <- "data/automaticDetection/cultivation"

# Calling the function
source("R/automaticCiliaDetection.R")
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 2.6 Combine results of automatic detection from cultivation images #######

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

# 2.7 Convert original results from manual detection of cultivation ########
#     images to csv

# Data input
input_dir <- "data/manualDetection/cultivation/originalFiles"
output_dir <- "data/manualDetection/cultivation/originalFiles_csv"
language <- "en"

# Calling the function
source("R/convertXLSXtoCSV.R")
convertXLSXtoCSV(input_dir, output_dir, language)

# Removing objects
rm(list = c("input_dir", "output_dir", "language"))


# 2.8 Combine results of manual detection from cultivation images ##########

# Data input
input_dir <- "data/manualDetection/cultivation/originalFiles_csv"
output_dir <- "data/manualDetection/cultivation"
# File name of metadata from the execution of readCzi
metadata_file <- "data/automaticDetection/cultivation/summary_metadata.csv"

# Mapping of cilia numbers
cilia_mapping_file <- "cilia_numbers_clemens_kai.csv"
# Results names
manual_result_files <- c("kai_results", "clemens_results", "nadja_results")

# Calling the function
source("R/combineManualDetectionResults.R")
combineManualDetectionResults(input_dir, output_dir, metadata_file,
                              cilia_mapping_file, manual_result_files)

# Removing objects
rm(list = c("input_dir", "output_dir", "metadata_file",
            "cilia_mapping_file", "manual_result_files"))

# 3 Plot results                                                  ##########

# 3.1 Plot results of automatic measurement of cultivation images ##########

# Data input

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file <- "data/automaticDetection/cultivation/summary_cilia_edited.csv"
output_dir <- "plots"

# Calling the function
source("R/plotAutomaticDetection_Cultivation.R")
plotAutomaticDetection_Cultivation(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))


# 3.2 Plot comparison of manual and automatic measurement of      ##########
#     seven test images

# Data input

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file_automatic             <- "data/automaticDetection/cultivation/summary_cilia_edited.csv"
input_file_automatic_parameters  <- "data/automaticDetection/cultivation/summary_parameters.csv"
input_file_metadata              <- "data/automaticDetection/cultivation/summary_metadata.csv"

input_file_manual <- "data/manualDetection/cultivation/df_manual_results.csv"
input_file_cilium_numbers <- "data/manualDetection/cultivation/originalFiles_csv/cilia_numbers_clemens_automatic.csv"

# Output directory
output_dir <- "plots/manualAutomaticComparison"

# Calling the function
source("R/plotComparisonManualAutomaticDetection_Cultivation.R")
plotComparisonManualAutomaticDetection_Cultivation(
  input_file_automatic,
  input_file_automatic_parameters,
  input_file_metadata,
  input_file_manual,
  input_file_cilium_numbers,
  output_dir)

# Removing objects
rm(list = c("input_file_automatic", "input_file_automatic_parameters",
            "input_file_metadata", "input_file_manual",
            "input_file_cilium_numbers", "output_dir"))








# 4 Detection of cilia with ACDC                                  ##########

# 4.1 Stack and convert color layers for ACDC analysis            ##########

# Data input
input_dir <- "E:/PhD/Daten/Cilia/allImages"
output_dir <- "ACDC"

# Calling the function
source("R/convertColorLayersForACDC.R")
convertColorLayersForACDC(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 4.2 Analyze stacked images with ACDC                            ##########

# This is manually done.

# 4.3 Read results as xlsx file and convert to csv                ##########

input_file <- file.path("ACDC","Cilia Report 19-Dec-2022 14-08-05.xlsx")
output_dir <- "ACDC"

# Calling the function
source("R/ACDC_readXLSX.R")
ACDC_readXLSX(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))

# 4.4 Plot detection results of ACDC                              ##########

input_file_acdc <- file.path("ACDC","ciliaData.csv")
input_file_metadata <- "data/automaticDetection/cultivation/summary_metadata.csv"
output_dir <- "plots/ACDC"

# Calling the function
source("R/ACDC_plotAutomaticDetection.R")
ACDC_plotAutomaticDetection(input_file_acdc, input_file_metadata, output_dir)

# Removing objects
rm(list = c("input_file_acdc", "input_file_metadata", "output_dir"))








# 5 Detection of cilia with ciliaQ                                ##########

# 5.1 Read results as txt file and convert to csv                 ##########

# Directory with analysis results (CiliaQ files)
input_dir <- "E:/PhD/Daten/Cilia/allImages/ACDC/histogram_equalized_converted_multistack"
# Output directory
output_dir <- "CiliaQ_horizontal"


# Calling the function
source("R/ciliaQ_getResults.R")
ciliaQ_getResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 5.2 Plot detection results of ACDC                              ##########

input_file_ciliaq <- file.path("CiliaQ_horizontal","ciliaq_data.csv")
# input_file_metadata <- "data/automaticDetection/cultivation/summary_metadata.csv"
output_dir <- "plots/ciliaQ"

# Calling the function
source("R/ciliaQ_plotAutomaticDetection.R")
ciliaQ_plotAutomaticDetection(input_file_ciliaq, output_dir)

# Removing objects
rm(list = c("input_file_acdc", "input_file_metadata", "output_dir"))







# 6 Create test cilium images                                     ##########

number_of_images <- 10
output_dir <- "testImages"

# Calling the function
source("R/createTestCiliumImages.R")
create2DTestCiliumImages(number_of_images, output_dir)

# Removing objects
rm(list = c("number_of_images", "output_dir"))
