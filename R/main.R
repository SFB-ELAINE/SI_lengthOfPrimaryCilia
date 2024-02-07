# Main script for reproducing all results from the publication +++++++++++++
# "detectCilia: An R package for automated detection and       +++++++++++++
# 3D-measurement of primary cilia - Studying the influence of  +++++++++++++
# cultivation methods on the lengths of primary cilia"         +++++++++++++
# Author: Kai Budde-Sagert
# Created: 2022/10/11
# Last changed: 2024/01/19

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Please adapt the following parameters ####################################
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

download_directory_resolution  <- file.path("E:", "PhD", "Data",
                                            "Cilia", "resolutionImages")
download_directory_cultivation <- file.path("E:", "PhD", "Data",
                                            "Cilia", "cultivationImages")
directory_artificial_images <- file.path("artificialImages")

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# 0. Housekeeping: Check which version of R packages are installed #########

# # Set groundhog day for reproducibility (see https://groundhogr.com)
# groundhog.day <- "2023-01-01"
# 
# if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
#   install.packages("groundhog")
# }
# 
# # Load packages
# library(groundhog)
# pkgs <- c("coin", "EnvStats", "ggbeeswarm", "ggpubr", "rquery", "rstatix",
#           "scales", "tidyverse")
# groundhog.library(pkgs, groundhog.day)
# 
# # Cite packages
# options(citation.bibtex.max=999)
# for(i in 1:length(pkgs)){
#   print(citation(pkgs[i]))
# }
# rm(i)


# 1. Manual detection of cilia in resolution and cultivation images ########

# See appendix of the publication.


# 2. Automatic detection of cilia                         ##################

# 2.1 Download raw resolution data from Zenodo #############################

# Data input
download_dir <- download_directory_resolution
doi <- "10.5281/zenodo.8038570"

# Calling the function
source(file.path("R", "downloadFromZenodo.R"))
downloadFromZenodo(download_dir, doi)

# Removing objects
rm(list = c("download_dir", "doi"))

# 2.2 Automatic detection of cilia from resolution images ##################

# Data input
input_dir <- download_directory_resolution
output_dir <- file.path("data", "automaticDetection", "resolution")

# Calling the function
source(file.path("R", "automaticCiliaDetection.R"))
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 2.3 Combine results of automatic detection from resolution images ########

# Data input
input_dir <- download_directory_resolution
output_dir <- file.path("data", "automaticDetection", "resolution")
use_directory_filter <- "none"
remove_directory_filter <- "none"

# Calling the function
source(file.path("R", "combineDetectCiliaResults.R"))
combineDetectCiliaResults(input_dir, output_dir,
                          use_directory_filter, remove_directory_filter)

# Removing objects
rm(list = c("input_dir", "output_dir",
            "use_directory_filter", "remove_directory_filter"))


# 2.4 Convert original results from manual detection of resolution #########
#    images to csv

# Data input
input_dir <- file.path("data", "manualDetection", "resolution", "originalFiles")
output_dir <- file.path("data", "manualDetection", "resolution", "originalFiles_csv")
language <- "en"

# Calling the function
source(file.path("R", "convertXLSXtoCSV.R"))
convertXLSXtoCSV(input_dir, output_dir, language)

# Removing objects
rm(list = c("input_dir", "output_dir", "language"))


# 2.5 Combine results of manual detection from resolution images ###########

# Data input
input_dir <- file.path("data", "manualDetection", "resolution", "originalFiles_csv")
output_dir <- file.path("data", "manualDetection", "resolution")

# File name of metadata from the execution of readCzi
metadata_file <- file.path("data", "automaticDetection", "resolution", "summary_metadata.csv")

# Calling the function
source(file.path("R", "combineManualDetectionResults.R"))
combineManualDetectionResults(input_dir, output_dir, metadata_file)

# Removing objects
rm(list = c("input_dir", "output_dir", "metadata_file"))


# 2.6 Download raw cultivation data from Zenodo ############################

# Data input
download_dir <- download_directory_cultivation
doi <- "10.5281/zenodo.7994588"

# Calling the function
source(file.path("R", "downloadFromZenodo.R"))
downloadFromZenodo(download_dir, doi)

# Removing objects
rm(list = c("download_dir", "doi"))

# 2.7 Automatic detection of cilia from cultivation images #################

# Data input
input_dir <- download_directory_cultivation
output_dir <- file.path("data", "automaticDetection", "cultivation")

# Calling the function
source(file.path("R", "automaticCiliaDetection.R"))
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 2.8 Combine results of automatic detection from cultivation images #######

# Data input
input_dir <- download_directory_cultivation
output_dir <- file.path("data", "automaticDetection", "cultivation")
use_directory_filter <- "none"
remove_directory_filter <- "none"

# Calling the function
source(file.path("R", "combineDetectCiliaResults.R"))
combineDetectCiliaResults(input_dir, output_dir,
                          use_directory_filter,
                          remove_directory_filter)

# Removing objects
rm(list = c("input_dir", "output_dir",
            "use_directory_filter", "remove_directory_filter"))


# 2.9 Calculate the number of found nuclei #################################
# Data input
input_file <- file.path("data", "automaticDetection", "cultivation", "summary_nuclei.csv")

# Calling the function
source(file.path("R", "numberOfNuclei.R"))
numberOfNuclei(input_file)

# Removing objects
rm(list = c("input_file"))


# 2.10 Convert original results from manual detection of cultivation #######
#      images to csv

# Data input
input_dir <- file.path("data", "manualDetection", "cultivation", "originalFiles")
output_dir <- file.path("data", "manualDetection", "cultivation", "originalFiles_csv")
language <- "en"

# Calling the function
source(file.path("R", "convertXLSXtoCSV.R"))
convertXLSXtoCSV(input_dir, output_dir, language)

# Removing objects
rm(list = c("input_dir", "output_dir", "language"))


# 2.11 Combine results of manual detection from cultivation images #########

# Data input
input_dir <- file.path("data", "manualDetection", "cultivation", "originalFiles_csv")
output_dir <- file.path("data", "manualDetection", "cultivation")
# File name of metadata from the execution of readCzi
metadata_file <- file.path("data", "automaticDetection", "cultivation", "summary_metadata.csv")

# Mapping of cilia numbers
cilia_mapping_file <- "cilia_numbers_clemens_kai.csv"
# Results names
manual_result_files <- c("kai_results", "clemens_results", "nadja_results")

# Calling the function
source(file.path("R", "combineManualDetectionResults.R"))
combineManualDetectionResults(input_dir, output_dir, metadata_file,
                              cilia_mapping_file, manual_result_files)

# Removing objects
rm(list = c("input_dir", "output_dir", "metadata_file",
            "cilia_mapping_file", "manual_result_files"))



# 3 Plot results  ##########################################################

# 3.1 Plot results of automatic measurement of cultivation images ##########
#     and print a tibble with total lengths

# Data input

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file <-  file.path("data", "automaticDetection", "cultivation",
                         "summary_cilia_edited.csv")
input_file_compare <-  file.path("data", "automaticDetection",
                                 "cultivation", "summary_cilia.csv")
output_dir <-  file.path("plots", "automaticDetectionCultivation")

# Calling the function
source(file.path("R", "plotAutomaticDetection_Cultivation.R"))
plotAutomaticDetection_Cultivation(input_file, input_file_compare, output_dir)

# Removing objects
rm(list = c("input_file", "input_file_compare", "output_dir"))


# 3.2 Plot comparison of man&aut measurement of seven test images ##########

# Data input

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file_automatic             <- file.path("data", "automaticDetection",
                                              "cultivation", "summary_cilia_edited.csv")
input_file_automatic_parameters  <- file.path("data", "automaticDetection",
                                              "cultivation", "summary_parameters.csv")
input_file_metadata              <- file.path("data", "automaticDetection",
                                              "cultivation", "summary_metadata.csv")

input_file_manual <- file.path("data", "manualDetection", "cultivation", "df_manual_results.csv")
input_file_cilium_numbers <- file.path("data", "manualDetection", "cultivation", "originalFiles_csv",
                                       "cilia_numbers_clemens_automatic.csv")

# Output directory
output_dir <- file.path("plots", "manualAutomaticComparison_cultivation")

# Calling the function
source(file.path("R", "plotComparisonManualAutomaticDetection_Cultivation.R"))
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


# 3.3 Plot comparison of man&aut measurement of resolution images ##########

# Data input
input_file_automatic <- file.path("data", "automaticDetection",
                                  "resolution", "summary_cilia_edited.csv")
input_file_manual <- file.path("data", "manualDetection", "resolution",
                               "df_manual_results.csv")

# Output directory
output_dir <- file.path("plots", "manualAutomaticComparison_resolution")

# Calling the function
source(file.path("R", "plotComparisonManualAutomaticDetection_Resolution.R"))
plotComparisonManualAutomaticDetection_Resolution(
  input_file_automatic,
  input_file_manual,
  output_dir)

# Removing objects
rm(list = c("input_file_automatic", "input_file_manual", "output_dir"))


# 4 Detection of cilia with ACDC ###########################################
# 4.1 Project z-stacks and convert color layers for ACDC analysis ##########

# Data input
input_dir <- download_directory_cultivation
output_dir <- "ACDC"
projection_method <- "max"

# Calling the function
source(file.path("R", "convertColorLayersForACDC.R"))
convertColorLayersForACDC(input_dir = input_dir,
                          output_dir = output_dir,
                          projection_method = projection_method)

# Removing objects
rm(list = c("input_dir", "output_dir", "projection_method"))


# 4.2 Analyze stacked images with ACDC #####################################

# This needs to be manually done because it cannot be invoked from R.
# See ACDC/ReadMe.txt for further information.

# 4.3 Read results as xlsx file and convert to csv #########################

# input_file <- file.path("ACDC","NoCorrection_Cilia Report 19-Jan-2024 19-49-57.xlsx")
input_file <- file.path("ACDC","Correction_Cilia Report 21-Jan-2024 09-41-30.xlsx")
output_dir <- "ACDC"

# Calling the function
source(file.path("R", "ACDC_readXLSX.R"))
ACDC_readXLSX(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))


# 4.4 Plot detection results of ACDC #######################################

input_file_acdc <- file.path("ACDC","ciliaData.csv")
input_file_metadata <- "data/automaticDetection/cultivation/summary_metadata.csv"
output_dir <- file.path("plots","ACDC")

# Calling the function
source(file.path("R", "ACDC_plotAutomaticDetection.R"))
ACDC_plotAutomaticDetection(input_file_acdc, input_file_metadata, output_dir)

# Removing objects
rm(list = c("input_file_acdc", "input_file_metadata", "output_dir"))



# 5 Detection of cilia with ciliaQ #########################################
# 5.1 Analyze stacked images with ciliaQ ###################################

# This needs to be manually done because it cannot be invoked from R.


# 5.2 Read ciliaQ results (txt files) and convert to csv ###################

# Directory with analysis results (CiliaQ files)
# input_dir <- file.path("E:", "PhD", "Data", "Cilia", "cultivationImages", "ciliaQ", "zstack")
# input_dir <- file.path("E:", "PhD", "Data", "Cilia", "cultivationImages", "ciliaQ", "zstack_max")
input_dir <- file.path("E:", "PhD", "Data", "Cilia", "cultivationImages", "ciliaQ")
# Output directory
output_dir <- "CiliaQ"

# Calling the function
source(file.path("R", "ciliaQ_getResults.R"))
ciliaQ_getResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 5.3 Plot detection results of CiliaQ #####################################

input_file_ciliaq <- file.path("CiliaQ","ciliaq_data_edited.csv")
output_dir <- file.path("plots","ciliaQ")

# Calling the function
source(file.path("R", "ciliaQ_plotAutomaticDetection.R"))
ciliaQ_plotAutomaticDetection(input_file_ciliaq, output_dir)

# Removing objects
rm(list = c("input_file_ciliaq", "output_dir"))


# 6 Comparison of detectCilia, ACDC, and ciliaQ ############################

# 6.1 Plot results of horizontal cilia length of detectCilia, ACDC, and ciliaQ ####
input_file_detectCilia <- file.path("plots", "automaticDetectionCultivation", "horizontalLength_detectCilia.csv")
input_file_ACDC <- file.path("plots", "ACDC", "horizontalLength_ACDC.csv")
input_file_ciliaq <- file.path("plots", "ciliaQ", "horizontalLength_ciliaQ.csv")
output_dir <- file.path("plots", "allTools")

# Calling the function
source(file.path("R", "plotResultsFromAllTools.R"))
plotResultsFromAllTools(input_file_detectCilia, input_file_ACDC,
                        input_file_ciliaq, output_dir)

# Removing objects
rm(list = c("input_file_detectCilia", "input_file_ACDC",
            "input_file_ciliaq", "output_dir"))


# 6.2 Plot results of horizontal cilia length of detectCilia, ACDC, and ciliaQ of 7 test images only ####
input_file_manual <- file.path("data","manualDetection","cultivation",
                               "df_manual_results.csv")
input_file_cilium_numbers <- file.path("data","manualDetection",
                                       "cultivation",
                                       "originalFiles_csv",
                                       "cilia_numbers_clemens_automatic.csv")
input_file_detectCilia    <- file.path("data", "automaticDetection",
                                       "cultivation", "summary_cilia_edited.csv")
input_file_ACDC           <- file.path("ACDC","ciliaData.csv")
input_file_ciliaq         <- file.path("CiliaQ_horizontal","ciliaq_data.csv")
output_dir <- file.path("plots", "allToolsTestImages")

# input_file_automatic_parameters  <- "data/automaticDetection/cultivation/summary_parameters.csv"
# input_file_metadata              <- "data/automaticDetection/cultivation/summary_metadata.csv"


# Calling the function
source(file.path("R", "plotTestImageResultsFromAllTools.R"))
plotTestImageResultsFromAllTools(input_file_manual,
                                 input_file_cilium_numbers,
                                 input_file_detectCilia,
                                 input_file_ACDC,
                                 input_file_ciliaq,
                                 output_dir)

# Removing objects
rm(list = c("input_file_manual", "input_file_cilium_numbers",
            "input_file_detectCilia", "input_file_ACDC",
            "input_file_ciliaq", "output_dir"))

# 7 Detection of artificial cilia with all three tools #####################
# 7.1 Create images with horizontal cilia images ###########################
source(file.path("R", "createTestCiliumImage.R"))
createTestCiliumImage(output_dir = directory_artificial_images,
                      number_of_pixels_x_y = 100,
                      number_of_layers_z = 20,
                      cilium_length_in_pixels = 10,
                      cilium_width_in_pixels = 3,
                      cilium_height_in_pixels = 3,
                      relative_start_position = c(0.5,0.5,0.5),
                      rotatation_angles_degree = c(0,0,0),
                      gblur_sigma = 0,
                      cilium_color = "green")


