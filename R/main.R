# Main script for reproducing all results from the publication +++++++++++++
# "detectCilia: An R package for automated detection and       +++++++++++++
# 3D-measurement of primary cilia - Studying the influence of  +++++++++++++
# cultivation methods on the lengths of primary cilia"         +++++++++++++
# Author: Kai Budde
# Created: 2022/10/11
# Last changed: 2023/03/27

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()



# 0. Check which version of R packages are installed #######################

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


# 2.4 Combine results of manual detection from resolution images ###########

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


# 2.7 Calculate the number of found nuclei #################################
# Data input
input_file <- "data/automaticDetection/cultivation/summary_nuclei.csv"

# Calling the function
source("R/numberOfNuclei.R")
numberOfNuclei(input_file)

# Removing objects
rm(list = c("input_file"))


# 2.8 Convert original results from manual detection of cultivation ########
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


# 2.9 Combine results of manual detection from cultivation images ##########

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



# 3 Plot results  ##########################################################

# 3.1 Plot results of automatic measurement of cultivation images ##########
#     and print a tibble with total lengths

# Data input

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file <- "data/automaticDetection/cultivation/summary_cilia_edited.csv"
output_dir <- "plots/automaticDetectionCultivation"

# Calling the function
source("R/plotAutomaticDetection_Cultivation.R")
plotAutomaticDetection_Cultivation(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))


# 3.2 Plot comparison of man&aut measurement of seven test images ##########

# Data input

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file_automatic             <- "data/automaticDetection/cultivation/summary_cilia_edited.csv"
input_file_automatic_parameters  <- "data/automaticDetection/cultivation/summary_parameters.csv"
input_file_metadata              <- "data/automaticDetection/cultivation/summary_metadata.csv"

input_file_manual <- "data/manualDetection/cultivation/df_manual_results.csv"
input_file_cilium_numbers <- "data/manualDetection/cultivation/originalFiles_csv/cilia_numbers_clemens_automatic.csv"

# Output directory
output_dir <- "plots/manualAutomaticComparison_cultivation"

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


# 3.3 Plot comparison of man&aut measurement of resolution images ##########

# Data input
input_file_automatic <- "data/automaticDetection/resolution/summary_cilia.csv"
input_file_manual <- "data/manualDetection/resolution/df_manual_results.csv"

# Output directory
output_dir <- "plots/manualAutomaticComparison_resolution"

# Calling the function
source("R/plotComparisonManualAutomaticDetection_Resolution.R")
plotComparisonManualAutomaticDetection_Resolution(
  input_file_automatic,
  input_file_manual,
  output_dir)

# Removing objects
rm(list = c("input_file_automatic", "input_file_manual", "output_dir"))



# 4 Detection of cilia with ACDC ###########################################
# 4.1 Stack and convert color layers for ACDC analysis #####################

# Data input
input_dir <- "E:/PhD/Daten/Cilia/allImages"
output_dir <- "ACDC"

# Calling the function
source("R/convertColorLayersForACDC.R")
convertColorLayersForACDC(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 4.2 Analyze stacked images with ACDC #####################################

# This needs to be manually done because it cannot be invoked from R.


# 4.3 Read results as xlsx file and convert to csv #########################

input_file <- file.path("ACDC","Cilia Report 19-Dec-2022 14-08-05.xlsx")
output_dir <- "ACDC"

# Calling the function
source("R/ACDC_readXLSX.R")
ACDC_readXLSX(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))


# 4.4 Plot detection results of ACDC #######################################

input_file_acdc <- file.path("ACDC","ciliaData.csv")
input_file_metadata <- "data/automaticDetection/cultivation/summary_metadata.csv"
output_dir <- file.path("plots","ACDC")

# Calling the function
source("R/ACDC_plotAutomaticDetection.R")
ACDC_plotAutomaticDetection(input_file_acdc, input_file_metadata, output_dir)

# Removing objects
rm(list = c("input_file_acdc", "input_file_metadata", "output_dir"))



# 5 Detection of cilia with ciliaQ #########################################
# 5.1 Analyze stacked images with ciliaQ ###################################

# This needs to be manually done because it cannot be invoked from R.


# 5.2 Read ciliaQ results (txt files) and convert to csv ###################

# Directory with analysis results (CiliaQ files)
input_dir <- "E:/PhD/Daten/Cilia/allImages/ACDC/histogram_equalized_converted_multistack"
# Output directory
output_dir <- "CiliaQ_horizontal"

# Calling the function
source("R/ciliaQ_getResults.R")
ciliaQ_getResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 5.3 Plot detection results of CiliaQ #####################################

input_file_ciliaq <- file.path("CiliaQ_horizontal","ciliaq_data.csv")
output_dir <- file.path("plots","ciliaQ")

# Calling the function
source("R/ciliaQ_plotAutomaticDetection.R")
ciliaQ_plotAutomaticDetection(input_file_ciliaq, output_dir)

# Removing objects
rm(list = c("input_file_ciliaq", "output_dir"))



# 6 Comparison of detectCilia, ACDC, and ciliaQ ############################
# 6.1 Plot results of horizontal cilia length of dc, ACDC, and ciliaQ ######
input_file_detectCilia <- file.path("plots", "automaticDetectionCultivation", "horizontalLength_detectCilia.csv")
input_file_ACDC <- file.path("plots", "ACDC", "horizontalLength_ACDC.csv")
input_file_ciliaq <- file.path("plots", "ciliaQ", "horizontalLength_ciliaQ.csv")
output_dir <- file.path("plots", "allTools")

# Calling the function
source("R/plotResultsFromAllTools.R")
plotResultsFromAllTools(input_file_detectCilia, input_file_ACDC,
                        input_file_ciliaq, output_dir)

# Removing objects
rm(list = c("input_file_ciliaq", "output_dir"))


