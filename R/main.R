# Main script for reproducing all results from the publication +++++++++++++
# "detectCilia: An R package for automated detection and       +++++++++++++
# 3D-measurement of primary cilia - Studying the influence of  +++++++++++++
# cultivation methods on the lengths of primary cilia"         +++++++++++++
# Author: Kai Budde-Sagert
# Created: 2022/10/11
# Last changed: 2024/04/08

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
directory_artificial_images <- file.path("E:", "PhD", "Data",
                                         "Cilia", "artificialCilia",
                                         "horizontal_blur")
directory_artificial_images_cshaped <- file.path("E:", "PhD", "Data",
                                                 "Cilia", "artificialCilia",
                                                 "c_shaped")

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
# pkgs <- c("coin", "devEMF", "EnvStats", "ggbeeswarm", "ggpubr", "rquery", "rstatix",
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

# Calling the function
source(file.path("R", "combineDetectCiliaResults.R"))
combineDetectCiliaResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 2.4 Check difference of replications #####################################

# Data input
input_file <-  file.path("data", "automaticDetection", "cultivation",
                         "summary_cilia.csv")

# Calling the function
source(file.path("R", "checkReplications.R"))
checkReplications(input_dir)

# Removing objects
rm(list = c("input_dir"))

# 2.5 Convert original results from manual detection of resolution #########
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


# 2.6 Combine results of manual detection from resolution images ###########

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


# 2.7 Download raw cultivation data from Zenodo ############################

# Data input
download_dir <- download_directory_cultivation
doi <- "10.5281/zenodo.7994588"

# Calling the function
source(file.path("R", "downloadFromZenodo.R"))
downloadFromZenodo(download_dir, doi)

# Removing objects
rm(list = c("download_dir", "doi"))

# 2.8 Automatic detection of cilia from cultivation images #################

# Data input
input_dir <- download_directory_cultivation
output_dir <- file.path("data", "automaticDetection", "cultivation")

# Calling the function
source(file.path("R", "automaticCiliaDetection.R"))
automaticCiliaDetection(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 2.9 Combine results of automatic detection from cultivation images #######

# Data input
input_dir <- download_directory_cultivation
output_dir <- file.path("data", "automaticDetection", "cultivation")

# Calling the function
source(file.path("R", "combineDetectCiliaResults.R"))
combineDetectCiliaResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 2.10 Calculate the number of found nuclei #################################
# Data input
input_file <- file.path("data", "automaticDetection", "cultivation", "summary_nuclei.csv")

# Calling the function
source(file.path("R", "numberOfNuclei.R"))
numberOfNuclei(input_file)

# Removing objects
rm(list = c("input_file"))


# 2.11 Convert original results from manual detection of cultivation #######
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


# 2.12 Combine results of manual detection from cultivation images #########

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

# !!! ATTENTION: You need to copy                                        !!!
# !!! "data/automaticDetection/cultivation/summary_cilia.csv" to         !!!
# !!! "data/automaticDetection/cultivation/summary_cilia_edited.csv" for !!!
# !!! manually editing the results (if wanted)                           !!!    

input_file <-  file.path("data", "automaticDetection", "cultivation",
                         "summary_cilia_edited.csv")
# Include original file to check whether the manual correction has changed
# any other input
input_file_compare <-  file.path("data", "automaticDetection",
                                 "cultivation", "summary_cilia.csv")
output_dir <-  file.path("plots", "automaticDetectionCultivation_corrected")

# Calling the function
source(file.path("R", "plotAutomaticDetection_Cultivation.R"))
plotAutomaticDetection_Cultivation(input_file, input_file_compare,
                                   output_dir,
                                   exclude_cilia_touching_z_borders = TRUE,
                                   corrected_data = TRUE)

output_dir <-  file.path("plots", "automaticDetectionCultivation_corrected_allz")
plotAutomaticDetection_Cultivation(input_file, input_file_compare,
                                   output_dir,
                                   exclude_cilia_touching_z_borders = FALSE,
                                   corrected_data = TRUE)

output_dir <-  file.path("plots", "automaticDetectionCultivation_uncorrected")
plotAutomaticDetection_Cultivation(input_file, input_file_compare,
                                   output_dir,
                                   exclude_cilia_touching_z_borders = FALSE,
                                   corrected_data = FALSE)

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
input_file_ciliaq3D              <- file.path("CiliaQ_3D", "ciliaq_data_edited.csv")

input_file_manual <- file.path("data", "manualDetection", "cultivation", "df_manual_results.csv")
input_file_cilium_numbers <- file.path("data", "manualDetection", "cultivation", "originalFiles_csv",
                                       "cilia_numbers_clemens_automatic.csv")

# Output directory
output_dir <- file.path("plots", "manualAutomaticComparison_cultivation")

# Calling the function
source(file.path("R", "plotComparisonManualAutomaticDetection_Cultivation.R"))
plotComparisonManualAutomaticDetection_Cultivation(
  input_file_automatic = input_file_automatic,
  input_file_automatic_parameters = input_file_automatic_parameters,
  input_file_metadata = input_file_metadata,
  input_file_manual = input_file_manual,
  input_file_cilium_numbers = input_file_cilium_numbers,
  input_file_ciliaq3D = input_file_ciliaq3D,
  output_dir = output_dir)

# Removing objects
rm(list = c("input_file_automatic", "input_file_automatic_parameters",
            "input_file_metadata", "input_file_manual",
            "input_file_cilium_numbers", "output_dir"))
if(exists("input_file_ciliaq3D")){
  rm(input_file_ciliaq3D)
}


# 3.3 Plot comparison of man&aut measurement of resolution images ##########

# Data input
input_file_automatic <- file.path("data", "automaticDetection",
                                  "resolution", "summary_cilia_edited.csv")
input_file_manual <- file.path("data", "manualDetection", "resolution",
                               "df_manual_results.csv")
remove_false_positives <- TRUE

# Output directory
output_dir <- file.path("plots", "manualAutomaticComparison_resolution_corrected")

# Calling the function
source(file.path("R", "plotComparisonManualAutomaticDetection_Resolution.R"))
plotComparisonManualAutomaticDetection_Resolution(
  input_file_automatic,
  input_file_manual,
  output_dir,
  remove_false_positives)

# Output directory
output_dir <- file.path("plots", "manualAutomaticComparison_resolution_uncorrected")
remove_false_positives <- FALSE

# Calling the function
plotComparisonManualAutomaticDetection_Resolution(
  input_file_automatic,
  input_file_manual,
  output_dir,
  remove_false_positives)

# Removing objects
rm(list = c("input_file_automatic", "input_file_manual", "output_dir", "remove_false_positives"))


# 4 Detection of cilia with ACDC ###########################################
# 4.1 Calculate z-stack projections and convert color layers      ##########
#     for ACDC analysis

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


# 4.2 Analyze z-stack projection images with ACDC ##########################

# This needs to be manually done because it cannot be invoked from R.
# See ACDC/ReadMe.txt for further information.

# 4.3 Read results as xlsx file and convert to csv #########################

input_file <- file.path("ACDC_corrected","Correction_Cilia Report 21-Jan-2024 09-41-30.xlsx")
output_dir <- "ACDC_corrected"

# Calling the function
source(file.path("R", "ACDC_readXLSX.R"))
ACDC_readXLSX(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))


input_file <- file.path("ACDC_uncorrected", "NoCorrection_Cilia Report 19-Jan-2024 19-49-57.xlsx")
output_dir <- "ACDC_uncorrected"

# Calling the function
source(file.path("R", "ACDC_readXLSX.R"))
ACDC_readXLSX(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))


# 4.4 Plot detection results of ACDC #######################################


# Corrected data
input_file_acdc <- file.path("ACDC_corrected","ciliaData.csv")
input_file_metadata <- file.path("data", "automaticDetection", "cultivation", "summary_metadata.csv")
output_dir <- file.path("plots","ACDC_corrected")

# Calling the function
source(file.path("R", "ACDC_plotAutomaticDetection.R"))
ACDC_plotAutomaticDetection(input_file_acdc, input_file_metadata, output_dir)

# Removing objects
rm(list = c("input_file_acdc", "input_file_metadata", "output_dir"))

# Uncorrected data
input_file_acdc <- file.path("ACDC_uncorrected","ciliaData.csv")
input_file_metadata <- file.path("data", "automaticDetection", "cultivation", "summary_metadata.csv")
output_dir <- file.path("plots","ACDC_uncorrected")

# Calling the function
source(file.path("R", "ACDC_plotAutomaticDetection.R"))
ACDC_plotAutomaticDetection(input_file_acdc, input_file_metadata, output_dir)

# Removing objects
rm(list = c("input_file_acdc", "input_file_metadata", "output_dir"))


# 5 Detection of 2D cilia with CiliaQ ######################################
# 5.1 Analyze z-stack projection images with CiliaQ ########################

# This needs to be manually done because it cannot be invoked from R.
# See CiliaQ/HowTo for further information.

# 5.2 Read CiliaQ results (txt files) and convert to csv ###################

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

# 5.3 Find false positives in the results of CiliaQ ########################

# This needs to be manually done.
print(paste0("Please copy 'ciliaq_data.csv' and rename it to ",
             "'ciliaq_data_edited.csv' and mark all false positives (last column)."))

# 5.4 Plot detection results of CiliaQ #####################################

input_file_ciliaq <- file.path("CiliaQ","ciliaq_data_edited.csv")
output_dir <- file.path("plots","ciliaQ_corrected")

# Calling the function
source(file.path("R", "ciliaQ_plotAutomaticDetection.R"))
ciliaQ_plotAutomaticDetection(input_file_ciliaq, output_dir, corrected_data = TRUE)

# Plot uncorrected data
output_dir <- file.path("plots","ciliaQ_uncorrected")

# Calling the function
ciliaQ_plotAutomaticDetection(input_file_ciliaq, output_dir, corrected_data = FALSE)

# Removing objects
rm(list = c("input_file_ciliaq", "output_dir"))

# 5.5 Analyze original czi files (in 3D) with CiliaQ #######################

# This needs to be manually done because it cannot be invoked from R.
# See CiliaQ/HowTo for further information. (Cilia channel: 3)

# 5.6 Read CiliaQ 3d results (txt files) and convert to csv ################

# Directory with analysis results (CiliaQ files)
input_dir <- file.path("E:", "PhD", "Data", "Cilia", "cultivationImages", "ciliaQ_3D")
# Output directory
output_dir <- "CiliaQ_3D"

# Calling the function
source(file.path("R", "ciliaQ_getResults.R"))
ciliaQ_getResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))


# 5.7 Find false positives in the resultsof  CiliaQ ########################

# This needs to be manually done.
print(paste0("Please copy 'ciliaq_data.csv' and rename it to ",
             "'ciliaq_data_edited.csv' and mark all false positives (last column)."))

# 5.8 Plot 3d detection results of CiliaQ ##################################

input_file_ciliaq <- file.path("CiliaQ_3D","ciliaq_data_edited.csv")
output_dir <- file.path("plots","ciliaQ_3D")

# Calling the function
source(file.path("R", "ciliaQ_plotAutomaticDetection.R"))
ciliaQ_plotAutomaticDetection(input_file_ciliaq, output_dir)

# Removing objects
rm(list = c("input_file_ciliaq", "output_dir"))


# 6 Comparison of detectCilia, ACDC, and CiliaQ ############################

# 6.1 Plot results of horizontal cilia length of detectCilia, ACDC, and CiliaQ ####
input_file_detectCilia <- file.path("data", "automaticDetection",
                                    "cultivation", "summary_cilia_edited.csv")
input_file_ACDC <- file.path("ACDC_corrected", "ciliaData.csv")
input_file_ciliaq <-file.path("CiliaQ", "ciliaq_data_edited.csv")
input_file_metadata <- file.path("data", "automaticDetection", "cultivation", "summary_metadata.csv")
# input_file_detectCilia <- file.path("plots", "automaticDetectionCultivation", "horizontalLength_detectCilia.csv")
# input_file_ACDC <- file.path("plots", "ACDC_corrected", "horizontalLength_ACDC.csv")
# input_file_ciliaq <- file.path("plots", "ciliaQ", "horizontalLength_ciliaQ.csv")
output_dir <- file.path("plots", "allTools2D")

# Calling the function
source(file.path("R", "plot2DResultsFromAllTools.R"))
plot2DResultsFromAllTools(input_file_detectCilia, input_file_ACDC,
                          input_file_ciliaq, output_dir)

# Removing objects
rm(list = c("input_file_detectCilia", "input_file_ACDC",
            "input_file_ciliaq", "output_dir"))


# 6.2 Plot results of total cilia length of detectCilia and CiliaQ ####
input_file_detectCilia <- file.path("data", "automaticDetection",
                                    "cultivation", "summary_cilia_edited.csv")
input_file_ciliaq <-file.path("CiliaQ_3D","ciliaq_data_edited.csv")
output_dir <- file.path("plots", "allTools3D")

# Calling the function
source(file.path("R", "plot3DResultsFromAllTools.R"))
plot3DResultsFromAllTools(input_file_detectCilia, input_file_ciliaq,
                          output_dir)

# Removing objects
rm(list = c("input_file_detectCilia",
            "input_file_ciliaq", "output_dir"))


# 6.3 Plot results of horizontal cilia length of detectCilia, ACDC, and CiliaQ of 7 test images only ####
input_file_manual <- file.path("data","manualDetection","cultivation",
                               "df_manual_results.csv")
input_file_cilium_numbers <- file.path("data","manualDetection",
                                       "cultivation",
                                       "originalFiles_csv",
                                       "cilia_numbers_clemens_automatic.csv")
input_file_detectCilia    <- file.path("data", "automaticDetection",
                                       "cultivation", "summary_cilia_edited.csv")
input_file_ACDC           <- file.path("ACDC_corrected","ciliaData.csv")
input_file_ciliaq         <- file.path("CiliaQ","ciliaq_data_edited.csv")
input_file_ciliaq_3d      <- file.path("CiliaQ_3D","ciliaq_data_edited.csv")
output_dir <- file.path("plots", "allToolsTestImages")


# Calling the function
source(file.path("R", "plotTestImageResultsFromAllTools.R"))
plotTestImageResultsFromAllTools(input_file_manual,
                                 input_file_cilium_numbers,
                                 input_file_detectCilia,
                                 input_file_ACDC,
                                 input_file_ciliaq,
                                 input_file_ciliaq_3d,
                                 output_dir)

# Removing objects
rm(list = c("input_file_manual", "input_file_cilium_numbers",
            "input_file_detectCilia", "input_file_ACDC",
            "input_file_ciliaq", "input_file_ciliaq_3d",
            "output_dir"))


# 7 Detection of artificial cilia with all three tools #####################

# 7.1 Create images with horizontal cilia images ###########################
# as well as one large image containing all the blurred cilia

number_of_images <- 50
blurring_start <- 0
blurring_end <- 5
number_of_images_per_row <- 10
number_of_rows <- number_of_images / number_of_images_per_row
number_of_pixels_x_y <- 100
number_of_layers_z <- 20
number_of_channels <- 3
i <- 1 # current column
j <- 1 # current row
start <- 1

# Create empty image
# empty_image <- array(data = 0, dim = c(number_of_images_per_row*number_of_pixels_x_y,
#                                        number_of_rows*number_of_pixels_x_y,
#                                        number_of_layers_z,
#                                        number_of_channels))
big_image <- EBImage::Image(data = array(data = 0, dim = c(number_of_images_per_row*number_of_pixels_x_y,
                                                           number_of_rows*number_of_pixels_x_y,
                                                           number_of_channels,
                                                           number_of_layers_z)), colormode = Color)

source(file.path("R", "createTestCiliumImage.R"))
for(current_blur in seq(from = blurring_start, to = blurring_end, by = (blurring_end-blurring_start)/number_of_images)){
  
  current_image <- createTestCiliumImage(output_dir = directory_artificial_images,
                                         cilium_shape = "horizontal",
                                         number_of_pixels_x_y = number_of_pixels_x_y,
                                         number_of_layers_z = number_of_layers_z,
                                         cilium_length_in_pixels = 10,
                                         cilium_width_in_pixels = 3,
                                         cilium_height_in_pixels = 3,
                                         relative_start_position = c(0.5,0.5,0.5),
                                         rotatation_angles_degree = c(0,0,0),
                                         gblur_sigma = current_blur,
                                         cilium_color = "green")
  
  
  # Add current image to empty image
  if(start <= number_of_images){
    big_image[((i-1)*number_of_pixels_x_y+1):(i*number_of_pixels_x_y),
              ((j-1)*number_of_pixels_x_y+1):(j*number_of_pixels_x_y),,] <- current_image
    
    if(i == number_of_images_per_row){
      i <- 1
      j <- j+1
    }else{
      i <- i+1
    }
  }
  
  start <- start + 1
  
}
# display(Image(big_image, colormode = Color))
output_dir <- file.path(gsub(pattern = basename(directory_artificial_images), replacement = "", x = directory_artificial_images), "horizontal_blur_combined", "horizontal_blur_combined")
dir.create(path = output_dir, showWarnings = FALSE, recursive = TRUE)

big_image <- Image(big_image, colormode = Color)

for(i in 1:number_of_layers_z){
  image_name <- paste0("combined_blurred_cilia",
                       "_z",
                       i,
                       ".tif")
  EBImage::writeImage(x = big_image[,,,i],
                      bits.per.sample = 16,
                      files = file.path(output_dir, image_name))
  
}

# EBImage::writeImage(x = Image(big_image, colormode = Color),
#                     bits.per.sample = 16,
#                     files = file.path(output_dir, "combined_blurred_cilia.tif"))
# Save final image

rm(list = c("big_image", "current_image", "current_blur", "number_of_images",
            "blurring_start", "blurring_end", "number_of_images_per_row",
            "number_of_rows", "number_of_pixels_x_y", "number_of_layers_z",
            "number_of_channels", "i", "j", "start"))

# 7.2 Automatic detection of cilia from synthetic cilia images #############

# Data input
input_dir <- directory_artificial_images

# Calling the function
source(file.path("R", "automaticCiliaDetection.R"))
automaticCiliaDetection(input_dir)

# Removing objects
rm(list = c("input_dir"))

# 7.3 Combine results of aut. detection from synthetic cilia images ########

# Data input
input_dir <- directory_artificial_images
output_dir <- file.path("data", "automaticDetection", "artificialCilia")

# Calling the function
source(file.path("R", "combineDetectCiliaResults.R"))
combineDetectCiliaResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 7.4 Copy all z-stack max projection into one folder ######################
# Data input
input_dir <- directory_artificial_images

# Calling the function
source(file.path("R", "copyAllProjectionImages.R"))
copyAllProjectionImages(input_dir)

# Removing objects
rm(list = c("input_dir"))

# 7.5 Analyze z-stack projection images with ACDC ##########################

# This needs to be manually done because it cannot be invoked from R.
# See ACDC_artificialCilia/ReadMe.txt for further information.

# 7.6 Read results as xlsx file and convert to csv #########################

input_file <- file.path("ACDC_artificialCilia","Cilia Report 05-Apr-2024 10-12-47.xlsx")
output_dir <- "ACDC_artificialCilia"

# Calling the function
source(file.path("R", "ACDC_readXLSX.R"))
ACDC_readXLSX(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))

# 7.7 Analyze z-stack projection images with CiliaQ ########################

# This needs to be manually done because it cannot be invoked from R.
# See CiliaQ/HowTo for further information.

# 7.8 Read CiliaQ results (txt files) and convert to csv ###################

# Directory with analysis results (CiliaQ files)
input_dir <- file.path("E:", "PhD", "Data", "Cilia", "artificialCilia", "horizontal_blur_zprojection_ciliaQ")
# Output directory
output_dir <- "CiliaQ_artificialCilia"

# Calling the function
source(file.path("R", "ciliaQ_getResults.R"))
ciliaQ_getResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 7.9 Plot results of artificialCilia detections ###########################
input_file_detectCilia <- file.path("data", "automaticDetection", "artificialCilia", "summary_cilia.csv")
input_file_ACDC        <- file.path("ACDC_artificialCilia","ciliaData.csv")
input_file_ciliaq      <- file.path("CiliaQ_artificialCilia", "ciliaq_data.csv")
output_dir             <- file.path("plots", "artificialCilia")

# Calling the function
source(file.path("R", "plotArtificialCiliaResultsFromAllTools.R"))
plotArtificialCiliaResultsFromAllTools(input_file_detectCilia,
                                       input_file_ACDC,
                                       input_file_ciliaq,
                                       output_dir)

# Removing objects
rm(list = c("input_file_detectCilia", "input_file_ACDC",
            "input_file_ciliaq", "output_dir"))



# 7.10 Automatic detection of combined cilia in combined image  ############
# Data input
input_dir <- file.path(
  gsub(pattern = basename(directory_artificial_images),
       replacement = "", x = directory_artificial_images),
  "horizontal_blur_combined")

# Calling the function
source(file.path("R", "automaticCiliaDetection.R"))
automaticCiliaDetection(input_dir = input_dir, number_of_expected_nulcei = 50)

# Removing objects
rm(list = c("input_dir"))

# 7.11 Combine results of aut. detection from combined cilia image #########

# Data input
input_dir <-  file.path(
  gsub(pattern = basename(directory_artificial_images),
       replacement = "", x = directory_artificial_images),
  "horizontal_blur_combined")
output_dir <- file.path("data", "automaticDetection", "artificialCilia_combined")

# Calling the function
source(file.path("R", "combineDetectCiliaResults.R"))
combineDetectCiliaResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 7.12 Copy all z-stack max projection of combined cilia into one folder ###
# Data input
input_dir <- file.path(
  gsub(pattern = basename(directory_artificial_images),
       replacement = "", x = directory_artificial_images),
  "horizontal_blur_combined")

# Calling the function
source(file.path("R", "copyAllProjectionImages.R"))
copyAllProjectionImages(input_dir)

# Removing objects
rm(list = c("input_dir"))

# 7.13 Analyze z-stack projection images of combined cilia with ACDC #######

# This needs to be manually done because it cannot be invoked from R.
# See ACDC_artificialCilia_combined/ReadMe.txt for further information.
# Manually change cilia numbering such that the cilia can be compared.

# 7.14 Read combined cilia results as xlsx file and convert to csv #########

input_file <- file.path("ACDC_artificialCilia_combined",
                        "Cilia Report 19-Apr-2024 11-01-29_editedCiliaNumbers.xlsx")
output_dir <- "ACDC_artificialCilia_combined"

# Calling the function
source(file.path("R", "ACDC_readXLSX.R"))
ACDC_readXLSX(input_file, output_dir)

# Removing objects
rm(list = c("input_file", "output_dir"))

# 7.15 Analyze z-stack projection images of combined cilia with CiliaQ #####

# This needs to be manually done because it cannot be invoked from R.
# See CiliaQ/HowTo for further information.

# 7.16 Read CiliaQ results of combined cilia and convert to csv ############

# Directory with analysis results (CiliaQ files)
input_dir <- file.path("E:", "PhD", "Data", "Cilia", "artificialCilia", "horizontal_blur_combined_zprojection_ciliaQ")
# Output directory
output_dir <- "CiliaQ_artificialCilia_combined"

# Calling the function
source(file.path("R", "ciliaQ_getResults.R"))
ciliaQ_getResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# Renumber the cilia IDs because CiliaQ enumerates them from bottom to top
# and detectCilia from top to bottom

# 7.17 Plot results of combined artificialCilia detections #################
input_file_detectCilia <- file.path("data", "automaticDetection", "artificialCilia_combined", "summary_cilia.csv")
input_file_ACDC        <- file.path("ACDC_artificialCilia_combined","ciliaData.csv")
input_file_ciliaq      <- file.path("CiliaQ_artificialCilia_combined", "ciliaq_data_editedCiliaNumbers.csv")
output_dir             <- file.path("plots", "artificialCilia_combined")

# Calling the function
source(file.path("R", "plotArtificialCiliaResultsFromAllTools.R"))
plotArtificialCiliaResultsFromAllTools(input_file_detectCilia,
                                       input_file_ACDC,
                                       input_file_ciliaq,
                                       output_dir)

# Removing objects
rm(list = c("input_file_detectCilia", "input_file_ACDC",
            "input_file_ciliaq", "output_dir"))



# 7.18 Create images with C-shaped cilia images ############################
source(file.path("R", "createTestCiliumImage.R"))
for(current_angle in seq(from = 0, to = 90, by = 5)){
  createTestCiliumImage(output_dir = directory_artificial_images_cshaped,
                        cilium_shape = "c",
                        number_of_pixels_x_y = 100,
                        number_of_layers_z = 20,
                        relative_start_position = c(0.5,0.5,0.5),
                        rotatation_angles_degree = c(0,current_angle,0),
                        gblur_sigma = 1,
                        cilium_color = "green")
}


# 7.19 Automatic detection of cilia from synthetic cilia images #############

# Data input
input_dir <- directory_artificial_images_cshaped

# Calling the function
source(file.path("R", "automaticCiliaDetection.R"))
automaticCiliaDetection(input_dir)

# Removing objects
rm(list = c("input_dir"))

# 7.20 Combine results of aut. detection from synthetic cilia images ########

# Data input
input_dir <- directory_artificial_images_cshaped
output_dir <- file.path("data", "automaticDetection", "artificialCilia_cshaped")

# Calling the function
source(file.path("R", "combineDetectCiliaResults.R"))
combineDetectCiliaResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 7.21 Copy all z-stack max projection into one folder ######################
# Data input
input_dir <- directory_artificial_images_cshaped

# Calling the function
source(file.path("R", "copyAllProjectionImages.R"))
copyAllProjectionImages(input_dir)

# Removing objects
rm(list = c("input_dir"))

# 7.22 Copy all tifs as one zstack image into one folder ###################
# Data input
input_dir <- directory_artificial_images_cshaped

# Calling the function
source(file.path("R", "copyAllTifsAsZstack.R"))
copyAllTifsAsZstack(input_dir)

# Removing objects
rm(list = c("input_dir"))

# 7.23 Analyze z-stack projection images with CiliaQ #######################

# This needs to be manually done because it cannot be invoked from R.
# See CiliaQ/HowTo for further information.

# 7.24 Read CiliaQ results (txt files) and convert to csv ##################

# Directory with analysis results (CiliaQ files)
input_dir <- file.path("E:", "PhD", "Data", "Cilia", "artificialCilia", "c_shaped_zstacks")
# Output directory
output_dir <- "CiliaQ_artificialCilia_cshaped"

# Calling the function
source(file.path("R", "ciliaQ_getResults.R"))
ciliaQ_getResults(input_dir, output_dir)

# Removing objects
rm(list = c("input_dir", "output_dir"))

# 7.25 Plot results of artificialCilia_c_shape detections ##################
input_file_detectCilia <- file.path("data", "automaticDetection", "artificialCilia_cshaped", "summary_cilia.csv")
input_file_ciliaq      <- file.path("CiliaQ_artificialCilia_cshaped", "ciliaq_data.csv")
output_dir             <- file.path("plots", "artificialCilia_cshaped")

# Calling the function
source(file.path("R", "plotArtificialCilia3DResults.R"))
plotArtificialCilia3DResults(input_file_detectCilia,
                             input_file_ciliaq,
                             output_dir)

# Removing objects
rm(list = c("input_file_detectCilia", "input_file_ACDC",
            "input_file_ciliaq", "output_dir"))
