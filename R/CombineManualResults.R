# Script for combining all results from manual detection +++++++++++++++++++
# Author: Kai Budde
# Created: 2022/03/30
# Last changed: 2022/06/09

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()
# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")

# Load packages
library(groundhog)
pkgs <- c("tidyverse", "rquery", "rqdatatable")
groundhog.library(pkgs, groundhog.day)


# new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# require(dplyr)
# require(rquery)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Directory with results
dir_with_csv_files <- "data/manualDetection/190815_AscDexa/originalFiles_csv"

# Output directory
dir_output <- "data/manualDetection/190815_AscDexa/"

# File name of metadata from the execution of readCzi
metadata_file <- "df_metadata_en.csv"

# Mapping of cilia numbers
cilia_mapping_file <- "cilia_numbers_clemens_kai.csv"

# Results names
manual_result_files <- c("kai_results", "clemens_results", "nadja_results")

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load meta data ###########################################################

df_metadata <- readr::read_csv(file = paste(
  gsub(pattern = "/originalFiles_csv", replacement = "",
       x = dir_with_csv_files, ignore.case = TRUE),
  metadata_file, sep = "/"),
  name_repair = "universal")

# Aggregate all needed information of manual detection #####################

input_files <- list.files(path = dir_with_csv_files, pattern = "csv", full.names = TRUE)
input_files <- input_files[
  grepl(pattern = paste(manual_result_files, collapse = "|"), x = input_files, ignore.case = TRUE)]


# Go through every result and build a large tibble with all of them
for(i in 1:length(input_files)){
  
  # Read files
  df_dummy <- readr::read_csv(file = input_files[i],
                              name_repair = "universal")
  
  # Remove columns with the name "old" in it
  df_dummy <- df_dummy %>% 
    dplyr::select(-grep(pattern = "old", x = names(df_dummy), ignore.case = TRUE))
  
  # Append information of file, researcher who's done the manual detection
  # and the pixel size
  
  if(dim(df_dummy)[1] > 0){
    researcher <- gsub(pattern = "(.+)_results.+", replacement = "\\1", x = basename(path = input_files[i]), ignore.case = TRUE)
    df_dummy <- cbind(Researcher = researcher, df_dummy)
    
    image_name <- gsub(pattern = ".+([0-9]{6,8}.+)\\.csv", replacement = "\\1", x = basename(path = input_files[i]), ignore.case = TRUE)
    image_name_short <- gsub(pattern = ".+_([0-9]{1,2}$)", replacement = "\\1", x = image_name, ignore.case = TRUE)
    df_dummy <- cbind(Image_name_short = image_name_short, df_dummy)
    df_dummy <- cbind(Image_name = image_name, df_dummy)
    
    df_dummy$horizontal_scaling_in_um <- df_metadata$scaling_x_in_um[grepl(pattern = paste(image_name, ".czi", sep=""), x = df_metadata$fileName, ignore.case = TRUE)]
    df_dummy$vertical_scaling_in_um   <- df_metadata$scaling_z_in_um[grepl(pattern = paste(image_name, ".czi", sep=""), x = df_metadata$fileName, ignore.case = TRUE)]
    
    # Rename cilium_number column (Depending whether Clemens' or Kai's
    # enumeration is used)
    if(researcher == "nadja"){
      researcher = "kai"
    }else if(researcher == "simone"){
      researcher = "clemens"
    }
    
    names(df_dummy)[names(df_dummy) == "Cilium_number"] <- paste("Cilium_number_",
                                                                 researcher, sep="")
    

    if(i == 1){
      df_detection_results <- df_dummy
    }else{
      df_detection_results <- dplyr::bind_rows(df_detection_results, df_dummy)
    }
  }
}

rm(df_dummy)

# Map the cilium numbers of Clemens to the other researchers ###############

# Relocate column
df_detection_results <- dplyr::relocate(.data = df_detection_results,
                                        Cilium_number_kai,
                                        .after = Cilium_number_clemens)
# Read mapping file
df_cilia_mapping <- readr::read_csv(file = paste(dir_with_csv_files, cilia_mapping_file, sep = "/"),
                                    name_repair = "universal")

# Join result and cilia mapping data frames
# kai_NAs <- !is.na(df_detection_results$Cilium_number_clemens) & is.na(df_detection_results$Cilium_number_kai)
df_detection_results_clemens <- df_detection_results[df_detection_results$Researcher == "clemens", ]
df_detection_results_clemens <- rquery::natural_join(a = df_detection_results_clemens, b = df_cilia_mapping, by = c("Image_name", "Cilium_number_clemens"), jointype= "FULL")
# df_detection_results_clemens$Cilium_number_nadja <- df_detection_results_clemens$Cilium_number_kai

# clemens_NAs <- is.na(df_detection_results$Cilium_number_clemens)
df_detection_results_kai <- df_detection_results[df_detection_results$Researcher == "kai", ]
# df_detection_results_kai$Cilium_number_nadja <- df_detection_results_kai$Cilium_number_kai
df_detection_results_kai <- rquery::natural_join(a = df_detection_results_kai, b = df_cilia_mapping, by = c("Image_name", "Cilium_number_kai"))

df_detection_results_nadja <- df_detection_results[df_detection_results$Researcher == "nadja", ]
# df_detection_results_nadja$Cilium_number_kai <- df_detection_results_nadja$Cilium_number_nadja
df_detection_results_nadja <- rquery::natural_join(a = df_detection_results_nadja, b = df_cilia_mapping, by = c("Image_name", "Cilium_number_kai"))

df_detection_results <- rbind(df_detection_results_clemens, df_detection_results_kai, df_detection_results_nadja)

rm(list = c("df_detection_results_clemens", "df_detection_results_kai", "df_detection_results_nadja"))

# Relocate column
df_detection_results <- dplyr::relocate(.data = df_detection_results, Image_name_short,
                                        .after = Image_name)

# Recalculate horizontal lengths of Clemens' ###############################
# There was a mistake when looking at the horizontal length (scaling
# of z direction was used instead of scaling or x/y direction

# df_detection_results[df_detection_results$Researcher == "clemens" &
#                        !is.na(df_detection_results$Researcher),] <-
#   df_detection_results %>%
#   dplyr::filter(Researcher == "clemens") %>%
#   dplyr::mutate(Horizontal_length_in_um = Horizontal_length_in_um * 2.19647042583026 /
#                   2.81399093816734)

# df_detection_results <- df_detection_results %>%
#   dplyr::mutate(Horizontal_length_in_um = Horizontal_length_in_um * 2.19647042583026 /
#            2.81399093816734,
#            Researcher == "clemens")

# Fill empty cells of "df_detection_results" ###############################

# Horizontal length in um
fill_these_cells <- is.na(df_detection_results$Horizontal_length_in_um)
df_detection_results$Horizontal_length_in_um[fill_these_cells] <- 
  df_detection_results$Horizontal_length_in_pixels[fill_these_cells] *
  df_detection_results$horizontal_scaling_in_um[fill_these_cells]

# Horizontal length in pixels
fill_these_cells <- is.na(df_detection_results$Horizontal_length_in_pixels)
df_detection_results$Horizontal_length_in_pixels[fill_these_cells] <- 
  df_detection_results$Horizontal_length_in_um[fill_these_cells] /
  df_detection_results$horizontal_scaling_in_um[fill_these_cells]

# Number of zstack layers
fill_these_cells <- is.na(df_detection_results$zstack_layers)
df_detection_results$zstack_layers[fill_these_cells] <- 
  df_detection_results$z_upper[fill_these_cells] -
  df_detection_results$z_lower[fill_these_cells] + 1

# Add some columns #########################################################

# Add vertical length in um
df_detection_results$Vertical_length_in_um <- 
  df_detection_results$zstack_layers *
  df_detection_results$vertical_scaling_in_um

# Add total length in um (using the Pythagorean theorem)
df_detection_results$Total_length_in_um <- 
  sqrt(df_detection_results$Vertical_length_in_um^2 +
         df_detection_results$Horizontal_length_in_um^2)

# Delete all rows that contain cilia at image borders ######################
df_detection_results <- df_detection_results %>%
  dplyr::filter(comments != "at border" | is.na(comments))


# Save resulting data frame ################################################

dir.create(dir_output, showWarnings = FALSE)
write.csv2(x = df_detection_results, file = paste(dir_output, "/df_manual_results_de.csv", sep=""),
           row.names=FALSE)
write.csv(x = df_detection_results, file = paste(dir_output, "/df_manual_results_en.csv", sep=""),
          row.names=FALSE)
