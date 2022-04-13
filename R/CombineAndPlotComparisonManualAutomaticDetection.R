# Script for combining manual and automatic results and plot them ++++++++++
# Author: Kai Budde
# Created: 2022/04/12
# Last changed: 2022/04/12

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()
# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")

# Load packages
library(groundhog)
pkgs <- c("tidyverse", "ggplot2")
groundhog.library(pkgs, groundhog.day)


# new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# require(dplyr)
# require(rquery)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Results from manual and automatic detection of 190815_AscDexa data
manual_detection_file <- "data/manualDetection/190815_AscDexa/output/df_manual_results_en.csv"
automatic_detection_file <- "data/automaticDetection/coating/summary_cilia.csv"
parameters_automatic_detection <- "data/automaticDetection/coating/summary_parameters.csv"

# Output directory
dir_output <- "data/manualDetection/190815_AscDexa/output"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Load data ################################################################
df_manual_detection <- readr::read_csv(file = manual_detection_file,
                                       name_repair = "universal")

df_automatic_detection <- readr::read_csv(file = automatic_detection_file,
                                          name_repair = "universal")

df_parameters_automatic_detection <- readr::read_csv(file = parameters_automatic_detection,
                                                     name_repair = "universal")

# Filter the automatic detection images ####################################

image_names <- df_manual_detection$Image_name
image_names <- gsub(pattern = "_[0-9]+$", replacement = "", x = image_names)
image_names <- image_names[!duplicated(image_names)]

df_test <- df_automatic_detection[
  grepl(pattern = image_names, x = df_automatic_detection$fileName,
        ignore.case = TRUE),]

# Convert to pixels and check parameters used ##############################

# Add meta information of automatic detection
df_automatic_detection <- dplyr::left_join(x = df_automatic_detection, y = df_parameters_automatic_detection, by = "fileName")


# Plot threshold_find values of automatic detection
plot_thresholds <- ggplot2::ggplot(data = df_automatic_detection,
                                   aes(x=threshold_find)) +
  geom_histogram() +
  xlim(c(0,0.1)) +
  labs(title = "Histogram of thresholds of automatic detection") +
  theme_bw()

ggsave(filename = paste(dir_output, "hist_threshold_automatic_detection.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(dir_output, "hist_threshold_automatic_detection.png", sep="/"),
       width = 297, height = 210, units = "mm")

# Convert to lengths to pixel values

df_automatic_detection$Horizontal_length_in_pixels <- 
  df_automatic_detection$horizontal_length / df_automatic_detection$pixel_size

df_automatic_detection$zstack_layers <- 
  df_automatic_detection$vertical_length / df_automatic_detection$slice_distance

# Prepare automatic detection tibble for binding with manual ###############

# Rename columns
df_automatic_detection <- df_automatic_detection %>% 
  dplyr::rename("Image_name" = "fileName",
                "Cilium_number_automatic" = "cilium",
                "Vertical_length_in_um" = "vertical_length",
                "Horizontal_length_in_um" = "horizontal_length",
                "Total_length_in_um" = "total_length")

# Add columns
df_automatic_detection$Image_name_short <- as.numeric(
  gsub(pattern = ".+_([0-9]{1,2}$)", replacement = "\\1",
       x = df_automatic_detection$Image_name, ignore.case = TRUE))

df_automatic_detection$Researcher <- "automatic"

# Keep specific columns that also occur in df_manual_detection
df_automatic_detection <- df_automatic_detection %>% 
  dplyr::select(names(df_automatic_detection)[names(df_automatic_detection)  %in% names(df_manual_detection)])

# Add data from automatic detection to manual det. tibble ##################

df_combined <- dplyr::add_row(df_manual_detection, df_automatic_detection)

# Save final tibble ########################################################

dir.create(dir_output, showWarnings = FALSE)
write.csv2(x = df_combined, file = paste(dir_output, "/df_combined_manual_automatic_results_de.csv", sep=""),
           row.names=FALSE)
write.csv(x = df_combined, file = paste(dir_output, "/df_combined_manual_automatic_results_en.csv", sep=""),
          row.names=FALSE)