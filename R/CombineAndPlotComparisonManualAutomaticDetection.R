# Script for combining manual and automatic results and plot them ++++++++++
# Author: Kai Budde
# Created: 2022/04/12
# Last changed: 2022/04/14

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()
# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")

# Load packages
library(groundhog)
pkgs <- c("tidyverse", "ggplot2", "rquery")
groundhog.library(pkgs, groundhog.day)


# new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# require(dplyr)
# require(rquery)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Results from manual and automatic detection of 190815_AscDexa data
manual_detection_file <- "data/manualDetection/190815_AscDexa/output/df_manual_results_en.csv"
cilia_automatic_mapping_file <- "data/manualDetection/190815_AscDexa/cilia_numbers_clemens_automatic.csv"
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

df_mapping <- readr::read_csv(file = cilia_automatic_mapping_file,
                                        name_repair = "universal")

df_parameters_automatic_detection <- readr::read_csv(file = parameters_automatic_detection,
                                                     name_repair = "universal")

# Filter the automatic detection images ####################################

image_names <- df_manual_detection$Image_name
image_names <- gsub(pattern = "_[0-9]+$", replacement = "", x = image_names)
image_names <- image_names[!duplicated(image_names)]

df_automatic_detection <- df_automatic_detection[
  grepl(pattern = image_names, x = df_automatic_detection$fileName,
        ignore.case = TRUE),]

# Convert to pixels and check parameters used ##############################

# Add meta information of automatic detection
df_automatic_detection <- dplyr::left_join(x = df_automatic_detection, y = df_parameters_automatic_detection, by = "fileName")


# Plot threshold_find values of automatic detection
plot_thresholds <- ggplot2::ggplot(data = df_automatic_detection,
                                   aes(x=threshold_find)) +
  geom_histogram(binwidth = 0.001) +
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

# Keep specific columns that also occur in df_manual_detection
df_automatic_detection <- df_automatic_detection %>% 
  dplyr::select(c(names(df_automatic_detection)[names(df_automatic_detection)  %in% names(df_manual_detection)],
                  "Cilium_number_automatic"))

# Add columns
df_automatic_detection$Image_name_short <- as.numeric(
  gsub(pattern = ".+_([0-9]{1,2}$)", replacement = "\\1",
       x = df_automatic_detection$Image_name, ignore.case = TRUE))

df_automatic_detection$Researcher <- "automatic"

# Add cilia mapping for automatic cilia detection ##########################

# df_mapping <- df_mapping %>%
# select(-comments)

df_mapping$Cilium_number_automatic[
  df_mapping$Cilium_number_automatic == "NA"] <- NA

df_mapping$Cilium_number_automatic <- suppressWarnings(
  as.numeric(df_mapping$Cilium_number_automatic))

# Checking for mapping mistakes
# (It is not a mistake if in one image, there are two cilia detected
mapping_mistakes <-
  df_mapping[duplicated(paste(df_mapping$Image_name, df_mapping$Cilium_number_automatic, sep=" ")) &
               !is.na(df_mapping$Cilium_number_automatic),]

if(nrow(mapping_mistakes) > 0){
  print(paste("There might be a mapping mistake with the automatic data.",
              "The following cilium number(s) occurs more than once.", sep=" "))
  print(mapping_mistakes)
}

mapping_mistakes <-
  df_mapping[duplicated(paste(df_mapping$Image_name, df_mapping$Cilium_number_clemens, sep=" ")) &
               !is.na(df_mapping$Cilium_number_clemens),]

if(nrow(mapping_mistakes) > 0){
  print(paste("There might be a mapping mistake with the manual data.",
              "The following cilium number(s) occurs more than once.", sep=" "))
  print(mapping_mistakes)
}

df_automatic_detection <- rquery::natural_join(a = df_automatic_detection,
                                               b = df_mapping,
                                               by = c("Image_name", "Cilium_number_automatic"))

# Add data from automatic detection to manual det. tibble ##################

# Add missing columns
df_automatic_detection$Cilium_number_kai <- NA
df_automatic_detection$horizontal_scaling <- df_parameters_automatic_detection$pixel_size[1]*1e-6
df_automatic_detection$vertical_scaling <- df_parameters_automatic_detection$slice_distance[1]*1e-6
df_automatic_detection$z_lower <- NA
df_automatic_detection$z_upper <- NA
df_manual_detection$Cilium_number_automatic <- NA

df_combined <- dplyr::add_row(df_manual_detection, df_automatic_detection)

# Plots ####################################################################

images <- unique(df_combined$Image_name)

for(i in 1:length(images)){
  current_image <- images[i]
  
  df_dummy <- df_combined[df_combined$Image_name == current_image,]
  df_dummy$Cilium_number_clemens <- as.factor(df_dummy$Cilium_number_clemens)
  
  plot_horizontal <- ggplot(df_dummy, aes(x=Cilium_number_clemens, y=Horizontal_length_in_pixels, fill=Researcher)) +
    geom_bar(stat="identity", color="black", position=position_dodge())
  
  plot_vertical <- ggplot(df_dummy, aes(x=Cilium_number_clemens, y=zstack_layers, fill=Researcher)) +
    geom_bar(stat="identity", color="black", position=position_dodge())

  #TODO from here
  # Durchsichtig, wenn keine automatic cilium?!
  # horizontal tendentiell zu wenig, vertical tendentiell zu viel
}

# Save final tibble ########################################################

dir.create(dir_output, showWarnings = FALSE)
write.csv2(x = df_combined, file = paste(dir_output, "/df_combined_manual_automatic_results_de.csv", sep=""),
           row.names=FALSE)
write.csv(x = df_combined, file = paste(dir_output, "/df_combined_manual_automatic_results_en.csv", sep=""),
          row.names=FALSE)