# Script for calculating statistical values                         ++++++++
# Author: Kai Budde
# Created: 2022/06/09
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
pkgs <- c("tidyverse", "DescTools")
groundhog.library(pkgs, groundhog.day)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

input_file_manual <- "data/manualDetection/190815_AscDexa/originalFiles_csv/cilia_numbers_clemens_kai.csv"
# output_dir <- "plots/manualComparison"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Import and clean data ####################################################

# Manual detection results
df_results <-  readr::read_csv(file = input_file_manual, name_repair = "universal")

# Calculate statistics #####################################################

df_detected <- df_results
df_detected <- df_detected %>% 
  dplyr::filter(is.na(comments)) %>% 
  dplyr::select("Cilium_number_kai","Cilium_number_clemens")

df_detected$Cilium_number_kai[!is.na(df_detected$Cilium_number_kai)] <- 1
df_detected$Cilium_number_kai[is.na(df_detected$Cilium_number_kai)] <- 0
df_detected$Cilium_number_clemens[!is.na(df_detected$Cilium_number_clemens)] <- 1
df_detected$Cilium_number_clemens[is.na(df_detected$Cilium_number_clemens)] <- 0

df_rater_result <- data.frame("kai_0" = c(NA, NA), "kai_1" = c(NA, NA))
rownames(df_rater_result) <- c("clemens_0", "clemens_1")

df_rater_result$kai_0 <- c(sum(df_detected$Cilium_number_kai == 0 & df_detected$Cilium_number_clemens == 0),
                            sum(df_detected$Cilium_number_kai == 0 & df_detected$Cilium_number_clemens == 1))
df_rater_result$kai_1 <- c(sum(df_detected$Cilium_number_kai == 1 & df_detected$Cilium_number_clemens == 0),
                            sum(df_detected$Cilium_number_kai == 1 & df_detected$Cilium_number_clemens == 1))

df_rater_result$kai_0[1] <- 150

# ratertab <- xtabs(~df_detected$Cilium_number_kai + df_detected$Cilium_number_clemens)

DescTools::CohenKappa(x = as.matrix(df_rater_result), conf.level = 0.95)


