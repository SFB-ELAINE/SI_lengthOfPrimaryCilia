# Script for creating test cilia images                            +++++++++
# Author: Kai Budde
# Created: 2022/06/22
# Last changed: 2022/06/22


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")

# Load packages
library(groundhog)
pkgs <- c("tiff")
groundhog.library(pkgs, groundhog.day)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# Output directory
output_dir <- "CiliaQ/testImages/2D/"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Create test images (5x5) #################################################

horizontal_cilium <- matrix(data = c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ncol = 10, nrow = 10)

vertical_cilium <- matrix(data = c(rep(0,5), 1, 1, 1, 1, 1, rep(0, 15)),
                          nrow = 5, ncol = 5)

diagonal_cilium <- matrix(data = c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ncol = 10, nrow = 10)

y_cilium <- matrix(data = c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ncol = 10, nrow = 10)

c_cilium <- matrix(data = c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ncol = 10, nrow = 10)

dir.create(output_dir, showWarnings = FALSE)

tiff::writeTIFF(what = horizontal_cilium,
                where = paste0(output_dir, "horizontal_cilium.tif"),
                bits.per.sample = 8L,
                compression = "none",
                reduce = TRUE)

tiff::writeTIFF(what = vertical_cilium,
                where = paste0(output_dir, "vertical_cilium.tif"),
                bits.per.sample = 8L,
                compression = "none",
                reduce = TRUE)

tiff::writeTIFF(what = diagonal_cilium,
                where = paste0(output_dir, "diagonal_cilium.tif"),
                bits.per.sample = 8L,
                compression = "none",
                reduce = TRUE)

tiff::writeTIFF(what = y_cilium,
                where = paste0(output_dir, "y_cilium.tif"),
                bits.per.sample = 8L,
                compression = "none",
                reduce = TRUE)

tiff::writeTIFF(what = c_cilium,
                where = paste0(output_dir, "c_cilium.tif"),
                bits.per.sample = 8L,
                compression = "none",
                reduce = TRUE)

