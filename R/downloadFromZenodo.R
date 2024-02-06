# Script for downloading data from Zenodo                           ++++++++
# Author: Kai Budde-Sagert
# Created: 2023/12/08
# Last changed: 2023/12/08


downloadFromZenodo <- function(download_dir, doi){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("remotes")
  groundhog.library(pkgs, groundhog.day)
  
  remotes::install_github("eblondel/zen4R")
  dir.create(path = download_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Download files
  zen4R::download_zenodo(path = download_dir, doi)
}

