# Script for combining the results (csv data) from  the R package ++++++++++
# detectCilia                                                     ++++++++++
# Author: Kai Budde
# Created: 2021/06/24
# Last changed: 2022/06/13


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
library(groundhog)
pkgs <- c("tidyverse")
groundhog.library(pkgs, groundhog.day)


# Please adapt the following parameters ####################################

# Directory containing the outputs
input_dir <- "E:/PhD/Daten/Cilia/allImages"
#input_dir <- "Y:/Manuelle Aufnahmen + Markierung/20210301_Manuelle Bildauswertungen Cilien/output"
# input_dir <-"C:/Users/Kai/Documents/git/gitHub/detectCilia/tests"

directory_filter <- "none" #none #190815
directory_remove_filter <- "compare" #none

# Filter results directories ###############################################
output_dirs <- list.dirs(input_dir, recursive = TRUE)
# Remove input dir from list
output_dirs <- output_dirs[!(output_dirs == input_dir)]
output_dirs <- output_dirs[grepl(pattern = "output$", x = output_dirs, ignore.case = TRUE)]
if(directory_filter != "none"){
  output_dirs <- output_dirs[grepl(pattern = directory_filter, x = output_dirs)]
}
if(directory_remove_filter != "none"){
  output_dirs <- output_dirs[!grepl(pattern = directory_remove_filter, x = output_dirs)]
}

# Read csv files and combine them into one data frame ######################
for(i in 1:length(output_dirs)){
  
  if(i == 1){
    
    # Information of found cilia
    df_cilia <- readr::read_csv(file.path(output_dirs[i],"cilium_summary.csv"), name_repair = "universal")
    df_cilia$to_be_removed <- "yes_or_no"
    df_cilia$fileName <- basename(output_dirs[i])
    df_cilia <- df_cilia %>% 
      select("fileName", "cilium", "vertical_length_in_um", "horizontal_length_in_um", "total_length_in_um", "to_be_removed")
    
    # Information of nuclei
    df_nuclei <- readr::read_csv(file.path(output_dirs[i],"nuclei_number.csv"), name_repair = "universal")
    df_nuclei$corrected_number <- NA
    df_nuclei$fileName <- basename(output_dirs[i])
    df_nuclei <- df_nuclei[,c(3,1,2)]
    
    # Information of parameter values
    df_parameters <- readr::read_csv(file.path(output_dirs[i],"parameter_list.csv"), name_repair = "universal")
    # df_parameters <- as.data.frame(t(df_parameters_dummy[,-1]))
    # colnames(df_parameters) <- df_parameters_dummy$parameterNames
    # df_parameters$fileName <- basename(output_dirs[i])
    # df_parameters <- df_parameters[,c(13,c(1:12))]
    # rm(df_parameters_dummy)
    
  }else{
    
    # Information of found cilia
    df_cilia_dummy <- readr::read_csv(file.path(output_dirs[i],"cilium_summary.csv"), name_repair = "universal")
    df_cilia_dummy$to_be_removed <- "yes_or_no"
    df_cilia_dummy$fileName <- basename(output_dirs[i])
    df_cilia_dummy <- df_cilia_dummy %>% 
      select("fileName", "cilium", "vertical_length_in_um", "horizontal_length_in_um", "total_length_in_um", "to_be_removed")
    
    df_cilia <- rbind(df_cilia, df_cilia_dummy)
    
    # Information of nuclei
    df_nuclei_dummy <- readr::read_csv(file.path(output_dirs[i],"nuclei_number.csv"), name_repair = "universal")
    df_nuclei_dummy$corrected_number <- NA
    df_nuclei_dummy$fileName <- basename(output_dirs[i])
    df_nuclei_dummy <- df_nuclei_dummy[,c(3,1,2)]
    
    df_nuclei <- rbind(df_nuclei, df_nuclei_dummy)
    
    # Information of parameter values
    df_parameters_dummy <- readr::read_csv(file.path(output_dirs[i],"parameter_list.csv"), name_repair = "universal")
    # df_parameters_dummy2 <- as.data.frame(t(df_parameters_dummy[,-1]))
    # colnames(df_parameters_dummy2) <- df_parameters_dummy$parameterNames
    # df_parameters_dummy2$fileName <- basename(output_dirs[i])
    # df_parameters_dummy2 <- df_parameters_dummy2[,c(13,c(1:12))]
    
    df_parameters <- rbind(df_parameters, df_parameters_dummy)
    rm(list = c("df_cilia_dummy", "df_nuclei_dummy",
                "df_parameters_dummy"))
    
  }
  
}

# Remove "output" from file name ###########################################

df_cilia$fileName <- gsub(pattern = "_output$", replacement = "", x = df_cilia$fileName)
df_nuclei$fileName <- gsub(pattern = "_output$", replacement = "", x = df_nuclei$fileName)

# Save resulting csv files #################################################

path_to_save_data <- getwd()
path_to_save_data <- paste(path_to_save_data, "/data/automaticDetection/cultivation", sep="")

readr::write_csv(df_cilia,
                 file = file.path(path_to_save_data, "summary_cilia.csv", sep=""))
readr::write_csv2(df_cilia,
                  file = file.path(path_to_save_data, "summary_cilia_de.csv", sep=""))

readr::write_csv(df_nuclei,
                 file = file.path(path_to_save_data, "summary_nuclei.csv", sep=""))
readr::write_csv2(df_nuclei,
                  file = file.path(path_to_save_data, "summary_nuclei_de.csv", sep=""))

readr::write_csv(df_parameters,
                 file = file.path(path_to_save_data, "summary_parameters.csv", sep=""))
readr::write_csv2(df_parameters,
           file = file.path(path_to_save_data, "summary_parameters_de.csv", sep=""))
