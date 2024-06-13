# Script for plotting total lengths of simulated cilia              ++++++++
# using detectCilia and CiliaQ                                      ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/04/15
# Last changed: 2024/04/15


plotSimulatedCilia3DResults <- function(
  input_file_detectCilia,
  input_file_ciliaq,
  output_dir){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("EnvStats", "tidyverse", "rstatix", "ggpubr", "ggbeeswarm",
            "coin", "scales", "devEMF")
  groundhog.library(pkgs, groundhog.day)
  
  # Import and clean data ####################################################
  df_results_detectCilia <- readr::read_csv(file = input_file_detectCilia, name_repair = "universal")
  df_results_ciliaQ      <- readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
  
  df_results_detectCilia$Tool <- "detectCilia"
  df_results_ciliaQ$Tool <- "CiliaQ"
  
  # Add blurring information
  # df_results_detectCilia$blurring <- as.numeric(
  #   gsub(pattern = ".+gblur_([[:digit:]]{0,1}\\.{0,1}[[:digit:]]{0,1})$",
  #        replacement = "\\1",
  #        x = df_results_detectCilia$dirName))
  
  # df_results_ciliaQ$blurring <- as.numeric(
  #   gsub(pattern = ".+gblur_([[:digit:]]{0,1}\\.{0,1}[[:digit:]]{0,1})_z.+",
  #        replacement = "\\1",
  #        x = df_results_ciliaQ$file_name))
  
  # Add angle information
  df_results_detectCilia$rotation_angle <- as.numeric(
    gsub(pattern = ".+rotated_[[:digit:]]{1,3}_([[:digit:]]{1,3})_[[:digit:]]{1,3}_.+",
         replacement = "\\1",
         x = df_results_detectCilia$dirName))
  
  df_results_ciliaQ$rotation_angle <- as.numeric(
    gsub(pattern = ".+rotated_[[:digit:]]{1,3}_([[:digit:]]{1,3})_[[:digit:]]{1,3}_.+",
         replacement = "\\1",
         x = df_results_ciliaQ$file_name))
  
  # Keep only specific columns
  df_results_detectCilia2 <- df_results_detectCilia %>% 
    dplyr::select("dirName", "cilium", "rotation_angle", total_length_in_pixels= "total_length_in_um", "Tool")
  
  df_results_ciliaQ2 <- df_results_ciliaQ %>% 
    dplyr::select(dirName = "file_name", cilium = "ID", "rotation_angle", total_length_in_pixels = "cilia.length..micron.", "Tool")
  df_results_ciliaQ2$dirName <- gsub(pattern = "_z_projection_CQP_CQs",replacement = "", x =  df_results_ciliaQ2$dirName)
  
  # Bind tibbles
  df_results <- dplyr::bind_rows(df_results_detectCilia2,
                                 df_results_ciliaQ2)
  
  rm(list = c("df_results_detectCilia2", "df_results_ciliaQ2"))
  
  # Change order of tools
  df_results$Tool <- factor(df_results$Tool, levels = c("detectCilia", "CiliaQ"))
  
  # Plot results ###########################################################
  dir.create(output_dir, showWarnings = FALSE)
  
  # Plot all data ----------------------------------------------------------
  # Horizontal lengths of cilia
  plot_horizontal_length <- ggplot(df_results_detectCilia, aes(x=rotation_angle, y=horizontal_length_in_pixels)) +
    geom_point(size = 4, color = "#009E73") +
    geom_line(linewidth = 1.5, color = "#009E73") +
    geom_hline(yintercept=10, linetype="dashed", color = "red", linewidth = 1) +
    theme_bw(base_size = 18) +
    scale_x_continuous(limits = c(0,90), breaks = scales::breaks_pretty()) +
    scale_y_continuous(limits = c(0,25), breaks = scales::breaks_pretty()) +
    ylab("Horizontal cilium length in pixels") +
    xlab("Rotation angle in degrees (around y-axis)")
  
  ggsave(filename = file.path(output_dir, "3D_tools_simulated_cilia_horizontal_lengths.pdf"),
         width = 297, height = 110, units = "mm")
  ggsave(filename = file.path(output_dir, "3D_tools_simulated_cilia_horizontal_lengths.png"),
         width = 297, height = 110, units = "mm")
  # ggsave(filename = file.path(output_dir, "3D_tools_simulated_cilia_horizontal_lengths.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  # Total lengths of cilia
  plot_total_length <- ggplot(df_results, aes(x=rotation_angle, y=total_length_in_pixels, color=Tool)) +
    geom_point(size = 4) +
    geom_line(linewidth = 1.5) +
    geom_hline(yintercept=4+6*sqrt(2), linetype="dashed", color = "red", linewidth = 1) +
    theme_bw(base_size = 18) +
    scale_x_continuous(limits = c(0,90), breaks = scales::breaks_pretty()) +
    scale_y_continuous(limits = c(0,25), breaks = scales::breaks_pretty()) +
    scale_color_manual(values=c("#009E73", "#1e3a80")) +
    ylab("Total cilium length in pixels") +
    xlab("Rotation angle in degrees (around y-axis)")
  
  
  ggsave(filename = file.path(output_dir, "3D_tools_simulated_cilia_total_lengths.pdf"),
         width = 297, height = 110, units = "mm")
  ggsave(filename = file.path(output_dir, "3D_tools_simulated_cilia_total_lengths.png"),
         width = 297, height = 110, units = "mm")
  # ggsave(filename = file.path(output_dir, "3D_tools_simulated_cilia_total_lengths.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
}

