# Script for plotting horizontal length of artificial cilia         ++++++++
# using all three tools                                             ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/02/20
# Last changed: 2024/02/20


plotArtificialCiliaResultsFromAllTools <- function(
  input_file_detectCilia,
  input_file_ACDC,
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
  df_results_ACDC        <- readr::read_csv(file = input_file_ACDC, name_repair = "universal")
  df_results_ciliaQ      <- readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
  
  df_results_detectCilia$Tool <- "detectCilia"
  df_results_ACDC$Tool <- "ACDC"
  df_results_ciliaQ$Tool <- "CiliaQ"
  
  # Add blurring information
  if(any(grepl(pattern = "gblur", x = df_results_detectCilia$dirName, ignore.case = TRUE))){
    df_results_detectCilia$blurring <- as.numeric(
      gsub(pattern = ".+gblur_([[:digit:]]{0,1}\\.{0,1}[[:digit:]]{0,1})$",
           replacement = "\\1",
           x = df_results_detectCilia$dirName))
    
    
    df_results_ACDC$blurring <- as.numeric(
      gsub(pattern = ".+gblur_([[:digit:]]{0,1}\\.{0,1}[[:digit:]]{0,1})_z.+",
           replacement = "\\1",
           x = df_results_ACDC$fileName))
    
    df_results_ciliaQ$blurring <- as.numeric(
      gsub(pattern = ".+gblur_([[:digit:]]{0,1}\\.{0,1}[[:digit:]]{0,1})_z.+",
           replacement = "\\1",
           x = df_results_ciliaQ$file_name))
    
    # Keep only specific columns
    df_results_detectCilia2 <- df_results_detectCilia %>% 
      dplyr::select("dirName", "cilium", "blurring", "horizontal_length_in_pixels", "Tool")
    
    df_results_ACDC2 <- df_results_ACDC %>% 
      dplyr::select(dirName = "fileName", cilium = "Cilium.ID", "blurring", horizontal_length_in_pixels = "Total.Length.pixel.", "Tool")
    df_results_ACDC2$dirName <- gsub(pattern = "_z_projection\\.tif",replacement = "", x =  df_results_ACDC2$dirName)
    
    df_results_ciliaQ2 <- df_results_ciliaQ %>% 
      dplyr::select(dirName = "file_name", cilium = "ID", "blurring", horizontal_length_in_pixels = "cilia.length..micron.", "Tool")
    df_results_ciliaQ2$dirName <- gsub(pattern = "_z_projection_CQP_CQs",replacement = "", x =  df_results_ciliaQ2$dirName)
    
  }else{
    # Keep only specific columns
    df_results_detectCilia2 <- df_results_detectCilia %>% 
      dplyr::select("dirName", "cilium", "horizontal_length_in_pixels", "Tool")
    
    df_results_ACDC2 <- df_results_ACDC %>% 
      dplyr::select(dirName = "fileName", cilium = "Cilium.ID", horizontal_length_in_pixels = "Total.Length.pixel.", "Tool")
    df_results_ACDC2$dirName <- gsub(pattern = "_z_projection\\.tif",replacement = "", x =  df_results_ACDC2$dirName)
    
    df_results_ciliaQ2 <- df_results_ciliaQ %>% 
      dplyr::select(dirName = "file_name", cilium = "ID", horizontal_length_in_pixels = "cilia.length..micron.", "Tool")
    df_results_ciliaQ2$dirName <- gsub(pattern = "_z_projection_CQP_CQs",replacement = "", x =  df_results_ciliaQ2$dirName)
    
  }
  
  # Bind tibbles
  df_results <- dplyr::bind_rows(df_results_detectCilia2,
                                 df_results_ACDC2,
                                 df_results_ciliaQ2)
  
  rm(list = c("df_results_detectCilia2", "df_results_ACDC2", "df_results_ciliaQ2"))
  
  # Change order of tools
  df_results$Tool <- factor(df_results$Tool, levels = c("detectCilia", "ACDC", "CiliaQ"))
  
  # # Change the order of facets
  # df_results$cultivation <- factor(df_results$cultivation,
  #                                  levels = c("ITS", "ITS with Dexa",
  #                                             "ITS with Dexa + IGF + TGF", "FBS" ))
  
  # Plot results ###########################################################
  dir.create(output_dir, showWarnings = FALSE)
  
  # Plot all data ----------------------------------------------------------
  
  # Horizontal lengths of cilia
  if("blurring" %in% names(df_results)){
    plot_horizontal_length <- ggplot(df_results, aes(x=blurring, y=horizontal_length_in_pixels, color=Tool)) +
      geom_point(size = 4) +
      geom_line(linewidth = 1.5) +
      geom_hline(yintercept=10, linetype="dashed", color = "red", linewidth = 1) +
      theme_bw(base_size = 18) +
      scale_y_continuous(limits = c(0,25), breaks = scales::breaks_pretty()) +
      scale_color_manual(values=c("#009E73", "#762855", "#1e3a80")) +
      ylab("Horizontal cilium length in pixels") +
      xlab("Gaussian blurring standard deviation")
    
    
    ggsave(filename = file.path(output_dir, "all_tools_artificial_cilia_horizontal_lengths.pdf"),
           width = 297, height = 110, units = "mm")
    ggsave(filename = file.path(output_dir, "all_tools_artificial_cilia_horizontal_lengths.png"),
           width = 297, height = 110, units = "mm")
    # ggsave(filename = file.path(output_dir, "all_tools_artificial_cilia_horizontal_lengths.emf"),
    #        width = 297, height = 210, units = "mm", device = emf)
  }else{
    df_results$cilium <- df_results$cilium-1
    plot_horizontal_length <- ggplot(df_results, aes(x=cilium, y=horizontal_length_in_pixels, color=Tool)) +
      geom_point(size = 4) +
      geom_line(linewidth = 1.5) +
      geom_hline(yintercept=10, linetype="dashed", color = "red", linewidth = 1) +
      theme_bw(base_size = 18) +
      scale_y_continuous(limits = c(0,25), breaks = scales::breaks_pretty()) +
      scale_color_manual(values=c("#009E73", "#762855", "#1e3a80")) +
      ylab("Horizontal cilium length in pixels") +
      xlab("Cilium")
    
    
    ggsave(filename = file.path(output_dir, "all_tools_artificial_cilia_combined_horizontal_lengths.pdf"),
           width = 297, height = 110, units = "mm")
    ggsave(filename = file.path(output_dir, "all_tools_artificial_cilia_combined_horizontal_lengths.png"),
           width = 297, height = 110, units = "mm")
    # ggsave(filename = file.path(output_dir, "all_tools_artificial_cilia_combined_horizontal_lengths.emf"),
    #        width = 297, height = 210, units = "mm", device = emf)
  }
  
  
  
}

