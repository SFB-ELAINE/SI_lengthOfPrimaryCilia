# Script for plotting horizontal length of test images              ++++++++
# of all 3 tools and manual readers                                 ++++++++
# Author: Kai Budde-Sagert
# Created: 2023/04/20
# Last changed: 2024/07/24


plotTestImageResultsFromAllTools <- function(
  input_file_manual,
  input_file_cilium_numbers,
  input_file_detectCilia,
  input_file_ACDC,
  input_file_ciliaq,
  input_file_ciliaq_3d = NULL,
  output_dir){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse", "rquery", "ggbeeswarm", "devEMF")
  groundhog.library(pkgs, groundhog.day)
  
  # Import and clean data ####################################################
  df_results_manual         <- readr::read_csv(file = input_file_manual, name_repair = "universal")
  df_cilium_numbers_mapping <- readr::read_csv(file = input_file_cilium_numbers, name_repair = "universal")
  
  df_results_detectCilia <- readr::read_csv(file = input_file_detectCilia, name_repair = "universal")
  df_results_ACDC <- readr::read_csv(file = input_file_ACDC, name_repair = "universal")
  df_results_ciliaQ <- readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
  
  # Delete false positive cilium (manually labelled)
  # (We used the ACDC corrected file, therefore no cleaning is necessary.)
  df_results_detectCilia <- df_results_detectCilia[!grepl(pattern = "^yes$", x = df_results_detectCilia$to_be_removed, ignore.case = TRUE),]
  df_results_ciliaQ <- df_results_ciliaQ[!grepl(pattern = "^yes$", x = df_results_ciliaQ$to_be_removed, ignore.case = TRUE),]
  

  # Add original file name to CiliaQ data
  df_results_ciliaQ$file_name_czi <- gsub(
    pattern = "(.+)_projection_CQP_CQs",
    replacement = "\\1.czi",
    df_results_ciliaQ$file_name)
  
  # Add original file name to ACDC data
  df_results_ACDC$file_name_czi <- gsub(
    pattern = "(.+)_projection\\.tif",
    replacement = "\\1.czi",
    df_results_ACDC$fileName)
  
  # Calculate cilium length in pixels of CiliaQ results
  df_results_ciliaQ$horizontal_length_in_pixels <-
    df_results_ciliaQ$cilia.length..micron.
  
  for(i in unique(df_results_detectCilia$fileName)){
    current_line <- which(df_results_detectCilia$fileName == i)[1]
    pixels_per_micron <-
      df_results_detectCilia$horizontal_length_in_pixels[current_line] /
      df_results_detectCilia$horizontal_length_in_um[current_line]
    
    df_results_ciliaQ$horizontal_length_in_pixels[df_results_ciliaQ$file_name_czi == i] <- 
      df_results_ciliaQ$cilia.length..micron.[df_results_ciliaQ$file_name_czi == i] *
      pixels_per_micron
  }
  
  
  # Only keep specific columns
  df_results_detectCilia <- df_results_detectCilia %>% 
    dplyr::select("fileName", "cilium", "horizontal_length_in_pixels", "total_length_in_um")
  df_results_ACDC <- df_results_ACDC %>% 
    dplyr::select("fileName" = "file_name_czi", "cilium" = "Cilium.ID",
                  "horizontal_length_in_pixels" = "Manual.Total.Length.pixel.")
  df_results_ciliaQ <- df_results_ciliaQ %>% 
    dplyr::select("fileName" = "file_name_czi", "cilium" = "ID",
                  "horizontal_length_in_pixels")
  
  # Combine data
  df_results_detectCilia$tool <- "detectCilia"
  df_results_ACDC$tool <- "ACDC"
  df_results_ciliaQ$tool <- "CiliaQ"
  
  
  df_results_automatic <- dplyr::bind_rows(df_results_detectCilia,
                                           df_results_ACDC,
                                           df_results_ciliaQ)
  
  
  # Change order of tools
  df_results_automatic$tool <- factor(
    df_results_automatic$tool, levels = c("detectCilia", "ACDC", "CiliaQ"))
  
  # Add cultivation
  df_results_automatic$cultivation <- ""
  
  names_of_experiments <- c("ITS", "ITS with Dexa",
                            "ITS with Dexa + IGF + TGF",
                            "FBS")
  
  df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[1]
  df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[2]
  df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[3]
  df_results_automatic$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[4]
  
  if(sum(is.na(df_results_automatic$cultivation)) > 0){
    print("Something went wrong with naming the cultivation conditiosn.")
  }
  
  df_results_automatic$cultivation <- factor(df_results_automatic$cultivation, levels = names_of_experiments)
  
  # Filter data (remove NAs)
  df_results_automatic <- df_results_automatic[!is.na(df_results_automatic$horizontal_length_in_pixels),]
  
  # Filter data (remove outliers and NAs from CiliaQ and ACDC results)
  df_results_automatic <- df_results_automatic %>% 
    dplyr::group_by(tool, cultivation) %>% 
    dplyr::mutate(outlier = ( (horizontal_length_in_pixels <
                                 (quantile(horizontal_length_in_pixels, 1/4) -
                                    1.5*IQR(horizontal_length_in_pixels))) |
                                (horizontal_length_in_pixels >
                                   (quantile(horizontal_length_in_pixels, 3/4) +
                                      1.5*IQR(horizontal_length_in_pixels))))) %>% 
    # dplyr::mutate(outlier = ((abs(horizontal_length_in_pixels - median(horizontal_length_in_pixels)) > 2*sd(horizontal_length_in_pixels))) ) %>% 
    dplyr::ungroup()
  
  
  # Keep only results of seven test images
  name_of_test_images <- gsub(pattern = "(.+)_[0-9]{1,2}.czi$",
                              replacement = "\\1",
                              x = df_results_manual$fileName)
  name_of_test_images <- unique(name_of_test_images)
  if(length(name_of_test_images) > 1){
    print("Something went wrong. We have more than one test image name.")
  }
  
  df_results_automatic <- df_results_automatic[grepl(
    pattern = name_of_test_images,
    x = df_results_automatic$fileName, fixed = TRUE), ]
  
  
  # Filter manual reading
  df_results_manual <- df_results_manual[is.na(df_results_manual$comments) |
                                           !(df_results_manual$comments == "at border"),]
  
  # Keep specific columns
  df_results_manual <- df_results_manual %>% 
    dplyr::select("fileName" = "fileName", "cilium" = "cilium_number_clemens",
                  "horizontal_length_in_pixels", "tool" = "researcher",
                  "total_length_in_um")
  
  # Rename researchers
  df_results_manual$tool[df_results_manual$tool == "clemens"] <- "m1"
  df_results_manual$tool[df_results_manual$tool == "kai"] <- "m2"
  df_results_manual$tool[df_results_manual$tool == "nadja"] <- "m3"
  
  # Add cultivation
  df_results_manual$cultivation <- ""
  
  names_of_experiments <- c("ITS", "ITS with Dexa",
                            "ITS with Dexa + IGF + TGF",
                            "FBS")
  
  df_results_manual$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results_manual$fileName, fixed = TRUE)] <- names_of_experiments[1]
  df_results_manual$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results_manual$fileName, fixed = TRUE)] <- names_of_experiments[2]
  df_results_manual$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results_manual$fileName, fixed = TRUE)] <- names_of_experiments[3]
  df_results_manual$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results_manual$fileName, fixed = TRUE)] <- names_of_experiments[4]
  
  if(sum(is.na(df_results_manual$cultivation)) > 0){
    print("Something went wrong with naming the cultivation conditiosn.")
  }
  
  df_results_manual$cultivation <- factor(df_results_manual$cultivation, levels = names_of_experiments)
  
  # Combine manual and automatic results
  df_combined <- dplyr::add_row(df_results_automatic, df_results_manual)
  
  # Add information automatic of manual
  df_combined <- df_combined %>% 
    dplyr::mutate(Type=ifelse( (tool=="m1" | tool=="m2" | tool=="m3"),"manual","automatic"))
  
  # Add short image name
  df_combined$image_name_short <- as.numeric(
    gsub(pattern = ".+_([0-9]{1,2})\\.czi$", replacement = "\\1",
         x = df_combined$fileName, ignore.case = TRUE))
  
  df_combined$image_name_short <- as.factor(df_combined$image_name_short)
  
  # New facet label names for image_name_short variable
  image_name_short.lab <- c("Image 1", "Image 2", "Image 3", "Image 4", "Image 5", "Image 6", "Image 7")
  names(image_name_short.lab) <- c(1, 2, 3, 4, 5, 6, 7)
  
  # Rename tools
  df_combined$tool[df_combined$tool == "detectCilia"] <- "dc"
  df_combined$tool[df_combined$tool == "ACDC"] <- "AC\nDC"
  df_combined$tool[df_combined$tool == "CiliaQ"] <- "cq"
  
  df_combined$tool <- factor(df_combined$tool, levels = c("dc", "AC\nDC", "cq", "m1", "m2", "m3"))
  
  # Plot results ###########################################################
  dir.create(output_dir, showWarnings = FALSE)
  
  # Plot all data ----------------------------------------------------------
  
  
  # Horizontal lengths of cilia
  legend_name <- "Rater"
  plot_horizontal_length <- ggplot(df_combined, aes(x=tool, y=horizontal_length_in_pixels, color=tool, fill = Type)) +
    stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1, color = "black") +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    #geom_jitter(color="black", size=0.5, alpha=0.9) +
    # geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
    geom_beeswarm() +
    scale_color_manual(values=c("#009E73", "#762855", "#1e3a80", "#F0E442", "#CC79A7", "#E69F00"), name = legend_name) + 
    scale_fill_manual(values=c("grey90", "white")) +
    # ylim(0,20) +
    coord_cartesian(ylim=c(0, 25)) +
    theme_bw(base_size = 18) +
    # theme(legend.position = "none") +
    # theme(#axis.title.y=element_text(size=12),
    #   #axis.text.x = element_blank(), 
    #   axis.ticks.x = element_blank()) +
    ylab("Horizontal cilium length in pixels") +
    xlab("Rater") +
    facet_grid(.~image_name_short,
               labeller = labeller(image_name_short = image_name_short.lab)) +
    theme(strip.background = element_rect(fill = "white")) +
    EnvStats::stat_n_text(
      y.pos = 25,angle = 60,
      color = "black",
      text.box = FALSE
    )
  # scale_color_discrete(name="Rater")
  
  
  ggsave(filename = file.path(output_dir, "all_tools_cilia_horizontal_lengths.pdf"),
         width = 442, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_tools_cilia_horizontal_lengths.png"),
         width = 442, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_tools_cilia_horizontal_lengths.eps"),
         width = 442, height = 210, units = "mm", device="eps")
  # ggsave(filename = file.path(output_dir, "all_tools_cilia_horizontal_lengths.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  # Use 3D data ############################################################
  # Import and clean data ####################################################
  
  if(!is.na(input_file_ciliaq_3d)){
    
    df_results_ciliaQ_3d    <- readr::read_csv(file = input_file_ciliaq_3d,
                                               name_repair = "universal")
    
    # Delete false positive cilium (manually labelled)
    # (We used the ACDC corrected file, therefore no cleaning is necessary.)
    df_results_ciliaQ_3d <- df_results_ciliaQ_3d[!grepl(pattern = "^yes$", x = df_results_ciliaQ_3d$to_be_removed, ignore.case = TRUE),]
    
    
    df_results_ciliaQ_3d$file_name_czi <- gsub(
      pattern = "(.+)_CQP_CQs",
      replacement = "\\1.czi",
      df_results_ciliaQ_3d$file_name)
    
    df_results_ciliaQ_3d <- df_results_ciliaQ_3d %>% 
      dplyr::select("fileName" = "file_name_czi", "cilium" = "ID",
                    "total_length_in_um" = "cilia.length..micron.")
    
    df_results_ciliaQ_3d$tool <- "CiliaQ"
    
    # Combine data
    df_results_automatic_3d <- dplyr::bind_rows(
      df_results_detectCilia,
      df_results_ciliaQ_3d)
    
    
    # Change order of tools
    df_results_automatic_3d$tool <- factor(
      df_results_automatic_3d$tool, levels = c("detectCilia", "CiliaQ"))
    
    # Add cultivation
    df_results_automatic_3d$cultivation <- ""
    
    names_of_experiments <- c("ITS", "ITS with Dexa",
                              "ITS with Dexa + IGF + TGF",
                              "FBS")
    
    df_results_automatic_3d$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results_automatic_3d$fileName, fixed = TRUE)] <- names_of_experiments[1]
    df_results_automatic_3d$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results_automatic_3d$fileName, fixed = TRUE)] <- names_of_experiments[2]
    df_results_automatic_3d$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results_automatic_3d$fileName, fixed = TRUE)] <- names_of_experiments[3]
    df_results_automatic_3d$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results_automatic_3d$fileName, fixed = TRUE)] <- names_of_experiments[4]
    
    if(sum(is.na(df_results_automatic_3d$cultivation)) > 0){
      print("Something went wrong with naming the cultivation conditiosn.")
    }
    
    df_results_automatic_3d$cultivation <- factor(
      df_results_automatic_3d$cultivation, levels = names_of_experiments)
    
    # Filter data (remove NAs)
    df_results_automatic_3d <- df_results_automatic_3d[
      !is.na(df_results_automatic_3d$total_length_in_um),]
    
    # Filter data (remove outliers and NAs from CiliaQ and ACDC results)
    df_results_automatic_3d <- df_results_automatic_3d %>% 
      dplyr::group_by(tool, cultivation) %>% 
      dplyr::mutate(outlier = ( (total_length_in_um <
                                   (quantile(total_length_in_um, 1/4) -
                                      1.5*IQR(total_length_in_um))) |
                                  (total_length_in_um >
                                     (quantile(total_length_in_um, 3/4) +
                                        1.5*IQR(total_length_in_um))))) %>% 
      dplyr::ungroup()
    
    
    # Keep only results of seven test images
    name_of_test_images <- gsub(pattern = "(.+)_[0-9]{1,2}.czi$",
                                replacement = "\\1",
                                x = df_results_manual$fileName)
    name_of_test_images <- unique(name_of_test_images)
    if(length(name_of_test_images) > 1){
      print("Something went wrong. We have more than one test image name.")
    }
    
    df_results_automatic_3d <- df_results_automatic_3d[grepl(
      pattern = name_of_test_images,
      x = df_results_automatic_3d$fileName, fixed = TRUE), ]
    
    # Combine manual and automatic results
    df_combined <- dplyr::add_row(df_results_automatic_3d, df_results_manual)
    
    # Add information automatic of manual
    df_combined <- df_combined %>% 
      dplyr::mutate(Type=ifelse( (tool=="m1" | tool=="m2" | tool=="m3"),"manual","automatic"))
    
    # Add short image name
    df_combined$image_name_short <- as.numeric(
      gsub(pattern = ".+_([0-9]{1,2})\\.czi$", replacement = "\\1",
           x = df_combined$fileName, ignore.case = TRUE))
    
    df_combined$image_name_short <- as.factor(df_combined$image_name_short)
    
    # New facet label names for image_name_short variable
    image_name_short.lab <- c("Image 1", "Image 2", "Image 3", "Image 4", "Image 5", "Image 6", "Image 7")
    names(image_name_short.lab) <- c(1, 2, 3, 4, 5, 6, 7)
    
    # Rename tools
    df_combined$tool[df_combined$tool == "detectCilia"] <- "dc"
    df_combined$tool[df_combined$tool == "CiliaQ"] <- "cq"
    
    df_combined$tool <- factor(df_combined$tool, levels = c("dc", "cq", "m1", "m2", "m3"))
    
    # Plot results ###########################################################
    
    # Plot all data ----------------------------------------------------------
    
    # Total lengths of cilia
    legend_name <- "Rater"
    plot_total_length <- ggplot(df_combined, aes(x=tool, y=total_length_in_um, color=tool, fill = Type)) +
      stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
      geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1, color = "black") +
      stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
      #geom_jitter(color="black", size=0.5, alpha=0.9) +
      # geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
      geom_beeswarm() +
      scale_color_manual(values=c("#009E73", "#1e3a80", "#F0E442", "#CC79A7", "#E69F00"), name = legend_name) + 
      scale_fill_manual(values=c("grey90", "white")) +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      coord_cartesian(ylim=c(0, 9)) +
      # ylim(0, 10) +
      theme_bw(base_size = 18) +
      # theme(legend.position = "none") +
      # theme(#axis.title.y=element_text(size=12),
      #   #axis.text.x = element_blank(), 
      #   axis.ticks.x = element_blank()) +
      ylab("Total cilium length in \u03BCm") +
      xlab("Rater") +
      facet_grid(.~image_name_short,
                 labeller = labeller(image_name_short = image_name_short.lab)) +
      theme(strip.background = element_rect(fill = "white"),
            legend.position = "none") +
      EnvStats::stat_n_text(
        y.pos = 9, angle = 60,
        color = "black",
        text.box = FALSE
      )
    # scale_color_discrete(name="Rater")
    
    
    ggsave(filename = file.path(output_dir, "all_tools_cilia_total_lengths.pdf"),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, "all_tools_cilia_total_lengths.png"),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, "all_tools_cilia_total_lengths.eps"),
           width = 297, height = 210, units = "mm", device="eps")
    # ggsave(filename = file.path(output_dir, "all_tools_cilia_total_lengths.emf"),
    #        width = 297, height = 210, units = "mm", device = emf)
    
  }
  
  
}

