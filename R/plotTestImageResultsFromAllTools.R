# Script for plotting horizontal length of test images              ++++++++
# of all 3 tools and manual readers                                 ++++++++
# Author: Kai Budde
# Created: 2023/04/20
# Last changed: 2023/04/20


plotTestImageResultsFromAllTools <- function(
  input_file_manual,
  input_file_cilium_numbers,
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
  pkgs <- c("tidyverse", "rquery", "ggbeeswarm")
  groundhog.library(pkgs, groundhog.day)
  
  # Import and clean data ####################################################
  df_results_manual         <- readr::read_csv(file = input_file_manual, name_repair = "universal")
  df_cilium_numbers_mapping <- readr::read_csv(file = input_file_cilium_numbers, name_repair = "universal")
  
  df_results_detectCilia <- readr::read_csv(file = input_file_detectCilia, name_repair = "universal")
  df_results_ACDC <- readr::read_csv(file = input_file_ACDC, name_repair = "universal")
  df_results_ciliaQ <- readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
  
  # Delete false positive cilium (manually labelled)
  print(paste("We are deleting ", sum(df_results_detectCilia$to_be_removed != "no"),
              " cilium(a) from the automatic cilia detection because we have manually ",
              "marked it being a non-cilium structure.", sep=""))
  
  df_results_detectCilia <- df_results_detectCilia[df_results_detectCilia$to_be_removed == "no",]
  
  # Add original file name to CiliaQ data
  df_results_ciliaQ$file_name_czi <- gsub(
    pattern = "(.+)_zstack_histogram_equalized_CQP_CQs",
    replacement = "\\1.czi",
    df_results_ciliaQ$file_name)
  
  # Add original file name to ACDC data
  df_results_ACDC$file_name_czi <- gsub(
    pattern = "(.+)_zstack_histogram_equalized\\.tif",
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
    dplyr::select("fileName", "cilium", "horizontal_length_in_pixels")
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
                  "horizontal_length_in_pixels", "tool" = "researcher")
  
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
  
  
  # Plot results ###########################################################
  dir.create(output_dir, showWarnings = FALSE)
  
  # Plot all data ----------------------------------------------------------
  
  # Horizontal lengths of cilia
  legend_name <- "Rater"
  plot_horizontal_length <- ggplot(df_combined, aes(x=tool, y=horizontal_length_in_pixels, color=tool, fill = Type)) +
    stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1, color = "black") +
    #geom_jitter(color="black", size=0.5, alpha=0.9) +
    # geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
    geom_beeswarm() +
    scale_color_manual(values=c("#762855", "#1e3a80", "#009E73", "#F0E442", "#CC79A7", "#E69F00"), name = legend_name) + 
    scale_fill_manual(values=c("grey90", "white")) +
    ylim(0,25) +
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
  
  
  ggsave(filename = paste(output_dir, "all_tools_cilia_horizontal_lengths.pdf", sep="/"),
         width = 442, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "all_tools_cilia_horizontal_lengths.png", sep="/"),
         width = 442, height = 210, units = "mm")
  
  
}

