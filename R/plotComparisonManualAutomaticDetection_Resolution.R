# Script for working with and plotting results (from csv data) from ++++++++
# the R package detectCilia as well as manual detection results     ++++++++
# Author: Kai Budde
# Created: 2021/11/11
# Last changed: 2023/03/27


# Color schema
#009E73 -> detectCilia (dc)
#D55E00 -> ciliaQ
#56B4E9 -> ACDC
#F0E442 -> m1
#CC79A7 -> m2
#E69F00 -> m3


plotComparisonManualAutomaticDetection_Resolution <- function(
  input_file_automatic,
  input_file_manual,
  output_dir){
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  # install.packages("groundhog")
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse", "rstatix", "ggbeeswarm", "ggpubr")
  groundhog.library(pkgs, groundhog.day)
  
  
  # Import and clean data ####################################################
  
  legend_name <- "Rater"
  
  # Automatic detection (using detectCilia)
  df_results_auto <- readr::read_csv(file = input_file_automatic, name_repair = "universal")
  df_results_auto <- df_results_auto[df_results_auto$to_be_removed != "yes", ]
  
  # Manual detection (using Fiji)
  df_results_man <- readr::read_csv(file = input_file_manual, name_repair = "universal")
  
  # Add information of experiment
  df_results_auto$location <- as.numeric(gsub(pattern = ".+stack_([0-9]+)_.+",
                                              replacement = "\\1",
                                              x = df_results_auto$fileName))
  # df_results_auto$location <- "TBA"
  # df_results_auto$location[grepl(pattern = "_1$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1"
  # df_results_auto$location[grepl(pattern = "_1_fail half", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1"
  # df_results_auto$location[grepl(pattern = "_2_looks ok$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "2"
  # df_results_auto$location[grepl(pattern = "_3_speed7$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "3"
  # df_results_auto$location[grepl(pattern = "_4_iO$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "4"
  # df_results_auto$location[grepl(pattern = "_5$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "5"
  
  df_results_auto$resolution <- as.numeric(gsub(pattern = ".+_[0-9]{4}x([0-9]{4})\\.czi",
                                                replacement = "\\1",
                                                x = df_results_auto$fileName))
  # df_results_auto$resolution <- "TBA"
  # df_results_auto$resolution[grepl(pattern = "1024", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1024"
  # df_results_auto$resolution[grepl(pattern = "2048", x = df_results_auto$fileName, ignore.case = TRUE)] <- "2048"
  # df_results_auto$resolution[grepl(pattern = "4096", x = df_results_auto$fileName, ignore.case = TRUE)] <- "4096"
  
  df_results_auto$magnification <- as.numeric(gsub(pattern = ".+_([0-9]+)x_z-stack.+",
                                                   replacement = "\\1",
                                                   x = df_results_auto$fileName))
  # df_results_auto$magnification <- "63x"
  # df_results_auto$magnification[grepl(pattern = "_100er", x = df_results_auto$fileName, ignore.case = TRUE)] <- "100x"
  
  df_results_auto$detectionMethod <- "automatic"
  
  
  
  
  # Add information of experiment
  df_results_man$location <- as.numeric(gsub(pattern = ".+stack_([0-9]+)_.+",
                                             replacement = "\\1",
                                             x = df_results_man$fileName))
  df_results_man$resolution <- as.numeric(gsub(pattern = ".+_[0-9]{4}x([0-9]{4})\\.czi",
                                               replacement = "\\1",
                                               x = df_results_man$fileName))
  df_results_man$magnification <- as.numeric(gsub(pattern = ".+_([0-9]+)x_z-stack.+",
                                                  replacement = "\\1",
                                                  x = df_results_man$fileName))
  df_results_man$detectionMethod <- "manual"
  
  # Combination of automatic and manual detection
  keep_the_columns <- c("fileName", "total_length_in_um", "horizontal_length_in_um", "vertical_length_in_um", "location", "resolution", "magnification", "detectionMethod")
  
  df_results <- dplyr::bind_rows(df_results_auto[,keep_the_columns],
                                 df_results_man[,keep_the_columns])
  rownames(df_results) <- NULL
  
  
  # df_results$image <- paste("loc", df_results$location, "\n", "magn", df_results$magnification, "\n", "res", df_results$resolution, sep = "")
  df_results$image <- paste("m", df_results$magnification, "\n", "r", df_results$resolution, sep = "")
  df_results$image <- factor(df_results$image, levels = c("m63\nr1024", "m100\nr1024", "m63\nr2048", "m63\nr4096"))
  
  
  df_results$location <- paste0("Location ", df_results$location)
  
  # Add missing rows such that the plot is equally designed for every
  # x-value even if a detectionMethod is missing
  
  #df_results <- tibble::add_row(.data = df_results, total_length = 0, image = "loc1\nmagn63x\nres2048", detectionMethod = "manual")
  
  # Remove unnecessary tibbles
  rm(list = c("df_results_auto", "df_results_man"))
  
  # Create directory for saving plots
  dir.create(output_dir, showWarnings = FALSE)
  
  # Calculate statistics ###################################################

  # New function for changing the x values of a free facet
  change_x_position_of_facets <- function(df, facet_variable = "location"){
    df <- df %>% 
      dplyr::group_by(location) %>%
      dplyr::rename(x_old = x) %>% 
      dplyr::arrange(x_old) %>% 
      dplyr::mutate(x=1:length(x_old)) %>%
      dplyr::mutate(xmin=xmin-x_old+x) %>%
      dplyr::mutate(xmax=xmax-x_old+x) %>%
      dplyr::arrange(location) %>% 
      dplyr::ungroup()
    
    return(df)
  }
  
  # Horizontal lengths #---------------------------------------------------#

  detection_results_horizontal <- df_results %>%
    dplyr::group_by(location, resolution, magnification, detectionMethod) %>%
    rstatix::get_summary_stats(horizontal_length_in_um, type = "mean_sd")
  
  print(paste("The results of the cilia horizontal lengths measurements are:"))
  print(detection_results_horizontal)
  
  # Check for normality
  
  df_normality_horizontal <- df_results %>% 
    dplyr::group_by(location, resolution, magnification, detectionMethod) %>% 
    rstatix::shapiro_test(horizontal_length_in_um)
  
  df_normality_horizontal$data_normally_distributed <- FALSE
  df_normality_horizontal$data_normally_distributed[df_normality_horizontal$p > 0.05] <- TRUE
  
  print("According to the Shapiro-Wilk test, the data (horizontal length) of the following groups are normally distributed:")
  print(df_normality_horizontal)
  
  # Check differences
  
  if(all(df_normality_horizontal$data_normally_distributed)){
    
    # t test
    statistical_test_result_horizontal <- df_results %>%
      dplyr::group_by(location, image) %>%
      rstatix::t_test(horizontal_length_in_um ~ detectionMethod) %>% 
      rstatix::add_significance("p") %>% 
      rstatix::add_xy_position(x = "image", dodge = 0.75)
    
  }else{
    
    # Wilcoxon signed-rank test
    statistical_test_result_horizontal <- df_results %>%
      dplyr::group_by(location, image) %>%
      rstatix::wilcox_test(horizontal_length_in_um ~ detectionMethod) %>%
      rstatix::add_significance("p") %>% 
      rstatix::add_xy_position(x = "image", dodge = 0.75)
    
  }
  
  statistical_test_result_horizontal <- change_x_position_of_facets(
    df = statistical_test_result_horizontal)
  

  # Vertical lengths #-----------------------------------------------------#
  
  detection_results_vertical <- df_results %>%
    dplyr::group_by(location, resolution, magnification, detectionMethod) %>%
    rstatix::get_summary_stats(vertical_length_in_um, type = "mean_sd")
  
  print(paste("The results of the cilia horizontal lengths measurements are:"))
  print(detection_results_vertical)
  
  # Check for normality
  
  df_normality_vertical <- df_results %>% 
    dplyr::group_by(location, resolution, magnification, detectionMethod) %>% 
    rstatix::shapiro_test(vertical_length_in_um)
  
  df_normality_vertical$data_normally_distributed <- FALSE
  df_normality_vertical$data_normally_distributed[df_normality_vertical$p > 0.05] <- TRUE
  
  print("According to the Shapiro-Wilk test, the data (horizontal length) of the following groups are normally distributed:")
  print(df_normality_vertical)
  
  # Check differences
  
  if(all(df_normality_vertical$data_normally_distributed)){
    
    # t test
    statistical_test_result_vertical <- df_results %>%
      dplyr::group_by(location, image) %>%
      rstatix::t_test(vertical_length_in_um ~ detectionMethod) %>% 
      rstatix::add_significance("p") %>% 
      rstatix::add_xy_position(x = "image", dodge = 0.75)
    
  }else{
    
    # Wilcoxon signed-rank test
    statistical_test_result_vertical <- df_results %>%
      dplyr::group_by(location, image) %>%
      rstatix::wilcox_test(vertical_length_in_um ~ detectionMethod) %>%
      rstatix::add_significance("p") %>%
      rstatix::add_xy_position(x = "image", dodge = 0.75)
  }
  
  statistical_test_result_vertical <- change_x_position_of_facets(
    df = statistical_test_result_vertical)
  
  
  
  # Total lengths #--------------------------------------------------------#
  
  detection_results_total <- df_results %>%
    dplyr::group_by(location, resolution, magnification, detectionMethod) %>%
    rstatix::get_summary_stats(total_length_in_um, type = "mean_sd")
  
  print(paste("The results of the cilia horizontal lengths measurements are:"))
  print(detection_results_total)
  
  # Check for normality
  
  df_normality_total <- df_results %>% 
    dplyr::group_by(location, resolution, magnification, detectionMethod) %>% 
    rstatix::shapiro_test(total_length_in_um)
  
  df_normality_total$data_normally_distributed <- FALSE
  df_normality_total$data_normally_distributed[df_normality_total$p > 0.05] <- TRUE
  
  print("According to the Shapiro-Wilk test, the data (horizontal length) of the following groups are normally distributed:")
  print(df_normality_total)
  
  # Check differences
  
  if(all(df_normality_total$data_normally_distributed)){
    
    # t test
    statistical_test_result_total <- df_results %>%
      dplyr::group_by(location, image) %>%
      rstatix::t_test(total_length_in_um ~ detectionMethod) %>% 
      rstatix::add_significance("p") %>% 
      rstatix::add_xy_position(x = "image", dodge = 0.75)
    
  }else{
    
    # Wilcoxon signed-rank test
    statistical_test_result_total <- df_results %>%
      dplyr::group_by(location, image) %>%
      rstatix::wilcox_test(total_length_in_um ~ detectionMethod) %>%
      rstatix::add_significance("p") %>% 
      rstatix::add_xy_position(x = "image", dodge = 0.75)
    
  }
  
  statistical_test_result_total <- change_x_position_of_facets(
    df = statistical_test_result_total)
  

  # # Mean length depending on resolution/magnification
  # df_mean_lengths <- df_results2 %>%
  #   dplyr::group_by(detectionMethod, image) %>%
  #   dplyr::summarise(mean_in_um = mean(total_length_in_um)) %>%
  #   ungroup()
  # 
  # 
  # df_lengths_with_uncertainty <- df_results2 %>%
  #   dplyr::group_by(detectionMethod, image) %>%
  #   do(model = confint(lm(total_length_in_um ~ 1, data = .)), level = 0.95)
  # 
  # df_lengths_with_uncertainty$mean <- unlist(lapply(X = df_lengths_with_uncertainty$model, mean))
  # df_lengths_with_uncertainty$confInterval <- (unlist(lapply(X = df_lengths_with_uncertainty$model, max)) -
  #                                                unlist(lapply(X = df_lengths_with_uncertainty$model, min))) / 2
  # 
  # print(df_lengths_with_uncertainty)
  # 
  # # Calculate relative differences of the results
  # 
  # df_relative_difference <- df_results %>%
  #   dplyr::group_by(fileName, detectionMethod, image) %>%
  #   dplyr::summarise(mean_in_um = mean(total_length_in_um)) %>%
  #   ungroup()
  # 
  # df_relative_difference <- df_relative_difference %>%
  #   dplyr::group_by(image) %>%
  #   dplyr::summarise(relDiff = mean_in_um[detectionMethod == "manual"]/mean_in_um[detectionMethod == "automatic"]-1)
  # 
  # df_relative_difference_resolution <- df_relative_difference
  # df_relative_difference_resolution$image <- gsub(pattern = "loc[0-9]\\n(.+)", replacement = "\\1", x = df_relative_difference_resolution$image)
  # df_relative_difference_resolution <- df_relative_difference_resolution %>%
  #   dplyr::group_by(image) %>%
  #   dplyr::summarise(average_relDiff = mean(relDiff)) %>%
  #   ungroup()
  # 
  # print(df_relative_difference_resolution)
  # 
  
  # Plot results ###########################################################
  
  give_n <- function(x){
    # return(data.frame(y = 2*median(x), label=paste0("n = ",length(x))))
    return(data.frame(y = 5, label=paste0("n=",length(x))))
  }
  
  # Horizontal lengths of cilia
  plot_horizontal_length <- ggplot(df_results, aes(x=image, y=horizontal_length_in_um, group = interaction(detectionMethod, image))) +
    stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    geom_boxplot(aes(fill=detectionMethod), alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1, color = "black") +
    scale_fill_manual(labels = c("dc", "m2"), values=c("grey90", "white"), name = legend_name) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, position = position_dodge(width = 0.75), fill="black") +
    geom_beeswarm(aes(color=detectionMethod), dodge.width=0.75, corral = "random", corral.width = 0.3) +
    scale_color_manual(labels = c("dc", "m2"), values=c("#009E73", "#CC79A7"), name = legend_name) +
    ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      axis.text.x = element_text(size=10),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal cilium length in \u03BCm") +
    xlab("Magnification and Resolution") +
    facet_grid(.~location, scales = "free") +
    theme(strip.background = element_rect(fill = "white")) +
    stat_pvalue_manual(data = statistical_test_result_horizontal,  tip.length = 0.01, hide.ns = TRUE) +
    stat_summary(fun.data = give_n, geom = "text", position = position_dodge(width = 0.75), angle = 60)
  
  # print(plot_horizontal_length)
  
  ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_horizontal_length.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_horizontal_length.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  # Vertical lengths of cilia
  plot_vertical_length <- ggplot(df_results, aes(x=image, y=vertical_length_in_um, group = interaction(detectionMethod, image))) +
    stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    geom_boxplot(aes(fill=detectionMethod), alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1, color = "black") +
    scale_fill_manual(labels = c("dc", "m2"), values=c("grey90", "white"), name = legend_name) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, position = position_dodge(width = 0.75), fill="black") +
    geom_beeswarm(aes(color=detectionMethod), dodge.width=0.75, corral = "random", corral.width = 0.3) +
    scale_color_manual(labels = c("dc", "m2"), values=c("#009E73", "#CC79A7"), name = legend_name) +
    ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      axis.text.x = element_text(size=10),
      axis.ticks.x = element_blank()) +
    ylab("Vertical cilium length in \u03BCm") +
    xlab("Magnification and Resolution") +
    facet_grid(.~location, scales = "free") +
    theme(strip.background = element_rect(fill = "white")) +
    stat_pvalue_manual(data = statistical_test_result_vertical,  tip.length = 0.01, hide.ns = TRUE) +
    stat_summary(fun.data = give_n, geom = "text", position = position_dodge(width = 0.75), angle = 60)
  
  # print(plot_vertical_length)
  
  ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_vertical_length.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_vertical_length.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  # Total lengths of cilia
  plot_total_length <- ggplot(df_results, aes(x=image, y=total_length_in_um, group = interaction(detectionMethod, image))) +
    stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    geom_boxplot(aes(fill=detectionMethod), alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1, color = "black") +
    scale_fill_manual(labels = c("dc", "m2"), values=c("grey90", "white"), name = legend_name) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, position = position_dodge(width = 0.75), fill="black") +
    geom_beeswarm(aes(color=detectionMethod), dodge.width=0.75, corral = "random", corral.width = 0.3) +
    scale_color_manual(labels = c("dc", "m2"), values=c("#009E73", "#CC79A7"), name = legend_name) +
    ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      axis.text.x = element_text(size=10),
      axis.ticks.x = element_blank()) +
    ylab("Total cilium length in \u03BCm") +
    xlab("Magnification and Resolution") +
    facet_grid(.~location, scales = "free") +
    theme(strip.background = element_rect(fill = "white")) +
    stat_pvalue_manual(data = statistical_test_result_total,  tip.length = 0.01, hide.ns = TRUE) +
    stat_summary(fun.data = give_n, geom = "text", position = position_dodge(width = 0.75), angle = 60)

  # print(plot_total_length)
  
  ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_total_length.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_total_length.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  # # Relative deviation of total lengths of cilia
  # plot_relative_differences_length <- ggplot(df_relative_difference, aes(x=image, y=relDiff)) +
  #   geom_point(size = 4) +
  #   theme_bw(base_size = 18) +
  #   geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.5) +
  #   theme(#axis.title.y=element_text(size=12),
  #     #axis.text.x = element_blank(), 
  #     axis.ticks.x = element_blank()) +
  #   ylim(-0.3,0.3) +
  #   ylab("Relative deviation of mean total lengths (manual/automatic)") +
  #   xlab("Magnification and Resolution") +
  #   facet_grid(.~location) +
  #   theme(strip.background = element_rect(fill = "white"))
  # 
  # # print(plot_relative_differences_length)
  # 
  # ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_relativeDev.pdf", sep="/"),
  #        width = 297, height = 210, units = "mm")
  # ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_relativeDev.png", sep="/"),
  #        width = 297, height = 210, units = "mm")
  
}





