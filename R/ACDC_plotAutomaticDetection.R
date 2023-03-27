# Script for plotting results from automatic detection of           ++++++++
# of cultivation images using ACDC                                  ++++++++
# Author: Kai Budde
# Created: 2022/06/19
# Last changed: 2023/03/22


ACDC_plotAutomaticDetection <- function(input_file_acdc,
                                        input_file_metadata,
                                        output_dir){
  
  # # Delete everything in the environment
  # rm(list = ls())
  # # close all open plots in RStudio
  # graphics.off()
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2023-01-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse", "rstatix", "ggpubr", "ggbeeswarm", "coin", "scales")
  groundhog.library(pkgs, groundhog.day)
  
  # Import and clean data ##################################################
  df_results  <-readr::read_csv(file = input_file_acdc, name_repair = "universal")
  df_metadata <- readr::read_csv(file = input_file_metadata, name_repair = "universal")
  
  dir.create(path = output_dir, showWarnings = FALSE)
  
  # Add pixel size
  pixel_size <- unique(df_metadata$scaling_x_in_um)
  df_results$horizontal_length_in_um <- df_results$Total.Length.pixel.*pixel_size
  
  # Add information of cultivation
  
  # cultivations <- df_results$fileName
  # cultivations <- gsub(pattern = "(.+)_zstack.+", replacement = "\\1", x = cultivations)
  # cultivations <- unique(cultivations)
  
  df_results$cultivation <- NA
  
  names_of_experiments <- c("ITS", "ITS w/ Dexa",
                            "ITS w/ Dexa + IGF + TGF",
                            "FBS")
  
  df_results$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[1]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[2]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[3]
  df_results$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[4]
  
  if(sum(is.na(df_results$cultivation)) > 0){
    print("Something went wrong with naming the cultivation conditiosn.")
  }
  
  df_results$cultivation <- factor(df_results$cultivation, levels = names_of_experiments)
  
  # Filter data (Remove outliers from data) ################################
  experiment_groups <- unique(df_results$cultivation)
  df_results$outlier <- "no"
  for(i in experiment_groups){
    outliers <- boxplot(x = df_results$horizontal_length_in_um[df_results$cultivation == i], plot = FALSE)$out
    df_results$outlier[df_results$cultivation == i][which(df_results$horizontal_length_in_um[df_results$cultivation == i] %in% outliers)] <- "yes"
  }
  rm(i)
  
  df_results_filtered <- df_results[df_results$outlier != "yes",]
  
  # Save horizontal lengths in a csv file
  df_horizontal_lengths <- df_results_filtered %>% 
    dplyr::select(cultivation, horizontal_length_in_um)
  readr::write_csv(x = df_horizontal_lengths, file = file.path(output_dir, "horizontalLength_ACDC.csv"))
  rm(df_horizontal_lengths)
  
  # Plot results ###########################################################
  
  # Horizontal lengths of cilia
  plot_horizontal_length <- ggplot(df_results, aes(x=cultivation, y=horizontal_length_in_um)) +
    stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(alpha = 1) +
    geom_beeswarm() +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="blue", fill="blue") +
    ylim(0,5) +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal horizontal cilium length in \u03BCm determined by ACDC") +
    xlab("Cultivation")
  
  ggsave(filename = file.path(output_dir, "ACDC_all_cilia_horizontal_lengths.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "ACDC_all_cilia_horizontal_lengths.png"),
         width = 297, height = 210, units = "mm")
  
  
  plot_total_length_violin <- ggplot(df_results, aes(x=cultivation, y=horizontal_length_in_um)) +
    geom_violin() +
    # stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(width=0.1) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal horizontal cilium length in \u03BCm determined by ACDC") +
    xlab("Cultivation")
  
  ggsave(filename = paste(output_dir, "ACDC_all_cilia_horizontal_lengths_violin_plot.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "ACDC_all_cilia_horizontal_lengths_violin_plot.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  # Plot filtered data #####################################################
  
  # Horizontal lengths of cilia
  plot_horizontal_length <- ggplot(df_results_filtered, aes(x=cultivation, y=horizontal_length_in_um)) +
    stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(alpha = 1) +
    geom_beeswarm() +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="blue", fill="blue") +
    ylim(0,5) +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal horizontal cilium length in \u03BCm determined by ACDC") +
    xlab("Cultivation")
  
  ggsave(filename = paste(output_dir, "ACDC_all_filtered_cilia_horizontal_lengths.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "ACDC_all_filtered_cilia_horizontal_lengths.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  plot_horizontal_length_violin <- ggplot(df_results_filtered, aes(x=cultivation, y=horizontal_length_in_um)) +
    geom_violin() +
    # stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(width=0.1) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal horizontal cilium length in \u03BCm determined by ACDC") +
    xlab("Cultivation")
  
  ggsave(filename = paste(output_dir, "ACDC_all_filtered_cilia_horizontal_lengths_violin_plot.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "ACDC_all_filtered_cilia_horizontal_lengths_violin_plot.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  # Check for normality of data ############################################
  
  df_normality <- tibble::tibble(Group = names_of_experiments, data_normally_distributed = FALSE)
  
  for(i in 1:length(names_of_experiments)){
    
    # Get filtered length data
    lengths_for_statistical_tests <-
      df_results_filtered$horizontal_length_in_um[
        df_results_filtered$cultivation == names_of_experiments[i]]
    
    # Perform Shapiro-Wilk test (check normal distribution)
    test_result <- stats::shapiro.test(lengths_for_statistical_tests)
    
    # # Check Qplot
    # scaled_lengths_for_statistical_tests <- scale(lengths_for_statistical_tests)
    # qqnorm(scaled_lengths_for_statistical_tests)
    # qqline(scaled_lengths_for_statistical_tests)
    
    if(test_result$p.value > 0.05){
      df_normality$data_normally_distributed[df_normality$Group == names_of_experiments[i]] <- TRUE
    }
  }
  
  rm(i)
  
  print("According to the Shapiro-Wilk test, the data (total length) of the following groups are normally distributed:")
  print(df_normality)
  
  
  # Compare horizontal length groups #######################################
  
  detection_results <- df_results_filtered %>%
    dplyr::group_by(cultivation) %>%
    rstatix::get_summary_stats(horizontal_length_in_um, type = "mean_sd")
  
  print(paste("The results of the filiterd cilia lengths measurements are:"))
  print(detection_results)
  
  if(all(df_normality$data_normally_distributed)){
    # One-way ANOVA (requires normality, equal Variances, independence)
    test_result <- df_results_filtered %>% rstatix::anova_test(horizontal_length_in_um ~ cultivation)
    
  }else{
    # Kruskal-Wallis test (requires ordinal or continuous response variable, similarly shaped distributions, independence )
    # (nonparametric equivalent of the one-way ANOVA)
    
    # test_result <- kruskal.test(horizontal_length_in_um ~ cultivation, df_results_filtered)
    # TODO: Why are the result of these two tests different?
    
    test_result <- df_results_filtered %>% rstatix::kruskal_test(horizontal_length_in_um ~ cultivation)
  }
  
  
  if(test_result$p < 0.05){
    print("There are significant differences of the horizontal lengths between the groups.")
    
    if(all(df_normality$data_normally_distributed)){
      # Pairwise t-tests with posthoc
      pairwise_comparison_result <- df_results_filtered %>%
        rstatix::pairwise_t_test(horizontal_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "ttest"
      
      # Calculate the effect size to measure the magnitude of the differences
      # TODO
      
    }else{
      
      # Pairwise Wilcoxon signed-rank test with posthoc
      pairwise_comparison_result <- df_results_filtered %>%
        rstatix::pairwise_wilcox_test(horizontal_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "wilcoxon"
      
      # Calculate the effect size to measure the magnitude of the differences
      # TODO
      pairwise_comparison_effect_size <- df_results_filtered %>%
        rstatix::wilcox_effsize(horizontal_length_in_um ~ cultivation,  ref.group = "all")
      
      
    }
    
    
    plot_horizontal_length_violin_statistical_test <-
      ggplot(df_results_filtered, aes(x=cultivation, y=horizontal_length_in_um)) +
      geom_violin() +
      geom_boxplot(width=0.2) +
      # ggviolin(df_results_filtered, x = "cultivation", y = "horizontal_length_in_um",
      #                                                          add = "boxplot", add.params = list(fill = "white")) +
      stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
      stat_pvalue_manual(data = pairwise_comparison_result,  tip.length = 0.01, step.increase = 0.05, hide.ns = TRUE, label = "{p.adj.signif}") +
      ylim(0,7) +
      # scale_y_continuous(breaks= pretty_breaks()) +
      # scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      # labs(
      # #   subtitle = get_test_label(test_result, detailed = TRUE),
      #   caption = get_pwc_label(pairwise_comparison_result)
      # ) +
      theme_bw(base_size = 18) +
      theme(#axis.title.y=element_text(size=12),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
      ylab("Horizontal horizontal cilium length in \u03BCm determined by ACDC") +
      xlab("Cultivation")
    
    ggsave(filename = paste(output_dir, paste0("ACDC_all_filtered_cilia_horizontal_lengths_violin_plot_", test_name, ".pdf"), sep="/"),
           width = 297, height = 210, units = "mm")
    ggsave(filename = paste(output_dir, paste0("ACDC_all_filtered_cilia_horizontal_lengths_violin_plot_", test_name, ".png"), sep="/"),
           width = 297, height = 210, units = "mm")
    
  }
  
}

  