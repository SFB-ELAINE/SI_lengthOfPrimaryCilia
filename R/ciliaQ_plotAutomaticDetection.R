# Script for plotting results from automatic detection of           ++++++++
# of cultivation images (z stack projection) using ciliaQ           ++++++++
# Author: Kai Budde
# Created: 2022/12/20
# Last changed: 2022/12/20


ciliaQ_plotAutomaticDetection <- function(input_file_ciliaq,
                                          output_dir){
  
  # # Delete everything in the environment
  # rm(list = ls())
  # # close all open plots in RStudio
  # graphics.off()
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2022-03-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse", "rstatix", "ggpubr")
  groundhog.library(pkgs, groundhog.day)
  
  # Import and clean data ####################################################
  df_results  <-readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
  # df_metadata <- readr::read_csv(file = input_file_metadata, name_repair = "universal")
  
  dir.create(path = output_dir, showWarnings = FALSE)
  
  # Add pixel size
  # pixel_size <- unique(df_metadata$scaling_x_in_um)
  # df_results$total_length_in_um <- df_results$Total.Length.pixel.*pixel_size
  
  # Add information of cultivation
  
  # cultivations <- df_results$fileName
  # cultivations <- gsub(pattern = "(.+)_zstack.+", replacement = "\\1", x = cultivations)
  # cultivations <- unique(cultivations)
  
  df_results$cultivation <- NA
  
  names_of_experiments <- c("ITS w/ Asc", "ITS w/ Asc + Dexa",
                            "ITS w/ Asc + Dexa and IGF + TGF",
                            "FBS w/ Asc")
  
  df_results$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results$Name, fixed = TRUE)] <- names_of_experiments[1]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results$Name, fixed = TRUE)] <- names_of_experiments[2]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results$Name, fixed = TRUE)] <- names_of_experiments[3]
  df_results$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results$Name, fixed = TRUE)] <- names_of_experiments[4]
  
  if(sum(is.na(df_results$cultivation)) > 0){
    print("Something went wrong with naming the cultivation conditiosn.")
  }
  
  df_results$cultivation <- factor(df_results$cultivation, levels = names_of_experiments)
  
  # Plot results #############################################################
  
  # Horizontal lengths of cilia
  plot_horizontal_length <- ggplot(df_results, aes(x=cultivation, y=cilia.length..micron.)) +
    stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(alpha = 1) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal horizontal cilium length in \u03BCm determined by ACDC") +
    xlab("Cultivation")
  
  ggsave(filename = file.path(output_dir, "ciliaQ_all_cilia_horizontal_lengths.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "ciliaQ_all_cilia_horizontal_lengths.png"),
         width = 297, height = 210, units = "mm")
  
  
  plot_horizontal_length_violin <- ggplot(df_results, aes(x=cultivation, y=cilia.length..micron.)) +
    geom_violin() +
    # stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(width=0.1) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab( "Horizontal cilium length in \u03BCm") +
    xlab("Cultivation")
  
  
  ggsave(filename = paste(output_dir, "ciliaQ_all_cilia_horizontal_lengths_violin_plot.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "ciliaQ_all_cilia_horizontal_lengths_violin_plot.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  # Remove outliers from data
  experiment_groups <- unique(df_results$cultivation)
  df_results$outlier <- "no"
  for(i in experiment_groups){
    outliers <- boxplot(x = df_results$cilia.length..micron.[df_results$cultivation == i], plot = FALSE)$out
    df_results$outlier[df_results$cultivation == i][which(df_results$cilia.length..micron.[df_results$cultivation == i] %in% outliers)] <- "yes"
  }
  rm(i)
  
  df_results_filtered <- df_results[df_results$outlier != "yes",]
  
  # Horizontal lengths of cilia
  plot_horizontal_length <- ggplot(df_results_filtered, aes(x=cultivation, y=cilia.length..micron.)) +
    stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(alpha = 1) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal cilium length in \u03BCm") +
    xlab("Cultivation")
  
  ggsave(filename = paste(output_dir, "ciliaQ_all_filtered_cilia_horizontal_lengths.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "ciliaQ_all_filtered_cilia_horizontal_lengths.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  plot_horizontal_length_violin <- ggplot(df_results_filtered, aes(x=cultivation, y=cilia.length..micron.)) +
    geom_violin() +
    # stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(width=0.1) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Horizontal cilium length in \u03BCm") +
    xlab("Cultivation")
  
  
  ggsave(filename = paste(output_dir, "ciliaQ_all_filtered_cilia_horizontal_lengths_violin_plot.pdf", sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, "ciliaQ_all_filtered_cilia_horizontal_lengths_violin_plot.png", sep="/"),
         width = 297, height = 210, units = "mm")
  
  # Check for normality of data ##############################################
  
  all_normal <- TRUE
  
  for(i in length(names_of_experiments)){
    # make this example reproducible
    set.seed(2701)
    
    #create data that follows a normal distribution
    lengths_for_statistical_tests <-
      df_results_filtered$cilia.length..micron.[
        df_results_filtered$cultivation == names_of_experiments[i]]
    
    #perform shapiro-wilk test
    test_result <- shapiro.test(lengths_for_statistical_tests)
    if(test_result$p.value <= 0.05){
      print(paste("We have a p-value <= 0.05 for the results of ", names_of_experiments[i],".", sep=""))
      print("The results of this group is not normally distributed.")
      all_normal <- FALSE
    }
  }
  
  rm(i)
  
  if(all_normal){
    print("All measurements are normally distributed according to the Shapiro-Wilk test.")
  }
  
  # Compare groups ###########################################################
  
  detection_results <- df_results_filtered %>%
    dplyr::group_by(cultivation) %>%
    rstatix::get_summary_stats(cilia.length..micron., type = "mean_sd")
  
  res.aov <- df_results_filtered %>% anova_test(cilia.length..micron. ~ cultivation)
  
  
  if(res.aov$`p<.05` == "*"){
    print("There are significant differences between the groups.")
    
    # Pairwise t-tests
    pairwise_comparison <- df_results_filtered %>%
      rstatix::pairwise_t_test(cilia.length..micron. ~ cultivation, p.adjust.method = "bonferroni") %>% 
      add_xy_position(x = "cultivation")
    
    
    plot_horizontal_length_violin_t_test <- ggviolin(df_results_filtered, x = "cultivation", y = "cilia.length..micron.",
                                                     add = "boxplot", add.params = list(fill = "white")) +
      stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
      stat_pvalue_manual(pairwise_comparison, label = "p.adj", tip.length = 0.01, step.increase = 0.05) +
      ylim(0,8) +
      labs(
        subtitle = get_test_label(res.aov, detailed = TRUE),
        caption = get_pwc_label(pairwise_comparison)
      ) +
      theme_bw(base_size = 18) +
      theme(#axis.title.y=element_text(size=12),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
      ylab("Horizontal cilium length in \u03BCm") +
      xlab("Cultivation")
    
    ggsave(filename = paste(output_dir, "ciliaQ_all_filtered_cilia_horizontal_lengths_violin_plot_t_test.pdf", sep="/"),
           width = 297, height = 210, units = "mm")
    ggsave(filename = paste(output_dir, "ciliaQ_all_filtered_cilia_horizontal_lengths_violin_plot_t_test.png", sep="/"),
           width = 297, height = 210, units = "mm")
    
  }
  
  
}

