# Script for plotting results from automatic detection of           ++++++++
# of cultivation images                                             ++++++++
# Author: Kai Budde-Sagert
# Created: 2021/11/08
# Last changed: 2024/07/24


plotAutomaticDetection_Cultivation <- function(input_file,
                                               input_file_compare,
                                               output_dir,
                                               exclude_cilia_touching_z_borders = FALSE,
                                               corrected_data = TRUE){
  
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
  pkgs <- c("tidyverse", "rstatix", "ggpubr", "ggbeeswarm", "coin", "scales", "devEMF")
  groundhog.library(pkgs, groundhog.day)
  
  # Import and clean data ####################################################
  dir.create(dirname(output_dir), showWarnings = FALSE)
  dir.create(output_dir, showWarnings = FALSE)
  df_results_automatic <- readr::read_csv(file = input_file, name_repair = "universal")
  df_results_compare <- readr::read_csv(file = input_file_compare, name_repair = "universal")
  
  if(dim(df_results_automatic)[1] != dim(df_results_compare)[1]){
    print("ERROR. Please update summary_cilia_edited.csv. The number of cilia is different from the automatic results.")
    return()
  }
  
  # Add information of cultivation
  
  # cultivations <- df_results$fileName
  # cultivations <- gsub(pattern = "(.+)_zstack.+", replacement = "\\1", x = cultivations)
  # cultivations <- unique(cultivations)
  
  df_results_automatic$cultivation <- NA
  
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
  
  # Filter data
  if(corrected_data){
    df_results_automatic <- df_results_automatic[!grepl(pattern = "^yes$", x = df_results_automatic$to_be_removed, ignore.case = TRUE),]
    df_results_automatic <- df_results_automatic[!is.na(df_results_automatic$total_length_in_um),]
  }
  
  df_horizontal_lengths <- df_results_automatic %>% 
    dplyr::select(cultivation, horizontal_length_in_um)
  readr::write_csv(x = df_horizontal_lengths, file = file.path(output_dir, "horizontalLength_detectCilia.csv"))
  rm(df_horizontal_lengths)
  
  # Remove cilia touching a z border
  if(exclude_cilia_touching_z_borders){
    df_results_automatic <- df_results_automatic[
      !(df_results_automatic$lowest_cilium_layer == 1 |
          df_results_automatic$uppermost_cilium_layer == df_results_automatic$number_of_z_stack_layers),]
  }
  
  # Plot results ###########################################################
  
  
  # Plot all data ----------------------------------------------------------
  
  # Total lengths of cilia
  plot_total_length <- ggplot(df_results_automatic, aes(x=cultivation, y=total_length_in_um)) +
    stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(alpha = 1, fill="grey90") +
    geom_beeswarm(color="#009E73") +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    ylim(1,7) +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab("Total cilium length in \u03BCm") +
    xlab("Cultivation") +
    EnvStats::stat_n_text(
      y.pos = 6.5,
      color = "black",
      text.box = FALSE
    )
  
  ggsave(filename = file.path(output_dir, "all_cilia_total_lengths.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_total_lengths.png"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_total_lengths.eps"),
         width = 297, height = 210, units = "mm", device="eps")
  # ggsave(filename = file.path(output_dir, "all_cilia_total_lengths.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  plot_total_length_violin <- ggplot(df_results_automatic, aes(x=cultivation, y=total_length_in_um)) +
    geom_violin(color="#009E73") +
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width=0.1, fill="grey90") +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    ylim(1,7) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab( "Total cilium length in \u03BCm") +
    xlab("Cultivation")
  
  ggsave(filename = file.path(output_dir, "all_cilia_total_lengths_violin_plot.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_total_lengths_violin_plot.png"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_total_lengths_violin_plot.eps"),
         width = 297, height = 210, units = "mm", device="eps")
  # ggsave(filename = file.path(output_dir, "all_cilia_total_lengths_violin_plot.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  # Horizontal lengths of cilia
  plot_horizontal_length_violin <- ggplot(df_results_automatic, aes(x=cultivation, y=horizontal_length_in_um)) +
    geom_violin(color="#009E73") +
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width=0.1, fill="grey90") +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab( "Horizontal cilium length in \u03BCm") +
    xlab("Cultivation")
  
  ggsave(filename = file.path(output_dir, "all_cilia_horizontal_lengths_violin_plot.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_horizontal_lengths_violin_plot.png"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_horizontal_lengths_violin_plot.eps"),
         width = 297, height = 210, units = "mm", device="eps")
  # ggsave(filename = file.path(output_dir, "all_cilia_horizontal_lengths_violin_plot.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  # Vertical lengths of cilia
  plot_vertical_length_violin <- ggplot(df_results_automatic, aes(x=cultivation, y=vertical_length_in_um)) +
    geom_violin(color="#009E73") +
    stat_boxplot(geom ='errorbar', width = 0.1) +
    geom_boxplot(width=0.1, fill="grey90") +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
    ylab( "Vertical cilium length in \u03BCm") +
    xlab("Cultivation")
  
  ggsave(filename = file.path(output_dir, "all_cilia_vertical_lengths_violin_plot.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_vertical_lengths_violin_plot.png"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_cilia_vertical_lengths_violin_plot.eps"),
         width = 297, height = 210, units = "mm", device="eps")
  # ggsave(filename = file.path(output_dir, "all_cilia_vertical_lengths_violin_plot.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  # Plot filtered data -----------------------------------------------------
  
  # # Total lengths of cilia (filtered data)
  # plot_total_length <- ggplot(df_results_automatic_filtered, aes(x=cultivation, y=total_length_in_um)) +
  #   stat_boxplot(geom ='errorbar', width = 0.3) +
  #   geom_boxplot(alpha = 1, fill="grey90") +
  #   geom_beeswarm(color="#009E73") +
  #   stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
  #   ylim(1,7) +
  #   # geom_jitter(color="black", size=0.5, alpha=0.9) +
  #   #ylim(0,20) +
  #   theme_bw(base_size = 18) +
  #   theme(#axis.title.y=element_text(size=12),
  #     #axis.text.x = element_blank(),
  #     axis.ticks.x = element_blank()) +
  #   ylab("Total cilium length in \u03BCm") +
  #   xlab("Cultivation")
  # 
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_total_lengths.pdf"),
  #        width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_total_lengths.png"),
  #        width = 297, height = 210, units = "mm")
  # # ggsave(filename = file.path(output_dir, "all_filtered_cilia_total_lengths.emf"),
  # #        width = 297, height = 210, units = "mm", device = emf)
  # 
  # 
  # plot_total_length_violin <- ggplot(df_results_automatic_filtered, aes(x=cultivation, y=total_length_in_um)) +
  #   geom_violin(color="#009E73") +
  #   stat_boxplot(geom ='errorbar', width = 0.1) +
  #   geom_boxplot(width=0.1, fill="grey90") +
  #   stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
  #   ylim(1,7) +
  #   theme_bw(base_size = 18) +
  #   theme(#axis.title.y=element_text(size=12),
  #     #axis.text.x = element_blank(),
  #     axis.ticks.x = element_blank()) +
  #   ylab( "Total cilium length in \u03BCm") +
  #   xlab("Cultivation")
  # 
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_total_lengths_violin_plot.pdf"),
  #        width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_total_lengths_violin_plot.png"),
  #        width = 297, height = 210, units = "mm")
  # # ggsave(filename = file.path(output_dir, "all_filtered_cilia_total_lengths_violin_plot.emf"),
  # #        width = 297, height = 210, units = "mm", device = emf)
  # 
  # 
  # # Horizontal lengths of cilia (filtered data)
  # plot_horizontal_length_violin <- ggplot(df_results_automatic_filtered, aes(x=cultivation, y=horizontal_length_in_um)) +
  #   geom_violin(color="#009E73") +
  #   stat_boxplot(geom ='errorbar', width = 0.1) +
  #   geom_boxplot(width=0.1, fill="grey90") +
  #   stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
  #   ylim(0,5) +
  #   theme_bw(base_size = 18) +
  #   theme(#axis.title.y=element_text(size=12),
  #     #axis.text.x = element_blank(),
  #     axis.ticks.x = element_blank()) +
  #   ylab( "Horizontal cilium length in \u03BCm") +
  #   xlab("Cultivation")
  # 
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_horizontal_lengths_violin_plot.pdf"),
  #        width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_horizontal_lengths_violin_plot.png"),
  #        width = 297, height = 210, units = "mm")
  # # ggsave(filename = file.path(output_dir, "all_filtered_cilia_horizontal_lengths_violin_plot.emf"),
  # #        width = 297, height = 210, units = "mm", device = emf)
  # 
  # 
  # # Vertical lengths of cilia (filtered data)
  # plot_vertical_length_violin <- ggplot(df_results_automatic_filtered, aes(x=cultivation, y=vertical_length_in_um)) +
  #   geom_violin(color="#009E73") +
  #   stat_boxplot(geom ='errorbar', width = 0.1) +
  #   geom_boxplot(width=0.1, fill="grey90") +
  #   stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
  #   ylim(0,5) +
  #   theme_bw(base_size = 18) +
  #   theme(#axis.title.y=element_text(size=12),
  #     #axis.text.x = element_blank(),
  #     axis.ticks.x = element_blank()) +
  #   ylab( "Vertical cilium length in \u03BCm") +
  #   xlab("Cultivation")
  # 
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_vertical_lengths_violin_plot.pdf"),
  #        width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, "all_filtered_cilia_vertical_lengths_violin_plot.png"),
  #        width = 297, height = 210, units = "mm")
  # # ggsave(filename = file.path(output_dir, "all_filtered_cilia_vertical_lengths_violin_plot.emf"),
  # #        width = 297, height = 210, units = "mm", device = emf)
  
  
  
  
  
  # Check for normality of total length data ###############################
  
  df_normality <- tibble::tibble(Group = names_of_experiments, data_normally_distributed = FALSE)
  
  for(i in 1:length(names_of_experiments)){
    
    # Get filtered length data
    lengths_for_statistical_tests <-
      df_results_automatic$total_length_in_um[
        df_results_automatic$cultivation == names_of_experiments[i]]
    
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
  
  # Welch anova
  # anova
  # kruskal-wallis
  
  
  # Compare total length groups ############################################

  detection_results <- df_results_automatic %>%
    dplyr::group_by(cultivation) %>%
    rstatix::get_summary_stats(total_length_in_um, type = "full")
  
  print("The results of the filiterd cilia lengths measurements are:")
  print(detection_results)
  
  if(all(df_normality$data_normally_distributed)){
    # One-way ANOVA (requires normality, equal Variances, independence)
    test_result <- df_results_automatic %>% rstatix::anova_test(total_length_in_um ~ cultivation)
    
  }else{
    # Kruskal-Wallis test (requires ordinal or continuous response variable, similarly shaped distributions, independence )
    # (nonparametric equivalent of the one-way ANOVA)
    
    # test_result <- kruskal.test(total_length_in_um ~ cultivation, df_results_automatic)
    
    test_result <- df_results_automatic %>% rstatix::kruskal_test(total_length_in_um ~ cultivation)
  }
  
  
  if(test_result$p < 0.05){
    print("There are significant differences of the total lengths between the groups.")
    
    if(all(df_normality$data_normally_distributed)){
      # Pairwise t-tests with posthoc
      pairwise_comparison_result <- df_results_automatic %>%
        rstatix::pairwise_t_test(total_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "ttest"
      
      # Calculate the effect size to measure the magnitude of the differences (TODO)
      
    }else{
      
      # Pairwise Wilcoxon signed-rank test with posthoc
      pairwise_comparison_result <- df_results_automatic %>%
        rstatix::pairwise_wilcox_test(total_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "wilcoxon"
      
      # Calculate the effect size to measure the magnitude of the differences (TODO)
      
      pairwise_comparison_effect_size <- df_results_automatic %>%
        rstatix::wilcox_effsize(total_length_in_um ~ cultivation,  ref.group = "all")
      
      
    }
    
    
    plot_total_length_violin_statistical_test <-
      ggplot(df_results_automatic, aes(x=cultivation, y=total_length_in_um)) +
      geom_violin(color="#009E73") +
      stat_boxplot(geom ='errorbar', width = 0.1) +
      geom_boxplot(width=0.2, fill="grey90") +
      # ggviolin(df_results_automatic, x = "cultivation", y = "total_length_in_um",
      #                                           add = "boxplot", add.params = list(fill = "white")) +
      stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
      stat_pvalue_manual(data = pairwise_comparison_result,  tip.length = 0.01, step.increase = 0.05, hide.ns = TRUE, label = "{p.adj.signif}") +
      scale_y_continuous(breaks= pretty_breaks()) +
      coord_cartesian(ylim=c(0, 9)) +
      # scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      # labs(
      # #   subtitle = get_test_label(test_result, detailed = TRUE),
      #   caption = get_pwc_label(pairwise_comparison_result)
      # ) +
      theme_bw(base_size = 18) +
      theme(#axis.title.y=element_text(size=12),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
      ylab("Total cilium length in \u03BCm") +
      xlab("Cultivation")
    
    ggsave(filename = file.path(output_dir, paste0("all_cilia_total_lengths_violin_plot_", test_name, ".pdf")),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, paste0("all_cilia_total_lengths_violin_plot_", test_name, ".png")),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, paste0("all_cilia_total_lengths_violin_plot_", test_name, ".eps")),
           width = 297, height = 210, units = "mm", device="eps")
    # ggsave(filename = file.path(output_dir, paste0("all_cilia_total_lengths_violin_plot_", test_name, ".emf")),
    #        width = 297, height = 210, units = "mm", device = emf)
    
  }
  
  
  
  # Check for normality of horizontal length data ##########################
  
  df_normality <- tibble::tibble(Group = names_of_experiments, data_normally_distributed = FALSE)
  
  for(i in 1:length(names_of_experiments)){
    
    # Get filtered length data
    lengths_for_statistical_tests <-
      df_results_automatic$horizontal_length_in_um[
        df_results_automatic$cultivation == names_of_experiments[i]]
    
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
  
  print("According to the Shapiro-Wilk test, the data (horizontal length) of the following groups are normally distributed:")
  print(df_normality)
  
  # Welch anova
  # anova
  # kruskal-wallis
  
  
  
  # Compare horizontal length groups #######################################
  
  detection_results <- df_results_automatic %>%
    dplyr::group_by(cultivation) %>%
    rstatix::get_summary_stats(horizontal_length_in_um, type = "full")
  
  print("The results of the filiterd cilia lengths measurements are:")
  print(detection_results)
  
  if(all(df_normality$data_normally_distributed)){
    # One-way ANOVA (requires normality, equal Variances, independence)
    test_result <- df_results_automatic %>% rstatix::anova_test(horizontal_length_in_um ~ cultivation)
    
  }else{
    # Kruskal-Wallis test (requires ordinal or continuous response variable, similarly shaped distributions, independence )
    # (nonparametric equivalent of the one-way ANOVA)
    
    # test_result <- kruskal.test(horizontal_length_in_um ~ cultivation, df_results_automatic)
    
    test_result <- df_results_automatic %>% rstatix::kruskal_test(horizontal_length_in_um ~ cultivation)
  }
  
  
  if(test_result$p < 0.05){
    print("There are significant differences of the horizontal lengths between the groups.")
    
    if(all(df_normality$data_normally_distributed)){
      # Pairwise t-tests with posthoc
      pairwise_comparison_result <- df_results_automatic %>%
        rstatix::pairwise_t_test(horizontal_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "ttest"
      
      # Calculate the effect size to measure the magnitude of the differences (TODO)
      
    }else{
      
      # Pairwise Wilcoxon signed-rank test with posthoc
      pairwise_comparison_result <- df_results_automatic %>%
        rstatix::pairwise_wilcox_test(horizontal_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "wilcoxon"
      
      # Calculate the effect size to measure the magnitude of the differences (TODO)
      
      pairwise_comparison_effect_size <- df_results_automatic %>%
        rstatix::wilcox_effsize(horizontal_length_in_um ~ cultivation,  ref.group = "all")
      
      
    }
    
    ypositions <- rep(x = seq(from = 6, by = 0.5,
                              length.out = length(pairwise_comparison_result$group1)))
    ypositions <- ypositions[!pairwise_comparison_result$p.adj.signif == "ns"]
    
    
    plot_horizontal_length_violin_statistical_test <-
      ggplot(df_results_automatic, aes(x=cultivation, y=horizontal_length_in_um)) +
      geom_violin(color="#009E73") +
      stat_boxplot(geom ='errorbar', width = 0.1) +
      geom_boxplot(width=0.1, fill="grey90") +
      # ggviolin(df_results_automatic, x = "cultivation", y = "horizontal_length_in_um",
      #                                           add = "boxplot", add.params = list(fill = "white")) +
      stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
      stat_pvalue_manual(data = pairwise_comparison_result, y.position = ypositions,  tip.length = 0.01, hide.ns = TRUE, label = "{p.adj.signif}") +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      coord_cartesian(ylim=c(0, 9)) +
      # ylim(0,7) +
      # scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      # labs(
      # #   subtitle = get_test_label(test_result, detailed = TRUE),
      #   caption = get_pwc_label(pairwise_comparison_result)
      # ) +
      theme_bw(base_size = 18) +
      theme(#axis.title.y=element_text(size=12),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
      # ylab("Horizontal cilium length in \u03BCm determined by detectCilia") +
      ylab("Horizontal cilium length in \u03BCm") +
      xlab("Cultivation")
    
    ggsave(filename = file.path(output_dir, paste0("all_cilia_horizontal_lengths_violin_plot_", test_name, ".pdf")),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, paste0("all_cilia_horizontal_lengths_violin_plot_", test_name, ".png")),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, paste0("all_cilia_horizontal_lengths_violin_plot_", test_name, ".eps")),
           width = 297, height = 210, units = "mm", device="eps")
    # ggsave(filename = file.path(output_dir, paste0("all_cilia_horizontal_lengths_violin_plot_", test_name, ".emf")),
    #        width = 297, height = 210, units = "mm", device = emf)
    
  }
  
  
  # Check for normality of vertical length data ############################
  
  df_normality <- tibble::tibble(Group = names_of_experiments, data_normally_distributed = FALSE)
  
  for(i in 1:length(names_of_experiments)){
    
    # Get filtered length data
    lengths_for_statistical_tests <-
      df_results_automatic$vertical_length_in_um[
        df_results_automatic$cultivation == names_of_experiments[i]]
    
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
  
  print("According to the Shapiro-Wilk test, the data (vertical length) of the following groups are normally distributed:")
  print(df_normality)
  
  # Welch anova
  # anova
  # kruskal-wallis
  
  
  # Compare vertical length groups #########################################
  
  detection_results <- df_results_automatic %>%
    dplyr::group_by(cultivation) %>%
    rstatix::get_summary_stats(vertical_length_in_um, type = "full")
  
  print("The results of the filiterd cilia lengths measurements are:")
  print(detection_results)
  
  if(all(df_normality$data_normally_distributed)){
    # One-way ANOVA (requires normality, equal Variances, independence)
    test_result <- df_results_automatic %>% rstatix::anova_test(vertical_length_in_um ~ cultivation)
    
  }else{
    # Kruskal-Wallis test (requires ordinal or continuous response variable, similarly shaped distributions, independence )
    # (nonparametric equivalent of the one-way ANOVA)
    
    # test_result <- kruskal.test(vertical_length_in_um ~ cultivation, df_results_automatic)
    # TODO: Why are the result of these two tests different?
    
    test_result <- df_results_automatic %>% rstatix::kruskal_test(vertical_length_in_um ~ cultivation)
  }
  
  
  if(test_result$p < 0.05){
    print("There are significant differences of the vertical lengths between the groups.")
    
    if(all(df_normality$data_normally_distributed)){
      # Pairwise t-tests with posthoc
      pairwise_comparison_result <- df_results_automatic %>%
        rstatix::pairwise_t_test(vertical_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "ttest"
      
      # Calculate the effect size to measure the magnitude of the differences (TODO)
      
    }else{
      
      # Pairwise Wilcoxon signed-rank test with posthoc
      pairwise_comparison_result <- df_results_automatic %>%
        rstatix::pairwise_wilcox_test(vertical_length_in_um ~ cultivation, p.adjust.method = "bonferroni") %>% 
        add_xy_position(x = "cultivation")
      
      test_name <- "wilcoxon"
      
      # Calculate the effect size to measure the magnitude of the differences (TODO)
      pairwise_comparison_effect_size <- df_results_automatic %>%
        rstatix::wilcox_effsize(vertical_length_in_um ~ cultivation,  ref.group = "all")
      
    }
    
    
    plot_vertical_length_violin_statistical_test <-
      ggplot(df_results_automatic, aes(x=cultivation, y=vertical_length_in_um)) +
      geom_violin(color="#009E73") +
      stat_boxplot(geom ='errorbar', width = 0.1) +
      geom_boxplot(width=0.1, fill="grey90") +
      # ggviolin(df_results_automatic, x = "cultivation", y = "vertical_length_in_um",
      #                                                add = "boxplot", add.params = list(fill = "white")) +
      stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
      stat_pvalue_manual(data = pairwise_comparison_result,  tip.length = 0.01, step.increase = 0.05, hide.ns = TRUE, label = "{p.adj.signif}") +
      scale_y_continuous(breaks= pretty_breaks()) +
      coord_cartesian(ylim=c(0, 9)) +
      # scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
      # labs(
      # #   subtitle = get_test_label(test_result, detailed = TRUE),
      #   caption = get_pwc_label(pairwise_comparison_result)
      # ) +
      theme_bw(base_size = 18) +
      theme(#axis.title.y=element_text(size=12),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
      ylab("Vertical cilium length in \u03BCm") +
      xlab("Cultivation")
    
    ggsave(filename = file.path(output_dir, paste0("all_cilia_vertical_lengths_violin_plot_", test_name, ".pdf")),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, paste0("all_cilia_vertical_lengths_violin_plot_", test_name, ".png")),
           width = 297, height = 210, units = "mm")
    ggsave(filename = file.path(output_dir, paste0("all_cilia_vertical_lengths_violin_plot_", test_name, ".eps")),
           width = 297, height = 210, units = "mm", device="eps")
    # ggsave(filename = file.path(output_dir, paste0("all_cilia_vertical_lengths_violin_plot_", test_name, ".emf")),
    #        width = 297, height = 210, units = "mm", device = emf)
    
  }
  
  
}

