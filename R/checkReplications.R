# Script for checking difference of replication results             ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/04/08
# Last changed: 2024/04/08


checkReplications <- function(input_file){
  
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
  df_results <- readr::read_csv(file = input_file, name_repair = "universal")

  
  # Add information of replication
  replications <- c(1,2)
  df_results$replication <- NA
  df_results$replication[grepl(pattern = "EV38_1", x = df_results$fileName, fixed = TRUE)] <- replications[1]
  df_results$replication[grepl(pattern = "EV38_2", x = df_results$fileName, fixed = TRUE)] <- replications[2]
  
  df_results$replication <- factor(df_results$replication)
  
  # Add information of cultivation
  df_results$cultivation <- NA
  names_of_experiments <- c("ITS", "ITS with Dexa",
                            "ITS with Dexa + IGF + TGF",
                            "FBS")
  
  df_results$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[1]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[2]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[3]
  df_results$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[4]
  
  df_results$cultivation <- factor(df_results$cultivation, levels = names_of_experiments)
  
  # Compare replication groups:
  groups_with_replications <- unique(df_results$cultivation[df_results$replication == max(df_results$replication)])
  
  for(i in groups_with_replications){
    print(paste0("Compare replication results of: ", i))
    
    df_dummy <- df_results[df_results$cultivation == i,]
    
    # Check for normality of total length data ###############################
    
    df_normality <- tibble::tibble(Group = replications, data_normally_distributed = FALSE)
    
    for(j in 1:length(replications)){
      
      # Get filtered length data
      lengths_for_statistical_tests <-
        df_dummy$total_length_in_um[
          df_dummy$replication == replications[j]]
      
      # Perform Shapiro-Wilk test (check normal distribution)
      test_result <- stats::shapiro.test(lengths_for_statistical_tests)
      
      # # Check Qplot
      # scaled_lengths_for_statistical_tests <- scale(lengths_for_statistical_tests)
      # qqnorm(scaled_lengths_for_statistical_tests)
      # qqline(scaled_lengths_for_statistical_tests)
      
      if(test_result$p.value > 0.05){
        df_normality$data_normally_distributed[df_normality$Group == replications[j]] <- TRUE
      }
    }
    
    rm(j)
    
    print("According to the Shapiro-Wilk test, the data (total length) of the following groups are normally distributed:")
    print(df_normality)
    
    # Welch anova
    # anova
    # kruskal-wallis
    
    
    # Compare total length groups ############################################
    
    detection_results <- df_dummy %>%
      dplyr::group_by(replication) %>%
      rstatix::get_summary_stats(total_length_in_um, type = "full")
    
    print("The results of the cilia lengths measurements are:")
    print(detection_results)
    
    if(all(df_normality$data_normally_distributed)){
      # One-way ANOVA (requires normality, equal Variances, independence)
      test_result <- df_dummy %>% rstatix::anova_test(total_length_in_um ~ replication)
      
    }else{
      # Kruskal-Wallis test (requires ordinal or continuous response variable, similarly shaped distributions, independence )
      # (nonparametric equivalent of the one-way ANOVA)
      
      # test_result <- kruskal.test(total_length_in_um ~ replication, df_dummy)
      
      test_result <- df_dummy %>% rstatix::kruskal_test(total_length_in_um ~ replication)
    }
    
    
    if(test_result$p < 0.05){
      print("There are significant differences of the total lengths between the groups.")
      
      if(all(df_normality$data_normally_distributed)){
        # Pairwise t-tests with posthoc
        pairwise_comparison_result <- df_dummy %>%
          rstatix::pairwise_t_test(total_length_in_um ~ replication, p.adjust.method = "bonferroni") %>% 
          add_xy_position(x = "replication")
        
        test_name <- "ttest"
        
        # Calculate the effect size to measure the magnitude of the differences (TODO)
        
      }else{
        
        # Pairwise Wilcoxon signed-rank test with posthoc
        pairwise_comparison_result <- df_dummy %>%
          rstatix::pairwise_wilcox_test(total_length_in_um ~ replication, p.adjust.method = "bonferroni") %>% 
          add_xy_position(x = "replication")
        
        test_name <- "wilcoxon"
        
        # Calculate the effect size to measure the magnitude of the differences (TODO)
        
        pairwise_comparison_effect_size <- df_dummy %>%
          rstatix::wilcox_effsize(total_length_in_um ~ replication,  ref.group = "all")
        
        
      }
      
      
      plot_total_length_violin_statistical_test <-
        ggplot(df_dummy, aes(x=replication, y=total_length_in_um)) +
        geom_violin(color="#009E73") +
        stat_boxplot(geom ='errorbar', width = 0.1) +
        geom_boxplot(width=0.2, fill="grey90") +
        # ggviolin(df_dummy, x = "replication", y = "total_length_in_um",
        #                                           add = "boxplot", add.params = list(fill = "white")) +
        stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
        stat_pvalue_manual(data = pairwise_comparison_result,  tip.length = 0.01, step.increase = 0.05, hide.ns = TRUE, label = "{p.adj.signif}") +
        scale_y_continuous(limits = c(0, NA), breaks= pretty_breaks()) +
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
        xlab("replication")
      # 
      # ggsave(filename = file.path(output_dir, paste0("all_filtered_cilia_total_lengths_violin_plot_", test_name, ".pdf")),
      #        width = 297, height = 210, units = "mm")
      # ggsave(filename = file.path(output_dir, paste0("all_filtered_cilia_total_lengths_violin_plot_", test_name, ".png")),
      #        width = 297, height = 210, units = "mm")
      # # ggsave(filename = file.path(output_dir, paste0("all_filtered_cilia_total_lengths_violin_plot_", test_name, ".emf")),
      # #        width = 297, height = 210, units = "mm", device = emf)
      
    }else{
      print("no significance")
    }
    
    
    
    
    # Check for normality of horizontal length data ########################
    
    df_normality <- tibble::tibble(Group = replications, data_normally_distributed = FALSE)
    
    for(j in 1:length(replications)){
      
      # Get filtered length data
      lengths_for_statistical_tests <-
        df_dummy$horizontal_length_in_um[
          df_dummy$replication == replications[j]]
      
      # Perform Shapiro-Wilk test (check normal distribution)
      test_result <- stats::shapiro.test(lengths_for_statistical_tests)
      
      # # Check Qplot
      # scaled_lengths_for_statistical_tests <- scale(lengths_for_statistical_tests)
      # qqnorm(scaled_lengths_for_statistical_tests)
      # qqline(scaled_lengths_for_statistical_tests)
      
      if(test_result$p.value > 0.05){
        df_normality$data_normally_distributed[df_normality$Group == replications[j]] <- TRUE
      }
    }
    
    rm(j)
    
    print("According to the Shapiro-Wilk test, the data (horizontal length) of the following groups are normally distributed:")
    print(df_normality)
    
    # Welch anova
    # anova
    # kruskal-wallis
    
    
    # Compare total length groups ############################################
    
    detection_results <- df_dummy %>%
      dplyr::group_by(replication) %>%
      rstatix::get_summary_stats(horizontal_length_in_um, type = "full")
    
    print("The results of the cilia lengths measurements are:")
    print(detection_results)
    
    if(all(df_normality$data_normally_distributed)){
      # One-way ANOVA (requires normality, equal Variances, independence)
      test_result <- df_dummy %>% rstatix::anova_test(horizontal_length_in_um ~ replication)
      
    }else{
      # Kruskal-Wallis test (requires ordinal or continuous response variable, similarly shaped distributions, independence )
      # (nonparametric equivalent of the one-way ANOVA)
      
      # test_result <- kruskal.test(horizontal_length_in_um ~ replication, df_dummy)
      
      test_result <- df_dummy %>% rstatix::kruskal_test(horizontal_length_in_um ~ replication)
    }
    
    
    if(test_result$p < 0.05){
      print("There are significant differences of the total lengths between the groups.")
      
      if(all(df_normality$data_normally_distributed)){
        # Pairwise t-tests with posthoc
        pairwise_comparison_result <- df_dummy %>%
          rstatix::pairwise_t_test(horizontal_length_in_um ~ replication, p.adjust.method = "bonferroni") %>% 
          add_xy_position(x = "replication")
        
        test_name <- "ttest"
        
        # Calculate the effect size to measure the magnitude of the differences (TODO)
        
      }else{
        
        # Pairwise Wilcoxon signed-rank test with posthoc
        pairwise_comparison_result <- df_dummy %>%
          rstatix::pairwise_wilcox_test(horizontal_length_in_um ~ replication, p.adjust.method = "bonferroni") %>% 
          add_xy_position(x = "replication")
        
        test_name <- "wilcoxon"
        
        # Calculate the effect size to measure the magnitude of the differences (TODO)
        
        pairwise_comparison_effect_size <- df_dummy %>%
          rstatix::wilcox_effsize(horizontal_length_in_um ~ replication,  ref.group = "all")
        
        
      }
      
      
      plot_horizontal_length_violin_statistical_test <-
        ggplot(df_dummy, aes(x=replication, y=horizontal_length_in_um)) +
        geom_violin(color="#009E73") +
        stat_boxplot(geom ='errorbar', width = 0.1) +
        geom_boxplot(width=0.2, fill="grey90") +
        # ggviolin(df_dummy, x = "replication", y = "horizontal_length_in_um",
        #                                           add = "boxplot", add.params = list(fill = "white")) +
        stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
        stat_pvalue_manual(data = pairwise_comparison_result,  tip.length = 0.01, step.increase = 0.05, hide.ns = TRUE, label = "{p.adj.signif}") +
        scale_y_continuous(limits = c(0, NA), breaks= pretty_breaks()) +
        # scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
        # labs(
        # #   subtitle = get_test_label(test_result, detailed = TRUE),
        #   caption = get_pwc_label(pairwise_comparison_result)
        # ) +
        theme_bw(base_size = 18) +
        theme(#axis.title.y=element_text(size=12),
          #axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
        ylab("Horizontal cilium length in \u03BCm") +
        xlab("replication")
      # 
      # ggsave(filename = file.path(output_dir, paste0("all_filtered_cilia_horizontal_lengths_violin_plot_", test_name, ".pdf")),
      #        width = 297, height = 210, units = "mm")
      # ggsave(filename = file.path(output_dir, paste0("all_filtered_cilia_horizontal_lengths_violin_plot_", test_name, ".png")),
      #        width = 297, height = 210, units = "mm")
      # # ggsave(filename = file.path(output_dir, paste0("all_filtered_cilia_horizontal_lengths_violin_plot_", test_name, ".emf")),
      # #        width = 297, height = 210, units = "mm", device = emf)
      
    }else{
      print("no significance")
    }
    
    
    
  }
  
  rm(df_dummy)
  
  
}

