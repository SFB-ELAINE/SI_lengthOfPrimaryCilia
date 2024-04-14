# Script for plotting total length of 2 tools (dc and CiliaQ)       ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/04/07
# Last changed: 2024/04/13


plot3DResultsFromAllTools <- function(input_file_detectCilia,
                                      input_file_ciliaq,
                                      output_dir,
                                      exclude_cilia_touching_z_borders = TRUE){
  
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
  df_results_ciliaQ <- readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
  
  # Delete false positive cilium (manually labelled)
  df_results_detectCilia <- df_results_detectCilia[!grepl(pattern = "^yes$", x = df_results_detectCilia$to_be_removed, ignore.case = TRUE),]
  df_results_ciliaQ <- df_results_ciliaQ[!grepl(pattern = "^yes$", x = df_results_ciliaQ$to_be_removed, ignore.case = TRUE),]
  
  
  # Remove cilia touching a z border
  if(exclude_cilia_touching_z_borders){
    df_results_detectCilia <- df_results_detectCilia[
      !(df_results_detectCilia$lowest_cilium_layer == 1 |
          df_results_detectCilia$uppermost_cilium_layer == df_results_detectCilia$number_of_z_stack_layers),]
  }
  
  # Add original file name to CiliaQ data
  df_results_ciliaQ$file_name_czi <- gsub(
    pattern = "(.+)_projection_CQP_CQs",
    replacement = "\\1.czi",
    df_results_ciliaQ$file_name)  
  
  # Only keep specific columns
  df_results_detectCilia <- df_results_detectCilia %>% 
    dplyr::select("fileName", "cilium", "total_length_in_um")
  df_results_ciliaQ <- df_results_ciliaQ %>% 
    dplyr::select("fileName" = "file_name_czi", "cilium" = "ID",
                  "total_length_in_um" = "cilia.length..micron.")
  
  # Combine data
  df_results_detectCilia$tool <- "detectCilia"
  df_results_ciliaQ$tool <- "CiliaQ"
  
  
  df_results <- dplyr::bind_rows(df_results_detectCilia,
                                           df_results_ciliaQ)
  
  
  # Change order of tools
  df_results$tool <- factor(df_results$tool, levels = c("detectCilia", "CiliaQ"))
  
  
  # Add cultivation
  df_results$cultivation <- ""
  
  names_of_experiments <- c("ITS", "ITS with Dexa",
                            "ITS with Dexa + IGF + TGF",
                            "FBS")
  
  df_results$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[1]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[2]
  df_results$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[3]
  df_results$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results$fileName, fixed = TRUE)] <- names_of_experiments[4]
  
  
  # Change the order of facets
  df_results$cultivation <- factor(df_results$cultivation,
                                   levels = c("ITS", "ITS with Dexa",
                                              "ITS with Dexa + IGF + TGF", "FBS" ))
  
  # Plot results ###########################################################
  dir.create(output_dir, showWarnings = FALSE)
  
  # Plot all data ----------------------------------------------------------
  
  # Total lengths of cilia
  plot_total_length <- ggplot(df_results, aes(x=tool, y=total_length_in_um, color=tool, fill = tool)) +
    stat_boxplot(geom ='errorbar', width = 0.3, color="black") +
    geom_boxplot(alpha = 1, color="black") +
    geom_beeswarm() +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="blue", fill="blue") +
    scale_y_continuous(limits = c(0,10), breaks = scales::breaks_pretty()) +
    # ylim(0,10) +
    coord_cartesian(ylim=c(0, 9)) +
    scale_color_manual(values=c("#009E73", "#1e3a80")) +
    scale_fill_manual(values=c("grey90", "grey90", "grey90")) +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      legend.position = "none",
      axis.ticks.x = element_blank()) +
    ylab("Total cilium length in \u03BCm") +
    xlab("Detection tool") +
    facet_grid(.~cultivation) +
    theme(strip.background = element_rect(fill = "white"))
  
  
  ggsave(filename = file.path(output_dir, "all_tools_cilia_total_lengths.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_tools_cilia_total_lengths.png"),
         width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, "all_tools_cilia_total_lengths.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  # Total lengths of cilia violin plot
  plot_total_length_violin <- ggplot(df_results, aes(x=tool, y=total_length_in_um, fill = tool)) +
    geom_violin() +
    # stat_boxplot(geom ='errorbar', width = 0.3) +
    geom_boxplot(width=0.1) +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    scale_fill_manual(values=c("grey90", "grey90", "grey90")) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    coord_cartesian(ylim=c(0, 9)) +
    # ylim(0,5) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      legend.position = "none",
      axis.ticks.x = element_blank()) +
    ylab("Total cilium length in \u03BCm") +
    xlab("Detection tool") +
    facet_grid(.~cultivation) +
    theme(strip.background = element_rect(fill = "white"))
  
  ggsave(filename = file.path(output_dir, "all_tools_total_lengths_violin_plot.pdf"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, "all_tools_total_lengths_violin_plot.png"),
         width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, "all_tools_total_lengths_violin_plot.emf"),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  
  # Check for normality of total length data ###############################
  
  df_normality <- df_results %>% 
    dplyr::group_by(cultivation, tool) %>% 
    rstatix::shapiro_test(total_length_in_um)
  
  df_normality$data_normally_distributed <- FALSE
  df_normality$data_normally_distributed[df_normality$p > 0.05] <- TRUE
  
  print("According to the Shapiro-Wilk test, the data (total length) of the following groups are normally distributed:")
  print(df_normality)
  
  # Welch anova
  # anova
  # kruskal-wallis
  
  
  # Compare total length groups ############################################
  
  detection_results <- df_results %>%
    dplyr::group_by(cultivation, tool) %>%
    rstatix::get_summary_stats(total_length_in_um, type = "full")
  
  print("The results of the filiterd cilia lengths measurements are:")
  print(detection_results)
  
  if(all(df_normality$data_normally_distributed)){
    # One-way ANOVA (requires normality, equal Variances, independence)
    test_result <- df_results %>%
      dplyr::group_by(cultivation) %>%
      rstatix::anova_test(total_length_in_um ~ tool)
    
  }else{
    # Kruskal-Wallis test (requires ordinal or continuous response variable, similarly shaped distributions, independence )
    # (nonparametric equivalent of the one-way ANOVA)
    
    # test_result <- kruskal.test(total_length_in_um ~ cultivation, df_results)
    
    test_result <- df_results %>%
      dplyr::group_by(cultivation) %>%
      rstatix::kruskal_test(total_length_in_um ~ tool)
  }
  
  
  if(all(test_result$p < 0.05)){
    print("There are significant differences of the total lengths between the tools in all cultivation compositions.")
  }
  
  if(all(df_normality$data_normally_distributed)){
    # Pairwise t-tests with posthoc
    pairwise_comparison_result <- df_results %>%
      dplyr::group_by(cultivation) %>% 
      rstatix::pairwise_t_test(total_length_in_um ~ tool, p.adjust.method = "bonferroni") %>% 
      rstatix::add_xy_position(x = "cultivation")
    
    test_name <- "ttest"
    
    # Calculate the effect size to measure the magnitude of the differences (TODO)
    
  }else{
    
    # Pairwise Wilcoxon signed-rank test with posthoc
    pairwise_comparison_result <- df_results %>%
      dplyr::group_by(cultivation) %>% 
      rstatix::pairwise_wilcox_test(total_length_in_um ~ tool, p.adjust.method = "bonferroni") %>% 
      rstatix::add_xy_position(x = "tool")
    
    test_name <- "wilcoxon"
    
    # Calculate the effect size to measure the magnitude of the differences (TODO)
    
    pairwise_comparison_effect_size <- df_results %>%
      dplyr::group_by(cultivation) %>% 
      rstatix::wilcox_effsize(total_length_in_um ~ tool)
    
    
  }
  
  ypositions <- rep(x = seq(from = 8, by = 0.5,
                            length.out = length(pairwise_comparison_result$cultivation) / length(unique(pairwise_comparison_result$cultivation)) ),
                    length(unique(pairwise_comparison_result$cultivation)))
  ypositions <- ypositions[!pairwise_comparison_result$p.adj.signif == "ns"]
  
  plot_total_length_statistical_test <-
    ggplot(df_results, aes(x=tool, y=total_length_in_um, color=tool)) +
    stat_boxplot(geom ='errorbar', width = 0.3, color="black") +
    geom_boxplot(aes(fill = tool), alpha = 1, color="black") +
    geom_beeswarm() +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="blue", fill="blue") +
    scale_y_continuous(breaks= pretty_breaks()) +
    coord_cartesian(ylim=c(0, 9)) +
    # ylim(0,5) +
    scale_color_manual(values=c("#009E73", "#1e3a80")) +
    scale_fill_manual(values=c("grey90", "grey90", "grey90")) +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      legend.position = "none",
      axis.ticks.x = element_blank()) +
    ylab("Total cilium length in \u03BCm") +
    xlab("Detection tool") +
    facet_grid(.~cultivation) +
    theme(strip.background = element_rect(fill = "white")) +
    # ggprism::add_pvalue(pairwise_comparison_result, tip.length = 0.01, hide.ns = FALSE)
    stat_pvalue_manual(data = pairwise_comparison_result, y.position = ypositions, tip.length = 0.01, hide.ns = TRUE) +
    EnvStats::stat_n_text(
      y.pos = 9,
      color = "black",
      text.box = FALSE
    )
  
  
  ggsave(filename = file.path(output_dir, paste0("all_tools_cilia_total_lengths_", test_name, ".pdf")),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, paste0("all_tools_cilia_total_lengths_", test_name, ".png")),
         width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, paste0("all_tools_cilia_total_lengths_", test_name, ".emf")),
  #        width = 297, height = 210, units = "mm", device = emf)
  
  
  plot_total_length_violin_statistical_test <-
    ggplot(df_results, aes(x=tool, y=total_length_in_um, color=tool)) +
    geom_violin() +
    stat_boxplot(geom ='errorbar', width = 0.3, color="black") +
    geom_boxplot(aes(fill = tool), width=0.2, alpha = 1, color="black") +
    # geom_beeswarm() +
    stat_summary(fun=mean, geom="point", size = 3, shape=23, color="black", fill="black") +
    scale_y_continuous(breaks= pretty_breaks()) +
    coord_cartesian(ylim=c(0, 9)) +
    # ylim(0,5) +
    scale_color_manual(values=c("#009E73", "#1e3a80")) +
    scale_fill_manual(values=c("grey90", "grey90", "grey90")) +
    # geom_jitter(color="black", size=0.5, alpha=0.9) +
    #ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(),
      legend.position = "none",
      axis.ticks.x = element_blank()) +
    ylab("Total cilium length in \u03BCm") +
    xlab("Detection tool") +
    facet_grid(.~cultivation) +
    theme(strip.background = element_rect(fill = "white")) +
    # ggprism::add_pvalue(pairwise_comparison_result, tip.length = 0.01, hide.ns = FALSE)
    stat_pvalue_manual(data = pairwise_comparison_result, y.position = ypositions, tip.length = 0.01, hide.ns = TRUE) +
    EnvStats::stat_n_text(
      y.pos = 6.2,
      color = "black",
      text.box = FALSE
    )
  
  ggsave(filename = file.path(output_dir, paste0("all_tools_cilia_total_lengths_violin_plot_", test_name, ".pdf")),
         width = 297, height = 210, units = "mm")
  ggsave(filename = file.path(output_dir, paste0("all_tools_cilia_total_lengths_violin_plot_", test_name, ".png")),
         width = 297, height = 210, units = "mm")
  # ggsave(filename = file.path(output_dir, paste0("all_tools_cilia_total_lengths_violin_plot_", test_name, ".emf")),
  #        width = 297, height = 210, units = "mm", device = emf)
  
}
