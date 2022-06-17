# Script for plotting results from manual and automatic detection   ++++++++
# of cilia in seven test images                                     ++++++++
# Author: Kai Budde
# Created: 2021/11/08
# Last changed: 2022/06/17


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Load packages ############################################################

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
  install.packages("groundhog")
}

# Load packages
library(groundhog)
pkgs <- c("tidyverse")
groundhog.library(pkgs, groundhog.day)

# Please adapt the following parameters ####################################

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file_automatic <- "data/automaticDetection/cultivation/summary_cilia_edited.csv"
output_dir <- "plots"

# Import and clean data ####################################################
df_results_automatic <- readr::read_csv(file = input_file_automatic, name_repair = "universal")


# Add information of cultivation

# cultivations <- df_results$fileName
# cultivations <- gsub(pattern = "(.+)_zstack.+", replacement = "\\1", x = cultivations)
# cultivations <- unique(cultivations)

df_results_automatic$cultivation <- NA

names_of_experiments <- c("ITS w/ Asc", "ITS w/ Asc + Dexa",
                          "ITS w/ Asc + Dexa and IGF + TGF",
                          "FBS w/ Asc")

df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[1]
df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc+Dexa_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[2]
df_results_automatic$cultivation[grepl(pattern = "ITSwithAsc+Dexa+IGF+TGF_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[3]
df_results_automatic$cultivation[grepl(pattern = "FBSwithAsc_", x = df_results_automatic$fileName, fixed = TRUE)] <- names_of_experiments[4]

if(sum(is.na(df_results_automatic$cultivation)) > 0){
  print("Something went wrong with naming the cultivation conditiosn.")
}

df_results_automatic$cultivation <- factor(df_results_automatic$cultivation, levels = names_of_experiments)

# Plot results #############################################################

# Total lengths of cilia
plot_total_length <- ggplot(df_results_automatic, aes(x=cultivation, y=total_length_in_um)) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(alpha = 1) +
  geom_jitter(color="black", size=0.5, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ylab("Total cilium length in \u03BCm") +
  xlab("Cultivation")

ggsave(filename = paste(output_dir, "all_cilia_total_lengths.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "all_cilia_total_lengths.png", sep="/"),
       width = 297, height = 210, units = "mm")


plot_total_length_violin <- ggplot(df_results_automatic, aes(x=cultivation, y=total_length_in_um)) +
  geom_violin() +
  # stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width=0.1) +
  # geom_jitter(color="black", size=0.5, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ylab( "Total cilium length in \u03BCm") +
  xlab("Cultivation")


ggsave(filename = paste(output_dir, "all_cilia_total_lengths_violin_plot.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "all_cilia_total_lengths_violin_plot.png", sep="/"),
       width = 297, height = 210, units = "mm")



# Plot data that shall not be removed
#TODO: Go through every image and check for bad cilia

df_results_automatic_filtered <- df_results_automatic[!grepl(pattern = "yes", x = df_results_automatic$to_be_removed, ignore.case = TRUE),]



# Total lengths of cilia
plot_total_length <- ggplot(df_results_automatic_filtered, aes(x=cultivation, y=total_length_in_um)) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(alpha = 1) +
  geom_jitter(color="black", size=0.5, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ylab("Total cilium length in \u03BCm") +
  xlab("Cultivation")

ggsave(filename = paste(output_dir, "all_filtered_cilia_total_lengths.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "all_filtered_cilia_total_lengths.png", sep="/"),
       width = 297, height = 210, units = "mm")


plot_total_length_violin <- ggplot(df_results_automatic_filtered, aes(x=cultivation, y=total_length_in_um)) +
  geom_violin() +
  # stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(width=0.1) +
  # geom_jitter(color="black", size=0.5, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ylab( "Total cilium length in \u03BCm") +
  xlab("Cultivation")


ggsave(filename = paste(output_dir, "all_filtered_cilia_total_lengths_violin_plot.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "all_filtered_cilia_total_lengths_violin_plot.png", sep="/"),
       width = 297, height = 210, units = "mm")


