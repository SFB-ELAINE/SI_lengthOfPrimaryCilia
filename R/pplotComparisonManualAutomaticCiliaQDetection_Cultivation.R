# Script for plotting results from manual, automatic and ciliaq detection   ++++++++
# of cilia in seven test images                                             ++++++++
# Author: Kai Budde
# Created: 2022/06/17
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
pkgs <- c("tidyverse", "rqdatatable", "rquery")
groundhog.library(pkgs, groundhog.day)

# Please adapt the following parameters ####################################

input_file_ciliaq <- "CiliaQ/output/ciliaq_data.csv"
input_file_manaul_automatic_results <- "plots/manualAutomaticComparison/combined_manual_automatic_results.csv"

# Output directory
output_dir <- "plots/manualAutomaticCiliaQComparison/"

# Import data ##############################################################
df_results_ciliaq <- readr::read_csv(file = input_file_ciliaq, name_repair = "universal")
df_combined <- readr::read_csv(file = input_file_manaul_automatic_results, name_repair = "universal")


# Make new combined tibble including results from CiliaQ ###################

df_combined_ciliaQ <- df_combined %>% dplyr::select(image_name_short, "Cilium_ID" = cilium_number_clemens, total_length_in_um, researcher)
df_results_ciliaq <- df_results_ciliaq %>% dplyr::select(image_name_short, "Cilium_ID"=ID, "total_length_in_um" = cilia.length..micron.)
df_results_ciliaq$researcher <- "ciliaq"

df_combined_ciliaQ <- dplyr::bind_rows(df_combined_ciliaQ, df_results_ciliaq)
df_combined_ciliaQ$image_name_short <- as.factor(df_combined_ciliaQ$image_name_short)

# df_combined_ciliaQ$researcher <- as.factor(df_combined_ciliaQ$researcher)


# Plots results including ciliaq measurements
dir.create(output_dir, showWarnings = FALSE)

plot_total_length_ciliaq <- ggplot(df_combined_ciliaQ, aes(x=image_name_short, y=total_length_in_um, color=researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
  ylim(0,7) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Total cilium length in \u03BCm") +
  xlab("Image number") +
  scale_color_discrete(name="Rater")

# print(plot_total_length)

ggsave(filename = paste(output_dir, "comparison_man_aut_ciliaq_length.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_ciliaq_length.png", sep="/"),
       width = 297, height = 210, units = "mm")


df_combined_ciliaQ <- df_combined_ciliaQ[!is.na(df_combined_ciliaQ$total_length_in_um),]

df_dummy <- df_combined_ciliaQ %>% 
  dplyr::group_by(image_name_short, researcher) %>% 
  dplyr::summarise(mean_length = mean(total_length_in_um), sd_length = sd(total_length_in_um))

plot_total_length_ciliaq_mean_sd <- ggplot(df_dummy, aes(x=image_name_short, y=mean_length, color=researcher)) + 
  geom_point(size=3, alpha=0.7, position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=mean_length-sd_length, ymax=mean_length+sd_length), width=.2, size=1, alpha=0.7,
                position=position_dodge(0.5)) +
  ylim(0,6) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ylab("Total cilium length in \u03BCm (mean and sd)") +
  xlab("Image number") +
  scale_color_discrete(name="Rater")

# print(plot_total_length_mean_sd)

ggsave(filename = paste(output_dir, "comparison_man_aut_ciliaq_length_mean_sd.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_ciliaq_length_mean_sd.png", sep="/"),
       width = 297, height = 210, units = "mm")
