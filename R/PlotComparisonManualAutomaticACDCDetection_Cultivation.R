# Script for plotting results from manual, automatic and ACDC detection ++++
# of cilia in seven test images                                         ++++
# Author: Kai Budde
# Created: 2022/07/22
# Last changed: 2022/07/25


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
library(groundhog)  #used version: 2.0.0
pkgs <- c("tidyverse", "rqdatatable", "rquery")
groundhog.library(pkgs, groundhog.day)

# Please adapt the following parameters ####################################

input_file_ACDC <- "ACDC/ACDCresults/Cilia Report 22-Jul-2022 15-53-35.csv"
input_file_manaul_automatic_results <- "plots/manualAutomaticComparison/combined_manual_automatic_results.csv"
pixel_length <- 0.219647042583026 #in um

# Output directory
output_dir <- "plots/manualAutomaticACDCComparison/"

# Import data ##############################################################
df_results_ACDC <- readr::read_csv(file = input_file_ACDC, name_repair = "universal")
df_combined <- readr::read_csv(file = input_file_manaul_automatic_results, name_repair = "universal")

# Data cleaning ############################################################
df_results_ACDC$image_name_short <- as.numeric(sub(pattern = ".+zstack_([0-9]{1,2})_zstack.+", replacement = "\\1", x = df_results_ACDC$File_name))
# Calculate cilium length in um (it is given in numbers of pixels and not in nm)
df_results_ACDC$total_length_in_um <- df_results_ACDC$Total.Length.nm.*pixel_length
  
# Make new combined tibble including results from ACDC #####################

df_combined_ACDC <- df_combined %>% dplyr::select(image_name_short, "Cilium_ID" = cilium_number_clemens, horizontal_length_in_um, researcher)
df_results_ACDC <- df_results_ACDC %>% dplyr::select(image_name_short, "Cilium_ID"=Cilium.ID, "horizontal_length_in_um" = total_length_in_um)
df_results_ACDC$researcher <- "ACDC"

df_combined_ACDC <- dplyr::bind_rows(df_combined_ACDC, df_results_ACDC)
df_combined_ACDC$image_name_short <- as.factor(df_combined_ACDC$image_name_short)

# ATTENTION: The Cilium IDs of the two researchers do not point towards
# the same object!

# df_combined_ACDC$researcher <- as.factor(df_combined_ACDC$researcher)


# Plots results including ACDC measurements
dir.create(output_dir, showWarnings = FALSE)

plot_total_length_ACDC <- ggplot(df_combined_ACDC, aes(x=image_name_short, y=horizontal_length_in_um, color=researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
  ylim(0,7) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Horizontal cilium length in \u03BCm") +
  xlab("Image number") +
  scale_color_discrete(name="Rater")

# print(plot_total_length_ACDC)

ggsave(filename = paste(output_dir, "comparison_man_aut_ACDC_horizontal_length.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_ACDC_horizontal_length.png", sep="/"),
       width = 297, height = 210, units = "mm")


df_combined_ACDC <- df_combined_ACDC[!is.na(df_combined_ACDC$horizontal_length_in_um),]

df_dummy <- df_combined_ACDC %>% 
  dplyr::group_by(image_name_short, researcher) %>% 
  dplyr::summarise(mean_length = mean(horizontal_length_in_um), sd_length = sd(horizontal_length_in_um))

plot_total_length_ACDC_mean_sd <- ggplot(df_dummy, aes(x=image_name_short, y=mean_length, color=researcher)) + 
  geom_point(size=3, alpha=0.7, position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=mean_length-sd_length, ymax=mean_length+sd_length), width=.2, size=1, alpha=0.7,
                position=position_dodge(0.5)) +
  ylim(0,6) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ylab("Horizontal cilium length in \u03BCm (mean and sd)") +
  xlab("Image number") +
  scale_color_discrete(name="Rater")

# print(plot_total_length_ACDC_mean_sd)

ggsave(filename = paste(output_dir, "comparison_man_aut_ACDC_horizontal_length_mean_sd.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_ACDC_horizontal_length_mean_sd.png", sep="/"),
       width = 297, height = 210, units = "mm")

