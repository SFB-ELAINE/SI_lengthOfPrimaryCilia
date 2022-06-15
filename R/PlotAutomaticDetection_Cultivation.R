# Script for plotting results from manual and automatic detection   ++++++++
# of cilia in seven test images                                     ++++++++
# Author: Kai Budde
# Created: 2021/11/08
# Last changed: 2022/06/15


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
input_file_manual <- "data/manualDetection/cultivation/df_manual_results.csv"
output_dir <- "plots"

# Import and clean data ####################################################
df_results_automatic <- readr::read_csv(file = input_file_automatic, name_repair = "universal")
df_results_manual <- readr::read_csv(file = input_file_manual, name_repair = "universal")


# Keep only results of seven test images
name_of_test_images <- gsub(pattern = "(.+)_[0-9]{1,2}$", replacement = "\\1", x = df_results_manual$Image_name)
name_of_test_images <- unique(name_of_test_images)
if(length(name_of_test_images) > 1){
  print("Something went wrong. We have more than one test image name.")
}

df_results_automatic <- df_results_automatic[grepl(
  pattern = name_of_test_images,
  x = df_results_automatic$fileName, fixed = TRUE), ]

print(paste("We are deleting ", sum(df_results_automatic$to_be_removed != "no"),
            " cilium(a) from the automatic cilia detection because we have manually ",
            "marked it being a non-cilium structure.", sep=""))

df_results_automatic <- df_results_automatic[df_results_automatic$to_be_removed == "no",]


df_results_manual <- df_results_manual[grepl(
  pattern = name_of_test_images,
  x = df_results_manual$Image_name, fixed = TRUE), ]


# Add information of cultivation

# cultivations <- df_results$fileName
# cultivations <- gsub(pattern = "(.+)_zstack.+", replacement = "\\1", x = cultivations)
# cultivations <- unique(cultivations)

df_results_automatic$cultivation <- "TBA"

ITS with Asc, ITS with Asc + Dexa, ITS with Asc + Dexa and IGF + TGF, FBS with Asc 

df_results_automatic$cultivation[grepl(pattern = "Kollagen mit WF", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ GF"
df_results_automatic$cultivation[grepl(pattern = "Kollagen mit FKS", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ FCS"
df_results_automatic$cultivation[grepl(pattern = "Kollagen nur Asc", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ Asc"
df_results_automatic$cultivation[grepl(pattern = "Kollagen mit Asc\\+Dexa", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ Asc & Dexa"
df_results_automatic$cultivation[grepl(pattern = "Kollagen mit Asc u Dexa", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ Asc & Dexa"
df_results_automatic$cultivation[grepl(pattern = "Glas mit WF", x = df_results$fileName, ignore.case = TRUE)] <- "Glass w/ GF"

# df_test <- df_results[df_results$cultivation == "TBA",]

# Plot results #############################################################

#TODO: Reihenfolge der Daten:
# ITS with Asc, ITS with Asc + Dexa, ITS with Asc + Dexa and IGF + TGF, FBS with Asc 

# Total lengths of cilia
plot_total_length <- ggplot(df_results, aes(x=cultivation, y=total_length)) +
  stat_boxplot(geom ='errorbar', width = 0.3) +
  geom_boxplot(alpha = 1) +
  geom_jitter(color="black", size=0.5, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ylab( "Total cilium length in \u03BCm") +
  xlab("Cultivation")

#print(plot_total_length)

ggsave(filename = paste(output_dir, "cilia_total_lengths.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "cilia_total_lengths.png", sep="/"),
       width = 297, height = 210, units = "mm")
