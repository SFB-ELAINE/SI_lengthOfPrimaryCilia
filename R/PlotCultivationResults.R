# Script for working with and plotting results (from csv data) from ++++++++
# the R package detectCilia                                         ++++++++
# Author: Kai Budde
# Created: 2021/11/08
# Last changed: 2022/05/09


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

# File containing the edited results of detectCilia (including which cilia
# are to be removed)
input_file <- "data/automaticDetection/cultivation/summary_cilia_de_edited.csv"
output_dir <- "plots"

# Import and clean data ####################################################
df_results <- readr::read_csv2(file = input_file)

df_results <- df_results[df_results$to_be_removed == "no", ]

# Add information of cultivation

# cultivations <- df_results$fileName
# cultivations <- gsub(pattern = "(.+)_zstack.+", replacement = "\\1", x = cultivations)
# cultivations <- unique(cultivations)

df_results$cultivation <- "TBA"
df_results$cultivation[grepl(pattern = "ES3", x = df_results$fileName, ignore.case = TRUE)] <- "ES3-ToRemove"
df_results$cultivation[grepl(pattern = "Kollagen mit WF", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ GF"
df_results$cultivation[grepl(pattern = "Kollagen mit FKS", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ FCS"
df_results$cultivation[grepl(pattern = "Kollagen nur Asc", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ Asc"
df_results$cultivation[grepl(pattern = "Kollagen mit Asc\\+Dexa", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ Asc & Dexa"
df_results$cultivation[grepl(pattern = "Kollagen mit Asc u Dexa", x = df_results$fileName, ignore.case = TRUE)] <- "Coll. w/ Asc & Dexa"
df_results$cultivation[grepl(pattern = "Glas mit WF", x = df_results$fileName, ignore.case = TRUE)] <- "Glass w/ GF"

# df_test <- df_results[df_results$cultivation == "TBA",]

# Plot results #############################################################

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
