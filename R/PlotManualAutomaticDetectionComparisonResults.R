# Script for working with and plotting results (from csv data) from ++++++++
# the R package detectCilia as well as manual detection results     ++++++++
# Author: Kai Budde
# Created: 2021/11/11
# Last changed: 2021/11/1


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

library(ggplot2)

# Please adapt the following parameters ####################################

# File containing the edited results of detectCilia (including which cilia
# are to be removed)
input_file_automatic <- "data/automaticDetection/resolution/summary_cilia_de_edited.csv"
input_file_manual <- "data/manualDetection/resolution/additional_images_manual_detection_de.csv"
output_dir <- "plots"

# Import and clean data ####################################################

# Automatic detection (using detectCilia)
df_results_auto <- read.csv2(file = input_file_automatic)

df_results_auto <- df_results_auto[df_results_auto$to_be_removed == "no", ]

# Add information of experiment
df_results_auto$location <- "TBA"
df_results_auto$location[grepl(pattern = "_1$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1"
df_results_auto$location[grepl(pattern = "_1_fail half", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1"
df_results_auto$location[grepl(pattern = "_2_looks ok$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "2"
df_results_auto$location[grepl(pattern = "_3_speed7$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "3"
df_results_auto$location[grepl(pattern = "_4_iO$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "4"
df_results_auto$location[grepl(pattern = "_5$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "5"

df_results_auto$resolution <- "TBA"
df_results_auto$resolution[grepl(pattern = "1024", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1024"
df_results_auto$resolution[grepl(pattern = "2048", x = df_results_auto$fileName, ignore.case = TRUE)] <- "2048"
df_results_auto$resolution[grepl(pattern = "4096", x = df_results_auto$fileName, ignore.case = TRUE)] <- "4096"

df_results_auto$magnification <- "63x"
df_results_auto$magnification[grepl(pattern = "_100er", x = df_results_auto$fileName, ignore.case = TRUE)] <- "100x"

df_results_auto$detectionMethod <- "automatic"


# Manual detection (using ZEN and Fiji)
df_results_man <- read.csv2(file = input_file_manual)

# Add information of experiment
df_results_man$location <- "TBA"
df_results_man$location[grepl(pattern = "_1$", x = df_results_man$fileName, ignore.case = TRUE)] <- "1"
df_results_man$location[grepl(pattern = "_1_fail half", x = df_results_man$fileName, ignore.case = TRUE)] <- "1"
df_results_man$location[grepl(pattern = "_2_looks ok$", x = df_results_man$fileName, ignore.case = TRUE)] <- "2"
df_results_man$location[grepl(pattern = "_2_bleaching$", x = df_results_man$fileName, ignore.case = TRUE)] <- "2"
df_results_man$location[grepl(pattern = "_3_speed7$", x = df_results_man$fileName, ignore.case = TRUE)] <- "3"
df_results_man$location[grepl(pattern = "_4_iO$", x = df_results_man$fileName, ignore.case = TRUE)] <- "4"
df_results_man$location[grepl(pattern = "_5$", x = df_results_man$fileName, ignore.case = TRUE)] <- "5"

df_results_man$resolution <- "TBA"
df_results_man$resolution[grepl(pattern = "1024", x = df_results_man$fileName, ignore.case = TRUE)] <- "1024"
df_results_man$resolution[grepl(pattern = "2048", x = df_results_man$fileName, ignore.case = TRUE)] <- "2048"
df_results_man$resolution[grepl(pattern = "4096", x = df_results_man$fileName, ignore.case = TRUE)] <- "4096"

df_results_man$magnification <- "63x"
df_results_man$magnification[grepl(pattern = "_100er", x = df_results_man$fileName, ignore.case = TRUE)] <- "100x"

df_results_man$detectionMethod <- "manual"

# Combination of automatic and manual detection
keep_the_columns <- c("fileName", "total_length", "location", "resolution", "magnification", "detectionMethod")
df_results <- rbind(df_results_auto[,keep_the_columns],
                    df_results_man[,keep_the_columns])
df_results$image <- paste("loc", df_results$location, "\n", "magn", df_results$magnification, "\n", "res", df_results$resolution, sep = "")

rownames(df_results) <- NULL

# Plot results #############################################################

# Total lengths of cilia
plot_total_length <- ggplot(df_results, aes(x=image, y=total_length, color=detectionMethod)) +
  geom_boxplot(alpha = 0.9) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(), size=1, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  ylab( "Total cilium length in \u03BCm") +
  xlab("Location")

print(plot_total_length)

ggsave(filename = paste(output_dir, "comparison_man_aut.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut.png", sep="/"),
       width = 297, height = 210, units = "mm")
