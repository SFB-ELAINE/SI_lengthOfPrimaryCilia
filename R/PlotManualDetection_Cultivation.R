# Script for working with and plotting results (from csv data) from ++++++++
# the manual detection results                                      ++++++++
# Author: Kai Budde
# Created: 2021/12/12
# Last changed: 2022/06/09


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")

# Load packages
library(groundhog)
pkgs <- c("tidyverse")
groundhog.library(pkgs, groundhog.day)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Please adapt the following parameters ####################################

# File containing the edited results of detectCilia (including which cilia
# are to be removed)
input_file_manual <- "data/manualDetection/190815_AscDexa/df_manual_results_en.csv"
output_dir <- "plots/manualComparison"

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Import and clean data ####################################################

# Manual detection results
df_results <-  readr::read_csv(file = input_file_manual, name_repair = "universal")

df_results$image <- as.factor(as.numeric(gsub(pattern = ".+_([0-9]{1,2})", replacement = "\\1", x = df_results$Image_name)))

# Plot results #############################################################

dir.create(output_dir, showWarnings = FALSE)

# Total lengths of cilia in um
plot_total_length <- ggplot(df_results, aes(x=image, y=Total_length_in_um, color=Researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(width = 0.9, preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(), size=1, alpha=0.9) +
  ylim(0,8) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Total cilium length in \u03BCm") +
  xlab("Image number") +
  scale_color_discrete(name="Researcher")

# print(plot_total_length)

ggsave(filename = paste(output_dir, "manual_detection_total_length_per_researchers.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "manual_detection_total_length_per_researchers.png", sep="/"),
       width = 297, height = 210, units = "mm")


# Mean+-sd of total lengths of cilia in um

df_dummy <- df_results %>% 
  dplyr::group_by(Image_name_short, Researcher) %>% 
  dplyr::summarise(mean_length = mean(Total_length_in_um), sd_length = sd(Total_length_in_um))

plot_total_length_meand_sd <- ggplot(df_dummy, aes(x=Image_name_short, y=mean_length, color=Researcher)) + 
  geom_point(size=3, alpha=0.7, position=position_dodge(0.5))+
  geom_errorbar(aes(ymin=mean_length-sd_length, ymax=mean_length+sd_length), width=.2, size=1, alpha=0.7,
                position=position_dodge(0.5)) +
  ylim(0,8) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  ylab("Total cilium length in \u03BCm (mean and sd)") +
  xlab("Image number") +
  scale_color_discrete(name="Researcher")

# print(plot_total_length_meand_sd)

ggsave(filename = paste(output_dir, "manual_detection_total_length_mean_sd_per_researchers.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "manual_detection_total_length_mean_sd_per_researchers.png", sep="/"),
       width = 297, height = 210, units = "mm")


# Horizontal lengths of cilia in pixels
plot_horizontal_length <- ggplot(df_results, aes(x=image, y=Horizontal_length_in_pixels, color=Researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(width = 0.9, preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(), size=1, alpha=0.9) +
  ylim(0,30) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Horizontal cilium length in pixels") +
  xlab("Image number") +
  scale_color_discrete(name="Researcher")

print(plot_horizontal_length)

ggsave(filename = paste(output_dir, "manual_detection_horizontal_length_per_researchers.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "manual_detection_horizontal_length_per_researchers.png", sep="/"),
       width = 297, height = 210, units = "mm")

# Subplots for every image number
image_numbers <- unique(df_results$Image_name_short)

for(i in image_numbers){
  
  df_dummy <- df_results[df_results$Image_name_short == i,]
  
  df_dummy$Cilium_number_clemens <- as.factor(df_dummy$Cilium_number_clemens)
  
  plot_horizontal_length_image <- ggplot(df_dummy, aes(x=Cilium_number_clemens, y=Horizontal_length_in_pixels, color=Researcher)) +
    # stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    # geom_boxplot(alpha = 1, position = position_dodge2(width = 0.9, preserve = "single"), outlier.shape = 1) +
    # #geom_jitter(color="black", size=0.5, alpha=0.9) +
    geom_point(size=3, alpha=0.7, shape=19) +
    ylim(0,25) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    ylab("Horizontal cilium length in pixels") +
    xlab("Cilium number") +
    ggtitle(paste("Image ", i, sep="")) +
    scale_color_discrete(name="Researcher")
  
  # print(plot_horizontal_length_image)
  
  ggsave(filename = paste(output_dir, paste("manual_detection_horizontal_length_per_researchers_per_image_",i,".pdf",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, paste("manual_detection_horizontal_length_per_researchers_per_image_",i,".png",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  
  df_dummy2 <- df_dummy %>% 
    dplyr::group_by(Cilium_number_clemens) %>% 
    dplyr::summarise(mean_length = mean(Horizontal_length_in_pixels), sd_length = sd(Horizontal_length_in_pixels))
  
  plot_horizontal_length_image_mean <- ggplot(df_dummy2, aes(x=Cilium_number_clemens, y=mean_length)) + 
    geom_point()+
    geom_errorbar(aes(ymin=mean_length-sd_length, ymax=mean_length+sd_length), width=.2,
                  position=position_dodge(0.05)) +
    ylim(0,25) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    ylab("Horizontal cilium length in pixels (mean and sd)") +
    xlab("Cilium number") +
    ggtitle(paste("Image ", i, sep=""))
  
  # print(plot_horizontal_length_image_mean)
  
  ggsave(filename = paste(output_dir, paste("manual_detection_horizontal_length_mean_sd_per_image_",i,".pdf",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, paste("manual_detection_horizontal_length_mean_sd_per_image_",i,".png",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  
}


# Vertical lengths of cilia in z-stack layers
plot_height <- ggplot(df_results, aes(x=image, y=zstack_layers, color=Researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(width = 0.9, preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(), size=1, alpha=0.9) +
  ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Height (vertical cilium length) in z-stack layers") +
  xlab("Image number") +
  scale_color_discrete(name="Researcher")

# print(plot_height)

ggsave(filename = paste(output_dir, "manual_detection_vertical_length_per_researchers.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "manual_detection_vertical_length_per_researchers.png", sep="/"),
       width = 297, height = 210, units = "mm")


# Subplots for every image number

for(i in image_numbers){
  
  df_dummy <- df_results[df_results$Image_name_short == i,]
  
  df_dummy$Cilium_number_clemens <- as.factor(df_dummy$Cilium_number_clemens)
  
  plot_height_image <- ggplot(df_dummy, aes(x=Cilium_number_clemens, y=zstack_layers, color=Researcher)) +
    # stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    # geom_boxplot(alpha = 1, position = position_dodge2(width = 0.9, preserve = "single"), outlier.shape = 1) +
    #geom_jitter(color="black", size=0.5, alpha=0.9) +
    geom_point(size=3, alpha=0.7, shape=19) +
    ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    ylab("Height (vertical cilium length) in z-stack layers") +
    xlab("Cilium number") +
    ggtitle(paste("Image ", i, sep="")) +
    scale_color_discrete(name="Researcher")
  
  # print(plot_height_image)
  
  ggsave(filename = paste(output_dir, paste("manual_detection_vertical_length_per_researchers_per_image_",i,".pdf",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, paste("manual_detection_vertical_length_per_researchers_per_image_",i,".png",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  
  df_dummy2 <- df_dummy %>% 
    dplyr::group_by(Cilium_number_clemens) %>% 
    dplyr::summarise(mean_layer_number = mean(zstack_layers), sd_layer_number = sd(zstack_layers))
  
  plot_height_image_mean <- ggplot(df_dummy2, aes(x=Cilium_number_clemens, y=mean_layer_number)) + 
    geom_point()+
    geom_errorbar(aes(ymin=mean_layer_number-sd_layer_number, ymax=mean_layer_number+sd_layer_number), width=.2,
                  position=position_dodge(0.05)) +
    ylim(0,30) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    ylab("Height (vertical cilium length) (mean and sd)") +
    xlab("Cilium number") +
    ggtitle(paste("Image ", i, sep=""))
  
  # print(plot_height_image_mean)
  
  ggsave(filename = paste(output_dir, paste("manual_detection_vertical_length_mean_sd_per_image_",i,".pdf",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, paste("manual_detection_vertical_length_mean_sd_per_image_",i,".png",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  
}
