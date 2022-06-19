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
pkgs <- c("tidyverse", "rqdatatable", "rquery")
groundhog.library(pkgs, groundhog.day)

# Please adapt the following parameters ####################################

# File containing the results of detectCilia (including which cilia
# are to be removed)
input_file_automatic <- "data/automaticDetection/cultivation/summary_cilia_edited.csv"
input_file_automatic_parameters  <- "data/automaticDetection/cultivation/summary_parameters.csv"
input_file_metadata  <- "data/automaticDetection/cultivation/summary_metadata.csv"

input_file_manual <- "data/manualDetection/cultivation/df_manual_results.csv"
input_file_cilium_numbers <- "data/manualDetection/cultivation/originalFiles_csv/cilia_numbers_clemens_automatic.csv"

# parameters_automatic_detection <- "data/automaticDetection/coating/summary_parameters.csv"

# Output directory
output_dir <- "plots/manualAutomaticComparison"

# Import data ##############################################################
df_results_automatic <- readr::read_csv(file = input_file_automatic, name_repair = "universal")
df_results_manual <- readr::read_csv(file = input_file_manual, name_repair = "universal")
df_cilium_numbers_mapping <- readr::read_csv(file = input_file_cilium_numbers, name_repair = "universal")
df_metadata <- readr::read_csv(file = input_file_metadata, name_repair = "universal")
df_parameters <- readr::read_csv(file = input_file_automatic_parameters, name_repair = "universal")

# Filter and clean the automatic detection images ##########################

# Keep only results of seven test images
name_of_test_images <- gsub(pattern = "(.+)_[0-9]{1,2}$", replacement = "\\1", x = df_results_manual$image_name)
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



# Add meta information of automatic detection

#TODO: add/remove .czi in filename in df_results_automatic
df_results_automatic$fileName <- paste(df_results_automatic$fileName, ".czi", sep="")

df_results_automatic <- dplyr::left_join(x = df_results_automatic, y = df_metadata, by = "fileName")

df_parameters$fileName <- basename(df_parameters$input_file_czi)
df_results_automatic <- dplyr::left_join(x = df_results_automatic, y = df_parameters, by = "fileName")


# Convert to lengths/heights to pixel/z-stack layer values

df_results_automatic$horizontal_length_in_pixels <- 
  df_results_automatic$horizontal_length_in_um / df_results_automatic$pixel_size

df_results_automatic$zstack_layers <- 
  df_results_automatic$vertical_length_in_um / df_results_automatic$scaling_z_in_um


# Prepare automatic detection tibble for binding with manual ###############

# Rename columns
df_results_automatic <- df_results_automatic %>% 
  dplyr::rename("image_name" = "fileName",
                "cilium_number_automatic" = "cilium")

# Keep specific columns that also occur in df_results_manual
df_results_automatic <- df_results_automatic %>% 
  dplyr::select(c(names(df_results_automatic)[names(df_results_automatic)  %in% names(df_results_manual)],
                  "cilium_number_automatic"))

# Add columns
df_results_automatic$image_name_short <- as.numeric(
  gsub(pattern = ".+_([0-9]{1,2}).czi$", replacement = "\\1",
       x = df_results_automatic$image_name, ignore.case = TRUE))

df_results_automatic$researcher <- "automatic"

# Add cilia mapping for automatic cilia detection ##########################

# Checking for mapping mistakes
# (It is not a mistake if in one image, there are two cilia detected
mapping_mistakes <-
  df_cilium_numbers_mapping[duplicated(paste(df_cilium_numbers_mapping$image_name, df_cilium_numbers_mapping$cilium_number_automatic, sep=" ")) &
               !is.na(df_cilium_numbers_mapping$cilium_number_automatic),]

if(nrow(mapping_mistakes) > 0){
  print(paste("There might be a mapping mistake with the automatic data.",
              "The following cilium number(s) occurs more than once.", sep=" "))
  print(mapping_mistakes)
}

mapping_mistakes <-
  df_cilium_numbers_mapping[duplicated(paste(df_cilium_numbers_mapping$image_name, df_cilium_numbers_mapping$cilium_number_clemens, sep=" ")) &
               !is.na(df_cilium_numbers_mapping$cilium_number_clemens),]

if(nrow(mapping_mistakes) > 0){
  print(paste("There might be a mapping mistake with the manual data.",
              "The following cilium number(s) occurs more than once.", sep=" "))
  print(mapping_mistakes)
}

df_results_automatic$image_name <- gsub(pattern = "\\.czi",
                                        replacement = "",
                                        x = df_results_automatic$image_name,
                                        ignore.case = TRUE)

df_results_automatic <- rquery::natural_join(a = df_results_automatic,
                                             b = df_cilium_numbers_mapping,
                                             by = c("image_name", "cilium_number_automatic"))

# Filter the manual detection images #######################################

df_results_manual <- df_results_manual[grepl(
  pattern = name_of_test_images,
  x = df_results_manual$image_name, fixed = TRUE), ]

# Add data from automatic detection to manual detection. tibble ############

# Add missing columns
df_results_automatic$cilium_number_kai <- NA
df_results_automatic$horizontal_scaling_in_um <- df_parameters$pixel_size[1]
df_results_automatic$vertical_scaling_in_um <- df_parameters$slice_distance[1]
df_results_automatic$z_lower <- NA
df_results_automatic$z_upper <- NA

df_results_manual$cilium_number_automatic <- NA

df_combined <- dplyr::add_row(df_results_manual, df_results_automatic)


# Save final tibble ########################################################

dir.create(output_dir, showWarnings = FALSE)

readr::write_csv(x = df_combined, file = paste(output_dir, "/combined_manual_automatic_results.csv", sep=""))
readr::write_csv2(x = df_combined, file = paste(output_dir, "/combined_manual_automatic_results_de.csv", sep=""))




# Plot results #############################################################


# Plot threshold_find values of automatic detection
plot_thresholds <- ggplot2::ggplot(data = df_results_automatic,
                                   aes(x=threshold_find)) +
  geom_histogram(binwidth = 0.001) +
  xlim(c(0,0.1)) +
  labs(title = "Histogram of thresholds of automatic detection") +
  ggtitle(paste("Threshold (find) of ", name_of_test_images, sep=""))+
  theme_bw()

ggsave(filename = paste(output_dir, "hist_threshold_automatic_detection.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "hist_threshold_automatic_detection.png", sep="/"),
       width = 297, height = 210, units = "mm")


# Plot results of every image

images <- unique(df_combined$image_name)

for(i in 1:length(images)){
  current_image <- images[i]
  
  df_dummy <- df_combined[df_combined$image_name == current_image,]
  df_dummy$cilium_number_clemens <- as.factor(df_dummy$cilium_number_clemens)
  df_dummy$researcher <- as.factor(df_dummy$researcher)
  
  legend_name <- "Rater"
  plot_horizontal_length_image <- ggplot(df_dummy, aes(x=cilium_number_clemens, y=horizontal_length_in_pixels, color=researcher)) +
    # stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    # geom_boxplot(alpha = 1, position = position_dodge2(width = 0.9, preserve = "single"), outlier.shape = 1) +
    # #geom_jitter(color="black", size=0.5, alpha=0.9) +
    geom_point(size=4, alpha=0.7, aes(shape=researcher)) +
    scale_shape_manual(values=c(15, 17, 18, 19),  name = legend_name) +
    scale_color_discrete(name = legend_name) +
    ylim(0,25) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    ylab("Horizontal cilium length in pixels") +
    xlab("Cilium number") +
    ggtitle(paste(current_image, sep=""))
  
  # print(plot_horizontal_length_image)
  
  ggsave(filename = paste(output_dir, paste("horizontal_length_per_rater_per_image_",df_dummy$image_name_short[1],".pdf",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, paste("horizontal_length_per_rater_per_image_",df_dummy$image_name_short[1],".png",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  
  
  plot_height_image <- ggplot(df_dummy, aes(x=cilium_number_clemens, y=zstack_layers, color=researcher)) +
    # stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
    # geom_boxplot(alpha = 1, position = position_dodge2(width = 0.9, preserve = "single"), outlier.shape = 1) +
    #geom_jitter(color="black", size=0.5, alpha=0.9) +
    geom_point(size=4, alpha=0.7, aes(shape=researcher)) +
    scale_shape_manual(values=c(15, 17, 18, 19),  name = legend_name) +
    scale_color_discrete(name = legend_name) +
    ylim(0,20) +
    theme_bw(base_size = 18) +
    theme(#axis.title.y=element_text(size=12),
      #axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5)) +
    ylab("Height (vertical cilium length) in z-stack layers") +
    xlab("Cilium number") +
    ggtitle(paste(current_image, sep=""))
  
  # print(plot_height_image)
  
  ggsave(filename = paste(output_dir, paste("vertical_length_per_rater_per_image_",df_dummy$image_name_short[1],".pdf",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  ggsave(filename = paste(output_dir, paste("vertical_length_per_rater_per_image_",df_dummy$image_name_short[1],".png",sep=""), sep="/"),
         width = 297, height = 210, units = "mm")
  

}

rm(list = c("i", "df_dummy"))


# Total lengths of cilia
df_combined$image_name_short <- as.factor(df_combined$image_name_short)

plot_total_length <- ggplot(df_combined, aes(x=image_name_short, y=total_length_in_um, color=researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
  ylim(1,7) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Total cilium length in \u03BCm") +
  xlab("Image number") +
  scale_color_discrete(name="Rater")

# print(plot_total_length)

ggsave(filename = paste(output_dir, "comparison_man_aut_length.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_length.png", sep="/"),
       width = 297, height = 210, units = "mm")

# Vertical lengths of cilia in z-stack layers
plot_height <- ggplot(df_combined, aes(x=image_name_short, y=zstack_layers, color=researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
  ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Height (vertical cilium length) in z-stack layers") +
  xlab("Image number") +
  scale_color_discrete(name="Rater")

# print(plot_height)

ggsave(filename = paste(output_dir, "comparison_man_aut_height.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_height.png", sep="/"),
       width = 297, height = 210, units = "mm")


# Horizontal lengths of cilia in pixels
plot_horizontal_length <- ggplot(df_combined, aes(x=image_name_short, y=horizontal_length_in_pixels, color=researcher)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15)) +
  ylim(0,25) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Horizontal cilium length in pixels") +
  xlab("Image number") +
  scale_color_discrete(name="Rater")

# print(plot_horizontal_length)

ggsave(filename = paste(output_dir, "comparison_man_aut_width.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_width.png", sep="/"),
       width = 297, height = 210, units = "mm")


# Mean+-sd of total lengths of cilia in um

df_dummy <- df_combined %>% 
  dplyr::group_by(image_name_short, researcher) %>% 
  dplyr::summarise(mean_length = mean(total_length_in_um), sd_length = sd(total_length_in_um))

plot_total_length_mean_sd <- ggplot(df_dummy, aes(x=image_name_short, y=mean_length, color=researcher)) + 
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
  scale_color_discrete(name="Rater")

# print(plot_total_length_mean_sd)

ggsave(filename = paste(output_dir, "comparison_man_aut_length_mean_sd.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_man_aut_length_mean_sd.png", sep="/"),
       width = 297, height = 210, units = "mm")

