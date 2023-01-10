# Script for working with and plotting results (from csv data) from ++++++++
# the R package detectCilia as well as manual detection results     ++++++++
# Author: Kai Budde
# Created: 2021/11/11
# Last changed: 2022/09/16

# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()

# Set groundhog day for reproducibility (see https://groundhogr.com)
groundhog.day <- "2022-03-01"

# install.packages("groundhog")

# Load packages
library(groundhog)
pkgs <- c("tidyverse", "tidymodels", "rstatix")
groundhog.library(pkgs, groundhog.day)


# Please adapt the following parameters ####################################

# File containing the edited results of detectCilia (including which cilia
# are to be removed)
input_file_automatic <- "data/automaticDetection/resolution/summary_cilia.csv"
input_file_manual <- "data/manualDetection/resolution/df_manual_results.csv"
output_dir <- "plots"

# Import and clean data ####################################################

# Automatic detection (using detectCilia)
df_results_auto <- readr::read_csv(file = input_file_automatic, name_repair = "universal")
df_results_auto <- df_results_auto[df_results_auto$to_be_removed != "yes", ]

# Manual detection (using Fiji)
df_results_man <- readr::read_csv(file = input_file_manual, name_repair = "universal")

# Add information of experiment
df_results_auto$location <- as.numeric(gsub(pattern = ".+stack_([0-9]+)_.+",
                                            replacement = "\\1",
                                            x = df_results_auto$fileName))
# df_results_auto$location <- "TBA"
# df_results_auto$location[grepl(pattern = "_1$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1"
# df_results_auto$location[grepl(pattern = "_1_fail half", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1"
# df_results_auto$location[grepl(pattern = "_2_looks ok$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "2"
# df_results_auto$location[grepl(pattern = "_3_speed7$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "3"
# df_results_auto$location[grepl(pattern = "_4_iO$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "4"
# df_results_auto$location[grepl(pattern = "_5$", x = df_results_auto$fileName, ignore.case = TRUE)] <- "5"

df_results_auto$resolution <- as.numeric(gsub(pattern = ".+_[0-9]{4}x([0-9]{4})\\.czi",
                                            replacement = "\\1",
                                            x = df_results_auto$fileName))
# df_results_auto$resolution <- "TBA"
# df_results_auto$resolution[grepl(pattern = "1024", x = df_results_auto$fileName, ignore.case = TRUE)] <- "1024"
# df_results_auto$resolution[grepl(pattern = "2048", x = df_results_auto$fileName, ignore.case = TRUE)] <- "2048"
# df_results_auto$resolution[grepl(pattern = "4096", x = df_results_auto$fileName, ignore.case = TRUE)] <- "4096"

df_results_auto$magnification <- as.numeric(gsub(pattern = ".+_([0-9]+)x_z-stack.+",
                                              replacement = "\\1",
                                              x = df_results_auto$fileName))
# df_results_auto$magnification <- "63x"
# df_results_auto$magnification[grepl(pattern = "_100er", x = df_results_auto$fileName, ignore.case = TRUE)] <- "100x"

df_results_auto$detectionMethod <- "automatic"




# Add information of experiment
df_results_man$location <- as.numeric(gsub(pattern = ".+stack_([0-9]+)_.+",
                                            replacement = "\\1",
                                            x = df_results_man$fileName))
df_results_man$resolution <- as.numeric(gsub(pattern = ".+_[0-9]{4}x([0-9]{4})\\.czi",
                                              replacement = "\\1",
                                              x = df_results_man$fileName))
df_results_man$magnification <- as.numeric(gsub(pattern = ".+_([0-9]+)x_z-stack.+",
                                                 replacement = "\\1",
                                                 x = df_results_man$fileName))
df_results_man$detectionMethod <- "manual"

# Combination of automatic and manual detection
keep_the_columns <- c("fileName", "total_length_in_um", "horizontal_length_in_um", "vertical_length_in_um", "location", "resolution", "magnification", "detectionMethod")

df_results <- dplyr::bind_rows(df_results_auto[,keep_the_columns],
                               df_results_man[,keep_the_columns])

df_results$image <- paste("loc", df_results$location, "\n", "magn", df_results$magnification, "\n", "res", df_results$resolution, sep = "")
rownames(df_results) <- NULL

# Add missing rows such that the plot is equally designed for every
# x-value even if a detectionMethod is missing

#df_results <- tibble::add_row(.data = df_results, total_length = 0, image = "loc1\nmagn63x\nres2048", detectionMethod = "manual")


# Calculate p-values (t-test)
ttest_horizontal <- df_results %>% 
  group_by(image) %>%
  rstatix::t_test(horizontal_length_in_um ~ detectionMethod)
print(ttest_horizontal)

ttest_total <- df_results %>% 
  group_by(image) %>%
  rstatix::t_test(total_length_in_um ~ detectionMethod)
print(ttest_total)

# significant results for:
# horizontal_length: loc3\nmagn63\nres1024
# total_length: loc2\nmagn63\nres2048, loc3\nmagn63\nres1024, and loc4\nmagn63\nres1024

# ttest combining locations
df_results2 <- df_results
df_results2$image <- gsub(pattern = "loc[0-9]\\n(.+)", replacement = "\\1", x = df_results2$image)

ttest_total_resolution <- df_results2 %>% 
  group_by(image) %>% 
  rstatix::t_test(total_length_in_um ~ detectionMethod)

print(ttest_total_resolution)


ttest_total_resolution_automatic <- df_results2 %>% 
  filter(detectionMethod == "automatic") %>% 
  rstatix::t_test(total_length_in_um ~ image)
print(ttest_total_resolution_automatic)

ttest_total_resolution_manual <- df_results2 %>% 
  filter(detectionMethod == "manual") %>% 
  rstatix::t_test(horizontal_length_in_um ~ image)
print(ttest_total_resolution_manual)

ttest_total_resolution_manual <- df_results2 %>% 
  filter(detectionMethod == "manual") %>% 
  rstatix::t_test(total_length_in_um ~ image)
print(ttest_total_resolution_manual)


# Mean length depending on resolution/magnification
df_mean_lengths <- df_results2 %>% 
  dplyr::group_by(detectionMethod, image) %>%
  dplyr::summarise(mean_in_um = mean(total_length_in_um)) %>% 
  ungroup()


df_lengths_with_uncertainty <- df_results2 %>%
  dplyr::group_by(detectionMethod, image) %>%
  do(model = confint(lm(total_length_in_um ~ 1, data = .)), level = 0.95)

df_lengths_with_uncertainty$mean <- unlist(lapply(X = df_lengths_with_uncertainty$model, mean))
df_lengths_with_uncertainty$confInterval <- (unlist(lapply(X = df_lengths_with_uncertainty$model, max)) -
  unlist(lapply(X = df_lengths_with_uncertainty$model, min))) / 2

print(df_lengths_with_uncertainty)

# Calculate relative differences of the results

df_relative_difference <- df_results %>% 
  dplyr::group_by(fileName, detectionMethod, image) %>%
  dplyr::summarise(mean_in_um = mean(total_length_in_um)) %>% 
  ungroup()

df_relative_difference <- df_relative_difference %>%
  dplyr::group_by(image) %>%
  dplyr::summarise(relDiff = mean_in_um[detectionMethod == "manual"]/mean_in_um[detectionMethod == "automatic"]-1)

df_relative_difference_resolution <- df_relative_difference
df_relative_difference_resolution$image <- gsub(pattern = "loc[0-9]\\n(.+)", replacement = "\\1", x = df_relative_difference_resolution$image)
df_relative_difference_resolution <- df_relative_difference_resolution %>% 
  dplyr::group_by(image) %>%
  dplyr::summarise(average_relDiff = mean(relDiff)) %>% 
  ungroup()

print(df_relative_difference_resolution)
  
# Plot results #############################################################

# Horizontal lengths of cilia
plot_horizontal_length <- ggplot(df_results, aes(x=image, y=horizontal_length_in_um, color=detectionMethod)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  stat_summary(fun=mean, geom="point", size = 3, shape=17, position = position_dodge(width = 0.75)) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(), size=1, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Horizontal cilium length in \u03BCm") +
  xlab("Location, Magnification, Resolution") +
  scale_color_discrete(name="Cilia\ndetection\nmethod")

# print(plot_horizontal_length)

ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_horizontal_length.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_horizontal_length.png", sep="/"),
       width = 297, height = 210, units = "mm")

# Vertical lengths of cilia
plot_vertical_length <- ggplot(df_results, aes(x=image, y=vertical_length_in_um, color=detectionMethod)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  stat_summary(fun=mean, geom="point", size = 3, shape=17, position = position_dodge(width = 0.75)) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(), size=1, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Vertical cilium length in \u03BCm") +
  xlab("Location, Magnification, Resolution") +
  scale_color_discrete(name="Cilia\ndetection\nmethod")

# print(plot_vertical_length)

ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_vertical_length.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_vertical_length.png", sep="/"),
       width = 297, height = 210, units = "mm")

# Total lengths of cilia
plot_total_length <- ggplot(df_results, aes(x=image, y=total_length_in_um, color=detectionMethod)) +
  stat_boxplot(geom ='errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(alpha = 1, position = position_dodge2(preserve = "single"), outlier.shape = 1) +
  stat_summary(fun=mean, geom="point", size = 3, shape=17, position = position_dodge(width = 0.75)) +
  #geom_jitter(color="black", size=0.5, alpha=0.9) +
  geom_point(position = position_jitterdodge(), size=1, alpha=0.9) +
  #ylim(0,20) +
  theme_bw(base_size = 18) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylab("Total cilium length in \u03BCm") +
  xlab("Location, Magnification, Resolution") +
  scale_color_discrete(name="Cilia\ndetection\nmethod")

# print(plot_total_length)

ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_total_length.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_total_length.png", sep="/"),
       width = 297, height = 210, units = "mm")


# Relative deviation of total lengths of cilia
plot_relative_differences_length <- ggplot(df_relative_difference, aes(x=image, y=relDiff)) +
  geom_point(size = 4) +
  theme_bw(base_size = 18) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1.5) +
  theme(#axis.title.y=element_text(size=12),
    #axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()) +
  ylim(-0.3,0.3) +
  ylab("Relative deviation of mean total lengths (manual/automatic)") +
  xlab("Location, Magnification, Resolution")

# print(plot_relative_differences_length)

ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_relativeDev.pdf", sep="/"),
       width = 297, height = 210, units = "mm")
ggsave(filename = paste(output_dir, "comparison_resolution_man_aut_relativeDev.png", sep="/"),
       width = 297, height = 210, units = "mm")
