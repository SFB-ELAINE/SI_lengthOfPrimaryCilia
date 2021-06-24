# Script for combining the results (csv data) from  the R package ++++++++++
# detectCilia                                                     ++++++++++
# Author: Kai Budde
# Created: 20210/06/24
# Last changed: 2021/06/24


# Delete everything in the environment
rm(list = ls())
# close all open plots in RStudio
graphics.off()


# Please adapt the following parameters ####################################

# Directory containing the outputs
input_dir <- "E:/PhD/Daten/Cilia/allImages"


# Filter results directories ###############################################
output_dirs <- list.dirs(input_dir)
output_dirs <- output_dirs[grepl(pattern = "output$", x = output_dirs, ignore.case = TRUE)]

# Read csv files and combine them into one data frame ######################
for(i in 1:length(output_dirs)){
  
  if(i == 1){
    
    # Information of found cilia
    df_cilia <- read.csv(file.path(output_dirs[i],"cilium_summary.csv"))
    df_cilia$to_be_removed <- "yes_or_no"
    df_cilia$fileName <- basename(output_dirs[i])
    df_cilia <- df_cilia[,c(6,1,2,3,4,5)]
    
    # Information of nuclei
    df_nuclei <- read.csv(file.path(output_dirs[i],"nuclei_number.csv"))
    df_nuclei$corrected_number <- NA
    df_nuclei$fileName <- basename(output_dirs[i])
    df_nuclei <- df_nuclei[,c(3,1,2)]
    
    # Information of parameter values
    df_parameters_dummy <- read.csv(file.path(output_dirs[i],"parameter_list.csv"))
    df_parameters <- as.data.frame(t(df_parameters_dummy[,-1]))
    colnames(df_parameters) <- df_parameters_dummy$parameterNames
    df_parameters$fileName <- basename(output_dirs[i])
    df_parameters <- df_parameters[,c(13,c(1:12))]
    
    rm(df_parameters_dummy)
    
  }else{
    
    # Information of found cilia
    df_cilia_dummy <- read.csv(file.path(output_dirs[i],"cilium_summary.csv"))
    df_cilia_dummy$to_be_removed <- "yes_or_no"
    df_cilia_dummy$fileName <- basename(output_dirs[i])
    df_cilia_dummy <- df_cilia_dummy[,c(6,1,2,3,4,5)]
    
    df_cilia <- rbind(df_cilia, df_cilia_dummy)
    
    # Information of nuclei
    df_nuclei_dummy <- read.csv(file.path(output_dirs[i],"nuclei_number.csv"))
    df_nuclei_dummy$corrected_number <- NA
    df_nuclei_dummy$fileName <- basename(output_dirs[i])
    df_nuclei_dummy <- df_nuclei_dummy[,c(3,1,2)]
    
    df_nuclei <- rbind(df_nuclei, df_nuclei_dummy)
    
    # Information of parameter values
    df_parameters_dummy <- read.csv(file.path(output_dirs[i],"parameter_list.csv"))
    df_parameters_dummy2 <- as.data.frame(t(df_parameters_dummy[,-1]))
    colnames(df_parameters_dummy2) <- df_parameters_dummy$parameterNames
    df_parameters_dummy2$fileName <- basename(output_dirs[i])
    df_parameters_dummy2 <- df_parameters_dummy2[,c(13,c(1:12))]
    
    df_parameters <- rbind(df_parameters, df_parameters_dummy2)
    rm(list = c("df_cilia_dummy", "df_nuclei_dummy",
                "df_parameters_dummy", "df_parameters_dummy2"))
    
  }
  
}

# Remove "output" from file name ###########################################

df_cilia$fileName <- gsub(pattern = "_output$", replacement = "", x = df_cilia$fileName)
df_nuclei$fileName <- gsub(pattern = "_output$", replacement = "", x = df_nuclei$fileName)
df_parameters$fileName <- gsub(pattern = "_output$", replacement = "", x = df_parameters$fileName)

# Save resulting csv files #################################################

write.csv(df_cilia,
          file = file.path(input_dir, "summary_cilia.csv", sep=""), row.names = FALSE)
write.csv2(df_cilia,
          file = file.path(input_dir, "summary_cilia_de.csv", sep=""), row.names = FALSE)

write.csv(df_nuclei,
          file = file.path(input_dir, "summary_nuclei.csv", sep=""), row.names = FALSE)
write.csv2(df_nuclei,
           file = file.path(input_dir, "summary_nuclei_de.csv", sep=""), row.names = FALSE)

write.csv(df_parameters,
          file = file.path(input_dir, "summary_parameters.csv", sep=""), row.names = FALSE)
write.csv2(df_parameters,
           file = file.path(input_dir, "summary_parameters_de.csv", sep=""), row.names = FALSE)
