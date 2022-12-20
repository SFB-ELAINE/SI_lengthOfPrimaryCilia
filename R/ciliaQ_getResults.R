# Function for reading CiliaQ results                             +++++++++
# Adapted from https://github.com/hansenjn/CiliaQ/tree/master/R%20Scripts
# Author: Kai Budde
# Created: 2022/06/20
# Last changed: 2022/12/20


ciliaQ_getResults <- function(input_dir, output_dir){
  
  # Load packages ##########################################################
  
  # Set groundhog day for reproducibility (see https://groundhogr.com)
  groundhog.day <- "2022-03-01"
  
  if(!any(grepl(pattern = "groundhog", x = installed.packages(), ignore.case = TRUE))){
    install.packages("groundhog")
  }
  
  # Load packages
  library(groundhog)
  pkgs <- c("tidyverse")
  groundhog.library(pkgs, groundhog.day)
  
  # Common name of file names
  # (Indicate as expID how all file names begin (in case they start with the
  # same name). If they do not have a common beginning, enter nothing here.)
  # expID <- "190815_EV38_2_Collagen_ITSwithAsc+Dexa_63x_zstack_"
  # expID <- ""
  
  # Image-specific suffixes
  # (As conditions enter the filename parts that are individual to each file.
  # For example, see below a combination of genotype (KO vs WT) and time
  # (t000, t015, t030)). Note, that you should not enter here any filetype
  # suffixes (e.g. .tif, .lif, ...) or suffixes that are generated during the
  # CiliaQ pipeline (e.g., the suffix "_s1_CQP_CQS.txt").)
  
  # conditions <- sort(c("6", "7", "8", "9", "10", "11", "12"))
  
  
  # Import and clean data ####################################################
  
  # # Create list of CiliaQ file names
  # ciliaq_files <- paste0(expID, conditions,"_CQP_CQ.txt")
  # 
  # ciliaq_files <- sort(ciliaq_files)
  # 
  # ciliaq_files <- ifelse(file.exists(paste0(input_dir, ciliaq_files)), ciliaq_files,
  #                        sort(paste0(expID," ", conditions, "_CQP_ed_CQ.txt")))
  
  # Create list of CiliaQ file names
  ciliaq_files <- list.files(path = input_dir, pattern = "CQs\\.txt")
  ciliaq_files <- sort(ciliaq_files)
  
  
  # if(!all(file.exists(paste0(input_dir, ciliaq_files)))){
  #   print("Not all given files exist.")
  # }
  
  
  # Define column names in files
  # (Define a vector that indicates in which columns in the CiliaQ output
  # file which preferences are stored.)
  
  column_names <- c("Name",
                    "Note",
                    "ID",
                    "x center [micron]",
                    "y center [micron]",
                    "z center [micron]",
                    "Volume [voxel]",
                    "Volume [micron^3]",
                    "N surface voxels",
                    "Surface [micron^2]",
                    "Shape complexity index",
                    "Sphere radius [micron]",
                    "Maximum span [micron]",
                    "A Coloc volume [micron^3]",
                    "A Coloc volume [% tot vol]",
                    "B Coloc volume [micron^3]",
                    "B Coloc volume [% tot vol]",
                    "A Coloc comp BG [micron^3]",
                    "A Coloc comp BG [% tot vol]",
                    "B Coloc comp BG [micron^3]",
                    "B Coloc comp BG [% tot vol]",
                    "minimum intensity (rec.C)",
                    "maximum intensity (rec.C)",
                    "average intensity 10% (rec.C)",
                    "average intensity (rec.C)",
                    "SD of intensity (rec.C)",
                    "minimum A intensity",
                    "maximum A intensity",
                    "average A intensity of hi 10%",
                    "average A intensity",
                    "SD of A intensity",
                    "minimum B intensity",
                    "maximum B intensity",
                    "average B intensity of hi 10%",
                    "average B intensity",
                    "SD of B intensity",
                    "N of found skeletons",
                    "N branches",
                    "tree length [micron]",
                    "cilia length [micron]",
                    "orientation vector x [micron]",
                    "orientation vector y [micron]",
                    "orientation vector z [micron]",
                    "cilia bending index",
                    "Intensity threshold A",
                    "Intensity threshold B",
                    "Intensity threshold Basal Stain",
                    "Integrated A intensity",
                    "Average A intensity on center",
                    "Integrated B intensity",
                    "Average B intensity on center",
                    "A Coloc center vs BG [micron]",
                    "A Coloc center vs BG [% tot l]",
                    "B Coloc center vs BG [micron]",
                    "B Coloc center vs BG [% tot l]")
  
  ## Load and merge data
  
  for(i in 1:length(ciliaq_files)){
    # number_of_lines <- length(readr::read_lines(file = paste0(input_dir, ciliaq_files[i])))
    # lines_to_be_skipped_beginning <- 29
    # lines_to_be_skipped_end <- 4
    
    # Read data (encoded in ANSI)
    # df_dummy <- readr::read_delim(file = paste0(input_dir, ciliaq_files[i]),
    #                               delim = "\t",
    #                               escape_double = FALSE, col_names = FALSE,
    #                               trim_ws = TRUE,
    #                               skip = lines_to_be_skipped_beginning,
    #                               n_max = number_of_lines-lines_to_be_skipped_beginning-lines_to_be_skipped_end,
    #                               locale = locale('se', encoding = 'ISO8859-1'))[,1:length(column_names)]
    
    df_dummy <- readr::read_delim(file = file.path(input_dir, ciliaq_files[i]),
                                  col_types = paste0("c", paste0(rep("c", 56), collapse = "")),
                                  delim = "\t",
                                  escape_double = FALSE, col_names = FALSE,
                                  trim_ws = TRUE,
                                  locale = locale('se', encoding = 'ISO8859-1'))[,1:length(column_names)]
    
    colnames(df_dummy) <- column_names
    
    df_dummy$file_name <- gsub(pattern = "\\.txt", replacement = "", x = ciliaq_files[i])
    if(grepl(pattern = "zstack", x = ciliaq_files[i], ignore.case = TRUE)){
      df_dummy$image_name_short <- as.numeric(gsub(pattern = ".+zstack_([0-9]{1,2}).+", replacement = "\\1", x = ciliaq_files[i]))
    }
    
    if(i == 1){
      df_data <- df_dummy
    }else{
      df_data <- dplyr::bind_rows(df_data, df_dummy)
    }
    
  }
  
  rm(list = c("i", "df_dummy"))
  
  df_data <- df_data %>% dplyr::relocate("file_name", "image_name_short", .before = "Name")
  
  # Save data as csv files
  dir.create(output_dir, showWarnings = FALSE)
  write.csv(x = df_data,
            file = file.path(output_dir, "ciliaq_data.csv"),
            row.names = FALSE)
  # write.csv2(x = df_data,
  #            file = file.path(output_dir, "ciliaq_data_de.csv"),
  #            row.names = FALSE)
  
  
  
}
