# Function for copying all z stack projection images                ++++++++
# Author: Kai Budde-Sagert
# Created: 2024/02/20
# Last changed: 2024/02/20


copyAllProjectionImages <- function(input_dir){
  
  # Create output directory
  output_dir <- paste0(input_dir,"_zprojection")
  dir.create(output_dir, showWarnings = FALSE)
  
  # Filter results directories #############################################
  output_dirs <- list.dirs(input_dir, recursive = TRUE, full.names = TRUE)
  # Check for real detectCilia output
  output_dirs <- output_dirs[grepl(pattern = "output$", x = output_dirs, ignore.case = TRUE)]
  
  # Go through every output folder and copy z-stack projection image #######
  
  for(i in 1:length(output_dirs)){
    current_file <- list.files(path = output_dirs[i],
                               pattern = "z_projection.tif",
                               full.names = TRUE)
    file.copy(from = current_file, to = output_dir)
  }
  rm(i)
  rm(current_file)

}
