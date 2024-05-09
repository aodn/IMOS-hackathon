# Pre-processing of satellite tracks
base_folder <- "2024/Projects/Fish-DAT"

#Load scripts
source(file.path(base_folder, "code/fishdat_functions.R"))

#Getting list of daily files
gp3_files <- list.files(base_folder, 
                          pattern = "-GPE3.csv",
                          recursive = T, full.names = T)

#Apply daily positions function
for(file in gp3_files){
  daily_positions(file)
  }