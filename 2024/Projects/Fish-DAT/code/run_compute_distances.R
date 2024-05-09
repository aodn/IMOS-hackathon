base_folder <- "2024/Projects/Fish-DAT"

#Load scripts
source(file.path(base_folder, "code/fishdat_functions.R"))

#Getting list of daily files
daily_files <- list.files(base_folder, 
                          pattern = "daily-positions.csv",
                          recursive = T, full.names = T)

#Output folder for bathymetry data
save_bathy_path <- file.path(base_folder, "data")

#Apply computing function
for(file in daily_files){
  compute_distances(file, save_bathy_path)
}

