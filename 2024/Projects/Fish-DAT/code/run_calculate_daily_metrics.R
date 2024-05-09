base_folder <- "2024/Projects/Fish-DAT/"

#Load scripts
source(file.path(base_folder, "code/fishdat_functions.R"))

#Calculating daily movement
folder_names <- c("47618", "47622", "227150", "227151")

for (folder in folder_names) {
  basedir <- file.path(base_dir, "data", folder)
  output <- calculate_daily_metrics(basedir)
  # save to 2024/Projects/Fish-DAT/outputs/[foldername]_summaries.csv
  write_csv(output, file = paste0("2024/Projects/Fish-DAT/outputs/", 
                                  folder, "_summaries.csv"))
  
  glimpse(output)
}

# Get data for one animal
basedir <- "2024/Projects/Fish-DAT/data/47622"

# Load the data
filename <- dir(file.path(base_dir, "data/47622"), 
                pattern = "daily-positions.csv", 
                full.names = TRUE)

d <- read_csv(filename); d; glimpse(d);

# Calculate distance travelled between each pair of consecutive points
d <- d %>% 
  mutate(DistTravelled = track_distance_to(Longitude, Latitude, lag(Longitude), 
                                           lag(Latitude)) / 1000)
d
