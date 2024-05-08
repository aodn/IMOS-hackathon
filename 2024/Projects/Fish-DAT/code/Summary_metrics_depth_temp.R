#Summaries metrics for each tagged individual
# Author: Jiaying
# Date: 2024-05-07

# this code calculates these metrics:
# distance traveled (daily) 
#   Depth (min,max, mean) 
#   Temperature (min,max, mean)


# Loading libraries -------------------------------------------------------
library(tidyverse)
library(sf)
library(dplyr)


# Overview of the data ----------------------------------------------------

list.files(path = "2024/Projects/Fish-DAT/data/", 
           pattern = ".csv",
           recursive = T,
           full.names = T)


## Depth profile 
# Identifying datasets ----------------------------------------------------
Series_files <- list.files("2024/Projects/Fish-DAT/data/",
           pattern = "Series.csv", recursive = T,
           full.names = T)



# define a function -------------------------------------------------------

summarise_depth_temp <- function(filename){
  DT_profile <- read_csv(filename) %>% 
    #fomating day time
    mutate(Day = parse_date(Day, format = "%d-%b-%Y")) %>%    
    #calculate daily depth and temperature file
    group_by(Day,DeployID) %>%
    summarise(
      MinDepth = min(Depth, na.rm = T),
      MaxDepth = max(Depth, na.rm = T),
      MeanDepth = mean(Depth, na.rm = T),
      MinTemp = min(Temperature, na.rm = T),
      MaxTemp = max(Temperature, na.rm = T),
      MeanTemp = mean(Temperature, na.rm = T))
  #return summary
  return(DT_profile)
}



# Loading depth dataset --------------------------------------------------

for (n in seq_along(Series_files)) {
  file_name <- str_replace(basename(Series_files[n]), 
                           "Series", "summary_depth_temp")
  DT_file <- summarise_depth_temp(Series_files[n])
  write.csv(DT_file, 
            file = file.path("2024/Projects/Fish-DAT/data/", file_name),
            row.names = F)
}

