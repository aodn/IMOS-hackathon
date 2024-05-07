#Summaries metrics for each tagged individual
# Author: Jiaying
# Date: 2024-05-07


# Loading libraries -------------------------------------------------------
library(tidyverse)


# Identifying datasets ----------------------------------------------------
depth_files <- list.files("2024/Projects/Fish-DAT/data/",
           pattern = "MinMaxDepth", recursive = T,
           full.names = T)

# Loading depth datasets --------------------------------------------------
da <- read_csv(depth_files[1])


# Cleaning datasets -------------------------------------------------------
da <- da %>% 
  #Change Date column to datetime format
  mutate(Date = parse_date_time(Date, "%H:%M:%S %d-%b-%Y"))


glimpse(da)


#shark
# calculate these metrics:
# distance traveled (cumulative; daily) #sf package "st_distance"
#   Depth (min,max, mean) # as_
#   Temperature (min,max, mean)
# Distance to (shelf, shore)
# Bathymetry (max depth at each position)



# calculate these metrics:
# distance traveled (cumulative; daily)
#   Depth (min,max, mean)
#   Temperature (min,max, mean)
# Distance to (shelf, shore)
# Bathymetry (max depth at each position)