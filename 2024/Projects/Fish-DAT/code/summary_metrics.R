#Summaries metrics for each tagged individual
# Author: Jiaying
# Date: 2024-05-07


# Loading libraries -------------------------------------------------------
library(tidyverse)
library(sf)


# Identifying datasets ----------------------------------------------------
depth_files <- list.files("2024/Projects/Fish-DAT/data/",
           pattern = "MinMaxDepth", recursive = T,
           full.names = T)
# Identifying datasets ----------------------------------------------------
position_files <- list.files("2024/Projects/Fish-DAT/data/",
                          pattern = "daily-positions", recursive = T,
                          full.names = T)

# Loading depth datasets --------------------------------------------------
da <- read_csv(depth_files[1])

position_data <- read_csv(position_files[1])


# Cleaning datasets -------------------------------------------------------
da <- da %>% 
  #Change Date column to datetime format
  mutate(Date = parse_date_time(Date, "%H:%M:%S %d-%b-%Y"))

glimpse(da)
glimpse(position_data)

# Convert to an sf object
data_sf <- st_as_sf(position_data, coords = c("Longitude", "Latitude"), crs = 4326)

#shark
# calculate these metrics:
# distance traveled (cumulative; daily) #sf package "st_distance"
#   Depth (min,max, mean) # as_
#   Temperature (min,max, mean)
# Distance to (shelf, shore)
# Bathymetry (max depth at each position)

# Calculate daily distance traveled
daily_distances <- data_sf %>%
  group_by(Date) %>%
  arrange(Date) %>% # Ensure data is ordered by Date
  summarise(distance = sum(st_distance(geometry, lag(geometry)), na.rm = TRUE)) # Calculate cumulative daily distance

# View the results
print(daily_distances, n=357) 



# calculate these metrics:
# distance traveled (cumulative; daily)
#   Depth (min,max, mean)
#   Temperature (min,max, mean)
# Distance to (shelf, shore)
# Bathymetry (max depth at each position)