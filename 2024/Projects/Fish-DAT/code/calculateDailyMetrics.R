# Calculate temperature min, mean, max for each location 
# Author: Dahlia Foo

# Setup
library(tidyverse)
library(lubridate)

# Set to project root directory
setwd("~/Documents/Projects/dev/IMOS-hackathon-1")

# Get for one animal
basedir <- "2024/Projects/Fish-DAT/data/47622"





calculate_daily_metrics <- function(basedir) {
  # Load the data
  filename <- dir(file.path(basedir), pattern = "daily-positions.csv", full.names = TRUE)
  d <- read_csv(filename); d; glimpse(d)


# Init output data
out <- d

# Load hi-res series data


if (file.exists(file.path(basedir, "Series.csv"))) {
  filename <- dir(file.path(basedir), pattern = "Series.csv", full.names = TRUE)
  series_dat <- read_csv(filename) %>% 
    mutate(Day = dmy(Day)) %>% 
    group_by(Ptt, Day) %>% 
    summarise(
      MinTempFromSeries = min(Temperature, na.rm = TRUE),
      MeanTempFromSeries = mean(Temperature, na.rm = TRUE),
      MaxTempFromSeries = max(Temperature, na.rm = TRUE),
      MinDepthFromSeries = min(Depth, na.rm = TRUE),
      MaxDepthFromSeries = max(Depth, na.rm = TRUE),
      MeanDepthFromSeries = mean(Depth, na.rm = TRUE)
    ) %>% 
    select(Ptt, Day, MinTempFromSeries, MeanTempFromSeries, MaxTempFromSeries, MinDepthFromSeries, MaxDepthFromSeries, MeanDepthFromSeries)
  
  # env_dat; glimpse(env_dat)
  
  out <- left_join(out, series_dat, by = c("Date" = "Day", "Ptt" = "Ptt"))
  
}  else {
  # message
  message("No Series.csv found in folder")
}
  

# IF animal doesn't have Series.csv use the summary data
# use DailyData.csv

if(
  file.exists(file.path(basedir, "DailyData.csv"))
) {
  filename <- dir(file.path(basedir), pattern = "DailyData.csv", full.names = TRUE)
  daily_dat <- read_csv(filename) %>% 
    mutate(Day = mdy(Date)) %>% 
    select(Ptt, Day, MinTemp, MaxTemp, MinDepth, MaxDepth); 
  
  # daily_dat
  # glimpse(daily_dat)
  
  out <- left_join(out, daily_dat, by = c("Date" = "Day", "Ptt" = "Ptt"))
} else {
  # message
  message("No DailyData.csv found in folder")
}


if (
  file.exists(file.path(basedir, "PDTs.csv"))
) {
  # use PDTs.csv
  filename <- dir(file.path(basedir), pattern = "PDTs.csv", full.names = TRUE)
  pdt_dat <- read_csv(filename) %>% 
    # split Date by " " and create string of second element + first element
    mutate(Date = str_split(Date, pattern = " ")) %>% 
    mutate(Date = map_chr(Date, ~paste(.x[2], .x[1], sep = " "))) %>% 
    mutate(Date = dmy_hms(Date)) %>% 
    mutate(Day = as.Date(Date)) %>% 
    mutate(MeanTempPDT = rowMeans(select(., contains("Temp")), na.rm = TRUE)) %>% 
    mutate(MeanDepthPDT = rowMeans(select(., contains("Depth")), na.rm = TRUE)) %>% 
    select(Ptt, Day, MeanTempPDT, MeanDepthPDT); pdt_dat; 
  
  # pdt_dat
  # glimpse(pdt_dat)
  
  out <- left_join(out, pdt_dat, by = c("Date" = "Day", "Ptt" = "Ptt"))
} else {
  # message
  message("No PDTs.csv found in folder")
}


return(out)
}