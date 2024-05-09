#Customised functions for Fish-DAT
# Authors: Dahlia Foo

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(traipse)

# Calculate daily metrics -------------------------------------------------
# Calculate temperature min, mean, max for each location 
calculate_daily_metrics <- function(basedir) {
  #Inputs:
  # - basedir (character): Path to directory where data is stored 
  
  # Load the data
  filename <- dir(basedir, pattern = "daily-positions.csv", 
                  full.names = TRUE)
  d <- read_csv(filename)
  
  # Init output data
  out <- d
  
  # Load hi-res series data
  filename <- dir(basedir, pattern = "Series.csv", 
                  full.names = TRUE)
  
  
  if (is_empty(filename) == FALSE) {
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
      select(Ptt, Day, MinTempFromSeries, MeanTempFromSeries, 
             MaxTempFromSeries, MinDepthFromSeries, MaxDepthFromSeries,
             MeanDepthFromSeries)
    
    out <- left_join(out, series_dat, 
                     by = c("Date" = "Day", "Ptt" = "Ptt"))
    
  }  else {
    # message
    message(paste("No 'Series.csv' file found in folder: ", base_dir))
  }
    
  # IF animal doesn't have Series.csv use the summary data
  # use DailyData.csv
  filename <- dir(file.path(basedir), pattern = "DailyData.csv", 
                  full.names = TRUE)
  
  if(
    is_empty(filename) == FALSE
  ) {
  
    daily_dat <- read_csv(filename) %>% 
      mutate(Day = mdy(Date)) %>% 
      select(Ptt, Day, MinTemp, MaxTemp, MinDepth, MaxDepth)
    
    out <- left_join(out, daily_dat, by = c("Date" = "Day", "Ptt" = "Ptt"))
  } else {
    # message
    message(paste("No 'DailyData.csv' file found in folder: ", base_dir))
  }
  
  
  filename <- dir(file.path(basedir), pattern = "PDTs.csv", full.names = TRUE)
  if (
    is_empty(filename) == FALSE
  ) {
    # use PDTs.csv
    
    # Do temp and depth separately
    test <- read_csv(filename) %>% 
      select(Ptt, Date, contains("Temp"))
    
    glimpse(test)
    min_cols <- grep("MinTemp", names(test), value = TRUE)
    max_cols <- grep("MaxTemp", names(test), value = TRUE)
      
    means <- purrr::map2(min_cols, max_cols, function(x, y) {
      (test[[x]] + test[[y]]) / 2
    }) 
    
    # Convert means to a data frame and name columns
    means_df <- as.data.frame(means)
    names(means_df) <- paste0("Mean", seq_along(means))
    
    # Bind the new means data frame to the original data frame
    test <- bind_cols(test, means_df) %>% 
      mutate(DailyMeanTemp = rowMeans(select(., starts_with("Mean")), 
                                      na.rm = TRUE))
    
  
    pdt_dat <- read_csv(filename) %>% 
      # split Date by " " and create string of second element + first element
      mutate(Date = str_split(Date, pattern = " ")) %>% 
      mutate(Date = map_chr(Date, ~paste(.x[2], .x[1], sep = " "))) %>% 
      mutate(Date = dmy_hms(Date)) %>% 
      mutate(Day = as.Date(Date)) %>% 
      mutate(MeanTempPDT = rowMeans(select(., contains("Temp")),
                                    na.rm = TRUE)) %>% 
      mutate(MeanDepthPDT = rowMeans(select(., contains("Depth")), 
                                     na.rm = TRUE)) %>% 
      select(Ptt, Day, MeanTempPDT, MeanDepthPDT)
    
    out <- left_join(out, pdt_dat, by = c("Date" = "Day", "Ptt" = "Ptt"))
  } else {
    # message
    message("No PDTs.csv found in folder")
  }
  
  # Calculate daily distance travelled
  out <- out %>% 
    mutate(DistTravelled = track_distance_to(Longitude, Latitude, 
                                             lag(Longitude), 
                                             lag(Latitude)) / 1000)
  return(out)
  }
  
  
