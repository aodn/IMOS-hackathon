#Customised functions for Fish-DAT
# Authors: Dahlia Foo, Fabrice Jaine

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(traipse)
library(lutz)
library(marmap)

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


# Calculating distances ---------------------------------------------------
# Calculates: distance from shelf edge, distance from shore, and distance 
# from nearest estuary from every location included in the track
compute_distances <- function(path_daily, save_bathy_path){
  #Inputs:
  # - path_daily (character): Path to directory where data is stored 
  # - save_bathy_path (character): Path to directory where bathymetry is to be stored 
  
  #Load data
  dat <- read_csv(path_daily) %>% 
    mutate(Date = as_date(Date))
  
  #define region of interest (ROI) 
  # Total ROI for bathymetry
  LatMax = ceiling(max(dat$Latitude)) + 2
  LatMin = floor(min(dat$Latitude)) - 2
  LonMin = floor(min(dat$Longitude)) - 2
  LonMax = ceiling(max(dat$Longitude)) + 2
  ROI = c(LonMin, LatMin, LonMax, LatMax)
  
  #load bathymetry data
  bat <- getNOAA.bathy(LonMin, LonMax, LatMin, LatMax, 
                       res = 1, keep = T, 
                       path = save_bathy_path)
  
  #Extract bathymetry and cumulative distance travelled at each position
  #along the track
  dat.bathy <- get.depth(bat, x = dat$Longitude, y = dat$Latitude,
                         distance = T, locator = F, res = 1) 
  colnames(dat.bathy) <- c("Longitude", "Latitude", "DistfromStart", 
                           "bathy_depth")
  dat <- left_join(dat, dat.bathy, by = c("Latitude", "Longitude"))
  
  # Distance to nearest shelf edge (200m isobath):
  # distances between each sighting and nearest point on shelf edge
  d_shelf <- dist2isobath(bat, dat$Longitude, dat$Latitude,
                          isobath = -200)
  # in km instead of metres
  d_shelf$distance <- d_shelf$distance/1000 
  dat$DistToShelf <- d_shelf$distance
  
  # Distance to nearest coastline (0m isobath):
  # distances between each sighting and nearest point on coastline
  d_shore <- dist2isobath(bat, dat$Longitude, dat$Latitude,
                          isobath = 0)
  # in km instead of metres
  d_shore$distance <- d_shore$distance/1000 
  dat$DistToShore <- d_shore$distance
  
  #Save data
  write_csv(dat, path_file)
}


