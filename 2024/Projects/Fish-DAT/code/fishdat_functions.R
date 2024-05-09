#Customised functions for Fish-DAT
# Authors: Dahlia Foo, Fabrice Jaine

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(traipse)
library(marmap)
library(data.table)
library(janitor)

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


# Summarise GPE3 output data - local time, daily location -----------------
daily_positions <- function(file_path){
  #Load data
  dat <- fread(file_path) %>% 
    as.data.frame() %>% 
    clean_names()
  
  #Getting tag ID
  ptt_id <- unique(dat$ptt)
  
  # lookup local timezone
  timeZ <- tz_lookup_coords(lat = dat$most_likely_latitude, 
                               lon = dat$most_likely_longitude,
                               method = "accurate")
  #Timezone of deployment
  timeZ <- timeZ[1]
  
  # Do time conversion and extract Date and Time as separate columns
  dat$datetime.UTC <- as.POSIXct(as.character(dat$Date), 
                                 format = "%d-%b-%Y %H:%M:%S", tz = "UTC",
                                 origin = "1970-01-01")
  dat$datetime.LOCAL <- as.POSIXlt(dat$datetime.UTC, tz = timeZ)
  dat$time <- format(as.POSIXct(dat$datetime.LOCAL, 
                                format = "%Y:%m:%d %H:%M:%S"), 
                     "%H:%M:%S")
  dat$date <- format(as.POSIXct(dat$datetime.LOCAL, 
                                format = "%Y:%m:%d %H:%M:%S"),
                     "%Y-%m-%d")
  
  # Calculate average daily position based on 4 raw/estimated locations per day
  track <- dat %>%
    group_by(date) %>%
    summarise(ptt = unique(ptt), Latitude = mean(most_likely_latitude),
              Longitude = mean(most_likely_longitude))
  # Deleting the first and last location of track data to be replaced with more accurate locations 
  
  #Replacing first and last location with know deployment and pop-off location
  #load the masterfile with the known release (deployment) and detachments (pop-off) locations
  rawdata <- fread("HackathonMetadata.csv")
  #Replacing deployment longitude and latitude
  x = rawdata[which(rawdata$PTT_ID == ptt_id),]
  track$Latitude[1] <- x$Deployment_Lat
  track$Longitude[1] <- x$Deployment_Lon
  #Replacing detachment longitude and latitude (need to check what row of each data you are transferring over)
  track$Latitude[nrow(track)] <- x$Detachment_Lat
  #not sure why this line of code isn't working
  track$Longitude[nrow(track)] <- x$Detachment_Lon[1] 
  
  track <- track %>% 
    mutate(day.at.liberty = 1:n())
  track$month <- format(as.POSIXct(track$date, format="%Y-%m-%d"), "%b")
  
  #Save sample data
  write_csv(track, file_path)
}



# Histograms --------------------------------------------------------------
## set how data are input into the function 
## (it only requires the path to the folder where -Histo files are found)

histoviz <- function(folder_path){
  
  ## parse all the histo files in the folder
  file_names <-
    list.files(folder_path, pattern = '^[0-9]+$', 
               full.names = T) %>% 
    map(function(x){list.files(x, pattern = "*Histos.csv", 
                               full.names = T)}) %>% 
    unlist()
  
  ### Lets work with the first histo file.. this can be looped across all 
  #the files in histos
  fn <- function(file_names){
    
    ## Read in the file
    histos <- read_csv(file_names[1], show_col_types = F)
    
    ## Define the Time at Depth (tad) and Time at Temperature (tat) bins
    tad_bins <-
      histos %>% 
      filter(HistType %in% "TADLIMITS") %>% 
      dplyr::select(contains("Bin")) %>% 
      pivot_longer(-1, names_to = "Bins", values_to = "bin") %>% 
      dplyr::select(-NumBins) %>% 
      filter(!is.na(bin)) %>% 
      mutate(variable = "depth")
    
    tat_bins <-
      histos %>% 
      filter(HistType %in% "TATLIMITS") %>% 
      dplyr::select(contains("Bin")) %>% 
      pivot_longer(-1, names_to = "Bins", values_to = "bin") %>% 
      dplyr::select(-NumBins) %>% 
      filter(!is.na(bin)) %>% 
      mutate(variable = "temp")
    
    
    ## subset and configure depth and temp data into a data.frame
    depth_dat <-
      histos %>% 
      filter(HistType %in% c("TAD")) %>% 
      mutate(date = dmy_hms(paste(str_split(Date, pattern = " ", 
                                            simplify = T)[,2],
                                  str_split(Date, pattern = " ", 
                                            simplify = T)[,1])),
             id = as.character(Ptt)) %>% 
      dplyr::select(id, date, contains("Bin"), -NumBins) %>%
      pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
      left_join(tad_bins, by = "Bins") %>% 
      dplyr::select(-Bins)
    
    
    temp_dat <-
      histos %>% 
      filter(HistType %in% c("TAT")) %>% 
      mutate(date = dmy_hms(paste(str_split(Date, pattern = " ", 
                                            simplify = T)[,2],
                                  str_split(Date, pattern = " ", 
                                            simplify = T)[,1])),
             id = as.character(Ptt)) %>% 
      dplyr::select(id, date, contains("Bin"), -NumBins) %>%
      pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
      left_join(tat_bins, by = "Bins") %>% 
      dplyr::select(-Bins)
    
    combdat <- bind_rows(depth_dat, temp_dat)
    
    return(combdat)
  }
  
  outdat <- 
    map(.x = file_names, .f = fn, .progress = TRUE) %>% 
    list_rbind() %>% 
    mutate(bin = as.factor(bin)) %>% 
    filter(!is.na(variable))
  
  return(outdat)
}


