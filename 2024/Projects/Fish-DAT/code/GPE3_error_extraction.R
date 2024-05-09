## Function to extract error polygons from the .kmz file

# source("https://gist.githubusercontent.com/vinayudyawer/f84751026caa9cbc51e11e5b17a257af/raw/81e72c9b3eb16e8659fee55407d2f770cdb7dbd7/extract_polys.R")

library(tidyverse)
library(terra)
library(tidync)
library(sf)

## set how data are input into the function 
## (it only requires the path to the folder where -GPE3 files are found, and probability values)
folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"

## set probability contour 
prob <- c(0.99, 0.95, 0.5)

## parse all the histo files in the folder
csv_file <-
  list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
  map(function(x){list.files(x, pattern = "*GPE3.csv", full.names = T)}) %>% 
  unlist()

nc_files <-
  list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
  map(function(x){list.files(x, pattern = "*GPE3.nc", full.names = T)}) %>% 
  unlist()

kmz_files <-
  list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
  map(function(x){list.files(x, pattern = "*GPE3.kmz", full.names = T)}) %>% 
  unlist()


## Grab ncdf error raster
rast_timestamp <-
  tidync(nc_files[1], what = "twelve_hour_timestamps") %>% 
  hyper_tibble() %>% 
  mutate(twelve_hour_timestamps = as_datetime(twelve_hour_timestamps),
         twelve_hour = as.character(twelve_hour))
  
error_rast <- 
  tidync(nc_files[1], what = "twelve_hour_likelihoods") %>% 
  hyper_tibble() %>% 
  # left_join(rast_timestamp, by = "twelve_hour") %>% 
  dplyr::select(longitude, latitude, twelve_hour_likelihoods, twelve_hour) %>% 
  pivot_wider(id_cols = c(1:2), names_from = "twelve_hour", 
              values_from = "twelve_hour_likelihoods") %>% 
  rast(type = "xyz", crs = "epsg:4326") %>% 
  disagg(fact = 10, method = "bilinear")

scaled_error <- (error_rast - minmax(error_rast)[1,]) / (minmax(error_rast)[2,] - minmax(error_rast)[1,])

fn <-
  function(ras, levels){
    vec <- as.contour(ras, levels = levels)
    out <- st_as_sf(vec) %>% 
      st_cast("POLYGON") %>% 
      mutate(error_level = 1-level,
             twelve_hour = names(ras))
    out[-1]
  }

error_contour <- 
  scaled_error %>% 
  as.list() %>% 
  map(fn, levels = 1-prob) %>% 
  bind_rows() %>% 
  left_join(rast_timestamp, by = "twelve_hour") %>% 
  dplyr::select(-twelve_hour) %>% 
  rename(date_time = twelve_hour_timestamps)

 
mapview(error_contour, zcol = "error_level")@map %>%
  addTimeslider(data = error_contour,
                options = timesliderOptions(
                  position = "topright",
                  timeAttribute = "date_time",
                  range = TRUE,
                  alwaysShowDate = TRUE,
                  sameDate = TRUE))

















