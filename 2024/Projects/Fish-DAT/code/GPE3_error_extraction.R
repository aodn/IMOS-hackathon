## Function to extract error polygons from the .kmz file

library(tidyverse)
library(terra)
library(tidync)
library(sf)

## Error extraction function
## (it only requires the path to the folder where -GPE3 files are found, and probability values)

gpe3_error_polys <-
  function(folder_path, prob, verbose = F, aggregate = T){
    
    ## parse all the histo files in the folder
    # csv_file <-
    #   list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
    #   map(function(x){list.files(x, pattern = "*GPE3.csv", full.names = T)}) %>% 
    #   unlist()
    
    nc_files <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.nc", full.names = T)}) %>% 
      unlist()
    
    # kmz_files <-
    #   list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
    #   map(function(x){list.files(x, pattern = "*GPE3.kmz", full.names = T)}) %>% 
    #   unlist()
      
    ## internal function to work through each nc_file
    nc_pull <-
      function(nc_file, .prob = prob, .verbose = verbose){
        
        id <- basename(dirname(nc_file))
        
        if(.verbose){
          message("Extracting error polygons for ", id, " ")
        }
        
        ## Grab ncdf error raster
        rast_timestamp <-
          tidync(nc_file, what = "twelve_hour_timestamps") %>% 
          hyper_tibble() %>% 
          mutate(twelve_hour_timestamps = as_datetime(twelve_hour_timestamps),
                 twelve_hour = as.character(twelve_hour))
        
        error_rast <- 
          tidync(nc_file, what = "twelve_hour_likelihoods") %>% 
          hyper_tibble() %>% 
          # left_join(rast_timestamp, by = "twelve_hour") %>% 
          dplyr::select(longitude, latitude, twelve_hour_likelihoods, twelve_hour) %>% 
          pivot_wider(id_cols = c(1:2), names_from = "twelve_hour", 
                      values_from = "twelve_hour_likelihoods") %>% 
          rast(type = "xyz", crs = "epsg:4326") %>% 
          disagg(fact = 10, method = "bilinear")
        
        scaled_error <- (error_rast - minmax(error_rast)[1,]) / (minmax(error_rast)[2,] - minmax(error_rast)[1,])
        
        ## internal function to convert rasters to contour polygons (based on input prob)
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
          map(fn, levels = (1-.prob), .progress = verbose) %>% 
          bind_rows() %>% 
          left_join(rast_timestamp, by = "twelve_hour") %>% 
          dplyr::select(-twelve_hour) %>% 
          rename(date_time = twelve_hour_timestamps) %>% 
          mutate(Ptt = factor(id))
        
        error_contour
        
      }
    
    if(verbose){
      message("Calculating error likelihoods from .nc files...") 
    }
    
    error_polys <- map(.x = nc_files, .f = nc_pull, .progress = verbose)
    
    out_polys <- error_polys %>% bind_rows()
    
    if(aggregate){
      if(verbose){
        message("Aggregating individual 12 hour polygons into a single one per Ptt...") 
      }
      try(
        {sf::sf_use_s2(FALSE)
          out_polys <-
            out_polys %>%
            st_make_valid() %>%
            group_by(Ptt) %>%
            summarise(do_union = T) %>%
            st_as_sf()
          sf::sf_use_s2(TRUE)},
        silent = TRUE)
    }
    
    return(out_polys)
    
  }




## ------------------------------------------------------------------------------------------ ##
## Testing


# folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"
# prob <- c(0.99, 0.95, 0.5)
# 
# pols <- gpe3_error_polys(folder_path, prob, verbose = T)
# 
# view_ext <- ext(pols) %>% as.vector()
# 
# leaflet() %>%
#   # Base groups (you can add multiple basemaps):
#   addProviderTiles(providers$OpenStreetMap, group="Map") %>%   # Street Map view
#   # addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%   # typical Google Earth satellite view
#   # add error polys
#   addTimeslider(data = pols %>% filter(error_level %in% "0.99"), 
#                 stroke = FALSE,
#                 options = timesliderOptions(
#                   position = "bottomright",
#                   timeAttribute = "date_time",
#                   range = FALSE,
#                   alwaysShowDate = TRUE,
#                   sameDate = TRUE,
#                   follow = TRUE))

sf::sf_use_s2(FALSE)
pol_sum <-
  pols %>% 
  st_make_valid() %>%
  group_by(Ptt) %>%
  summarise(do_union = T) %>% 
  st_as_sf()
sf::sf_use_s2(TRUE)
















