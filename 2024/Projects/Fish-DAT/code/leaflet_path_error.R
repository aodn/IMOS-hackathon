## Function to plot leaflet map with GPE3 positions and error

## Load the other GPE3_error
source("https://raw.githubusercontent.com/aodn/IMOS-hackathon/fishdat/2024/Projects/Fish-DAT/code/GPE3_error_extraction.R")

library(tidyverse)
library(sf)
library(terra)
library(lubridate)
library(mapview)
library(leaflet)
library(leaflet.extras2)
library(terra)
library(tidync)


folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"
metadata <- list.files(folder_path, pattern = "*Metadata*.csv", full.names = T) %>% read_csv()
prob <- c(0.99)#, 0.95, 0.5)
colpal <- list.files(folder_path, pattern = "*colour_palette*.csv", full.names = T) %>% read_csv()

## Fishery data
fishing <- list.files(folder_path, pattern = "*GFW*.csv", full.names = T) %>% read_csv()
  

# plot_path <-
#   function(folder_name, metadata, prob = 0.99, colpal){
    
    ## parse all the csv and ncdf files in the folder
    csv_file <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.csv", full.names = T)}) %>% 
      unlist()
    
    nc_files <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.nc", full.names = T)}) %>% 
      unlist()
    
    id <- basename(dirname(csv_file))
    
    # if(.verbose){
    #   message("Grabbing GPE3 output to map...") 
    # }
    
    ## Lets convert the GPE3 csv_file data into a spatial object with positions per month
    pos_dat <-
      read_csv(csv_file, skip = 5) %>% 
      mutate(date_time = dmy_hms(Date),
             date = lubridate::date(date_time),
             month = lubridate::month(date, label = T, abbr = T)) %>% 
      transmute(Ptt = factor(Ptt), 
                lon = `Most Likely Longitude`,
                lat = `Most Likely Latitude`,
                date,
                month) %>% 
      left_join(colpal, by = "month")
      
    pos_sf <-
      pos_dat %>%
      group_by(Ptt) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
      st_shift_longitude()
    
    pos_path <-
      pos_sf %>%
      summarise(do_union = F) %>%
      st_cast("LINESTRING")
    
    ## Extract error polygons
    pols <- 
      gpe3_error_polys(folder_path = folder_path, 
                       prob = 0.99, 
                       verbose = F) %>% 
      mutate(Ptt = factor(Ptt))
      
    pol_sum <-
      pols %>% 
      st_shift_longitude()
      
    
    ## Leaflet plot
    mytext = paste(
      "Date: ", pos_dat$date, "<br/>",
      "Ptt: ", pos_dat$Ptt, "<br/>",
      # "Days at liberty: ", pos_dat$`day.at.liberty`, "<br/>",
      "Longitude: ", pos_dat$lon, "<br/>",
      "Latitude: ", pos_dat$lat, sep="") %>%
      lapply(htmltools::HTML)
    
    # m1 <- 
      mapview(pos_sf, zcol = "month", burst = T, alpha = 0, alpha.regions = 1,
              cex = 2, legend = F, homebutton = F) +
      mapview(pos_path, burst = T, cex = 2, legend = F, homebutton = F)
        
    
    m1@map %>% 
      addTimeslider(data = pol_sum %>% filter(error_level %in% "0.99"),
                    stroke = FALSE,
                    options = timesliderOptions(
                      position = "bottomright",
                      timeAttribute = "date_time",
                      range = FALSE,
                      alwaysShowDate = TRUE,
                      sameDate = TRUE,
                      follow = TRUE))
    
    # myleafletplot <-
    leaflet() %>%
      # Base groups (you can add multiple basemaps):
      addProviderTiles(providers$OpenStreetMap, group="Map") %>%   # Street Map view
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%   # typical Google Earth satellite view
      # Add location data:
      # addPolylines(lng = pos_sf$lon, lat = pos_sf$lat, 
      #              group = pos_sf$Ptt,
      #              weight = 1.5,
      #              labelOptions = labelOptions(noHide = TRUE)) %>%
      # # add the tag detection data
      # addCircleMarkers(lng = pos_sf$lon, lat = pos_sf$lat, group = pos_sf$Ptt,
      #                  weight = 2, radius = 4, color = pos_sf$colour,
      #                  stroke = FALSE, fillOpacity = 1, 
      #                  # group = pos_sf$month,
      #                  label=mytext) %>%  # don’t forget to assign a group to the markers
      addTimeslider(data = pol_sum %>% filter(error_level %in% "0.99"),
                    stroke = FALSE,
                    options = timesliderOptions(
                      position = "bottomright",
                      timeAttribute = "date_time",
                      range = FALSE,
                      alwaysShowDate = TRUE,
                      sameDate = TRUE,
                      follow = TRUE)) %>%
      addTimeslider(data = pos_sf,
                    radius = 5,
                    opacity = 0.9,
                    color = pos_sf$colour,
                    stroke = FALSE,
                    options = timesliderOptions(
                      position = "bottomright",
                      timeAttribute = "date",
                      range = FALSE,
                      alwaysShowDate = TRUE,
                      sameDate = TRUE,
                      follow = TRUE)) %>%
      # addTimeslider(data = pos_path,
      #               stroke = FALSE,
      #               options = timesliderOptions(
      #                 position = "bottomright",
      #                 timeAttribute = "date_time",
      #                 range = FALSE,
      #                 alwaysShowDate = TRUE,
      #                 sameDate = TRUE,
      #                 follow = TRUE)) %>% 
      # Layers control
      addLayersControl(
        overlayGroups = c("Satellite"),  # specify the desired basemap options for the output map
        baseGroups = as.character(unique(pos_sf$Ptt)),  # add the data groups to overlay on the map
        options = layersControlOptions(collapsed = FALSE, position = "topright")) %>%
      # Add legend
      addLegend(position = "topleft",
                colors = colpal$colour, labels = colpal$month,
                title = "Month:",
                opacity = 1
      ) %>% 
      leaflet::hideGroup("Satellite")
    
  # }





