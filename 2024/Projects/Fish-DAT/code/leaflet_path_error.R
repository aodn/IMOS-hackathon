## Function to plot leaflet map with GPE3 positions and error

## Load the other GPE3_error
source("https://raw.githubusercontent.com/aodn/IMOS-hackathon/fishdat/2024/Projects/Fish-DAT/code/GPE3_error_extraction.R")

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras2)
library(terra)
library(tidync)


# Set up a function all we provide is the location of the folder and metadata and it plots the movement with the errors

## data input required
# folder_name: location of folder with all the MiniPAT files and GPE3 outputs
# metadata: a metadata data.frame that 
# prop : a vector of probabilities for the error 

folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"
metadata <- list.files(folder_path, pattern = "*Metadata*.csv") %>% read_csv()
prob <- c(0.99, 0.95, 0.5)
i = 1

leaflet_path_error <-
  function(folder_name, metadata, prop, .verbose = F){
    
    ## parse all the histo files in the folder
    csv_file <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.csv", full.names = T)}) %>% 
      unlist()
    
    nc_files <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.nc", full.names = T)}) %>% 
      unlist()
    
    # kmz_files <-
    #   list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
    #   map(function(x){list.files(x, pattern = "*GPE3.kmz", full.names = T)}) %>% 
    #   unlist()
    
    
    if(.verbose){
      message("Grabbing GPE3 output to map...") 
    }
    
    ## Lets convert the GPE3 csv_file data into a spatial object with positions per month
    pos_dat <-
      read_csv(csv_file, skip = 5) %>% 
      transmute(Ptt = factor(Ptt), 
                lon = `Most Likely Longitude`,
                lat = `Most Likely Latitude`,
                col = viridisLite::cividis(n_distinct(Ptt))[Ptt])
      
    # pos_sf <-
    #   pos_dat %>% 
    #   group_by(Ptt) %>% 
    #   st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
    
    # pos_path <-
    #   pos_sf %>% 
    #   summarise(do_union = F) %>% 
    #   st_cast("LINESTRING")
    
    ## Extract error polygons
    pols <- gpe3_error_polys(folder_path, prob, verbose = .verbose)
    
    
    ## Leaflet plot
    mytext = paste(
      "Date: ", track$`Date`, "<br/>",
      "Ptt: ", track$Ptt, "<br/>",
      "Days at liberty: ", track$`day.at.liberty`, "<br/>",
      "Longitude: ", track$Longitude, "<br/>",
      "Latitude: ", track$Latitude, sep="") %>%
      lapply(htmltools::HTML)
    
    # myleafletplot <-
    leaflet() %>%
      # Base groups (you can add multiple basemaps):
      addProviderTiles(providers$OpenStreetMap, group="Map") %>%   # Street Map view
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%   # typical Google Earth satellite view
      # Add location data:
      addPolylines(lng = track_sf$Longitude, lat = track_sf$Latitude, 
                   color = "white", weight = 1.5,
                   labelOptions = labelOptions(noHide = TRUE)) %>%
      # add the tag detection data
      addCircleMarkers(lng = track_sf$Longitude, lat = track_sf$Latitude, 
                       weight = 2, radius = 4, color = track_sf$colour,
                       stroke = FALSE, fillOpacity = 1, 
                       group = track_sf$month,
                       label=mytext) %>%  # don’t forget to assign a group to the markers
      # Layers control
      addLayersControl(
        overlayGroups = c("Satellite"),  # specify the desired basemap options for the output map
        baseGroups = as.character(unique(track_sf$Ptt)),  # add the data groups to overlay on the map
        options = layersControlOptions(collapsed = FALSE, position = "topright")) %>%
      # Add legend
      addLegend(position = "topleft",
                colors = myColors, labels = names(myColors),
                title = "Month:",
                opacity = 1
      ) %>% 
      leaflet::hideGroup("Satellite")
    
  }





