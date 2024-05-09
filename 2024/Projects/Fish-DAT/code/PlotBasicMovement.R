
## ------------------------------------------------------------------------------------------ ##
## Loading in the daily position file
## ------------------------------------------------------------------------------------------ ##
rm(list=ls()) # to clear workspace

# Load the data
library(data.table)
track <- fread("2024/Projects/Fish-DAT/data/kingfish/227151/227151_daily-positions.csv"); head(track) # set manually to the right file you wish to load
track <- as.data.frame(track)
## ------------------------------------------------------------------------------------------ ##

setwd("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data") # set manually to your own working directory

## Packages needed
library(tidyverse)
library(lubridate)
library(data.table)
library(sf)
library(ggspatial)
library(leaflet)
library(leaflet.extras2)
library(htmlwidgets)

# Load the data
track <- read_csv("227150/227150_daily-positions.csv") #; head(track) # set manually to the right file you wish to load


# track <- as.data.frame(track)
## ------------------------------------------------------------------------------------------ ##

## Create a spatial map

#Create a custom color scale
monthly_colour_palette <- read_csv("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data/monthly_colour_palette.csv")
myColors <- unique(monthly_colour_palette$colour)
myColors <- setNames(myColors, unique(monthly_colour_palette$month))
# colScale <- scale_colour_manual(name = "Month:", values = myColors)     

# Assign colour depending on month
track <- inner_join(track, monthly_colour_palette, by = "month") # automatically assign colour as new column based on our colour palette
# track <- as.data.frame(track)
head(track)

# Convert data to Spatial Point Data Frame for mapping with correct projection
track_sf <- track %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%   #WGS84
  mutate(month = factor(month, levels = c(names(myColors))))

#------------------
# Static plot

esri_sat <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Imagery/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() +
  annotation_map_tile(esri_sat) +
  geom_spatial_path(data = track_sf, crs = 4326, 
                    aes(x = Longitude, y = Latitude, col = month)) +
  layer_spatial(data = track_sf, aes(col = month), size = 2.5) +
  labs(x = "Longitude", y = "Latitude") +
  annotation_scale(location = "br", line_col = "white", text_col = "white") +
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_fancy_orienteering(fill = c(NA, "white"), line_col = "white", 
                                                                text_col = "white", text_face = "bold")) +
  scale_colour_manual(name = "Month:", values = myColors) +
  facet_wrap(~Ptt) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())


#------------------
# Interactive plot
## Can add in variables as we calculate more for each tagged animal

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


myleafletplot # Print the map
