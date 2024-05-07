
## ------------------------------------------------------------------------------------------ ##
## Loading in the daily position file
## ------------------------------------------------------------------------------------------ ##
rm(list=ls()) # to clear workspace

# Set working directory
setwd("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data/") # set manually to your own working directory

# Load the data
library(data.table)
track <- fread("227150/227150_daily-positions.csv"); head(track) # set manually to the right file you wish to load
track <- as.data.frame(track)
## ------------------------------------------------------------------------------------------ ##

library(dplyr)

#Download satellite map
library("ggmap")
library(rstudioapi)
register_google(key = "AIzaSyCXpUm03UmbL-PFbEIOEWvg4sfeN17rewQ")
# For close-up of track:
esri_sat <- get_map(location = c(lon = mean(track$Longitude), lat = mean(track$Latitude)), zoom = 6, maptype = "satellite", source = "google")

#Create a custom color scale
monthly_colour_palette <- read.csv("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data/monthly_colour_palette.csv", header=TRUE); head(monthly_colour_palette)
myColors <- unique(monthly_colour_palette$colour)
myColors <- setNames(myColors, unique(monthly_colour_palette$month))
colScale <- scale_colour_manual(name = "Month:", values = myColors)     

# Assign colour depending on month
track <- inner_join(track, monthly_colour_palette, by = "month") # automatically assign colour as new column based on our colour palette
track <- as.data.frame(track)
head(track)

# Convert data to Spatial Point Data Frame for mapping with correct projection
library(sf); 
track_sf <- track %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) #WGS84

#------------------
# Static plot
library(ggplot2); library(ggspatial); library(MetBrewer)

#ggplot() +
ggmap(esri_sat) +
  geom_spatial_path(data = track_sf, aes(x = Longitude, y = Latitude, col = day.at.liberty), crs = 4326, colour = track_sf$colour) +
  layer_spatial(data = track_sf, aes(col = day.at.liberty), size = 2.5, colour = track_sf$colour) +
  labs(x = "Longitude", y = "Latitude") +
  #colScale +
  annotation_scale(location = "br", line_col = "white", text_col = "white") +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering(fill = c(NA, "white"), line_col = "white", text_col = "white", text_face = "bold")) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())


#------------------
# Interactive plot
library(leaflet); library(htmlwidgets)

## Can add in variables as we calculate more for each tagged animal

mytext = paste(
  "Date: ", track$`Date`, "<br/>",
  "Ptt: ", track$Ptt, "<br/>",
  "Days at liberty: ", track$`day.at.liberty`, "<br/>",
  "Longitude: ", track$Longitude, "<br/>",
  "Latitude: ", track$Latitude, sep="") %>%
  lapply(htmltools::HTML)

myleafletplot <- leaflet() %>%
  # Base groups (you can add multiple basemaps):
  addProviderTiles(providers$Esri.WorldImagery, group="Satellite") %>%   # typical Google Earth satellite view
  addProviderTiles(providers$OpenStreetMap, group="Map") %>%   # Street Map view
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
    baseGroups = c("Satellite", "Map"),  # specify the desired basemap options for the output map
    overlayGroups = as.character(unique(track_sf$month)),  # add the data groups to overlay on the map
    options = layersControlOptions(collapsed = FALSE)) %>%
  # Add legend
  addLegend(position = "bottomright",
            colors = unique(track_sf$colour), labels = unique(track_sf$month),
            title = "Month:",
            opacity = 1
  )


myleafletplot # Print the map
