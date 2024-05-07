#Load libraries
library(shiny)
library(shinyjs)
library(tidyverse)
library(tablerDash)
library(shinyWidgets)
library(pushbar)
library(waiter)
library(shinyEffects)
library(shinycssloaders)
library(leaflet)
library(sf)
library(htmlwidgets)

#Load data
track <- read.csv("data/227151/227151_daily-positions.csv")
photo <- "kingfish_test.jpg"

#Create map

#Create a custom color scale
monthly_colour_palette <- read.csv("data/monthly_colour_palette.csv", header=TRUE)
myColors <- setNames(myColors, unique(monthly_colour_palette$month))
colScale <- scale_colour_manual(name = "Month:", values = myColors) 

# Assign colour depending on month
track <- inner_join(track, monthly_colour_palette, by = "month") # automatically assign colour as new column based on our colour palette
track <- as.data.frame(track)

# Convert data to Spatial Point Data Frame for mapping with correct projection
library(sf); 
track_sf <- track %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) #WGS84

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

ui <- tablerDashPage(
  enable_preloader = TRUE,
  loading_duration = 4,
  navbar = tablerDashNav(
    id = "menu",
    div(
      style = "display: inline-block; width: 50%",
      img(src = "kingfish_logo.jpg",
          width = "80",
          style="margin-right:2000px !important;")
    ),
    div(
      style = "display: inline-block; width: 50%",
      img(src = "sims_logo.jpeg",
          width = "140",
          height ="70",
          style="margin-left:-940px !important;")
      ),
    
    
    navMenu = tablerNavMenu(
      tablerNavMenuItem(
        tabName = "Home",
        icon = "camera",
        "Kingfish Passport"
      )
    )
  ),
  title = "Kingfish Passport",
  body = tablerDashBody(
    # load pushbar dependencies
    pushbar_deps(),
    # load the waiter dependencies
    use_waiter(),
    # load shinyjs
    useShinyjs(),
    chooseSliderSkin("Round"),
    setShadow(class = "galleryCard"),
    setZoom(class = "galleryCard"),
    # test whether mobile or not
    tags$script(
      "$(document).on('shiny:connected', function(event) {
            var isMobile = /iPhone|iPad|iPod|Android/i.test(navigator.userAgent);
            Shiny.onInputChange('isMobile', isMobile);
          });
          "
    ),
    
    tablerTabItems(
      tablerTabItem(
        tabName = "Home",
        fluidRow(
          column(3,
            tablerProfileCard(
              title = "227151",
              subtitle = "Test tag photo",
              background = photo,
              width = 12),
            tablerStatCard(title = "Release Location",
                           value = "Sydney",
                           width = 12),
            tablerStatCard(title = "Release Date",
                           value = "2022",
                           width = 12),
            tablerStatCard(title = "Days at liberty",
                           value = "85",
                           width = 12)
            
          ),
          column(6,
                 tablerCard(
                   title = "Test track Kingfish 227151",
                   leafletOutput("map"),
                   width = 12
                 )
                )
            
            
            )
          )
        )
    )
  )
    




server <- function(input,output,session){
  isMobile <- reactive(input$isMobile)
  
  output$map <- renderLeaflet({
    myleafletplot
  })
}

shinyApp(ui,server)



