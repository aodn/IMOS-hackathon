## Fish tracking dashboard 
## Authors: Dahlia Foo, Dave Green, ChatGPT

library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(terra)
# library(plotly)
# library(shinyjs)
# library(logging)
library(ggiraph)

source(list.files(pattern="dashboard_setup.R", recursive=TRUE))

# Set the relative path to the data folder
base_folder <- ".."
tag_ids <- metadata$Ptt
species_id <- metadata$Species %>% unique()

# Define UI
header <- dashboardHeader(
  title = "Fish Tracking Dashboard"
)

sidebar <- dashboardSidebar(
  disable = TRUE
)

body <- dashboardBody(
  titlePanel("Fish Tracking Dashboard"),
  fluidRow(
    column(
      width = 2,
      pickerInput("species", "Species:",
        choices = c("all", species_id),
        options = list(`actions-box` = TRUE),
        selected = "all"
      ),
      pickerInput("tagID", "Tag ID:",
        choices = NULL,
        options = list(`actions-box` = TRUE)
      ),
      infoBoxOutput("totalLength", width = NULL),
      infoBoxOutput("deploymentLocation", width = NULL),
      infoBoxOutput("deploymentDate", width = NULL),
      infoBoxOutput("detachmentDate", width = NULL),
      infoBoxOutput("detachmentLocation", width = NULL),
    ),
    column(
      width = 10,
      leafletOutput("map", height = "400px"),
      # Insert attribution
      tags$a(
        href = "https://www.globalfishingwatch.org",
        "Powered by Global Fishing Watch"
      ),
      fluidRow(
        column(
          width = 6,
          plotOutput("envPlot", height = "150px")
        ),
        column(
          width = 6,
          plotOutput("divePlot")
        )
      )
    ),
  )
)

ui <- dashboardPage(
  header, sidebar, body
)

# Define server logic
server <- function(input, output, session) {
  # Update species choices directly in the server
  # Observe any changes in the species selection
  observe({
    # Filter tag IDs based on selected species


    if (input$species == "all") {
      validTags <- metadata %>%
        pull(Ptt) %>%
        unique()
    } else {
      validTags <- metadata %>%
        filter(Species %in% input$species) %>%
        pull(Ptt) %>%
        unique()
    }

    updatePickerInput(session, "tagID", choices = validTags)
  })

  # infoBoxes
  output$totalLength <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    infoBox(
      title = "Total Length",
      value = paste0(data$TotalLength, "cm"),
      icon = icon("ruler"),
      color = "blue"
    )
  })

  output$deploymentDate <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    infoBox(
      title = "Deployment Date",
      value = data$Deployment_Date,
      icon = icon("calendar-plus"),
      color = "green"
    )
  })

  output$detachmentDate <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    infoBox(
      title = "Detachment Date",
      value = data$Detachment_Date,
      icon = icon("calendar-minus"),
      color = "red"
    )
  })

  output$deploymentLocation <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    text <- paste(data$Deployment_Location, round(data$Deployment_Lat, 2), round(data$Deployment_Lon, 2), sep = ", ")
    infoBox(
      title = "Deployment Location",
      value = text,
      icon = icon("map-marker"),
      color = "green"
    )
  })

  output$detachmentLocation <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    text <- paste(round(data$Detachment_Lat, 2), round(data$Detachment_Lon, 2), sep = ", ")
    infoBox(
      title = "Detachment Location",
      value = text,
      icon = icon("map-marker"),
      color = "red"
    )
  })


  trackData <- reactive({
    req(input$tagID)
    load_track_data(input$tagID, base_folder, monthly_colour_palette)
  })


  # Reactive expression to manage map data processing
  mapData <- reactive({
    req(trackData())
    trackData <- trackData()

    if (is.null(trackData) == FALSE) {
      track_sf <- trackData$track_sf
      error_pols <- trackData$error_polys
      fish_ras <- rast(trackData$fish_dat[c(2,1,3)], type = "xyz", crs = 'epsg:4326')

      mytext <- paste(
        "Date: ", track_sf$`Date`, "<br/>",
        "Ptt: ", track_sf$Ptt, "<br/>",
        "Days at liberty: ", track_sf$`day.at.liberty`, "<br/>",
        "Longitude: ", track_sf$Longitude, "<br/>",
        "Latitude: ", track_sf$Latitude,
        sep = ""
      ) %>%
        lapply(htmltools::HTML)
      
      fish_pal <- colorNumeric("viridis", values(fish_ras), na.color = "transparent")


      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "Map") %>%
        addPolylines(
          lng = track_sf$Longitude, lat = track_sf$Latitude,
          color = "white", weight = 1.5,
          labelOptions = labelOptions(noHide = TRUE)
        ) %>%
        addPolygons(
          data = error_pols,
          stroke = T, weight = 1, color = "grey",
          fillOpacity = 0.15, fillColor = "lightgrey",
          labelOptions = labelOptions(noHide = TRUE)
        ) %>% 
        addRasterImage(
          x = fish_ras, 
          opacity = 0.5,
          color = fish_pal,
          layerId = "Fishing effort", group = "Fishing effort"
        ) %>% 
        addCircleMarkers(
          lng = track_sf$Longitude, lat = track_sf$Latitude,
          weight = 2, radius = 4, color = track_sf$colour,
          stroke = FALSE, fillOpacity = 1,
          group = track_sf$month,
          label = mytext,
          layerId = track_sf$`Date`
        ) %>%
        addLegend(pal = fish_pal, values = values(fish_ras), 
                  title = "Fishing<br>effort (h)", layerId = "Fishing effort",
                  position = "bottomright", opacity = 1, group = "Fishing effort"
        ) %>% 
        addLegend(
          position = "bottomleft",
          colors = unique(track_sf$colour), labels = unique(track_sf$month),
          title = "Month:",
          opacity = 1
        ) %>%
        addLayersControl(
          overlayGroups = c("Fishing effort"),
          # baseGroups = as.character(unique(track_sf$month)),
          baseGroups = c("Satellite", "Map"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup("Fishing")
    } else {
      leaflet() %>% addProviderTiles(providers$Esri.WorldImagery)
    }
  })


  # Render the map from the reactive expression
  output$map <- renderLeaflet({
    mapData() # This will only recompute when input$tagID changes or when the data changes
  })

  # Generate highlightData in reactive instead
  highlightData <- reactive({
    req(trackData())
    click <- input$map_marker_mouseover$id
    trackData <- trackData()
    track_sf <- trackData$track_sf
    track_sf[track_sf$`Date` == click, ]
  })

  # Plot for individual fish
  output$envPlot <- renderPlot({

    track <- trackData()$track
    
    is_highlighted <- nrow(highlightData()) > 0

    ggplot(track, aes(x = Date)) +
      geom_path(aes(y = MeanTempPDT), color = track$colour, na.rm = TRUE) +
      geom_path(aes(y = MinTemp), linetype = "dashed", na.rm = TRUE) +
      geom_path(aes(y = MaxTemp), linetype = "dashed", na.rm = TRUE) +
      theme_minimal() +
      list(if(is_highlighted) geom_point(data = highlightData(), aes(x = Date, y = MeanTempPDT), color = "red"))
  })

  # Bar plot of dive depth
  output$divePlot <- renderPlot({
    if(is_empty(input$tagID)) return()
    histoplot(tag_ids = as.character(input$tagID), folder_path = "../data")
  })

 
}

# Run the application
shinyApp(ui = ui, server = server)

