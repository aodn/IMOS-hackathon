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
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
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
      leafletOutput("map", height = "350px"),
      # Insert attribution
      tags$span(
        "(Fishing data sourced from ",
        tags$a(
          href = "https://www.globalfishingwatch.org",
          "Global Fishing Watch"
          ), ")",
        style = "font-size: 10px;"
        ),
      fluidRow(
        column(
          width = 6,
          plotOutput("envPlot", height = "350px")
        ),
        column(
          width = 6,
          plotOutput("divePlot", height = "350px")
        )
      )
    )
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
      title = HTML("Total<br>Length"),
      value = paste0(data$TotalLength, "cm"),
      icon = icon("ruler"),
      color = "blue"
    )
  })

  output$deploymentDate <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    infoBox(
      title = HTML("Tagging<br> Date"),
      value = format(data$Deployment_Date, "%d\n%b\n%Y"),
      icon = icon("calendar-plus"),
      color = "green"
    )
  })

  output$detachmentDate <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    infoBox(
      title = HTML("Pop-off<br>Date"),
      value = format(data$Detachment_Date, "%d\n%b\n%Y"),
      icon = icon("calendar-minus"),
      color = "red"
    )
  })

  output$deploymentLocation <- renderInfoBox({
    req(input$tagID)
    data <- metadata[metadata$Ptt == input$tagID, ]
    text <- paste(
      # data$Deployment_Location, 
      round(data$Deployment_Lat, 2), round(data$Deployment_Lon, 2), sep = ", ")
    infoBox(
      title = HTML("Tagging<br>Location"),
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
      title = HTML("Pop-off<br>Location"),
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
      
      fish_pal <- colorNumeric("Reds", values(fish_ras),  na.color = "transparent")


      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "Map") %>%
        addRasterImage(
          x = fish_ras, 
          opacity = 0.5,
          color = fish_pal,
          layerId = "Fishing effort", group = "Fishing effort"
        ) %>% 
        addPolygons(
          data = error_pols,
          stroke = T, weight = 1, color = "grey",
          fillOpacity = 0.15, fillColor = "lightgrey",
          labelOptions = labelOptions(noHide = TRUE)
        )  %>% 
        addPolylines(
          lng = track_sf$Longitude, lat = track_sf$Latitude,
          color = "white", weight = 1.5,
          labelOptions = labelOptions(noHide = TRUE)
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
                  position = "bottomright", opacity = 1, group = "Fishing effort",
                  className = "custom-leaflet-legend"
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
        hideGroup("Fishing effort")
      
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
    print(track)

    is_highlighted <- nrow(highlightData()) > 0

    ggplot(track, aes(x = Date)) +
      geom_ribbon(aes(ymin = MinTemp, ymax = MaxTemp), fill = "grey", col = NA, alpha = 0.5) +
      geom_path(aes(y = MeanTempPDT), color = track$colour, na.rm = TRUE) +
      geom_path(aes(y = MinTemp), linetype = 3, col = "grey60", na.rm = TRUE) +
      geom_path(aes(y = MaxTemp), linetype = 3, col = "grey60", na.rm = TRUE) +
      theme_minimal() +
      list(if(is_highlighted) geom_point(data = highlightData(), aes(x = Date, y = MeanTempPDT), color = "red"))
  })

  # Bar plot of dive depth
  output$divePlot <- renderPlot({
    if(is_empty(input$tagID)) return()
    histoplot(tag_ids = as.character(input$tagID), folder_path = "../data")
  })
 
  ## Plot for individual fish
  # output$envPlot <- renderPlot({
  #   if(is_empty(input$tagID)) return()
  #   
  #   h <- histoviz("../data")
  #   
  #   ## summarise variables to larger time steps (daily) so it plots nicer
  #   h_summary <-
  #     h %>%
  #     filter(id %in% input$tagID) %>% 
  #     mutate(date = date(date)) %>% 
  #     group_by(id, date, bin, variable) %>% 
  #     summarise(prop = mean(prop))
  #   
  #   a <-
  #     h_summary %>% 
  #     filter(variable %in% "depth") %>% 
  #     ggplot(aes(x = date, y = fct_rev(bin), fill = prop, id = variable)) +
  #     geom_tile() +
  #     scale_x_date(expand = c(0,0)) +
  #     scale_fill_viridis_c(direction = -1, na.value = NA, limits = c(0,100)) +
  #     labs(x = "Date", y = "Depth (m)", fill = "Proportion\nof time") +
  #     theme_bw()
  #   
  #   b <- 
  #     h_summary %>% 
  #     filter(variable %in% "temp") %>% 
  #     ggplot(aes(x = date, y = bin, fill = prop, id = variable)) +
  #     geom_tile() +
  #     scale_x_date(expand = c(0,0)) +
  #     scale_fill_viridis_c(option = "A", direction = -1, na.value = NA, limits = c(0,100)) +
  #     labs(x = "Date", y = "Temperature (˚C)", fill = "Proportion\nof time") +
  #     theme_bw()
  #   
  #   a/b
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)

