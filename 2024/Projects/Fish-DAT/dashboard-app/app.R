## Fish tracking dashboard
## Authors: Dahlia Foo, Dave Green, ChatGPT

library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
# library(shinydashboard)
library(terra)
# library(plotly)
# library(shinyjs)
# library(logging)
library(ggiraph)
library(bslib)
library(bsicons)
library(plotly)

source(list.files(pattern = "dashboard_setup.R", recursive = TRUE))

# Set the relative path to the data folder
base_folder <- ".."
tag_ids <- metadata$Ptt
species_id <- metadata$Species %>% unique()


# ui ----------------------------------------------------------------------

sidebar <- sidebar(
  title = "Select individual",
  pickerInput("species", "Species:",
    choices = c("All", species_id),
    options = list(`actions-box` = TRUE),
    selected = "All"
  ),
  pickerInput("tagID", "Tag ID:",
    choices = NULL,
    options = list(`actions-box` = TRUE)
  ),
  card(
    card_header("Total length"),
    textOutput("totalLength"),
  ),
 card(
   card_header("Deployment period"),
   textOutput("deploymentPeriod")
 ),
 card(
   card_header("Start location"),
   textOutput("deploymentLocation")
 ),
 card(
   card_header("End location"),
   textOutput("detachmentLocation")
 )
)

body <- layout_columns(
  col_widths = c(12, 12, 6, 6),
  fill = FALSE,
 layout_columns(
   fill = F,
   value_box(
     title = "Distance to Shelf",
     value = textOutput("dist_to_shelf"),
     showcase = plotlyOutput("dist_to_shelf_showcase"),
     theme = "green",
     showcase_layout = "bottom",
     tags$p("(mean)", style = "font-size: 10px;")
   ),
   value_box(
     title = "Mean Depth",
     value = textOutput("mean_depth"),
     showcase = plotlyOutput("mean_depth_showcase"),
     theme = "green",
     showcase_layout = "bottom",
     tags$p("(mean)", style = "font-size: 10px;")
   ),
   value_box(
     title = "Daily Distance Travelled",
     value = textOutput("daily_dist"),
     showcase = plotlyOutput("daily_dist_showcase"),
     theme = "green",
     showcase_layout = "bottom",
     tags$p("(mean)", style = "font-size: 10px;")
   ),
 ),
  card(
    full_screen = T,
    leafletOutput("map", height = "350px"),
    # Insert attribution
    tags$span(
      "(Fishing data sourced from ",
      tags$a(
        href = "https://www.globalfishingwatch.org",
        "Global Fishing Watch"
      ), ")",
      style = "font-size: 10px;"
    )
  ),
  card(
    checkboxInput("show_active_date", "Show date marker on map hover", value = TRUE),
    plotOutput("envPlot1", height = "350px")
  ),
  card(
    plotOutput("divePlot", height = "350px")
  )
)


ui <- page_sidebar(title = div("Fish tracking dashboard", tags$span(textOutput("active_individual"), style = "font-size: 10px;")), sidebar = sidebar, body)



# server -----------------------------------------------------------------


server <- function(input, output, session) {
  # Update species choices directly in the server
  # Observe any changes in the species selection
  observe({
    # Filter tag IDs based on selected species
    if (input$species == "All") {
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


  # Sidebar info Cards --------------------------------------------------------------
  individual_data <- reactive({
    req(input$tagID)
    metadata %>% filter(Ptt == input$tagID)
  })

  output$active_individual <- renderText({
    paste0("Now showing: ", individual_data()$Ptt, " (", individual_data()$Species, ")")
  })

  output$totalLength <- renderText({
    paste0(individual_data()$TotalLength, "cm")
  })

  output$deploymentPeriod <- renderText({
    paste0(format(individual_data()$Deployment_Date, "%d\n%b\n%Y"), " - ",
           format(individual_data()$Detachment_Date, "%d\n%b\n%Y"))
  })

  output$deploymentLocation <- renderText({
    paste(round(individual_data()$Deployment_Lat, 2), round(individual_data()$Deployment_Lon, 2), sep = ", ")
  })

  output$detachmentLocation <- renderText({
    paste(round(individual_data()$Detachment_Lat, 2), round(individual_data()$Detachment_Lon, 2), sep = ", ")
  })

  
  trackData <- reactive({
    req(input$tagID)
    load_track_data(input$tagID, base_folder, monthly_colour_palette)
  })
  


# Value boxes --------------------------------------------------

  output$dist_to_shelf <- renderText({
    track_sf <- trackData()$track_sf
    paste0(mean(track_sf$DistToShelf, na.rm = T) %>%  round(0), " km")
  })  
  
  output$dist_to_shelf_showcase <- renderPlotly({
    track_sf <- trackData()$track_sf
    create_sparkline(track_sf, x = "Date", y = "DistToShelf")

  })
  
  output$mean_depth <- renderText({
    track_sf <- trackData()$track_sf
    paste0(mean(track_sf$MeanDepthPDT, na.rm = T) %>% round(0), " m")
  })
  
  output$mean_depth_showcase <- renderPlotly({
    track_sf <- trackData()$track_sf
    create_sparkline(track_sf, x = "Date", y = "MeanDepthPDT")
  })
  
  output$daily_dist <- renderText({
    track_sf <- trackData()$track_sf
    paste0(mean(track_sf$DistTravelled, na.rm = T) %>% round(0), " km")
  })
  
  output$daily_dist_showcase <- renderPlotly({
    track_sf <- trackData()$track_sf
    create_sparkline(track_sf, x = "Date", y = "DistTravelled")
  })
  
  
  # Map ---------------------------------------------------------------------

 
  
  

  # Reactive expression to manage map data processing
  mapData <- reactive({
    req(trackData())
    trackData <- trackData()
    
    print(trackData()$track)

    if (is.null(trackData) == FALSE) {
      track_sf <- trackData$track_sf
      error_pols <- trackData$error_polys
      fish_ras <- rast(trackData$fish_dat[c(2, 1, 3)], type = "xyz", crs = "epsg:4326")

      mytext <- paste(
        "Date: ", track_sf$`Date`, "<br/>",
        "Ptt: ", track_sf$Ptt, "<br/>",
        "Days at liberty: ", track_sf$`day.at.liberty`, "<br/>",
        "Longitude: ", track_sf$Longitude, "<br/>",
        "Latitude: ", track_sf$Latitude,
        sep = ""
      ) %>%
        lapply(htmltools::HTML)

      fish_pal <- colorNumeric("Reds", values(fish_ras), na.color = "transparent")


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
        ) %>%
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
        addLegend(
          pal = fish_pal, values = values(fish_ras),
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



  # Plots -------------------------------------------------------------------


  # Generate highlightData in reactive instead
  highlightData <- reactive({
    req(trackData())
    click <- input$map_marker_mouseover$id
    trackData <- trackData()
    track_sf <- trackData$track_sf
    track_sf[track_sf$`Date` == click, ]
  })

  # Plot for individual fish
  output$envPlot1 <- renderPlot({
    track <- trackData()$track

    if (input$show_active_date) is_highlighted <- nrow(highlightData()) > 0

    ggplot(track, aes(x = Date)) +
      geom_ribbon(aes(ymin = MinTemp, ymax = MaxTemp), fill = "grey", col = NA, alpha = 0.5) +
      geom_path(aes(y = MeanTempPDT), color = track$colour, na.rm = TRUE) +
      geom_path(aes(y = MinTemp), linetype = 3, col = "grey60", na.rm = TRUE) +
      geom_path(aes(y = MaxTemp), linetype = 3, col = "grey60", na.rm = TRUE) +
      theme_minimal() +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      list(if (input$show_active_date && is_highlighted) geom_vline(data = highlightData(), aes(xintercept = Date), color = "red"))
  })


  # Plot for individual fish
  output$envPlot2 <- renderPlot({
    track <- trackData()$track
    print(track)

    if (input$show_active_date) is_highlighted <- nrow(highlightData()) > 0

    ggplot(track, aes(x = Date)) +
      geom_ribbon(aes(ymin = MinTemp, ymax = MaxTemp), fill = "grey", col = NA, alpha = 0.5) +
      geom_path(aes(y = MeanDepthPDT), color = track$colour, na.rm = TRUE) +
      geom_path(aes(y = MinDepth), linetype = 3, col = "grey60", na.rm = TRUE) +
      geom_path(aes(y = MaxDepth), linetype = 3, col = "grey60", na.rm = TRUE) +
      theme_minimal() +
      list(if (input$show_active_date && is_highlighted) geom_vline(data = highlightData(), aes(xintercept = Date), color = "red"))
  })

  # Bar plot of dive depth
  output$divePlot <- renderPlot({
    if (is_empty(input$tagID)) {
      return()
    }
    histoplot(tag_ids = as.character(input$tagID), folder_path = "../data")
  })


  # output$seriesPlot <- renderPlot({
  #
  #   p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
  #     theme(legend.position = "bottom"),
  #     if (input$by_species) aes(color = Species),
  #     geom_point(),
  #     if (input$smooth) geom_smooth()
  #   )
  #
  #
  # })

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
