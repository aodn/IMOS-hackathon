## Boilerplate code created by ChatGPT
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(fs)
library(sf)
# library(shinyjs)
# library(logging)


# Get choices for species and tag ID
basedir <- "./2024/Projects/Fish-DAT/data/"
metadata <- read_csv(paste0(basedir, "HackathonMetadata.csv"))

# detect numeric dir filenames
tag_ids <- metadata$Ptt
species_id <- metadata$Species %>% unique()


# # construct file path
# fn <- list.files(paste0(basedir, dirs[1]), pattern = "daily-positions.csv", full.names = TRUE)

# Define UI
source(dir_ls(glob = "*HistoPlot.R", recurse = T))

header <- dashboardHeader(
  title = "Fish Tracking Dashboard"
)

sidebar <- dashboardSidebar(
  disable = TRUE
  # sidebarMenu(
  #   menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  #   menuItem("Visualizations", tabName = "visualizations", icon = icon("bar-chart-o")),
  #   menuItem("Maps", tabName = "maps", icon = icon("map"))
  # )
)

body <- dashboardBody(
  titlePanel("Fish Tracking Dashboard"),
  fluidRow(
    column(
      width = 4,
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
      width = 8,
      leafletOutput("map", height = "400px"),
      # Insert attribution
      tags$a(
        href = "https://www.globalfishingwatch.org",
        "Powered by Global Fishing Watch"
      ),
      fluidRow(
        column(
          width = 8
          ),
        column(
          width = 4,
          plotOutput("divePlot")
          )
        )
    ),
    
    # column(
    #   width = 4,
    #   plotOutput("divePlot"),
    #   plotOutput("behaviourPlot")
    # )
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


  # Reactive expression to manage map data processing
  mapData <- reactive({
    req(input$tagID) # Ensure tagID is selected


    files <- list.files(basedir, pattern = paste0(input$tagID, "_daily-positions.csv"), full.names = TRUE, recursive = TRUE)


    if (length(files) > 0) {
      track <- read_csv(files[1])

      # Create a custom color scale
      monthly_colour_palette <- read_csv(paste0(
        basedir, "monthly_colour_palette.csv"
      ))
      head(monthly_colour_palette)
      myColors <- unique(monthly_colour_palette$colour)
      myColors <- setNames(myColors, unique(monthly_colour_palette$month))
      colScale <- scale_colour_manual(name = "Month:", values = myColors)

      # Assign colour depending on month
      track <- inner_join(track, monthly_colour_palette, by = "month") # automatically assign colour as new column based on our colour palette
      track <- as.data.frame(track)

      track_sf <- track %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)

      mytext <- paste(
        "Date: ", track$`Date`, "<br/>",
        "Ptt: ", track$Ptt, "<br/>",
        "Days at liberty: ", track$`day.at.liberty`, "<br/>",
        "Longitude: ", track$Longitude, "<br/>",
        "Latitude: ", track$Latitude,
        sep = ""
      ) %>%
        lapply(htmltools::HTML)


      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "Map") %>%
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
          label = mytext
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Map"),
          overlayGroups = as.character(unique(track_sf$month)),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addLegend(
          position = "bottomright",
          colors = unique(track_sf$colour), labels = unique(track_sf$month),
          title = "Month:",
          opacity = 1
        )
    } else {
      leaflet() %>% addProviderTiles(providers$Esri.WorldImagery)
    }
  })

  # Render the map from the reactive expression
  output$map <- renderLeaflet({
    mapData() # This will only recompute when input$tagID changes or when the data changes
  })

  # # Sample data - replace this with your actual dataset
  # data <- reactive({
  #   # Simulate filtering based on inputs
  #   df <- data.frame(
  #     Species = sample(c("Species 1", "Species 2"), 100, replace = TRUE),
  #     TagID = sample(c("ID 1", "ID 2"), 100, replace = TRUE),
  #     Date = seq(as.Date("2020-01-01"), as.Date("2020-01-01") + 99, by = "day"),
  #     Longitude = rnorm(100, mean = -20),
  #     Latitude = rnorm(100, mean = 50),
  #     DiveDepth = rnorm(100, mean = 200, sd = 50),
  #     Behaviour = rnorm(100)
  #   )
  #   df <- df[df$Species %in% input$species & df$TagID %in% input$tagID, ]
  #   df
  # })
  #
  # # Information box output
  # output$infoBox <- renderPrint({
  #   if (nrow(data()) > 0) {
  #     paste(
  #       "Tag ID:", unique(data()$TagID),
  #       "\nRelease Date: 2020-01-01", # Placeholder
  #       "\nPop Off Date: 2020-12-31", # Placeholder
  #       "\nDays at Liberty:", 365
  #     ) # Placeholder
  #   } else {
  #     "No data available"
  #   }
  # })
  #
  # # Plot for individual fish
  # output$fishPlot <- renderPlot({
  #   ggplot(data(), aes(x = Date, y = DiveDepth)) +
  #     geom_line() +
  #     theme_minimal() +
  #     labs(title = "Dive Profile Over Time", x = "Date", y = "Depth (m)")
  # })
  #
  # Bar plot of dive depth
  output$divePlot <- renderPlot({
    histoplot(tag_ids = as.character(input$tagID), folder_path = basedir)
  })
  #
  # # Scatter plot of fish behaviour variable
  # output$behaviourPlot <- renderPlot({
  #   ggplot(data(), aes(x = Date, y = Behaviour)) +
  #     geom_point(alpha = 0.6, color = "red") +
  #     theme_minimal() +
  #     labs(title = "Fish Behaviour Over Time", x = "Date", y = "Behaviour")
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
