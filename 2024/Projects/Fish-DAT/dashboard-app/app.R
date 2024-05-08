## Boilerplate code created by ChatGPT 4

library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Fish Tracking Dashboard"),
  fluidRow(
    column(width = 4,
           pickerInput("species", "Species:",
                       choices = c("Species 1", "Species 2"), 
                       multiple = TRUE, options = list(`actions-box` = TRUE)),
           pickerInput("tagID", "Tag ID:",
                       choices = c("ID 1", "ID 2"), 
                       multiple = TRUE, options = list(`actions-box` = TRUE)),
           dateRangeInput("dateRange", "Date Range:"),
           textInput("region", "Deployment Region:"),
           textInput("angler", "Tagging Angler:"),
           verbatimTextOutput("infoBox")
    ),
    column(width = 4,
           plotOutput("fishPlot"),
           leafletOutput("map", height = "400px")
    ),
    column(width = 4,
           plotOutput("divePlot"),
           plotOutput("behaviourPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Sample data - replace this with your actual dataset
  data <- reactive({
    # Simulate filtering based on inputs
    df <- data.frame(
      Species = sample(c("Species 1", "Species 2"), 100, replace = TRUE),
      TagID = sample(c("ID 1", "ID 2"), 100, replace = TRUE),
      Date = seq(as.Date('2020-01-01'), as.Date('2020-01-01') + 99, by = "day"),
      Longitude = rnorm(100, mean = -20),
      Latitude = rnorm(100, mean = 50),
      DiveDepth = rnorm(100, mean = 200, sd = 50),
      Behaviour = rnorm(100)
    )
    df <- df[df$Species %in% input$species & df$TagID %in% input$tagID,]
    df
  })
  
  # Information box output
  output$infoBox <- renderPrint({
    if (nrow(data()) > 0) {
      paste("Tag ID:", unique(data()$TagID),
            "\nRelease Date: 2020-01-01",  # Placeholder
            "\nPop Off Date: 2020-12-31",  # Placeholder
            "\nDays at Liberty:", 365)     # Placeholder
    } else {
      "No data available"
    }
  })
  
  # Plot for individual fish
  output$fishPlot <- renderPlot({
    ggplot(data(), aes(x = Date, y = DiveDepth)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Dive Profile Over Time", x = "Date", y = "Depth (m)")
  })
  
  # Map for fish tracks
  output$map <- renderLeaflet({
    leaflet(data()) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 5,
                 color = '#ffa500', fillOpacity = 0.5)
  })
  
  # Bar plot of dive depth
  output$divePlot <- renderPlot({
    ggplot(data(), aes(x = factor(TagID), y = DiveDepth)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Dive Depth Distribution", x = "Tag ID", y = "Depth (m)")
  })
  
  # Scatter plot of fish behaviour variable
  output$behaviourPlot <- renderPlot({
    ggplot(data(), aes(x = Date, y = Behaviour)) +
      geom_point(alpha = 0.6, color = 'red') +
      theme_minimal() +
      labs(title = "Fish Behaviour Over Time", x = "Date", y = "Behaviour")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
