library(sf)

# Create function to load individual track summaries output data and combine monthly colour palette to it
## VU: modified function to pull error polygons, fishing effort and MPA data to plot
load_track_data <- function(tag_id, base_folder, monthly_colour_palette) {
  files <- list.files(base_folder, pattern = paste0(tag_id, "_summaries.csv"), full.names = TRUE, recursive = TRUE)
  
  if (length(files) > 0) {
    track <- read_csv(files[1], show_col_types = FALSE)
    
    # Assign colour depending on month
    track <- inner_join(track, monthly_colour_palette, by = "month")
    track <- as.data.frame(track)
    
    track_sf <- track %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
    
    ## error polygons
    ras_file <- list.files(base_folder, pattern = paste0(tag_id, "_error_polygons.geojson"), full.names = TRUE, recursive = TRUE)
    error_polys <- st_read(ras_file)
    
    ## fishing effort
    fish_file <- list.files(base_folder, pattern = paste0(tag_id, "_GFW.csv"), full.names = TRUE, recursive = TRUE)
    fish_dat <- read_csv(fish_file, show_col_types = FALSE)
    
    return(
      list(
        track = track,
        track_sf = track_sf,
        error_polys = error_polys,
        fish_dat = fish_dat
      )
    )
  } else {
    return(NULL)
  }
}



# Create my custom ggplot colour scale
create_colour_scale <- function(monthly_colour_palette) {
  myColors <- unique(monthly_colour_palette$colour)
  myColors <- setNames(myColors, unique(monthly_colour_palette$month))
  colScale <- scale_colour_manual(name = "Month:", values = myColors)
  return(colScale)
}


create_sparkline <- function(data, x, y) {
  data %>% 
    plot_ly(x = ~get(x), y = ~get(y)) %>%
    add_lines(
      color = I("white"), span = I(1),
      fill = 'tozeroy', alpha = 0.2
    ) %>%
    layout(
      xaxis = list(visible = F, showgrid = F, title = ""),
      yaxis = list(visible = F, showgrid = F, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>% 
    config(displayModeBar = F)
}