library(sf)

# Create function to load individual track summaries output data and combine monthly colour palette to it 
load_track_data <- function(tag_id, base_folder, monthly_colour_palette) {
  files <- list.files(base_folder, pattern = paste0(tag_id, "_summaries.csv"), full.names = TRUE, recursive = TRUE)
  
  
  
  if (length(files) > 0) {
    track <- read_csv(files[1], show_col_types = FALSE)
    

    # Assign colour depending on month
    track <- inner_join(track, monthly_colour_palette, by = "month")
    track <- as.data.frame(track)
    
    track_sf <- track %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
    
    return(
      list(
        track = track,
        track_sf = track_sf
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