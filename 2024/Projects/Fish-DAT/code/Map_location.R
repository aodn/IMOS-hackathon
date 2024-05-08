#Jiaying Guo

#date: 2024/5/8

# this script plot animal track on a map with moving time
# the latitude and logitude lable may need to be modified
# still working on adding more environmental metrics in

library(tidyverse)
library(sf)
library(mapview)

# Overview of the data ----------------------------------------------------

list.files(path = "2024/Projects/Fish-DAT/data/", 
           pattern = "daily-positions.csv",
           recursive = T,
           full.names = T)

# Identifying datasets ----------------------------------------------------
Position_files <- list.files(path = "2024/Projects/Fish-DAT/data/",
                           pattern = "daily-positions.csv", recursive = T,
                           full.names = T)

## Data to plot land
land <- map_data("world") 


## Using ggplot to visualise the data

sum_position <- data.frame()

for (n in seq_along(Position_files)) {
  df_position <- read_csv(Position_files[n])
  sum_position <- rbind(df_position, sum_position)
}


#use ggplot to plot the animal locations
sum_position$Ptt <- as.character(sum_position$Ptt)

  sum_position %>%
    ggplot(aes(x = Longitude, y = Latitude, col = Ptt)) +
    geom_polygon(data = land, aes(x = long, y = lat, group = group), 
                 fill = "grey", inherit.aes = F) +
    geom_point() +
    #coord_equal(xlim = c(142, 160), ylim = c(-43, -9)) +
    labs(x = NULL, y = NULL, col = "Date") +
    theme_bw()
  
## Lets use the `sf` and `mapview` to interactively plot these data
  sum_position %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
    mapview(zcol = "Ptt")
  

# Plot species track ---------------------------------------------------
  plot_track <- function(position_file) {
    
   for (Species_index in unique( position_file$Ptt)) {
     df_plot <- filter(position_file, Ptt == Species_index)
     
     p1<- ggplot(data = df_plot, aes(x = Longitude, y = Latitude, color = Date)) +
       geom_polygon(data = land, aes(x = long, y = lat, group = group), 
                    fill = "grey", inherit.aes = F) +
       geom_point() +
       coord_equal(
                   xlim = c(min(df_plot$Longitude), max(df_plot$Longitude)), 
                   ylim = c(min(df_plot$Latitude), max(df_plot$Latitude))) +
       #scale_color_viridis_c() +
       #scale_fill_viridis_c(option = "B") +
       labs(x = "Longitude", y = "Latitude", col = "Date", 
            title = paste(df_plot$Species, df_plot$Ptt, "moving track")) +
       theme_bw()
    print(p1)
    
   }
    
  }
  
  #plot animal track
  animal_info <- read.csv("2024/Projects/Fish-DAT/data/HackathonMetadata.csv")
  
  names(animal_info)[names(animal_info) == 'PTT_ID'] <- 'Ptt'
  
  animal_info$Ptt <- as.character(animal_info$Ptt)
  
  sum_position <- right_join(animal_info[1:2], sum_position, by = c("Ptt"))
  
  plot_track(sum_position)
  



