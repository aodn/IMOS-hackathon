## Function to plot leaflet map with GPE3 positions and error

## Load the other GPE3_error
source("https://raw.githubusercontent.com/aodn/IMOS-hackathon/fishdat/2024/Projects/Fish-DAT/code/GPE3_error_extraction.R")

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras2)
library(terra)
library(tidync)


# Set up a function all we provide is the location of the folder and metadata and it plots the movement with the errors

## data input required
# folder_name: location of folder with all the MiniPAT files and GPE3 outputs
# metadata: a metadata data.frame that 
# prop : a vector of probabilities for the error 

folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"
metadata <- list.files(folder_path, pattern = "*Metadata*.csv") %>% read_csv()
prob <- c(0.99, 0.95, 0.5)
i = 1

leaflet_path_error <-
  function(folder_name, metadata, prop){
    
    ## parse all the histo files in the folder
    csv_file <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.csv", full.names = T)}) %>% 
      unlist()
    
    nc_files <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.nc", full.names = T)}) %>% 
      unlist()
    
    kmz_files <-
      list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
      map(function(x){list.files(x, pattern = "*GPE3.kmz", full.names = T)}) %>% 
      unlist()
    
    
    ## Lets convert the GPE3 csv_file data into a spatial object with positions per month
    
    
    
    
  }


