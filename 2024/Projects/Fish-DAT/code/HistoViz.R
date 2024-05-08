## Function to visualise TAT and TAD histograms

library(tidyverse)
library(lubridate)


## set how data are input into the function 
## (it only requires the path to the folder where -Histo files are found)
folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"

file_names <-
  list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
  map(function(x){list.files(x, pattern = "*Histos.csv", full.names = T)}) %>% 
  unlist()

### Lets work with the first histo file.. this can be looped across all the files in histos
for(i in length(file_names)){
  
  ## Read in the file
  histos <- read_csv(file_names[i])
  
  ## Define the Time at Depth (tad) and Time at Temperature (tat) bins
  tad_bins <-
    histos %>% 
    filter(HistType %in% "TADLIMITS") %>% 
    dplyr::select(contains("Bin")) %>% 
    pivot_longer(-1, names_to = "Bins", values_to = "depth") %>% 
    dplyr::select(-NumBins) %>% 
    filter(!is.na(depth))
  
  tat_bins <-
    histos %>% 
    filter(HistType %in% "TATLIMITS") %>% 
    dplyr::select(contains("Bin")) %>% 
    pivot_longer(-1, names_to = "Bins", values_to = "temp") %>% 
    dplyr::select(-NumBins) %>% 
    filter(!is.na(temp))
  
  
  ## subset and 
  depth_dat <-
    histos %>% 
    filter(HistType %in% c("TAD")) %>% 
    mutate(date = lubridate::dmy_hms(paste(str_split(Date, pattern = " ", simplify = T)[,2],
                                           str_split(Date, pattern = " ", simplify = T)[,1])),
           id = as.character(Ptt)) %>% 
    dplyr::select(id, date, contains("Bin"), -NumBins) %>%
    pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
    left_join(tad_bins)
  
  temp_dat <-
    histos %>% 
    filter(HistType %in% c("TAD")) %>% 
    mutate(date = lubridate::dmy_hms(paste(str_split(Date, pattern = " ", simplify = T)[,2],
                                           str_split(Date, pattern = " ", simplify = T)[,1])),
           id = as.character(Ptt)) %>% 
    dplyr::select(id, date, contains("Bin"), -NumBins) %>%
    pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
    left_join(tad_bins)
  
  
}




