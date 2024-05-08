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
  
  histos <- read_csv(file_names[i])
  
  tat_bins <-
    histos %>% 
    filter(HistType %in% "TATLIMITS") %>% 
    dplyr::select(starts_with("Bin")) %>% 
    c() %>% unlist() %>% unname() %>% na.omit() %>% 
    as.vector()
  
  tad_bins <-
    histos %>% 
    filter(HistType %in% "TADLIMITS") %>% 
    dplyr::select(starts_with("Bin")) %>% 
    c() %>% unlist() %>% unname() %>% na.omit() %>% 
    as.vector()
  
  
  # depthdat <-
    histos %>% 
    filter(HistType %in% c("TAD")) %>% 
    mutate(date = lubridate::dmy_hms(paste(str_split(date, pattern = " ", simplify = T)[,2],
                                           str_split(date, pattern = " ", simplify = T)[,1])),
           id = as.character(ptt)) %>% 
    dplyr::select(-c(deploy_id, ptt, count, depth_sensor, source, instr, hist_type, 
                     time_offset, bad_therm, location_quality, latitude, longitude,
                     num_bins, sum)) %>% 
    dplyr::select(-c(paste0("bin", 13:72))) %>% 
    pivot_longer(-c(date, id), names_to = "depth", values_to = "prop") %>% 
    mutate(depth = factor(case_when(depth %in% "bin1" ~ 10,
                                    depth %in% "bin2" ~ 20,
                                    depth %in% "bin3" ~ 40,
                                    depth %in% "bin4" ~ 60,
                                    depth %in% "bin5" ~ 80,
                                    depth %in% "bin6" ~ 100,
                                    depth %in% "bin7" ~ 150,
                                    depth %in% "bin8" ~ 200,
                                    depth %in% "bin9" ~ 400,
                                    depth %in% "bin10" ~ 800,
                                    depth %in% "bin11" ~ 1000,
                                    depth %in% "bin12" ~ 2000),
                          levels = c(0, 10,20,40,60,80,100,150,200,400, 800, 1000, 2000))) %>% 
    ## remove bins with 100% values (likely shed/released tag at surface)
    mutate(prop = if_else(prop %in% 100, 0, prop)) %>%
    filter(!is.na(prop)) %>%
    filter(prop > 0) %>%
    left_join(traj_dat %>% dplyr::select(-species)) %>% 
    left_join(traj_dat %>% distinct(id, species)) %>% 
    fill(Zone, .direction = "up") %>% 
    fill(protection, .direction = "up") %>% 
    filter(!id %in% 53738) 
  
  
}




