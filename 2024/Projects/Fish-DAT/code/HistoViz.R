## Function to visualise TAT and TAD histograms

library(tidyverse)
library(lubridate)


## set how data are input into the function 
## (it only requires the path to the folder where -Histo files are found)

histoviz <- function(folder_path){
  
  ## parse all the histo files in the folder
  file_names <-
    list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
    map(function(x){list.files(x, pattern = "*Histos.csv", full.names = T)}) %>% 
    unlist()
  
  ### Lets work with the first histo file.. this can be looped across all the files in histos
  fn <- function(file_names){
    
    ## Read in the file
    histos <- read_csv(file_names[1], show_col_types = F)
    
    ## Define the Time at Depth (tad) and Time at Temperature (tat) bins
    tad_bins <-
      histos %>% 
      filter(HistType %in% "TADLIMITS") %>% 
      dplyr::select(contains("Bin")) %>% 
      pivot_longer(-1, names_to = "Bins", values_to = "bin") %>% 
      dplyr::select(-NumBins) %>% 
      filter(!is.na(bin)) %>% 
      mutate(variable = "depth")
    
    tat_bins <-
      histos %>% 
      filter(HistType %in% "TATLIMITS") %>% 
      dplyr::select(contains("Bin")) %>% 
      pivot_longer(-1, names_to = "Bins", values_to = "bin") %>% 
      dplyr::select(-NumBins) %>% 
      filter(!is.na(bin)) %>% 
      mutate(variable = "temp")
    
    
    ## subset and configure depth and temp data into a data.frame
    depth_dat <-
      histos %>% 
      filter(HistType %in% c("TAD")) %>% 
      mutate(date = lubridate::dmy_hms(paste(str_split(Date, pattern = " ", simplify = T)[,2],
                                             str_split(Date, pattern = " ", simplify = T)[,1])),
             id = as.character(Ptt)) %>% 
      dplyr::select(id, date, contains("Bin"), -NumBins) %>%
      pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
      left_join(tad_bins, by = "Bins") %>% 
      dplyr::select(-Bins)
      
    
    temp_dat <-
      histos %>% 
      filter(HistType %in% c("TAT")) %>% 
      mutate(date = lubridate::dmy_hms(paste(str_split(Date, pattern = " ", simplify = T)[,2],
                                             str_split(Date, pattern = " ", simplify = T)[,1])),
             id = as.character(Ptt)) %>% 
      dplyr::select(id, date, contains("Bin"), -NumBins) %>%
      pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
      left_join(tat_bins, by = "Bins") %>% 
      dplyr::select(-Bins)
    
    combdat <- bind_rows(depth_dat, temp_dat)
    
    return(combdat)
  }
  
  outdat <- 
    map(.x = file_names, .f = fn, .progress = TRUE) %>% 
    list_rbind() %>% 
    mutate(bin = as.factor(bin)) %>% 
    filter(!is.na(variable))
  
  return(outdat)
}


### ----------------------------------------------------------------------------------------------- ##
## test the function

folder_path <- "~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data"

h <- histoviz(folder_path)

## summarise variables to larger time steps (daily) so it plots nicer
h_summary <-
  h %>% 
  mutate(date = date(date)) %>% 
  group_by(id, date, bin, variable) %>% 
  summarise(prop = mean(prop))

h_summary %>% 
  filter(variable %in% "depth") %>% 
  ggplot(aes(x = date, y = fct_rev(bin), fill = prop, id = variable)) +
  geom_tile() +
  facet_wrap(~id, ncol = 1, scales = "free") +
  scale_fill_viridis_c(direction = -1, na.value = NA, limits = c(0,100)) +
  labs(x = "Date", y = "Depth (m)", fill = "Proportion\nof time") +
  theme_bw()

h_summary %>% 
  filter(variable %in% "temp") %>% 
  ggplot(aes(x = date, y = bin, fill = prop, id = variable)) +
  geom_tile() +
  facet_wrap(~id, ncol = 1, scales = "free") +
  scale_fill_viridis_c(option = "A", direction = -1, na.value = NA, limits = c(0,100)) +
  labs(x = "Date", y = "Temperature (˚C)", fill = "Proportion\nof time") +
  theme_bw()





