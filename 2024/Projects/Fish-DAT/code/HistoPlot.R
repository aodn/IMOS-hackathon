
library(tidyverse)
library(lubridate)
library(patchwork)

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

folder_path <- "2024/Projects/Fish-DAT/data/"

plot_dat <- histoviz(folder_path)


head(plot_dat)


histoplot <- function(tag_ids = c("47618", "47622", "227150", "227151"),
                      folder_path = "2024/Projects/Fish-DAT/data/",
                      TAD = TRUE,
                      TAT = TRUE,
                      species = TRUE,
                      OO_Shelf = TRUE){
  
  plot_dat <- histoviz(folder_path) %>% 
    filter(id %in% tag_ids) #%>% 
    # mutate(bin = as.numeric(as.character(bin)))
  
  plot_dat$species = "Cool species"
  
  if(species){  
    spec_dat <- plot_dat %>% 
      drop_na(prop, bin) %>% 
      group_by(species, bin, variable) %>% 
      summarise(prop_mean = mean(prop, na.rm = TRUE),
                prop_sd = sd(prop, na.rm = TRUE),
                id = id[1]) %>% 
      ungroup() %>% 
      arrange(bin)
  }
  
  plot_dat <- plot_dat %>% 
    drop_na(prop, bin) %>% 
    group_by(id, bin, variable) %>% 
    summarise(prop_mean = mean(prop, na.rm = TRUE),
              prop_sd = sd(prop, na.rm = TRUE)) %>% 
    ungroup()
    
  
  # Simple bar plots of TAT
  if(TAT){  
    g_TAT <- ggplot(plot_dat %>% filter(variable == "temp"),
               mapping = aes(y = bin,
                             x = prop_mean,
                             group = id,
                             # colour = id,
                             fill = id,
                             # weight = prop_mean
                             )
               ) +
          geom_bar(stat = "identity", alpha = 0.5) +
          geom_errorbar(aes(xmin = prop_mean - prop_sd,
                            xmax = prop_mean + prop_sd),
                        colour = "grey50",
                        width = 0.3) +
        
          scale_fill_viridis_d(name = "Tag ID") +
          scale_colour_viridis_d(name = "Tag ID") +
          coord_cartesian(expand = TRUE,
                          # ylim = range(plot_dat %>% 
                          #                filter(variable == "temp") %>% 
                          #                pull(bin)
                          #              )
                          ) +
          
          # geom_density(alpha = 0.2) +
          ylab(expr(paste("Temp ("^"o","C)"))) +
          xlab("Proportion of time (%)") +
          theme_bw() 
    g_TAT
  }
  if(TAD){
      # Simple bar plots of TAD
     g_TAD <- ggplot(plot_dat %>% filter(variable == "depth"),
               mapping = aes(y = bin,
                             x = prop_mean,
                             group = id,
                             # colour = id,
                             fill = id,
                             # weight = prop_mean
               )) +
          geom_bar(stat = "identity", alpha = 0.5) +
          geom_errorbar(aes(xmin = prop_mean - prop_sd,
                            xmax = prop_mean + prop_sd),
                        colour = "grey50",
                        width = 0.3) +
          
          scale_fill_viridis_d(name = "Tag ID") +
          scale_colour_viridis_d(name = "Tag ID") +
          coord_cartesian(expand = FALSE) +
          # coord_cartesian(expand = FALSE) +
          # geom_density(alpha = 0.2) +
          # scale_y_reverse() +
          ylab("Depth (m)") +
          xlab("Proportion of time (%)") +
          theme_bw() 
     # if(species){
     #   g_TAD + geom_path(data = spec_dat %>% filter(variable == "depth"),
     #                     mapping = aes(y = bin,
     #                                   # group = id,
     #                                   # colour = id,
     #                                   # fill = id,
     #                                   x = prop_mean)
     #   )
     # }
  }
  
  
}