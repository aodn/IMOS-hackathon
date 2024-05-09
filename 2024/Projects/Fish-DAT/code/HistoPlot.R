# 
library(tidyverse)
library(lubridate)
library(patchwork)
library(fs)
# 
# histoviz <- function(folder_path){
#   
#   ## parse all the histo files in the folder
#   file_names <-
#     list.files(folder_path, pattern = '^[0-9]+$', full.names = T) %>% 
#     map(function(x){list.files(x, pattern = "*Histos.csv", full.names = T)}) %>% 
#     unlist()
#   
#   ### Lets work with the first histo file.. this can be looped across all the files in histos
#   fn <- function(file_names){
#     
#     ## Read in the file
#     histos <- read_csv(file_names[1], show_col_types = F)
#     
#     ## Define the Time at Depth (tad) and Time at Temperature (tat) bins
#     tad_bins <-
#       histos %>% 
#       filter(HistType %in% "TADLIMITS") %>% 
#       dplyr::select(contains("Bin")) %>% 
#       pivot_longer(-1, names_to = "Bins", values_to = "bin") %>% 
#       dplyr::select(-NumBins) %>% 
#       filter(!is.na(bin)) %>% 
#       mutate(variable = "depth")
#     
#     tat_bins <-
#       histos %>% 
#       filter(HistType %in% "TATLIMITS") %>% 
#       dplyr::select(contains("Bin")) %>% 
#       pivot_longer(-1, names_to = "Bins", values_to = "bin") %>% 
#       dplyr::select(-NumBins) %>% 
#       filter(!is.na(bin)) %>% 
#       mutate(variable = "temp")
#     
#     
#     ## subset and configure depth and temp data into a data.frame
#     depth_dat <-
#       histos %>% 
#       filter(HistType %in% c("TAD")) %>% 
#       mutate(date = lubridate::dmy_hms(paste(str_split(Date, pattern = " ", simplify = T)[,2],
#                                              str_split(Date, pattern = " ", simplify = T)[,1])),
#              id = as.character(Ptt)) %>% 
#       dplyr::select(id, date, contains("Bin"), -NumBins) %>%
#       pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
#       left_join(tad_bins, by = "Bins") %>% 
#       dplyr::select(-Bins)
#     
#     
#     temp_dat <-
#       histos %>% 
#       filter(HistType %in% c("TAT")) %>% 
#       mutate(date = lubridate::dmy_hms(paste(str_split(Date, pattern = " ", simplify = T)[,2],
#                                              str_split(Date, pattern = " ", simplify = T)[,1])),
#              id = as.character(Ptt)) %>% 
#       dplyr::select(id, date, contains("Bin"), -NumBins) %>%
#       pivot_longer(-c(date, id), names_to = "Bins", values_to = "prop") %>% 
#       left_join(tat_bins, by = "Bins") %>% 
#       dplyr::select(-Bins)
#     
#     combdat <- bind_rows(depth_dat, temp_dat)
#     
#     return(combdat)
#   }
#   
#   outdat <- 
#     map(.x = file_names, .f = fn, .progress = TRUE) %>% 
#     list_rbind() %>% 
#     mutate(bin = as.factor(bin)) %>% 
#     filter(!is.na(variable))
#   
#   return(outdat)
# }
# 


histoplot <- function(tag_ids = c("47618"),
                      folder_path = "2024/Projects/Fish-DAT/data/",
                      TAD = TRUE,
                      TAT = TRUE,
                      species = TRUE){
  suppressMessages({
  plot_dat <- histoviz(folder_path) %>% 
    filter(id %in% tag_ids) 
  
  # Get meta data
  # plot_dat$species = "Cool species"
  spec_dat <- fs::dir_ls(folder_path, glob = "*metadata*.csv", ignore.case = TRUE)
  spec_dat <- read_csv(spec_dat) %>% 
    rename_with(.cols = everything(), .fn = tolower) %>% 
    mutate(id = as.character(ptt_id)) %>% 
    select(-ptt_id)
  
  plot_dat <- plot_dat %>% 
    left_join(spec_dat)
  
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
    
  
  # Time at Temperature plots
  
  if(TAT){  # do we want it plotted?
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
        
          scale_fill_viridis_d(name = "Tag ID", position = "top") +
          scale_colour_viridis_d(name = "Tag ID") +
          coord_cartesian(expand = TRUE
                          ) +
          ylab(expr(paste("Temp ("^"o","C)"))) +
          xlab("Proportion of time (%)") +
      
          theme_bw() +
      theme(legend.position = "top")
      
      
    
    if(species){ # adding species level plot if wanted
      sp_TAT <- ggplot(spec_dat %>% filter(variable == "temp"),
                       mapping = aes(y = bin,
                                     weight = prop_mean,
                                     fill = species,
                                     group = species)) +
        geom_density(alpha = 0.5) +
        theme_bw() +
        scale_fill_viridis_d() +
        coord_cartesian(expand = TRUE) +
        ylab("") +
        xlab("Proportion of time (%)") +
        theme(legend.position = "top")
        
        g_TAT <- g_TAT + sp_TAT
    
      }
    
  }
  
  # Time at depth plots
  if(TAD){ # do we want it plotted?
      # Simple bar plots of TAD
     g_TAD <- ggplot(plot_dat %>% filter(variable == "depth"),
               mapping = aes(y = bin,
                             x = prop_mean,
                             group = id,
                             fill = id
               )) +
          geom_bar(stat = "identity", alpha = 0.5) +
          geom_errorbar(aes(xmin = prop_mean - prop_sd,
                            xmax = prop_mean + prop_sd),
                        colour = "grey50",
                        width = 0.3) +
          
          scale_fill_viridis_d(name = "Tag ID") +
          scale_colour_viridis_d(name = "Tag ID") +
          coord_cartesian(expand = TRUE) +
          scale_y_discrete(limits=rev) +
          ylab("Depth (m)") +
          xlab("Proportion of time (%)") +
          theme_bw()  +
          theme(legend.position = "top")
     
     if(species){ # adding species level plot if wanted
       sp_TAD <- ggplot(spec_dat %>% filter(variable == "depth"),
                        mapping = aes(y = bin,
                                      weight = prop_mean,
                                      fill = species,
                                      group = species)) +
         geom_density(alpha = 0.5) +
         theme_bw() +
         scale_fill_viridis_d() +
         coord_cartesian(expand = TRUE) +
         scale_y_discrete(limits=rev) +
         # ylab(expr(paste("Temp ("^"o","C)"))) +
         ylab("") +
         xlab("Proportion of time (%)") +
         theme(legend.position = "top")
       
       g_TAD <- g_TAD + sp_TAD
       
     }
  }
  
  # Deciding which plots to return
  if(TAD & TAT){
    return(g_TAD/g_TAT)
  }
  if(TAD & !TAT){
    return(g_TAD)
  }
  if(!TAD & TAT){
    return(g_TAT)
  }
  })
}
