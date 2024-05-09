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



histoplot <- function(tag_ids = c("47618"),
                      folder_path = "2024/Projects/Fish-DAT/data/",
                      TAD = TRUE,
                      TAT = TRUE,
                      species = TRUE){
  suppressMessages({
  plot_dat <- histoviz(folder_path)
  
  # Get meta data
  # plot_dat$species = "Cool species"
  spec_dat <- fs::dir_ls(folder_path, glob = "*metadata*.csv", ignore.case = TRUE)
  spec_dat <- read_csv(spec_dat) %>% 
    rename_with(.cols = everything(), .fn = tolower) %>% 
    mutate(id = as.character(ptt_id)) %>% 
    select(id, species)
  
  
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
    filter(id %in% tag_ids)
  
  spec_dat <- spec_dat %>% 
    filter(species %in% unique(plot_dat$species))
  
  plot_dat <- plot_dat %>% 
    drop_na(prop, bin) %>% 
    group_by(id, bin, variable) %>% 
    summarise(prop_mean = mean(prop, na.rm = TRUE),
              prop_sd = sd(prop, na.rm = TRUE)) %>% 
    ungroup()
    
  
  # Time at Temperature plots
  
  if(TAT){  # do we want it plotted?
    
    if(species){
      TAT_dat <- plot_dat %>% 
        filter(variable == "temp")
      
      spec_TAT <- spec_dat %>% 
        filter(variable == "temp") %>% 
        select(-id) %>% 
        rename(id = species)
      
    }
    
    
    g_TAT <- ggplot(TAT_dat,
               mapping = aes(y = bin,
                             x = prop_mean,
                             group = id,
                             fill = id)
               ) +
              geom_bar(stat = "identity", 
                       alpha = 0.5
                       ) +
              geom_errorbar(aes(xmin = prop_mean - prop_sd,
                                xmax = prop_mean + prop_sd),
                            colour = "grey50",
                            width = 0.3) +
            
              scale_fill_viridis_d(name = "Tag ID", position = "top") +
              
              coord_cartesian(expand = TRUE
                              ) +
              ylab(expr(paste("Temp ("^"o","C)"))) +
              xlab("Proportion of time (%)") +
          
              theme_bw() +
              theme(legend.position = "top")
      
      if(species){
        g_TAT <- g_TAT + geom_density(data = spec_TAT,
                                      mapping = aes(y = bin,
                                                    x = prop_mean), stat = "identity", 
                                      orientation = "y", 
                                      fill = NA,
                                      colour = "goldenrod2",
                                      lty = "dashed",
                                      lwd = 1)
      }
      
    # g_TAT  
    
    
  }
  
  # Time at depth plots
  if(TAD){ # do we want it plotted?
      # Simple bar plots of TAD
    if(species){
      TAD_dat <- plot_dat %>% 
        filter(variable == "depth")
      
      spec_TAD <- spec_dat %>% 
        filter(variable == "depth") %>% 
        select(-id) %>% 
        rename(id = species)
      
    }
          g_TAD <- ggplot(TAD_dat,
                          mapping = aes(y = bin,
                                        x = prop_mean,
                                        group = id,
                                        fill = id)) +
            geom_bar(stat = "identity", 
                     alpha = 0.5) +
            geom_errorbar(aes(xmin = prop_mean - prop_sd,
                              xmax = prop_mean + prop_sd),
                          colour = "grey50",
                          width = 0.3) +
            
            scale_fill_viridis_d(name = "Tag ID", position = "top") +
            coord_cartesian(expand = TRUE) +
            scale_y_discrete(limits=rev) +
            ylab("Depth (m)") +
            xlab("Proportion of time (%)") +
            theme_bw()  +
            theme(legend.position = "top")
            if(species){
              g_TAD <- g_TAD + geom_density(data = spec_TAD,
                                            mapping = aes(y = bin,
                                                          x = prop_mean), stat = "identity", 
                                            orientation = "y", 
                                            fill = NA,
                                            colour = "goldenrod2",
                                            lty = "dashed",
                                            lwd = 1)}
          }
  
  # Deciding which plots to return
  if(TAD & TAT){
    return(g_TAD + g_TAT)
  }
  if(TAD & !TAT){
    return(g_TAD)
  }
  if(!TAD & TAT){
    return(g_TAT)
  }
 
  })
}
