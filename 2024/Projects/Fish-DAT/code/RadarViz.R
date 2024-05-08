# visulise movement metrics
# by Fabrice Jaine
# March 2024

rm(list=ls()) # to clear workspace

# Set working directory
setwd("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/") # set manually to your own working directory

# Load the data
library(data.table)
dat<-read.csv("outputs/47618_summaries.csv",header=TRUE); head(dat)
ptt_id = unique(dat$Ptt)

library(data.table)
met<-read.csv("data/HackathonMetadata.csv",header=TRUE); head(met)

#--------------------------------
# Compute overall min/mean/max summary metrics for each fish
library(dplyr)

daily_measurements <- dat %>%
  group_by(Date) %>%
  summarise(days_at_liberty = max(day.at.liberty),
            mean_daily_distance = mean(DistTravelled),
            distance_total = max(DistfromStart),
            mean_dist_to_shelf = mean(DistToShelf),
            mean_distance_to_shore = mean(DistToShore),
            min_temp = min(MinTempFromSeries), mean_temp = mean(MeanTempFromSeries), max_temp = max(MaxTempFromSeries),
            min_depth = min(MinDepthFromSeries), mean_depth = mean(MeanDepthFromSeries), max_depth = max(MaxDepthFromSeries),
            max_bathy = max(bathy_depth)
            )
daily_measurements

library(dplyr)
daily_measurements <- daily_measurements %>%
  mutate(mean_daily_distance = if_else(is.na(mean_daily_distance), 0, mean_daily_distance)) %>%
  filter(!is.na(mean_temp)) %>% mutate(mean_temp = mean(mean_temp)) %>%
  filter(!is.na(mean_depth)) %>% mutate(mean_depth = mean(mean_depth))


summary_metrics <- daily_measurements %>%
  summarise(days_at_liberty = max(days_at_liberty),
            mean_daily_distance = mean(mean_daily_distance),
            distance_total = max(distance_total),
            mean_dist_to_shelf = mean(mean_dist_to_shelf),
            mean_distance_to_shore = mean(mean_distance_to_shore),
            min_temp = min(min_temp), mean_temp = mean(mean_temp), max_temp = max(max_temp),
            min_depth = min(min_depth), mean_depth = mean(mean_depth), max_depth = max(max_depth),
            max_bathy = -max(max_bathy)
  )
summary_metrics

#Convert the data to dataframe
summary_metrics <- as.data.frame(summary_metrics); head(summary_metrics)

drop.cols <- c('distance_total', 'max_bathy')
library(dplyr)
summary_metrics <- summary_metrics %>% select(-drop.cols)


#--------------------------------
# Library
library(fmsb)

# To use the fmsb package with several individuals, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
#summary_metrics <- rbind(rep(800,10) , rep(0,10) , summary_metrics)

x <- data.frame(days_at_liberty=365, 
                              mean_daily_distance = 80,
                              mean_dist_to_shelf = 500,
                              mean_distance_to_shore = 500,
                              min_temp = 30,
                              mean_temp = 30,
                              max_temp = 30,
                              min_depth = 100,
                              mean_depth = 100,
                              max_depth = 700, stringsAsFactors=FALSE)

y <- data.frame(days_at_liberty=0, 
                mean_daily_distance = 0,
                mean_dist_to_shelf = 0,
                mean_distance_to_shore = 0,
                min_temp = 0,
                mean_temp = 0,
                max_temp = 0,
                min_depth = 0,
                mean_depth = 0,
                max_depth = 0, stringsAsFactors=FALSE)

summary_metrics <- rbind(x, y, summary_metrics)
summary_metrics

# Custom the radarChart !
radarchart( summary_metrics  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.6,
            
            #custom labels
            vlcex=0.9 
)

# #remotes::install_github("ricardo-bion/ggradar")
# library(ggradar)
# summary_metrics %>% ggradar(
#   font.radar = "roboto",
#   grid.label.size = 13,  # Affects the grid annotations (0%, 50%, etc.)
#   axis.label.size = 8.5, # Afftects the names of the variables
#   group.point.size = 3   # Simply the size of the point 
# )
