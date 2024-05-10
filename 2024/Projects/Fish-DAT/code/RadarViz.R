# visulise movement metrics
# by Fabrice Jaine
# March 2024

rm(list = ls()) # to clear workspace

# Set working directory
# setwd("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/") # set manually to your own working directory

base_dir <- "2024/Projects/Fish-DAT/"

# Load the metadata
library(data.table)
met <- read.csv(paste0(base_dir, "data/HackathonMetadata.csv"), header = TRUE)
head(met)

#--------------------------------

# Loading all datasets at once

all_files <- list.files(paste0(base_dir, "data/outputs/"),
  pattern = "_summaries", recursive = T,
  full.names = T
)

# Looping through each tag and computing and saving summary statistics

for (i in 1:length(all_files)) {
  # read each file
  dat <- read.csv(all_files[i], header = T)
  head(dat)
  ptt_id <- unique(dat$Ptt)
  print(ptt_id)
  # compute daily min/mean/max measurements
  library(dplyr)
  daily_measurements <- dat %>%
    mutate(mean_daily_distance = if_else(is.na(DistTravelled), 0, DistTravelled)) %>%
    filter(!is.na(MinTemp)) %>%
    mutate(min_temp = min(MinTemp)) %>%
    filter(!is.na(MeanTempPDT)) %>%
    mutate(mean_temp = mean(MeanTempPDT)) %>%
    filter(!is.na(MeanDepthPDT)) %>%
    mutate(mean_depth = mean(MeanDepthPDT))
  daily_measurements
  # compute overall summary metrics
  summary_metrics <- daily_measurements %>%
    reframe(
      Ptt = unique(ptt_id),
      DaysAtLiberty = unique(max(day.at.liberty)),
      mean_DailyDist = unique(mean(mean_daily_distance)),
      total_Dist = unique(max(DistfromStart)),
      mean_DistToShelf = unique(mean(DistToShelf)),
      mean_DistToShore = unique(mean(DistToShore)),
      min_Temp = unique(min_temp), mean_Temp = unique(mean_temp), max_Temp = unique(max(MaxTemp)),
      min_Depth = unique(min(MinDepth)), mean_Depth = unique(mean_depth), max_Depth = unique(max(MaxDepth)),
      max_Bathy = unique(max(bathy_depth))
    )
  # save output in temp df
  if (i %in% 1) {
    out_dat <- summary_metrics
  } else {
    out_dat <-
      out_dat %>%
      bind_rows(summary_metrics)
  }
}
# out_dat
met <- left_join(met, out_dat, by = c("Ptt"))
print(met)
# write.csv(met, "data/HackathonMetadata.csv", colnames=F)

# Setting max scales based on all available values

library(fmsb)
# To use the fmsb package with several individuals, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
# summary_metrics <- rbind(rep(800,10) , rep(0,10) , summary_metrics)

final_dat <- out_dat %>% select(-Ptt)

x <- data.frame(
  DaysAtLiberty = max(out_dat$DaysAtLiberty) + 10,
  mean_DailyDist = max(out_dat$mean_DailyDist) + 10,
  total_Dist = max(out_dat$total_Dist) + 100,
  mean_DistToShelf = max(out_dat$mean_DistToShelf) + 10,
  mean_DistToShore = max(out_dat$mean_DistToShore) + 10,
  min_Temp = max(out_dat$min_Temp) + 4,
  mean_Temp = max(out_dat$mean_Temp) + 4,
  max_Temp = max(out_dat$max_Temp) + 4,
  min_Depth = max(out_dat$min_Depth) + 10,
  mean_Depth = max(out_dat$mean_Depth) + 10,
  max_Depth = max(out_dat$max_Depth) + 10,
  max_Bathy = max(out_dat$max_Bathy) + 20, stringsAsFactors = FALSE
)

y <- data.frame(
  DaysAtLiberty = 0,
  mean_DailyDist = 0,
  total_Dist = 0,
  mean_DistToShelf = 0,
  mean_DistToShore = 0,
  min_Temp = 0,
  mean_Temp = 0,
  max_Temp = 0,
  min_Depth = 0,
  mean_Depth = 0,
  max_Depth = 0,
  max_Bathy = 0, stringsAsFactors = FALSE
)

summary_metrics <- rbind(x, y, final_dat)
summary_metrics

#-----------------------------------
# THE PLOT FUNCTION SHOULD BE A SEPARATE SCRIPT CALLED AFTER THE ABOVE SCRIPT HAS RUN
# Delete the loop when ready to split these scripts

for (k in 1:nrow(summary_metrics)) {
  temp_metrics <- summary_metrics[k, ]

  radarchart(temp_metrics,
    axistype = 1,

    # custom polygon
    pcol = rgb(0.2, 0.5, 0.5, 0.9), pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 4,

    # custom the grid
    cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.6,

    # custom labels
    vlcex = 0.9
  )
}
