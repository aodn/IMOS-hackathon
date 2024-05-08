# Calculate:
# Distance from shelf edge
# Distance from shore
# Distance from nearest estuary
# by Fabrice Jaine
# Last updated: May 2024

# Initialise
rm(list=ls(all.names = TRUE)) # to clear workspace
setwd("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data/227151") # set up your respective directory

#---- import data ----
dat<-read.csv("227151_daily-positions.csv",header=TRUE); head(dat)
dat$Date <- as.POSIXct(as.character(dat$Date), format="%Y-%m-%d", tz="Australia/Sydney", origin = "1970-01-01")
head(dat)

#---- define ROI ----
# Total ROI for bathymetry
LatMin = ceiling(max(dat$Latitude)) + 2
LatMax = floor(min(dat$Latitude)) - 2
LonMin = floor(min(dat$Longitude)) - 2
LonMax = ceiling(max(dat$Longitude)) + 2
ROI = c(LonMin, LonMax, LatMin, LatMax)

#---- load bathymetry data ----
library(marmap)
bat <- getNOAA.bathy(LonMin, LonMax, LatMin, LatMax, res = 1, keep=TRUE) 
summary(bat)

library(remora)

#---- Compute distances: ----
x <- as.numeric(dat$Longitude) 
y <- as.numeric(dat$Latitude)

### Distance to nearest shelf edge (200m isobath):
d_shelf <- dist2isobath(bat, x, y, isobath = -200) # distances between each sighting and nearest point on shelf edge
d_shelf$distance <- d_shelf$distance/1000 # in km instead of metres
DistToShelf <- d_shelf$distance

### Distance to nearest coastline (0m isobath):
d_shore <- dist2isobath(bat, x, y, isobath = 0) # distances between each sighting and nearest point on coastline
d_shore$distance <- d_shore$distance/1000 # in km instead of metres
DistToShore <- d_shore$distance

dat <- cbind(dat, DistToShelf, DistToShore); head(dat) # merge outputs with main data frame

#---- Save data ----
write.csv(dat,file="227151_daily-positions.csv",dec=".",col.names=TRUE) # save as .csv

