#' Pre-processing of satellite tracks

#-----------------------------

# 1. Summarise GPE3 output data - local time, daily location

#-----------------------------

rm(list=ls()) # to clear workspace

# Set working directory
setwd("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data/") # set manually to your own working directory

# Load the data
library(data.table)
dat <- fread("227151/227151-15-GPE3.csv"); head(dat); dat <- as.data.frame(dat)
ptt_id = unique(dat$Ptt)

# lookup local timezone
timeZ <- tz_lookup_coords(lat = dat$`Most Likely Latitude`, lon = dat$`Most Likely Longitude`, method = "accurate"); 
timeZ <- timeZ[1]

# Do time conversion and extract Date and Time as separate columns
dat$datetime.UTC <- as.POSIXct(as.character(dat$Date), format="%d-%b-%Y %H:%M:%S", tz="UTC", origin = "1970-01-01"); 
dat$datetime.LOCAL <- as.POSIXlt(dat$datetime.UTC, tz=timeZ); 
dat$Time <- format(as.POSIXct(dat$datetime.LOCAL,format="%Y:%m:%d %H:%M:%S"),"%H:%M:%S")
dat$Date <- format(as.POSIXct(dat$datetime.LOCAL,format="%Y:%m:%d %H:%M:%S"),"%Y-%m-%d")
head(dat)

# Calculate average daily position based on 4 raw/estimated locations per day
library(dplyr)
track <- dat %>%
  group_by(Date) %>%
  summarise(Ptt = unique(Ptt), Date = unique(Date), Latitude = mean(`Most Likely Latitude`), Longitude = mean(`Most Likely Longitude`))
track



# Deleting the first and last location of track data to be replaced with more accurate locations 

#Replacing first and last location with know deployment and pop-off location
#load the masterfile with the known release (deployment) and detachments (pop-off) locations
rawdata <- fread("HackathonMetadata.csv")
#Replacing deployment longitude and latitude
x = rawdata[which(rawdata$PTT_ID == ptt_id),]; head(x)
track$Latitude[1] <- x$Deployment_Lat
track$Longitude[1] <- x$Deployment_Lon
#Replacing detachment longitude and latitude (need to check what row of each data you are transferring over)
track$Latitude[nrow(track)] <- x$Detachment_Lat
track$Longitude[nrow(track)] <- x$Detachment_Lon[1] #not sure why this line of code isn't working

#####

library(lubridate)
track <- track %>% mutate(day.at.liberty = 1:n()); head(track)
track$month <- format(as.POSIXct(track$Date,format="%Y-%m-%d"),"%b"); head(track)

#Save sample data
# I only set working directory again because I couldn't figure out how to make write.csv go to the correct folder
setwd("~/Documents/GitHub/IMOS-hackathon/2024/Projects/Fish-DAT/data/")

write.csv(track,paste0(unique(track$Ptt),"/",unique(track$Ptt),"","_daily-positions.csv"), row.names=F)
