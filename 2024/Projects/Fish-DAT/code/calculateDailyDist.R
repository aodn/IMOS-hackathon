# Calculate Daily Distance Travelled
# Author: Dahlia Foo


# Setup
library(tidyverse)
library(lubridate)
library(traipse)

# Set to project root directory
setwd("~/Documents/Projects/dev/IMOS-hackathon-1")

# Get for one animal
basedir <- "2024/Projects/Fish-DAT/data/47622"

# Load the data
filename <- dir(file.path(basedir), pattern = "daily-positions.csv", full.names = TRUE)

d <- read_csv(filename); d; glimpse(d);

# Calculate distance travelled between each pair of consecutive points
d <- d %>% 
  mutate(DistTravelled = track_distance_to(Longitude, Latitude, lag(Longitude), lag(Latitude)) / 1000)

