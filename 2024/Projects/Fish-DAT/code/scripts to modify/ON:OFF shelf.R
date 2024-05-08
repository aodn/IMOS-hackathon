# Script to determine whether X,Y data from animal tracks are located ON or OFF the continental shelf
# by Fabrice Jaine
# May 2020

rm(list=ls()) # to clear workspace
setwd("~/Dropbox/Fabrice's and Francisca's manta ray diving project/") # Fabrice

WGS <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # WGA good for large-scale studies
GDA <- CRS("+init=epsg:28356") # The equivalent EPSG code for WGS 84 is 28356. 

#---- load data ----
dat <- read.csv("~/Dropbox/Fabrice's and Francisca's manta ray diving project/DATA/All-mantas-2014-Track&Loc_cropped.csv", header=T); head(dat) # Fabrice
str(dat)
dat$datetime.LOCAL <- as.POSIXct(as.character(dat$datetime.LOCAL), format="%Y-%m-%d", tz="Australia/Brisbane", origin = "1970-01-01")

MantaTracks <- data.frame(matrix("", ncol = 4, nrow = nrow(dat)))  
colnames(MantaTracks) <- c("manta_name", "datetime.LOCAL", "Longitude", "Latitude")
MantaTracks$manta_name <- dat$manta_name ;MantaTracks$datetime.LOCAL <- dat$datetime.LOCAL; MantaTracks$Longitude <- dat$Longitude; MantaTracks$Latitude <- dat$Latitude; head(MantaTracks)
NumOfMantas <- length(unique(MantaTracks$manta_name)) # Total number of mantas
Mantas <- unique(MantaTracks$manta_name) # Factor of all manta names

#xyz <- MantaTracks
#coordinates(MantaTracks) <- ~Longitude+Latitude
#xyz <- SpatialPoints(MantaTracks, proj4string = WGS)
  

#---- define ROI ----
# Total ROI for bathymetry
LatMin = -30
LatMax = -15
LonMin = 145
LonMax = 160
ROI = c(LonMin, LonMax, LatMin, LatMax)

# Extent for plotting
ExtLatMin = -30
ExtLatMax = -15
ExtLonMin = 145
ExtLonMax = 160
ExtROI = c(ExtLonMin, ExtLonMax, ExtLatMin, ExtLatMax)


#---- load bathy ----
library(marmap)

# load bathymetry data
bat <- getNOAA.bathy(LonMin, LonMax, LatMin, LatMax, res = 4, keep=TRUE) #(110, 157, -48, -7, res = 4, keep=TRUE)
summary(bat)

#####
# get surface area for a specific depth range (e.g. shelf)
shelf.area <- get.area(bat, level.inf = -200, level.sup = 0); str(shelf.area)
ba <- round(shelf.area$Square.Km, 0); str(ba)

plot(bat, lwd=0.2)
image(shelf.area$Lon, shelf.area$Lat, shelf.area$Area, col=c("transparent", rgb(0.7, 0, 0, 0.3)), add = TRUE)

# save shelf area measurement for later use
shelf.area.value <- shelf.area[[1]]
shelf.area[[1]] <- NULL
str(shelf.area)

# create shelf mask
shelf.mask <- shelf.area$Area # create shelf maks f shelf (on shelf = 1, off shelf  0)
shelf.mask <- shelf.mask[,c(225:1)] # flip matrix
shelf.mask <- t(shelf.mask) # rotate matrix by 90 degrees
str(shelf.mask)

# transform to a raster
library(raster)
shelf.raster <- raster(shelf.mask) # convert shelf.mask to a raster
extent(shelf.raster) <- c(min(shelf.area$Lon),max(shelf.area$Lon),min(shelf.area$Lat),max(shelf.area$Lat)) # Give it lat/lon coords 
#projection(shelf.raster) = GDA
plot(shelf.raster)


#---- determine which positions are on/off shelf ----

# Create empty proptime array
OnShelf.summary <- array(NaN, 5) # Preallocate array to record summary of daily results

# create empty dataframe to store results
Summary <- data.frame(matrix(ncol = 5, nrow = 1))
colnames(Summary) <- c("manta_name","datetime.LOCAL","Longitude","Latitude","IsOnShelf")

# Automated Loop for observed calculations across all individuals
for (j in 1:NumOfMantas) {
  MantaRecords <- which(MantaTracks$manta_name == Mantas[j]) # Select Manta
  IndivMantaTracks <- MantaTracks[MantaRecords,] # subset data
  
  for (i in 1:nrow(IndivMantaTracks)){
  
  MantaName <- as.character(Mantas[j])
  X <- IndivMantaTracks$Longitude [i]
  Y <- IndivMantaTracks$Latitude [i]
  Date <- as.character(IndivMantaTracks$datetime.LOCAL[i])
  OnShelf <- raster::extract(shelf.raster, cbind(X, Y)) # extract raster value to determine if that position falls on the shelf
  
  OnShelf.summary <- c(MantaName, Date, X, Y, OnShelf) # collate data for this individual tag
  Summary <- rbind(Summary, OnShelf.summary) # add results to summary for all tags

  }  
}
Summary = Summary[-1,]
#write.csv(Summary,file="AllTags_TimeOnShelf.csv",dec=".",col.names=TRUE)

# plot output to check that it worked:
plot(shelf.raster)
points(Summary$Longitude, Summary$Latitude, col=Summary$IsOnShelf, pch=20, cex=0.5)

#---- Plot ----
# Create nice looking color palettes
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
plot(bat, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(bat), greys), c(min(bat), 0, blues)))
plot(bat, lwd = 0.8, deep = 0, shallow = 0, step = 0, add = TRUE) # highlight coastline
plot(bat, deep = -200, shallow = -200, step = 0, lwd = 0.5, add = TRUE, drawlabels = TRUE) # Add -300m isobath
plotArea(shelf.area, col = "coral") # add shelf mask is required
points(Summary$Longitude, Summary$Latitude, col=Summary$IsOnShelf, pch=20, cex=0.5)
legend(x="bottomleft", legend=c(paste("shelf:",ba,"km2")), col="black", pch=21, pt.bg="coral") # add legend with shelf area


#---- Compute distances between each point and the nearest location along the coastline: ----
x <- as.numeric(Summary$Longitude) 
y <- as.numeric(Summary$Latitude)
d <- dist2isobath(bat, x, y, isobath = -200)
d$distance <- d$distance/1000 # in km instead of metres
d

## NEED TO IMPLEMENT:
# if IsOnShelf=1 --> distance = - distance
# else distance remains positive
# this will allow to investigate patterns in IVM based on distance occuring on and off the shelf

# merge with Summary data
Summary <- cbind(Summary, d); head(Summary)
#write.csv(Summary,file="AllTags_TimeOnShelf.csv",dec=".",col.names=TRUE)



######## other useful marmap functions for vertical analysis: ########

#### extract bathymetry along a cross section of the bathymetry 
trsect <- get.transect(papoue, 150, -5, 153, -7, distance = TRUE)
head(trsect)

#### plot vertical bathymetry profile along track
plotProfile(trsect)
# The function path.profile() takes advantage of both get.transect() and
#plotProfile() to retrieve and plot bathymetric information along a path that
#is not limited to a straight transect between 2 points. See the help file of
#plotProfile() for more details.

### extract bathymetry for positions along track
get.depth(papoue, distance = TRUE)
# The get.depth() function can be used to retrieve depth information by either
#clicking on the map or by providing a set of longitude/latitude pairs. This is
#helpfull to get depth information along a GPS track record for instance. If the
#argument distance is set to TRUE, the haversine distance (in km) from the first
#data point on will also be computed.

#### get surface area fr a specific depth range (e.g. shelf)
bathyal <- get.area(hawaii, level.inf = -4000, level.sup = -1000)
ba <- round(bathyal$Square.Km, 0)
# to add to a plot:
plotArea(bathyal, col = col.bath)
legend(x="bottomleft",
       legend=c(paste("bathyal:",ba,"km2"),
                paste("abyssal:",ab,"km2")),
       col="black", pch=21,
       pt.bg=c(col.meso,col.bath,col.abys))

### shortest great circle distance between point and isobath (i.e. distance from shelf edge)
# e.g. Compute distances between each point and the nearest location along the coastline:
d <- dist2isobath(atl, lon, lat, isobath = 0)
d

### 3D plotting
#atl <- as.bathy(nw.atlantic)
library(lattice)
# view from above:
bathy3d <- wireframe(unclass(bat), shade = TRUE, 
                     screen = list(x = 00), aspect = c(1/1, 0.15), # screen x=90 to view from above
                     zlab="Bathymetry (m)", xlab="Longitude", ylab="Latitude")
# view from the side:
bathy3d <- wireframe(unclass(bat), shade = TRUE, 
                     screen = list(x = -35, y=0 , z=-15), aspect = c(1/1, 0.15),
                     zlab="Bathymetry (m)", xlab="Longitude", ylab="Latitude")
bathy3d


#to plot with satter plots: -- DOESN'T WORK YET...
dive <- read.csv("~/Dropbox/Fabrice's and Francisca's manta ray diving project/DATA/AllTags_DiveData_Cropped_withTagMetadata.csv", header=T); head(dive) # Fabrice
dive <- dive[which(dive$manta_name == "Pinocchio"),]

pts <- data.frame(x=as.vector(dive$Longitude), as.vector(dive$Latitude), z=-(as.vector(dive$depth)))
with(pts,spheres3d(x,y,z,col="blue",radius=0.1))

wireframe(unclass(bat), shade = TRUE, 
          screen = list(x = -35, y=0 , z=-15), aspect = c(1/1, 0.15), # screen x=90 to view from above
          scales = list(arrows = FALSE),
          panel.3d.wireframe = function(x, y, z, ...) {
            panel.3dwire(x = x, y = y, z = z, ...)
            panel.3dscatter(x = dive$Longitude,
                            y = dive$Latitude,
                            z = -dive$depth,
                            ...)
          })

### SEE:
# https://astrostatistics.psu.edu/su07/R/library/lattice/html/cloud.html
# https://stackoverflow.com/questions/8608044/make-points-look-under-surface-in-r-using-lattice-and-wireframe
# http://search.r-project.org/library/TeachingDemos/html/rotate.cloud.html

library(TeachingDemos); library(tcltk); library(tcltk2)
rotate.wireframe(bathy3d)
