#################################################

############ Prepare data for RF ################

### Load libraries --

library(ggplot2)
library(cowplot)
library(sp)
library(rgdal)
library(raster)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- "Y:/GB_Habitat_Classification"
d.dir <- "Y:/GB_Habitat_Classification/data"
s.dir <- "Y:/GB_Habitat_Classification/spatial_data"
p.dir <- "Y:/GB_Habitat_Classification/plots"
o.dir <- "Y:/GB_Habitat_Classification/outputs"

#### PRESENCE-ABSENCE DATA ----

### Load data ----

# Habitat data --
df <- read.csv(paste(d.dir, "raw", "auv_broad.presence.absence.csv", sep='/'))
str(df) # 7 categories


# Bathy 250m all Geo Bay --
bgb <- raster(paste(s.dir, "GB_Bathy_250m.tif", sep='/'))
bgb
plot(bgb)

#b <- raster(paste(s.dir, "GBmultib_lidarUTM_CMR.tif", sep='/'))
#b # in UTM
#plot(b)

# Reproject lidat and multibeam data
#b2 <- projectRaster(b, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#b2
#plot(b2)

# save
#writeRaster(b2, paste(s.dir, "GBmultib_lidar_CMR.tif", sep='/'))

# Bathy Lidar and Multibeam in latlong --
b <- raster(paste(s.dir, "GBmultib_lidar_CMR.tif", sep='/'))
plot(b)
b

# Check data points --
dfs <- df
coordinates(dfs)  <- ~longitude+latitude
points(dfs)

# Filter data to use just where there is lidar and multib --

dnew <- raster::extract(b, dfs, sp=T)
str(dnew)

dfnew <- as.data.frame(dnew)
dfnew <- na.omit(dfnew)
str(dfnew)

dfsnew <- dfnew
coordinates(dfsnew) <- ~longitude+latitude
points(dfsnew, pch = 20, col="red")
h <- dfsnew

# save points --
#write.csv(dfnew, paste(d.dir, "GB_Bruvs_fine_bathy_habitat_presence_absence_broad.csv"))
#writeOGR(dfsnew, dsn= s.dir, layer= "GB_Bruvs_fine_bathy_habitat_presence_absence_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)

## load shape file with habitat data ----
#h <- readOGR(paste(s.dir, "GB_Bruvs_fine_bathy_habitat_presence_absence_broad.shp", sep='/')) 


######################################

#### Calculate Bathy Derivatives ####

slope4 <- raster::terrain(b, "slope", unit="degrees", neighboors=4)
slope4@data@names
slope4@data@names <- "slope4"
slope8 <- raster::terrain(b, "slope", unit="degrees", neighboors=8)
slope8@data@names
slope8@data@names <- "slope8"
aspect4 <- raster::terrain(b, "aspect", unit="degrees", neighboors=4)
aspect4@data@names
aspect4@data@names <- "aspect4"
aspect8 <- raster::terrain(b, "aspect", unit="degrees", neighboors=8)
aspect8@data@names
aspect8@data@names <- "aspect8"
tpi <- raster::terrain(b, "TPI")
tri <- raster::terrain(b, "TRI")
roughness <- raster::terrain(b, "roughness")
flowdir <- raster::terrain(b, "flowdir")
b@data@names <- "depth"

predictors <- stack(b, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
plot(predictors)

names(predictors)
namesp <- names(predictors)

# save
#writeRaster(predictors, paste(s.dir,"predictors.tif", sep='/'), overwrite=T)
#write.csv(namesp, paste(s.dir, "namespredictors.csv", sep='/'))

###  Extract predictor values for each observation ----
hp <- raster::extract(predictors, h, sp=T)
hp

hab_pred <- as.data.frame(hp)
head(hab_pred)


## make long data frame where hab classes are factors of one column ----
library(reshape2)

# choose ID vars
ids <- c("X",  "campaignid",  "sample", "depth", "slope4", "slope8", "aspect4", "aspect8","tpi",
         "tri",       "roughness", "flowdir",   "latitude", "longitude")


auv.broad <- melt(hab_pred, id.vars=ids)
head(auv.broad)
names(auv.broad)[names(auv.broad)=="variable"] <- "Class"
names(auv.broad)[names(auv.broad)=="value"] <- "presence.absence"
str(auv.broad)

sp1 <- auv.broad
coordinates(sp1) <- ~longitude+latitude
plot(b)
plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, col=rainbow(7), pch=20, fill=sp1$Class, add=TRUE)


### Filter so only one obs per cell of bathy raster ###
# UP TO HERE ----
## use buffer?


# save
writeOGR(sp1, dsn= s.dir, layer= "GB_Bruvs_fine_bathy_habitat_presence_absence_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
write.csv(Bruv.broad, paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_presence_absence_broad.csv", sep='/'))


