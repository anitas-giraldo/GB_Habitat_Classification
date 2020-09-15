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
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')

#### DOMINANT DATA ----

### Load data ----

# Habitat data --
df <- read.csv(paste(d.dir, "raw", "DTV_detailed_habitat_dominant.csv", sep='/'))
str(df) # 7 categories


# Bathy 250m all Geo Bay --
bgb <- raster(paste(s.dir, "GB_Bathy_250m.tif", sep='/'))
bgb
plot(bgb)

#bx <- raster(paste(s.dir, "GBmultib_lidarUTM_CMR.tif", sep='/'))
#bx # in UTM
#plot(bx)

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
head(df)
dfs <- df
coordinates(dfs)  <- ~Longitude+Latitude
points(dfs)

# Filter data to use just where there is lidar and multib --

dnew <- raster::extract(b, dfs, sp=T)
str(dnew)

dfnew <- as.data.frame(dnew)
dfnew <- na.omit(dfnew)
str(dfnew)

dfsnew <- dfnew
coordinates(dfsnew) <- ~Longitude+Latitude
points(dfsnew, pch = 20, col="red")
h <- dfsnew

# save points --
#write.csv(dfnew, paste(d.dir, "GB_dtv_fine_bathy_habitat_dominant_broad.csv"))
#writeOGR(dfsnew, dsn= s.dir, layer= "GB_dtv_fine_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)

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


predictors <- stack(paste(s.dir,"predictors.tif", sep='/'))
plot(predictors)
namesp <- read.csv(paste(s.dir,"namespredictors.csv", sep='/'))
names(predictors) <- namesp[,2]



###  Extract predictor values for each observation ----
hp <- raster::extract(predictors, h, sp=T)
hp

hab_pred <- as.data.frame(hp)
head(hab_pred)
str(hab_pred) # 837 observations

names(hab_pred)



## remove unwanted columns ----
auv_dom <- hab_pred[,-c(1,13,14,16)] # not sure what to remove for AUV
names(auv_dom)


sp1 <- auv_dom
coordinates(sp1) <- ~Longitude+Latitude
plot(b)
#plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, col=rainbow(7), pch=20, fill=sp1$dominant, add=TRUE)

# save
writeOGR(sp1, dsn= s.dir, layer= "GB_dtv_fine_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
write.csv(auv_dom, paste(d.dir, "tidy", "GB_dtv_fine_bathy_habitat_dominant_broad.csv", sep='/'))

