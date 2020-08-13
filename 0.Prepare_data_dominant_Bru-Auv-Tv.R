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

#### DOMINANT DATA ----

### BRUVS: ####

### Load data ----

# Habitat data --
df <- read.csv(paste(d.dir, "raw", "dominant-stereo-BRUVs_broad.percent.cover.csv", sep='/'))
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
str(hab_pred) # 837 observations

names(hab_pred)



## remove unwanted columns ----
bruv_dom <- hab_pred[,c(2:4,13, 15:23)] # not sure what to remove for TV
names(bruv_dom)


sp1 <- bruv_dom
coordinates(sp1) <- ~longitude+latitude
plot(b)
#plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, col=rainbow(7), pch=20, fill=sp1$Max_if_2_habitats_have_same, add=TRUE)

# save
#writeOGR(sp1, dsn= s.dir, layer= "GB_tv_fine_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(tv_dom, paste(d.dir, "tidy", "GB_tv_fine_bathy_habitat_dominant_broad.csv", sep='/'))





### AUV: ####

### Load data ----

# Habitat data --
df <- read.csv(paste(d.dir, "raw", "dominant-auv_broad.percent.cover.csv", sep='/'))
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

#slope4 <- raster::terrain(b, "slope", unit="degrees", neighboors=4)
#slope4@data@names
#slope4@data@names <- "slope4"
#slope8 <- raster::terrain(b, "slope", unit="degrees", neighboors=8)
#slope8@data@names
#slope8@data@names <- "slope8"
#aspect4 <- raster::terrain(b, "aspect", unit="degrees", neighboors=4)
#aspect4@data@names
#aspect4@data@names <- "aspect4"
#aspect8 <- raster::terrain(b, "aspect", unit="degrees", neighboors=8)
#aspect8@data@names
#aspect8@data@names <- "aspect8"
#tpi <- raster::terrain(b, "TPI")
#tri <- raster::terrain(b, "TRI")
#roughness <- raster::terrain(b, "roughness")
#flowdir <- raster::terrain(b, "flowdir")
#b@data@names <- "depth"

#predictors <- stack(b, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
#plot(predictors)

#names(predictors)
#namesp <- names(predictors)

# save
#writeRaster(predictors, paste(s.dir,"predictors.tif", sep='/'), overwrite=T)
#write.csv(namesp, paste(s.dir, "namespredictors.csv", sep='/'))

###  Extract predictor values for each observation ----
hp <- raster::extract(predictors, h, sp=T) # Predictors set in BRUV section
hp

hab_pred <- as.data.frame(hp)
head(hab_pred)
str(hab_pred) # 837 observations

names(hab_pred)



## remove unwanted columns ----
auv_dom <- hab_pred[,c(2:4,14, 16:24)] # not sure what to remove for TV
names(auv_dom)


sp1 <- auv_dom
coordinates(sp1) <- ~longitude+latitude
plot(b)
#plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, col=rainbow(7), pch=20, fill=sp1$Max_if_2_habitats_have_same, add=TRUE)

# save
#writeOGR(sp1, dsn= s.dir, layer= "GB_tv_fine_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(tv_dom, paste(d.dir, "tidy", "GB_tv_fine_bathy_habitat_dominant_broad.csv", sep='/'))





### TV: ####

### Load data ----

# Habitat data --
df <- read.csv(paste(d.dir, "raw", "dominant-towed_broad.percent.cover.csv", sep='/'))
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

#slope4 <- raster::terrain(b, "slope", unit="degrees", neighboors=4)
#slope4@data@names
#slope4@data@names <- "slope4"
#slope8 <- raster::terrain(b, "slope", unit="degrees", neighboors=8)
#slope8@data@names
#slope8@data@names <- "slope8"
#aspect4 <- raster::terrain(b, "aspect", unit="degrees", neighboors=4)
#aspect4@data@names
#aspect4@data@names <- "aspect4"
#aspect8 <- raster::terrain(b, "aspect", unit="degrees", neighboors=8)
#aspect8@data@names
#aspect8@data@names <- "aspect8"
#tpi <- raster::terrain(b, "TPI")
#tri <- raster::terrain(b, "TRI")
#roughness <- raster::terrain(b, "roughness")
#flowdir <- raster::terrain(b, "flowdir")
#b@data@names <- "depth"

#predictors <- stack(b, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
#plot(predictors)

#names(predictors)
#namesp <- names(predictors)

# save
#writeRaster(predictors, paste(s.dir,"predictors.tif", sep='/'), overwrite=T)
#write.csv(namesp, paste(s.dir, "namespredictors.csv", sep='/'))

###  Extract predictor values for each observation ----
hp <- raster::extract(predictors, h, sp=T) # Predictors set in BRUV section
hp

hab_pred <- as.data.frame(hp)
head(hab_pred)
str(hab_pred) # 837 observations

names(hab_pred)



## remove unwanted columns ----
tv_dom <- hab_pred[,c(1:3, 12, 14:22)] # not sure what to remove for TV
names(tv_dom)


sp1 <- tv_dom
coordinates(sp1) <- ~longitude+latitude
plot(b)
#plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, col=rainbow(7), pch=20, fill=sp1$Max_if_2_habitats_have_same, add=TRUE)

# save
#writeOGR(sp1, dsn= s.dir, layer= "GB_tv_fine_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(tv_dom, paste(d.dir, "tidy", "GB_tv_fine_bathy_habitat_dominant_broad.csv", sep='/'))





### COMBINE Bruv, Auv and Tv data into one df ----


names(bruv_dom)
names(auv_dom)
names(tv_dom)


all_dom <- rbind(bruv_dom, auv_dom, tv_dom)
str(all_dom) # 2068 obs
names(all_dom)
colnames(all_dom)[colnames(all_dom)=="Max_if_2_habitats_have_same"] <- "Class"
levels(all_dom$Class)
#  "Consolidated" "Macroalgae" "Seagrasses" "Sponges" "Turf.algae" "Unconsolidated" "Other"  "Stony.corals" 

# check Nas
any(is.na(all_dom))
which(is.na(all_dom))
all_dom <- na.omit(all_dom)
str(all_dom) # 2058 obs.


# Make spatial points to plot

all_sp <- all_dom
coordinates(all_sp) <- ~longitude+latitude
points(all_sp , pch = 20, col="red")
h <- all_sp 
plot(h, col=rainbow(7), pch=20, fill=sp1$Class, add=TRUE)


# save
writeOGR(h, dsn= s.dir, layer= "GB_all_fine_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
write.csv(all_dom, paste(d.dir, "tidy", "GB_all_fine_bathy_habitat_dominant_broad.csv", sep='/'))

