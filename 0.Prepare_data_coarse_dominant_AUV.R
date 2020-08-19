#################################################

############ Prepare data for RF ################

### Load libraries --

library(ggplot2)
library(cowplot)
library(sp)
library(rgdal)
library(raster)
library(sf)
library(dplyr)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- "Y:/GB_Habitat_Classification"
d.dir <- "Y:/GB_Habitat_Classification/data"
s.dir <- "Y:/GB_Habitat_Classification/spatial_data"
p.dir <- "Y:/GB_Habitat_Classification/plots"
o.dir <- "Y:/GB_Habitat_Classification/outputs"

#### DOMINANT DATA ----

### Load data ----

# Habitat data --
df <- read.csv(paste(d.dir, "raw", "dominant-auv_broad.percent.cover.csv", sep='/'))
str(df) # 7 categories


# Bathy 250m all Geo Bay --
b <- raster(paste(s.dir, "GB_Bathy_250m.tif", sep='/'))
b
plot(b)

#e <- drawExtent()
ext <- extent(115.029,115.6675,-33.65969,-33.29265)
b2 <- crop(b, ext)
plot(b2)

#bx <- raster(paste(s.dir, "GBmultib_lidarUTM_CMR.tif", sep='/'))
#bx # in UTM
#plot(bx)

# Reproject lidat and multibeam data
#b2 <- projectRaster(b, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#b2
#plot(b2)

# save
#writeRaster(b2, paste(s.dir, "GBmultib_lidar_CMR.tif", sep='/'))


# Check data points --
dfs <- df
coordinates(dfs)  <- ~longitude+latitude
points(dfs)

# Filter data to use just where there is lidar and multib --

dnew <- raster::extract(b2, dfs, sp=T)
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
#writeRaster(predictors, paste(s.dir,"predictors_coarse.tif", sep='/'), overwrite=T)
#write.csv(namesp, paste(s.dir, "namespredictors.csv", sep='/'))

# load predictors
predictors <- stack(paste(s.dir,"predictors_coarse.tif", sep='/'))
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
auv_dom <- hab_pred[,c(2:4,14, 16:24)] # not sure what to remove for AUV
names(auv_dom)


sp1 <- auv_dom
coordinates(sp1) <- ~longitude+latitude
plot(b)
#plot(sp1, border="white", col="lightgrey", add=TRUE)
plot(sp1, pch=20, col=sp1$Max_if_2_habitats_have_same, add=TRUE)


## Filter per cell ----
# AUV data has many images / pixel
# More frequent dominant class/pixel

# to get  coordinates of each cell and the value --
d <- data.frame(coordinates(b2), count=b2[])

head(d)
str(d)



# https://gis.stackexchange.com/questions/279079/extracting-value-of-raster-with-coordinates-using-r

# extract cell ids with points on them --
result <- raster::extract(b2, sp1, cellnumbers=T, df=T)
head(result)
str(result)
result$cells <- as.factor(result$cells)

cellid <- as.data.frame(result %>%
                          group_by(cells, GB_Bathy_250m) %>%
                          # group_by(Class) %>%
                          summarize(n()))
str(cellid) # 84 obs
head(cellid)

cellid1 <- aggregate(GB_Bathy_250m~cells, data=result, mean)
str(cellid1) # 84 obs -> 84 cells with AUV data



# extract as spatial points cell ids --
resultsp <- raster::extract(b2, sp1, cellnumbers=T, coordinates = T ,sp =T)
resultsp
head(resultsp)
# get the coordinates --
rdf <- as.data.frame(resultsp)
str(rdf) 
rdf$cells <- as.factor(rdf$cells)

rdflat <- aggregate(latitude ~ cells, data=rdf, FUN=mean)
rdflat
str(rdflat) # 84 obs
rdflon <- aggregate(longitude ~ cells, data=rdf, FUN=mean)
str(rdflon)
rdflon

rdfcoords <- merge(rdflon, rdflat)
str(rdfcoords)


# make auv points a df
sp1df <- as.data.frame(sp1)
head(sp1df)

# join auv poins and cell ids 
sp1cells <- cbind(sp1df, result)
head(sp1cells)


##  aggregate by Class --

# rename Class column 
colnames(sp1cells)[colnames(sp1cells)=="Max_if_2_habitats_have_same"] <- "Class"
names(sp1cells)
str(sp1cells)
sp1cells$cells <- as.factor(sp1cells$cells)


# to get the dominant class and the cell id --
letsee <- as.data.frame(sp1cells %>%
                          group_by(cells, Class) %>%
                          # group_by(Class) %>%
                          summarize(n()))
head(letsee) # has cellnumbers but not coordinates
str(letsee) # 208 obs
names(letsee) <- c("cells", "Class", "n")
str(letsee) # 208 obs

## aggregate data frame by cell and max n()
### Up to here ####
# https://stackoverflow.com/questions/6289538/aggregate-a-dataframe-on-a-given-column-and-display-another-column
domhcell <- aggregate(n ~ cells, data=letsee, max)
head(domhcell)
str(domhcell) # 84 obs ## I need class info..

# split the data using split: to get class with max n in each cell id
#https://stackoverflow.com/questions/6289538/aggregate-a-dataframe-on-a-given-column-and-display-another-column  

splitdf <- do.call(rbind,lapply(split(letsee,letsee$cells), function(chunk) chunk[which.max(chunk$n),]))
splitdf
str(splitdf) # 84 obs
 
# match cell ids to lats and longs ---

coarseauv <- merge(splitdf, rdfcoords)
str(coarseauv) # 84 obs
head(coarseauv)

# check!
csp <- coarseauv
coordinates(csp) <- ~longitude+latitude

plot(b2)
plot(b3)
points(csp, pch = 20, cex=0.2, col=csp$Class)


## Get predictor data for each point --

data <- readOGR(paste(s.dir, "GB_auv_coarse_bathy_filtered_habitat_dominant_broad.shp", sep='/'))

pts <- raster::extract(predictors, data, df=T)
head(pts)
str(pts)

datadf <- as.data.frame(data)
head(datadf)
str(datadf)

# join predictor info to classes --
dnew <- cbind(datadf, pts)
head(dnew)

# make sp points
dnews <- dnew
coordinates(dnews) <- ~coords.x1+coords.x2
plot(b2)
points(dnews)

# save
#writeOGR(dnews, dsn= s.dir, layer= "GB_auv_coarse_bathy_filtered_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(dnew, paste(d.dir, "tidy", "GB_auv_coarse_bathy_filtered_habitat_dominant_broad.csv", sep='/'))
