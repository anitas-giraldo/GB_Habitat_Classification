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
#predictors <- raster(paste(s.dir,"predictors_coarse.tif", sep='/'))

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


## Filter per pixel ----
# AUV data has many images / pixel
# More frequent dominant class/pixel

# to get  number of points per cell --
d <- data.frame(coordinates(b2), count=b2[])

head(d)

# https://gis.stackexchange.com/questions/279079/extracting-value-of-raster-with-coordinates-using-r

result <- raster::extract(b2, sp1, cellnumbers=T, df=T)

head(result)

resultsp <- raster::extract(b2, sp1, cellnumbers=T, sp =T)

head(resultsp)

sp1df <- as.data.frame(sp1)
head(sp1df)

sp1cells <- cbind(sp1df, result)
head(sp1cells)


##  aggregate by Class --

# rename Class column 
colnames(sp1cells)[colnames(sp1cells)=="Max_if_2_habitats_have_same"] <- "Class"
names(sp1cells)
str(sp1cells)
sp1cells$cells <- as.factor(sp1cells$cells)


letsee <- sp1cells %>%
  group_by(cells, Class) %>%
 # group_by(Class) %>%
  summarize(n())

letsee <- as.data.frame(sp1cells %>%
                          group_by(cells, Class) %>%
                          # group_by(Class) %>%
                          summarize(n()))
head(letsee) # has cellnumbers but not coordinates
str(letsee)
names(letsee) <- c("cells", "Class", "n")

## aggregate data frame by cell and max n()
### Up to here ####
# https://stackoverflow.com/questions/6289538/aggregate-a-dataframe-on-a-given-column-and-display-another-column
domhcell <- aggregate(n ~ cells, data=letsee, max)
head(domhcell)
str(domhcell)

domcells <- merge(domhcell, letsee, by.x= 'cells', all= F)
head(domcells)
str(domcells)

sp2 <- merge(domcells, sp1cells)
head(sp2)
str(sp2)



# save
writeOGR(domcells, dsn= s.dir, layer= "GB_auv_coarse_bathy_habitat_dominant_broad", driver="ESRI Shapefile", overwrite_layer=TRUE)
#write.csv(auv_dom, paste(d.dir, "tidy", "GB_coarse_fine_bathy_habitat_dominant_broad.csv", sep='/'))

