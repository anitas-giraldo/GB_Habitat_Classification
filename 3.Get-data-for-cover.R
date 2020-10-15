###     ###       ###       ###       ###

### Script to GET THE DATA to calculate percent cover of habitat classes ###


### Load libraries ----

library(ggplot2)
library(ggthemes)
library(cowplot)
library(sp)
library(spDta)
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(mapview)
library(tmap)
library(mapdata)
library(leaflet)
library(caTools)
library(reshape2)
library(tidyr)
library(car)
library(lattice)
library(latticeExtra)
library(dplyr)
library(raster)
library(rasterVis)
library(zoo)
library(sf)
library(fields)
library(geoR)
library(gstat)
library(ggsn)
library(ggspatial)
library(ggrepel)
library(patchwork)
#library(elsa)
#install.packages("corrplot")
#library(corrplot)
library(broman)


# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
dt.dir <- paste(w.dir, "data/tidy", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')

# http://oswaldosantos.github.io/ggsn/

# Read gb cmr poly ----
gb <- st_read(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
gbu <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gbu)
levels(gbu$ZoneName)

crs1 <- proj4string(gbu) # "+proj=longlat +ellps=GRS80 +no_defs"

# for transformations --
#gbu <- readOGR(dsn="G:/My Drive/Anita/Shapefiles/GeoBay_CMR_UTM.shp") 

#crs2 <- proj4string(gbu) #  "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"


# get poly for each zone --
NPZ <- gbu[gbu$ZoneName=="National Park Zone",]
plot(NPZ)
HPZ <- gbu[gbu$ZoneName=="Habitat Protection Zone",]
MUZ <- gbu[gbu$ZoneName=="Multiple Use Zone",]
plot(MUZ)
SPZ <- gbu[gbu$ZoneName=="Special Purpose Zone (Mining Exclusion)",]

# Pick colors ----
sg <- brocolors("crayons")["Fern"] # "#78dbe2"
alg <-  brocolors("crayons")["Raw Umber"] # "#1dacd6" 
sand <-  brocolors("crayons")["Unmellow Yellow"] # "#f75394"

pal1 <- c(sand, sg, alg )


##      ##      ##      ##      ##      ##

#### GET DATA : FINE NPZ ####

####    BRUVs Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
    group_by(category) %>%
    tally() %>%
    mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg

# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a



####    FTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine fine bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
fine.npz <- rbind(b, a, f, d)
head(fine.npz)
# add zone --
fine.npz$zone <- "NPZ" 
fine.npz


#### GET DATA : FINE HPZ ####

####    BRUVs Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# HPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# HPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a


####    FTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# HPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# HPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine fine bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
fine.hpz <- rbind(b, a, f, d)
head(fine.hpz)
# add zone --
fine.hpz$zone <- "HPZ" 
fine.hpz



#### GET DATA : FINE MUZ ####

####    BRUVs Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg

# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a


####    FTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine fine bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
fine.muz <- rbind(b, a, f, d)
head(fine.muz)
# add zone --
fine.muz$zone <- "MUZ" 
fine.muz



#### GET DATA : FINE SPZ ####

####    BRUVs Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a


####    FTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine fine bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
fine.spz <- rbind(b, a, f, d)
head(fine.spz)
# add zone --
fine.spz$zone <- "SPZ" 
fine.spz


##      ##      ##      ##      ##      ###

## Combine all fine data ----

fineall <- rbind(fine.npz, fine.hpz, fine.muz, fine.spz)
fineall
class(fineall)

# save --
write.csv(fineall, paste(dt.dir, "Areal-coverage-fine.csv", sep='/'))



###     ###       ###       ###       ###       ###

#### GET DATA : COARSE NPZ ####

####    BRUVs Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a



####    FTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, NPZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine Coarse bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
coarse.npz <- rbind(b, a, f, d)
head(coarse.npz)
# add zone --
coarse.npz$zone <- "NPZ" 
coarse.npz


#### GET DATA : COARSE HPZ ####

####    BRUVs Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# HPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a


####    FTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# NPZ ----
npzb <- mask(pred, HPZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine Coarse bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
coarse.hpz <- rbind(b, a, f, d)
head(coarse.hpz)
# add zone --
coarse.hpz$zone <- "HPZ" 
coarse.hpz



#### GET DATA : COARSE MUZ ####

####    BRUVs Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a


####    FTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# MUZ ----
npzb <- mask(pred, MUZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine Coarse bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
coarse.muz <- rbind(b, a, f, d)
head(coarse.muz)
# add zone --
coarse.muz$zone <- "MUZ" 
coarse.muz



#### GET DATA : COARSE SPZ ####

####    BRUVs Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-Bruvs.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

b <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

b

####    AUV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-AUV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

a <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

a


####    FTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-FTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

f <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

f

####    DTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-DTV.tif", sep='/'))
plot(pred) # 1= algae, 2=seagrass, 3=Unveg


# SPZ ----
npzb <- mask(pred, SPZ)
plot(npzb)
plot(gbu, add=T)

d <- as.data.frame(npzb) %>%
  group_by(category) %>%
  tally() %>%
  mutate(area = n * res(npzb)[1] * res(npzb)[2])

d


##      ##      ##      ##      ##      ##      ##

#### Combine Coarse bathy data ####
b$method <- "Stereo-BRUVs"
b

a$method <- "AUV"
a

f$method <- "FTV"
f

d$method <- "DTV"
d

# join --
coarse.spz <- rbind(b, a, f, d)
head(coarse.spz)
# add zone --
coarse.spz$zone <- "SPZ" 
coarse.spz


##      ##      ##      ##      ##      ###

## Combine all fine data ----

coarseall <- rbind(coarse.npz, coarse.hpz, coarse.muz, coarse.spz)
coarseall
class(coarseall)

# save --
write.csv(coarseall, paste(dt.dir, "Areal-coverage-coarse.csv", sep='/'))





