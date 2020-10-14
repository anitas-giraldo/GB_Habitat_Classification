

###     ###       ###       ###       ###

### Script to plot maps of predictions nicely ###


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
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')

# http://oswaldosantos.github.io/ggsn/

# Read gb cmr poly ----
gb <- st_read(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb)
levels(gb$ZoneName)
# get poly for each zone --
NPZ <- gb[gb$ZoneName=="National Park Zone",]
HPZ <- gb[gb$ZoneName=="Habitat Protection Zone",]
MUZ <- gb[gb$ZoneName=="Multiple Use Zones",]
SPZ <- gb[gb$ZoneName=="Special Purpose Zone (Mining Exclusion)",]

# Pick colors ----
sg <- brocolors("crayons")["Fern"] # "#78dbe2"
alg <-  brocolors("crayons")["Raw Umber"] # "#1dacd6" 
sand <-  brocolors("crayons")["Unmellow Yellow"] # "#f75394"

pal1 <- c(sand, sg, alg )




#### ####     FINE BATHY PREDICTIONS     #### 

####    BRUVs Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx
class(xx)
xx <- xx[-1,]
xx$ID <- xx$ID[xx$ID != "0",]
xx$ID <- c('3','2', '1')
xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
levels(pred) <- xx
pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 
  

map2
class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Fine-Bruvs.tiff", sep='/'))


####    AUV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-AUV.tif", sep='/'))
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx
class(xx)
xx <- xx[-1,]
xx$ID <- xx$ID[xx$ID != "0",]
xx$ID <- c('3','2', '1')
xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
levels(pred) <- xx
pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Fine-AUV.tiff", sep='/'))




####    FTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-FTV.tif", sep='/'))
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx
class(xx)
xx <- xx[-1,]
xx$ID <- xx$ID[xx$ID != "0",]
xx$ID <- c('3','2', '1')
xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
levels(pred) <- xx
pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Fine-FTV.tiff", sep='/'))




####    DTV Fine    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Fine-DTV.tif", sep='/'))
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx
class(xx)
xx <- xx[-1,]
#xx$ID <- xx$ID[xx$ID != "0",]
xx$ID <- c('3','2', '1')
xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
levels(pred) <- xx
pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Fine-DTV.tiff", sep='/'))





#### ####     COARSE BATHY PREDICTIONS     #### 


# Pick colors ----
sg <- brocolors("crayons")["Fern"] # "#78dbe2"
alg <-  brocolors("crayons")["Raw Umber"] # "#1dacd6" 
sand <-  brocolors("crayons")["Unmellow Yellow"] # "#f75394"

pal1 <- c('white', alg,  sg, sand  )

####    BRUVs Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-Bruvs.tif", sep='/'))
plot(pred)
#e <- drawExtent()
e <- extent(115.04, 115.606, -33.6258,  -33.3387 )
pred  <- crop(pred, e)
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
#xx <-levels(pred)[[1]]
#xx
#class(xx)
#xx <- xx[-1,]
#xx$ID <- xx$ID[xx$ID != "0",]
#xx
#xx$ID <- c('0', '3','2', '1')
#xx$category <- c('', 'Unconsolidated',  'Seagrasses', 'Algae')
#xx
#levels(pred) <- xx
#pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)
map1

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Coarse-Buvs.tiff", sep='/'))


####    AUV Coarse   ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-AUV.tif", sep='/'))
plot(pred)
e <- extent(115.04, 115.606, -33.6258,  -33.3387 )
pred  <- crop(pred, e)
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
#xx <-levels(pred)[[1]]
#xx
#class(xx)
#xx <- xx[-1,]
#xx$ID <- xx$ID[xx$ID != "0",]
#xx$ID <- c('3','2', '1')
#xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
#levels(pred) <- xx
#pred
#xx <-levels(pred)[[1]]

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Coarse-AUV.tiff", sep='/'))




####    FTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-FTV.tif", sep='/'))
plot(pred)
e <- extent(115.04, 115.606, -33.6258,  -33.3387 )
pred  <- crop(pred, e)
plot(pred)


# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx
class(xx)
xx <- xx[-1,]
xx <- xx[c(3,1,2),]
#xx$ID <- xx$ID[xx$ID != "0",]
#xx$ID <- ordered(xx$ID, levels = c('3','1', '2'))
#xx$category <-  ordered(xx$category, levels =c('Unconsolidated', 'Seagrasses', 'Algae'))
levels(pred) <- xx
pred
#xx <-levels(pred)[[1]]

# colors----
pal1 <- c( sand  ,  sg,  alg)

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)


map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Coarse-FTV.tiff", sep='/'))




####    DTV Coarse    ####

# Read data ----
pred <- raster(paste(o.dir, "GBpred-Coarse-DTV.tif", sep='/'))
plot(pred)
e <- extent(115.04, 115.606, -33.6258,  -33.3387 )
pred  <- crop(pred, e)
plot(pred)

# raster as data frame --
#redf <- as.data.frame(pred, xy=TRUE) %>% na.omit()
#head(predf)

# fix class levels for plotting --
#xx <-levels(pred)[[1]]
#xx
#class(xx)
#xx <- xx[-1,]
#xx$ID <- xx$ID[xx$ID != "0",]
#xx$ID <- c('3','2', '1')
#xx$category <- c('Unconsolidated',  'Seagrasses', 'Algae' )
#levels(pred) <- xx
#pred
#xx <-levels(pred)[[1]]

# colors ----
pal1 <- c('white', alg,  sg, sand  )

## Plot using tmap ----
# https://geocompr.robinlovelace.net/adv-map.html


map1 <- tm_shape(pred) + tm_raster(palette=pal1)

map2 <- map1 + tm_shape(gb)  + tm_borders(col ='black', lwd = 2) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "GB-Coarse-DTV.tiff", sep='/'))



##      ##       ##        ##        ##       ##

###    ###   ZOOM IN PLOTS   ####


## create polygon of the NPZ ----

plot(gb)
#e <- drawExtent()
e <- extent(115.346, 115.4214, -33.57695, -33.51839) # get extent
# make extent a polygon
npzbb <- as(e, 'SpatialPolygons')
npzbb
# check
plot(gb)
plot(npzbb, add=T)

# NPZ bbox map for reference
plot(gb, lwd = 2)
plot(npzbb, lwd = 3, line = 'green', add=T)



map1 <- tm_shape(gb) + tm_borders(col ='black', lwd = 3)

map2 <- map1 + tm_shape(npzbb)  + tm_borders(col ='red', lwd = 3) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "NPZ-reference.tiff", sep='/'))



## create polygon of the HPZ ----

plot(gb)
#e <- drawExtent()
e <- extent(115.19,115.2446 ,-33.60948,-33.52707) # get extent
# make extent a polygon
hpzbb <- as(e, 'SpatialPolygons')
hpzbb
# check
plot(gb)
plot(hpzbb, add=T)

# NPZ bbox map for reference
plot(gb, lwd = 2)
plot(hpzbb, lwd = 3, line = 'green', add=T)



map1 <- tm_shape(gb) + tm_borders(col ='black', lwd = 3)

map2 <- map1 + tm_shape(hpzbb)  + tm_borders(col ='red', lwd = 3) +
  tm_compass(type = "arrow", position = c(0.84, 0.25), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.text.size = 1,
            legend.position = c(0.76, 0.1),
            legend.title.color = 'white') +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map2
#class(map2)

## save map ----

tmap_save(map2, paste(p.dir, "HPZ-reference.tiff", sep='/'))


### HPZ ZOOM fine bathy ----

# extent of HPZ --
e <- extent(115.19,115.2446 ,-33.60948,-33.52707)

# Bruvs data ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred)

# crop to extent --
p2 <- crop(pred, e)
plot(p2)
