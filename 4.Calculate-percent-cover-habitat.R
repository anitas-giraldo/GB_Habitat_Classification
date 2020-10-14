###     ###       ###       ###       ###

### Script to calculate percent cover of habitat classes ###


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


## Read fine data ----
f <- read.csv(paste(dt.dir, "Areal-coverage-fine.csv", sep='/'))
head(f)
str(f)

## Read coarse data ----
c <- read.csv(paste(dt.dir, "Areal-coverage-coarse.csv", sep='/'))
head(c)
str(c)



## area from m2 to km2 ----

# fine ----
head(f)
f$areakm2 <- f$area/1000000
head(f)
f

# coarse ----
head(c)
c$areakm2 <- c$area/1000000
head(c)
c




