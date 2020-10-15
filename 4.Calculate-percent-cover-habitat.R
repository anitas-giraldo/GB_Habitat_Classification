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
summary(c)



## area 2 ---- this is the area times 10,000

# fine ----
head(f)
f$area2 <- round(f$area*10000, digits = 3)
head(f)
f

# coarse ----
head(c)
c$area2 <- round(c$area*10000, digits = 3)
head(c)
c


## area 3 ---- this is the area cell * area of each cell

af <- 10*10 # area of each cell in m 

# fine ----
head(f)
f$area3 <- round((f$n*af/1000000), digits = 3)
head(f)
f

# coarse ----
ac <- 250*250 # area of each cell in m
  
head(c)
c$area3 <- round((c$n*ac/1000000), digits = 5)
head(c)
c


## Plot Fine ----

# Pick colors ----
sg <- brocolors("crayons")["Fern"] # "#78dbe2"
alg <-  brocolors("crayons")["Raw Umber"] # "#1dacd6" 
sand <-  brocolors("crayons")["Unmellow Yellow"] # "#f75394"

pal1 <- c(sg, alg, sand )

# fine ----
str(f)

# change order of factors for ploting --  
levels(f$category) 
f$category <- ordered(f$category, levels =c("Seagrass", "Algae","Unvegetated"))
levels(f$zone)
f$zone <- ordered(f$zone, levels=c("NPZ", "HPZ", "MUZ", "SPZ"))
f$method 
f$method <- ordered(f$method, levels=c("Stereo-BRUVs", "AUV", "FTV", "DTV"))

# REMOVE NAs --
f <- na.omit(f)
str(f)

theme_set(theme_bw())

pf <-ggplot(data=f , aes(x=method, y=area2, fill=category)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  #geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  #facet_wrap(~zone, ncol = 2, scales = 'free') +
  facet_wrap(~zone, ncol = 2) +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(4)) +
  scale_fill_manual(values = c("#71bc78", "#714b23", "#ffff66")) +
  #labs(y = "Predicted areal cover (km2)") +
  ylab(expression (paste("Area of predicted presence ", "(", km^{2}, ")"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.title = element_blank(), legend.text = element_text(size = 14),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold"))

pf


ggsave("Fine-predicted-area.png", plot = pf, path = p.dir, scale=1, dpi = 300)



# Plot coarse ----
str(c)

# change order of factors for ploting --  
levels(c$category) 
levels(c$category)[levels(c$category)=="Seagrasses"] <- "Seagrass"
levels(c$category)[levels(c$category)=="Unconsolidated"] <- "Unvegetated"
c$category <- ordered(c$category, levels =c("Seagrass", "Algae","Unvegetated"))
levels(c$zone)
c$zone <- ordered(c$zone, levels=c("NPZ", "HPZ", "MUZ", "SPZ"))
c$method 
c$method <- ordered(c$method, levels=c("Stereo-BRUVs", "AUV", "FTV", "DTV"))

# REMOVE NAs --
c <- na.omit(c)
str(c)

theme_set(theme_bw())

pc <-ggplot(data=c , aes(x=method, y=area2, fill=category)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  #geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, cex = 1) +
  #geom_errorbar(aes(ymax = mean-sd, ymin = mean+sd), width = 0.2, color = "blue") +
  facet_wrap(~zone, ncol = 2, scales = 'free') +
  #facet_wrap(~zone, ncol = 2) +
  #scale_fill_manual(values = c("#77dde7", "#fc6c85","#b2ec5d", "#ffcf48")) +
  #scale_fill_manual(values = greenpal(4)) +
  scale_fill_manual(values = c("#71bc78", "#714b23", "#ffff66")) +
  #labs(y = "Predicted areal cover (km2)") +
  ylab(expression (paste("Area of predicted presence ", "(", km^{2}, ")"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.title = element_blank(), legend.text = element_text(size = 14),
        axis.title.x = element_blank(), axis.title.y = element_text(size = 14, face="bold"), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size=14, face="bold", color = "grey20", angle = 45, hjust = 1),
        title = element_text(size = 14, face= "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 14, color = "black", face ="bold"),
        strip.text.y = element_text(size = 14, color = "black", face ="bold"))

pc


ggsave("Coarse-predicted-area.png", plot = pc, path = p.dir, scale=1, dpi = 300)




## plot GB --
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb)
levels(gb$ZoneName)
# get poly for each zone --
NPZ <- gb[gb$ZoneName=="National Park Zone",]
HPZ <- gb[gb$ZoneName=="Habitat Protection Zone",]
MUZ <- gb[gb$ZoneName=="Multiple Use Zone",]
SPZ <- gb[gb$ZoneName=="Special Purpose Zone (Mining Exclusion)",]

# plot fine bathy --
fineb <- raster(paste(s.dir, "GBmultib_lidar_CMR.tif", sep='/'))
fineb
e <- extent(115.1205, 115.5698, 115.5698, -33.33903)
fineb <- crop(fineb, e)
plot(fineb, col = "blue")
plot(gb, add=T)


# MAP FINE NPZ ----

fnpz <- mask(fineb, NPZ)
plot(fineb, col = "grey80")
plot(fnpz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(fineb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fnpz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-Fine-NPZ.tiff", sep='/'))



# MAP FINE HPZ ----

fz <- mask(fineb, HPZ)
plot(fineb, col = "grey80")
plot(fz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(fineb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-Fine-HPZ.tiff", sep='/'))



# MAP FINE MUZ ----

fz <- mask(fineb, MUZ)
plot(fineb, col = "grey80")
plot(fz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(fineb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-Fine-MUZ.tiff", sep='/'))


# MAP FINE SPZ ----

fz <- mask(fineb, SPZ)
plot(fineb, col = "grey80")
plot(fz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(fineb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-Fine-SPZ.tiff", sep='/'))






##      ##       ##       ##      ##      ##      ##      ##      ##

# plot coarse bathy --
coarseb <- raster(paste(s.dir, "GB_Bathy_250m.tif", sep='/'))
plot(coarseb)
coarseb<- mask(coarseb, gb)
plot(coarseb)
e <- extent(115.0336 , 115.5823, -33.6416, -33.34702)
coarseb <- crop(coarseb, e)
plot(coarseb)
plot(gb, add=T)

# MAP COARSE NPZ ----

fnpz <- mask(coarseb, NPZ)
plot(fineb, col = "grey80")
plot(fnpz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(coarseb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fnpz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-coarse-NPZ.tiff", sep='/'))



# MAP COARSE HPZ ----

fz <- mask(coarseb, HPZ)
plot(fineb, col = "grey80")
plot(fz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(coarseb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-coarse-HPZ.tiff", sep='/'))



# MAP COARSE MUZ ----

fz <- mask(coarseb, MUZ)
plot(fineb, col = "grey80")
plot(fz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(coarseb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-coarsee-MUZ.tiff", sep='/'))


# MAP COARSE SPZ ----

fz <- mask(coarseb, SPZ)
plot(fineb, col = "grey80")
plot(fz, col = "red", add=T)
plot(gb, add=T)


map1 <- tm_shape(coarseb) + tm_raster(palette="grey80")

map2 <- map1 + tm_shape(fz)  + tm_raster(palette="red")

map3 <- map2 + tm_shape(gb) + tm_borders(col ='black', lwd = 6) +
  tm_compass(type = "arrow", position = c(0.84, 0.12), size = 4) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1) + 
  tm_layout(legend.show = FALSE) +
  #tm_graticules(ticks = FALSE) +
  tm_grid(n.x = 3, n.y = 3, labels.size = 1.5, lines = FALSE) 


map3

## save map ----
tmap_save(map3, paste(p.dir, "GB-coarse-SPZ.tiff", sep='/'))
















