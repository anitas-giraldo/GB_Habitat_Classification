##### Spatial uncertainty analysis Geographe Bay remote sensing BRUVS model v0.1
## calculate Moran's I and variogram 
## Ben Radford 

## background libraries (don't need all of these sorry)
library(caret)
library(raster)
library(rgdal)
library(ggplot2)
library(spatstat)
library(plotrix)
library(fields)
library(leaflet)
library(plotGoogleMaps)
library(maptools)
library(RColorBrewer)
library(lattice)
library(geoR)
library(plotrix)
library(sp)
library(sp)

### set working director

setwd("G:/GeographBay/SharynSentenal22019B321/Code")

# Moran's I and spatial dependencies
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(pgirmess) # Data Analysis in Ecology

# Attach libraries for point processes
library(spatstat)
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic



### import shapefile
dat <- shapefile("G:/GeographBay/SharynSentenal22019B321/b321/BRUVS_validation_Sent2_B321_Depth1819_sharyn.shp")
## add projection/check 
proj4string(dat) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ')

## calculate Kappa
confusionMatrix(table(dat@data$classvalue,dat@data$Classval_1))

## add correct classification field and make numeric 
dat$correct <- as.numaric(ifelse(dat$classvalue == dat$Classval_1,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", dat$correct)
leaflet(dat) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)


# Calculate the maximum distance between BRUVS validaton points 
maxDist<-max(dist(cbind(dat$longitude, dat$latitude)))
maxDist

xy=cbind(dat$longitude, dat$latitude)
pgi.cor <- correlog(coords=xy, z=dat$correct, method="Moran", nbclass=10)   # "pgirmess" package
# coords = xy cordinates, z= vector of values at each location and nbclass = the number of bins
plot(pgi.cor) # statistically significant values (p<0.05) are plotted in red

### spatial autocorrlelation p stats
pgi.cor


dat_geo<-as.geodata(dat[,c("longitude","latitude","correct")])

# Generate and plot a binned variogram (10 bins so maxdist /10) 
Vario <- variog(dat_geo,max.dist=0.6666559,uvec=seq(0.06666559,0.6666559,l=10))

## plots of semi variance and morans I in this case show clusting and spatial autocorrelation
## Think its a result of sampling but have to check with saptial regression/Geographic Weighted regression 
par(mfrow=c(2,1))
plot(Vario)
plot(pgi.cor)
