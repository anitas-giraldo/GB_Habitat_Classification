###################################

## script to clean data for classification analysis

library(ggplot2)
library(ggthemes)
library(extrafont)
library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
library(raster)
library(sp)
library(sf)
library(rgdal)
library(plyr)
library(maptools)
library(broom)
library(spatialEco) # to remove Nas in spatial points


## Clear workspace ----
rm(list = ls())

# Set working directory ####
w.dir <- "//uniwa.uwa.edu.au/userhome/staff1/00093391/Desktop/GitHubAnita/GB_Habitat_Classification"
# Set data directory - to read the data from
d.dir <- paste(w.dir, "data", sep='/')
# Set graph directory - to save plots
p.dir <- paste(w.dir, "plots", sep='/')
# Set up shapefile 
s.dir <- paste(w.dir, "spatial_data", sep='/')



## Load BRUV data ----

# set file name --
filen <- "dominant-stereo-BRUVs_broad.percent.cover.csv"

# Load file ---
bruv <- read.csv(paste(d.dir, "raw", filen, sep='/'))
str(bruv)

# turn into sp object
coordinates(bruv) <- ~longitude+latitude

## Load GEO BAY  shapefile ----

gb <- readOGR(paste(s.dir, "GeoBay.shp", sep='/'))
plot(gb)


## Load Bathy ----

b <- raster(paste(s.dir, "GB_Bathy_250m.tif", sep='/'))
plot(b)

points(bruv, add=T)

## Remove points that are not in GB ----

# Extract bathy from points

bp <- raster::extract(b, bruv, sp = T)
bp$GB_Bathy_250m # NA's are points outside GB

# Remove points with NAs

bp <- na.omit(bp$GB_Bathy_250m)
bp2 <- sp.na.omit(bp)

# check --
plot(b)
points(bp2, add=T)

## save spatial points and df ----

# sp save --
writeOGR(bp2, s.dir, "GB_bruvs.shp", driver = "ESRI Shapefile")

# df save --
bpdf <- as.data.frame(bp2)
head(bpdf)

write.csv(bpdf, paste(d.dir, "tidy", "GBonly_Bruv_broad_dominant.csv", sep='/'))

####################################################

### NOw CROP THE GB IMAGE to exclude land ----

sat <- raster(paste(s.dir, "Geographe_Min_pixel_sentine2v5_planning_2019.tif", sep='/'))
plot(sat)
sat2 <- stack(paste(s.dir, "Geographe_Min_pixel_sentine2v5_planning_2019.tif", sep='/')) # to read all 3 bands
plot(sat2)
sat # check resolution of different raster
sat2

b # check resolution

0.0025/0.0001796631 # 13.91493

## check if can mask

test <- raster::crop(sat, b)
plot(test)

test2 <- raster::mask(test, b)

# resample
test3 <- raster::disaggregate(b, 13.91493)
test3

test4 <- resample(b, sat2)
plot(test4)

test5 <- mask(sat2, test4)
plot(test5)

writeRaster(test5, paste(s.dir, "GB-Sat-image-cropped.tif",sep='/'), overwrite=T)





