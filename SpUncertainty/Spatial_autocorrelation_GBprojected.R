## spatial autocorrelatio using projected data to get info in metres ----


## libraries ----
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
library(caTools)

# Moran's I and spatial dependencies
library(spdep) # Spatial Dependence: Weighting Schemes, Statistics and Models
library(pgirmess) # Data Analysis in Ecology

# Attach libraries for point processes
library(spatstat)
library(splancs) # K-function
library(smacpod) # Spatial scanning statistic

# https://rfunctions.blogspot.com/2017/06/how-to-identify-and-remove-spatial.html?fbclid=IwAR0-UzcspeSxap0y3XRs7-Yr3sB6G9OKctlAmAzKM7EkbR7OqMlGxoWqzAk
library(nlme)
library(ape)
library(MuMIn)

w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
m.dir <- "Y:/GB_Habitat_Classification"
o.dir <- "Y:/GB_Habitat_Classification/outputs"
d.dir <- "Y:/GB_Habitat_Classification/data"
s.dir <- paste(m.dir, "spatial_data", sep='/')


#### BRUV fine ----

##    1. Get validation data: predicted vs actual ----

# read predicted file ----
pred <- raster(paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))
plot(pred)
pred

# get validation data ----
# Load data --

### Load data ----
df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# project points --- 
dfs <- df
coordinates(dfs) <- ~longitude+latitude
points(dfs, add=T)
proj4string(dfs)
proj4string(pred)
crs1 <- proj4string(pred)
proj4string(dfs) <- crs1
gbutm <- raster(paste(s.dir, "GBmultib_lidarUTM_CMR.tif", sep='/'))
crs2 <- proj4string(gbutm)
dfsutm <- spTransform(dfs, crs2)
pred2 <- projectRaster(pred, crs=crs2, method='ngb')
plot(pred2)
plot(dfsutm, add=T)

df <- as.data.frame(dfsutm)
head(df)


# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

# remove unneeded columns ---
names(df)
df2 <- df %>% dplyr::rename(Class = Max_if_2_habitats_have_same) %>%
  dplyr::select(Class, latitude, longitude, depth, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
head(df2)
# change name of class
names(df2)
#colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
df2$Class <- as.factor(df2$Class)
levels(df2$Class) # "Consolidated"   "Macroalgae"     "Other"          "Seagrasses"     "Unconsolidated"
summary(df2)
head(df2)

# check for NAs
any(is.na(df2))

### Get train and test data ----
levels(df2$Class) # "Consolidated"   "Macroalgae"     "Seagrasses"     "Turf.algae"     "Unconsolidated"
set.seed(777)
sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 109  12
dim(test) # [1]  35 12

summary(test)
levels(test$Class)

# remove class if needed----
test <- test[test$Class != "Sponges",]
#test <- test[test$Class != "Stony.corals",]
test <- droplevels(test)
summary(test)

# remove unwanted cols 
names(test)
test <- test[,-c(4:12)]
head(test)


#### Join predicitions w validation ----
# check predicted levels
pred # classes: 1 = Algae, 2 = Seagrass, 3 = Unvegetated
# check validation levels 
levels(test$Class)
# rename
library(plyr)
test$Class <- revalue(test$Class, c("Seagrasses"= "Seagrass", "Turf.algae"="Algae", "Unconsolidated"="Unvegetated"))
# reorder 
test$Class <- ordered(test$Class, levels = c("Algae", "Seagrass",  "Unvegetated"))
levels(test$Class)
head(test)

# create numeric values 
Class <- c("Algae", "Seagrass",  "Unvegetated")
classval <- c("1", "2", "3")
nclass <- cbind(Class, classval)
nclass
# merge w test
test2 <- merge(test, nclass, by="Class")
head(test2)


#### Extract predicted Values ----
# make test sp 
tests <- test2
coordinates(tests) <- ~longitude + latitude
points(tests)
# extract predicted
preds <- raster::extract(pred2, tests, sp = TRUE)
preds
# make df 
preds <- as.data.frame(preds)
head(preds)
str(preds)
# remove NAs (points outside CMR)
preds <- na.omit(preds)
preds
str(preds) # 32 obs
#preds$GBpred.Coarse.DTV <- as.factor(preds$GBpred.Coarse.DTV)
# There is no class 1 in validation data, so remove from factor ---
preds <- droplevels(preds)
str(preds) # 25 obs

##    ##    ##    ##    ##    ##

#### 2. Calculate Kappa - Confusion matix ----

# calculate Kappa
#confusionMatrix(table(tests@data$classval,tests@data$GBpred.Coarse.Bruvs))

confusionMatrix(table(preds$GBpred.Fine.Bruvs, preds$classval), byClass = T) # predicted, actual

## add correct classification field and make numeric 
preds$correct <- as.numeric(ifelse(preds$GBpred.Fine.Bruvs == preds$classval,1,0) )
#preds$correct <- as.numeric(ifelse(preds$classval == preds$GBpred.Coarse.AUV,1,0))

# Plot up and look evidence of spatial clustering? So yep in shallow partly because of sample design 
pal = colorNumeric("Oranges", preds$correct)
leaflet(preds) %>% 
  addTiles() %>% 
  addCircleMarkers(~longitude, ~latitude, fillOpacity=1,fillColor= ~pal(correct), radius=~correct*4, stroke=TRUE, weight=1) %>% 
  addLegend(pal = pal, values = ~correct)



