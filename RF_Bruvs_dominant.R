########################################################

###### Script --  Random Forest - BRUVs data - v1.0  ##############   


### Load libraries ----

library(ggplot2)
library(ggthemes)
library(cowplot)
library(randomForest)
library(sp)
library(rgdal)
library(raster)
library(caTools)
library(reshape2)
library(tidyr)
library(car)
library(lattice)
library(dplyr)
library(raster)
library(rasterVis)
library(zoo)
library(sf)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- "Y:/GB_Habitat_Classification"
d.dir <- "Y:/GB_Habitat_Classification/data"
s.dir <- "Y:/GB_Habitat_Classification/spatial_data"
p.dir <- "Y:/GB_Habitat_Classification/plots"
o.dir <- "Y:/GB_Habitat_Classification/outputs"


### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

p <- stack(paste(s.dir, "predictors.tif", sep='/'))
namesp <- read.csv(paste(s.dir, "namespredictors.csv", sep='/'))
namesp
names(p) <- namesp[,2]
names(p)


## Prepare data ----

# remove unneeded columns ---
names(df)
df2 <- df[,c(5:14)]
head(df2)
# change name of class
names(df2)
colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
levels(df2$Class)
summary(df2)
head(df2)

## Plot predictors correlations by class -----

# matrix scatterplot of just these 13 variables --
scatterplotMatrix(df2[2:10], col = "Class")

plot(df2[2:10], col = df2$Class)
legend("center", 
       legend = levels(df2$Class))

### Check Predicitor correlations ---

# define function mosthighlycorrelated --
# https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html

# linear correlation coefficients for each pair of variables in your data set, 
# in order of the correlation coefficient. This lets you see very easily which pair of variables are most highly correlated.

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}


mosthighlycorrelated(df2[2:10], 9) # This results in only depth, rough and slope 4 not being correlated above 0.95


## MAKE BETTER PLOT -- TO do still -----


### Get train and test data ----

sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 109  10
dim(test) # [1] 35 10

### MODEL 1 ----
### RF - 5 habitat classes ---
# this is using all the habitat classes = 5 in total
# Used only the preds that were not correlated: depth, slope4, roughness

model <- randomForest(Class ~ ., data=train %>% select(c(Class, depth, slope4, roughness)) , ntree=501, proximity=TRUE)
model #  OOB = 55.05%
model$importance
model$classes

ptest <- p
names(ptest)
ptest <- dropLayer(p, c(3:7,9))

## Predict ----

test <- raster::predict(ptest, model)

## Plot ----

plot(test)
e <- drawExtent()
testx <- crop(test, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
class(lp) # trellis



#### MODEL 2 ----
### RF - 5 habitat classes ---
# try using all predictors ---

# remove all the Classes that are not SG or MA
levels(df2$Class)

model2 <- randomForest(Class ~ ., data=df2, ntree=501, proximity=T, mtry=3)
model2 # this is OOB = 56.94%  for 2001 trees / OOB = 56.94% for 501 trees
model2$importance

# Predict ----
test <- raster::predict(p, model2)

# plot ----
plot(test)

e <- drawExtent()
testx <- crop(test, e)
plot(testx)

# Basic plot using lattice --
lp <- levelplot(testx)
lp

### MODEL 3 ----
### RF - 5 habitat classes ---
# this is using all the habitat classes = 5 in total
# Used all preds except flowdir

model3 <- randomForest(Class ~ ., data=train %>% select(-flowdir) , ntree=501, proximity=TRUE, mtry = 3)
model3 #  OOB = 53.21%
model3$importance


ptest <- p
names(ptest)
ptest <- dropLayer(p, c(9))

## Predict ----

test <- raster::predict(ptest, model)

## Plot ----

plot(test)
e <- drawExtent()
testx <- crop(test, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
class(lp) # trellis


### MODEL 4 ----
### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# Using all preds 
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr

model4 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")), 
                       ntree=501, proximity=TRUE, mtry = 3)
model4 #  OOB = 53.21%
model3$importance


ptest <- p
names(ptest)
ptest <- dropLayer(p, c(9))

## Predict ----

test <- raster::predict(ptest, model)

## Plot ----

plot(test)
e <- drawExtent()
testx <- crop(test, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
class(lp) # trellis



### RF - vegetated vs. unvegetated ----
# try seagrass vs. macroalgae

# Change the names of classes to vegetated and unvegetated
df4 <- df2
levels(df4$Class)
head(df4)

levels(df4$Class)[levels(df4$Class)=="Consolidated"] <- "unvegetated"
levels(df4$Class)[levels(df4$Class)=="Unconsolidated"] <- "unvegetated"


levels(df4$Class)[levels(df4$Class)=="Macroalgae"] <- "vegetated"
levels(df4$Class)[levels(df4$Class)=="Seagrasses"] <- "vegetated"
levels(df4$Class)[levels(df4$Class)=="Turf.algae"] <- "vegetated"
levels(df4$Class)


head(df4)
str(df4)
summary(df4) #17 unvegetated
any(is.na(df4))

length(df4[df4$Class=="unvegetated",])

model3 <- randomForest(Class ~ ., data=df4, ntree=2001, proximity=T, mtry=3)
model3 # this is OOB = 12.5 %
model3$importance


test1 <- raster::predict(p, model3)
plot(test1)

e <- drawExtent()
testx <- crop(test1, e)
plot(testx)

### RF - MA+TURF vs Seagrass ----
# using only 'vegetated' data obviously 


# remove unvegetated --
df5 <- df2
levels(df5$Class)
head(df5)

df5 <- df5[df5$Class!="Consolidated",]
df5 <- df5[df5$Class!="Unconsolidated",]

df5 <- droplevels(df5)
levels(df5$Class)

# change the names of vegetated
levels(df5$Class)[levels(df5$Class)=="Turf.algae"] <- "Macroalgae"
levels(df5$Class)


head(df5)
str(df5)
summary(df5) #58 MA, 69 Seagrass
any(is.na(df5))

model4 <- randomForest(Class ~ ., data=df5, ntree=2001, proximity=T, mtry=3)
model4 # this is OOB = 48.03 %
model4$importance

test <- raster::predict(p, model4)
plot(test)

e <- drawExtent()
testx <- crop(test, e)
plot(testx)
