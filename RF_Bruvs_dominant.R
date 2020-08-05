########################################################

###### Script --  Random Forest - BRUVs data - v1.0  ##############   


### Load libraries ----

library(ggplot2)
library(cowplot)
library(randomForest)
library(sp)
library(rgdal)
library(raster)
library(caTools)
library(reshape2)

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

# reshape df to long --
dfl <- melt(df2, id.vars= "Class", value.name = "Value", variable.name = "Predictor")
head(dfl)

p <- ggplot(dfl, aes(x = Predictor, y = Predictor, color = Class)) +
  geom_point() +
  facet_grid(rows = vars(Predictors), cols = vars(Predictors))

p


### Get train and test data ----

sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 109  10
dim(test) # [1] 35 10


### RF - 5 habitat classes ----
# this is using all the habitat classes = 5 in total

model <- randomForest(Class ~ ., data=train, ntree=501, proximity=TRUE)
model #  OOB = 59.63%
model$importance
model$classes

test <- raster::predict(p, model)
plot(test)

e <- drawExtent()
testx <- crop(test, e)
plot(testx)


### RF - 2 habitat classes ----
# try seagrass vs. macroalgae

# remove all the Classes that are not SG or MA
levels(df2$Class)

df3 <- df2[df2$Class!="Consolidated",]
df3 <- df3[df3$Class!="Turf.algae",]
df3 <- df3[df3$Class!="Unconsolidated",]

levels(df3$Class)
df3 <- droplevels(df3)
levels(df3$Class) ## "Macroalgae" "Seagrasses"

head(df3)
str(df3)
length(df3[df3$Class=="Macroalgae",])

model2 <- randomForest(Class ~ ., data=df3, ntree=2001, proximity=T, mtry=3)
model2 # this is OOB = 4.17 %
model2$importance


test <- raster::predict(p, model2)
plot(test)

e <- drawExtent()
testx <- crop(test, e)
plot(testx)

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
