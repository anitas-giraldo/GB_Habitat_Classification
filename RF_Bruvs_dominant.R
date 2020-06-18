########################################################

###### Script --  Random Forest - BRUVs data - v1.0  ##############   


### Load libraries ----

library(ggplot2)
library(cowplot)
library(randomForest)

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



### RF - 7 habitat classes ----
# this is using all the habitat classes = 7 in total

model <- randomForest(Class ~ ., data=df2, ntree=2001, proximity=TRUE)
model # this is really bad: OOB = 100%


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

test1 <- raster::predict(p, model3)
plot(test1)

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
model4 # this is OOB = 12.5 %

test <- raster::predict(p, model4)
plot(test)
