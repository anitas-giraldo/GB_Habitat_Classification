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
s.dir <- "Y:/GB_Habitat_Classification/spatial.data"
p.dir <- "Y:/GB_Habitat_Classification/plots"
o.dir <- "Y:/GB_Habitat_Classification/outputs"


### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_presence_absence_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data


## Prepare data ----

# remove unneeded columns ---
names(df)
df2 <- df[,c(5:13,16:17)]
head(df2)

# Currently the data includes presences and absences
## Filter for only presences
str(df2)
df2$presence.absence2 <- factor(df2$presence.absence, levels=c("0", "1"))

# remove the absences --
df2 <- df2[df2$presence.absence2!="0",]
df2 <- droplevels(df2)
str(df2)

# now remove unneeded columns again --
names(df2)
df2 <- df2[,-c(11,12)]
names(df2)
head(df2)
any(is.na(df2))
which(is.na(df2)) 
df3 <- na.omit(df2) # remove NAs
names(df3)
head(df3)
any(is.na(df3))
str(df3)

### RF - 7 habitat classes ----
# this is using all the habitat classes = 7 in total

model <- randomForest(Class ~ ., data=df3, ntree=2001, proximity=TRUE)
model # this is really bad: OOB = 100%


### RF - 2 habitat classes ----
# try seagrass vs. macroalgae

# remove all the Classes that are not SG or MA
levels(df3$Class)

df4 <- df3[df3$Class!="Consolidated",]
df4 <- df4[df4$Class!="Sponges",]
df4 <- df4[df4$Class!="Stony.corals",]
df4 <- df4[df4$Class!="Turf.algae",]
df4 <- df4[df4$Class!="Unconsolidated",]

levels(df4$Class)
df4 <- droplevels(df4)
levels(df4$Class) ## "Macroalgae" "Seagrasses"

head(df4)

model2 <- randomForest(Class ~ ., data=df4, ntree=2001, proximity=T, mtry=3)
model2 # this is really bad: OOB = 100%


### Gonna try using dominant rather than p-a data set ---
### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_presence_absence_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
