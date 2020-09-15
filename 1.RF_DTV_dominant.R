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
library(dplyr)
library(car)
library(lattice)
library(dplyr)
library(raster)
library(rasterVis)
library(zoo)
library(sf)
library(fields)
library(ROCR)
library(caret)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')


### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_dtv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
which(is.na(df))
df <- na.omit(df)
str(df) # 2663 obs

p <- stack(paste(s.dir, "predictors.tif", sep='/'))
namesp <- read.csv(paste(s.dir, "namespredictors.csv", sep='/'))
namesp
names(p) <- namesp[,2]
names(p)


## Prepare data ----

# remove unneeded columns ---
names(df)
df2 <- df[,c(10, 14:22)] # remove X, sample, lat, long
head(df2)
# change name of class
names(df2)
colnames(df2)[colnames(df2)=="dominant"] <- "Class"
names(df2)
str(df2)
df2$Class <- as.factor(df2$Class)
levels(df2$Class) # "Consolidated"   "Macroalgae"     "total.seagrass" "Turf.algae"     "Unconsolidated"  
summary(df2)
head(df2)

## Plot predictors correlations by class -----

# matrix scatterplot of just these 13 variables --
scatterplotMatrix(df2[2:10], col = df2$Class)

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


mosthighlycorrelated(df2[2:10], 20) # This results in only depth, rough and slope 4 not being correlated above 0.95


## MAKE BETTER PLOT -- TO do still -----






### Get train and test data ----

sample <- caTools::sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1]  1997   10
dim(test) # [1] 666  10



# remove classes if needed classes ----

#train <- train[train$Class != "Other",]
#train <- train[train$Class != "Stony.corals",]

train <- droplevels(train)
summary(train)
levels(train$Class)

#test <- test[test$Class != "Other",]
#test <- test[test$Class != "Stony.corals",]
test <- droplevels(test)
summary(test)

### MODEL 1 ----



### RF - 5 habitat classes ---
# this is using all the habitat classes = 5 in total
# Used only the preds that were not correlated: depth, tri, roughness
model <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'total.seagrass' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")), 
                      ntree=1001, proximity=TRUE, mtry = 3)
#model <- randomForest(Class ~ ., data=train %>% select(c(Class, depth, tri, roughness)) , ntree=501, proximity=TRUE)
model #  OOB =  41.63%
model$importance
model$classes

ptest <- p
names(ptest)
#ptest <- dropLayer(p, c(3:7,9))

## Predict ----

pred <- raster::predict(ptest, model)

## Plot ----

plot(pred)
e <- drawExtent()
testx <- crop(pred, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
#class(lp) # trellis



#### MODEL 2 ----
### RF - 5 habitat classes ---
# Using depth, tri and roughness ---

# remove all the Classes that are not SG or MA
levels(train$Class)
model2 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'total.seagrass' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(c(Class, depth, aspect4, slope4)), 
                      ntree=1001, proximity=TRUE, mtry = 3)
#model2 <- randomForest(Class ~ ., data=train %>% select(c(Class, depth, tri, roughness)) , ntree=501, proximity=TRUE)
#model2 <- randomForest(Class ~ ., data=df2, ntree=501, proximity=T, mtry=3)
model2 # this is OOB = 56.94%  for 2001 trees / OOB = 56.94% for 501 trees
model2$importance

# Predict ----
ptest <- p
names(ptest)
ptest <- dropLayer(p, c(3,5:9))

pred <- raster::predict(ptest, model2)

# plot ----
plot(pred)

#e <- drawExtent()
testx <- crop(pred, e)
plot(testx)

# Basic plot using lattice --
lp <- levelplot(testx)
lp


### UP TO HERE ####


### MODEL 3 ----
### RF - 5 habitat classes ---
# this is using all the habitat classes = 5 in total
# Use only depth and tri

model3 <- randomForest(Class ~ ., data=train %>% select(c(Class, depth, tri)) , ntree=501, proximity=TRUE)
model3 #  OOB = 53.21%
model3$importance


ptest <- p
names(ptest)
ptest <- dropLayer(p, c(2:6,8:9))

## Predict ----

pred <- raster::predict(ptest, model3)

## Plot ----

plot(pred)
e <- drawExtent()
testx <- crop(pred, e)
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
                       ntree=1001, proximity=TRUE, mtry = 3)
model4 #  OOB = 53.21%
model4$importance

# Remove predictors if needed --
#ptest <- p
#names(ptest)
#ptest <- dropLayer(p, c(9))

## Predict ----

pred <- raster::predict(p, model4)

## Plot ----

plot(pred)
e <- drawExtent()
testx <- crop(pred, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
#class(lp) # trellis


### MODEL 5 ----
### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# use all preds except flowdir
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr

model5 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(-flowdir), 
                       ntree=501, proximity=TRUE, mtry = 3)
model5 #  OOB = 40.33%
model5$importance

# Remove predictors if needed --
ptest <- p
names(ptest)
ptest <- dropLayer(p, c(9))

## Predict ----

pred <- raster::predict(ptest, model5)

## Plot ----

plot(pred)
e <- drawExtent()
testx <- crop(pred, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
#class(lp) # trellis



### MODEL 6 ----
### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# use depth, aspect 4, tri and tpi
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr

model6 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(Class, depth, aspect4, tri, tpi), 
                       ntree=501, proximity=TRUE, mtry = 3)
model6 #  OOB = 40.33%
model6$importance

# Remove predictors if needed --
ptest <- p
names(ptest)
ptest <- dropLayer(p, c(2:3,5,8,9))

## Predict ----

pred <- raster::predict(ptest, model6)

## Plot ----

plot(pred)
e <- drawExtent()
testx <- crop(pred, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
#class(lp) # trellis



### MODEL 7 ----
### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# use depth, aspect 4, tri and tpi
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr

model7 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(Class, depth, aspect4, tpi, roughness), 
                       ntree=501, proximity=TRUE, mtry = 3)
model7 #  OOB = 39.84%
model7$importance

# Remove predictors if needed --
ptest <- p
names(ptest)
ptest <- dropLayer(p, c(2:3,5,7,9))

## Predict ----

pred <- raster::predict(ptest, model7)

## Plot ----

plot(pred)
e <- drawExtent()
testx <- crop(pred, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp
#class(lp) # trellis



### MODEL 8 ----
# like model 6 but using the caret package

### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# Using all preds 
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr


# using different code --
# https://www.edureka.co/blog/random-forest-classifier/

# Training using ‘random forest’ algorithm
# Converting ‘Survived’ to a factor
train$Class <- factor(train$Class)
# Set a random seed
set.seed(51)

# had to use less classes, otherwise it wouldn't run,  I think because not enough replicates per class
t=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
head(t)

TrainData <- t[,c(2,5,7,8)]
TrainClasses <- t[,1]

model8 <- caret::train(TrainData, TrainClasses, # Class is a function of the variables we decided to include
               #data = train, # Use the train data frame as the training data
               #preProcess = c("center", "scale"),
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        search = 'random')) # Use 5 folds for cross-validation

model8
model8$finalModel
model8
#model7$importance
v<- varImp(model8, scale =F)
v
varImp(model8)
plot(v, top = 9)

# AREA UNDER THE CURVE --
roc_imp <- filterVarImp(x = train[, 2:10], y = train$Class)
roc_imp


# Remove predictors if needed --
ptest <- p
names(p)
ptest <- dropLayer(p, c(2,3,5,8,9))
names(ptest)
#ptest2 <- dropLayer(p, c(3:5,9))
#names(ptest2)

## Predict ----

pred <- raster::predict(ptest, model8)
pred2 <- raster::predict(ptest, model6)

## Plot ----

plot(pred)
e <- drawExtent()
testx <- crop(pred, e)
plot(testx)

plot(pred2)
e <- drawExtent()
testx2 <- crop(pred2, e)
plot(testx2)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(testx)
lp

lp2 <- levelplot(testx2)
lp2



#### Validation  model 6 and 8: looking at confusion matrix ----

# model 6

#prediction_for_table <- raster::predict(model6, test[,-c(1,4:8,10)])
prediction_for_table6 <- raster::predict(model6, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                                          select(c(Class, depth, aspect4, tri, tpi)))
#table(observed=test[,-c(2:10)],  predicted=prediction_for_table)

table(observed=test$Class %>%
        car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
      predicted=prediction_for_table6)

# confusion matrix model 6 ----
caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table6)



# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
install.packages("ROCR")
library(ROCR)


# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(model6,
                                    test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                                      select(c(Class, depth, aspect4, tri, tpi)),
                                    type="prob")

# Plot ROC curve ----
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(test$Class %>%
                    car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(test$Class %>%
                          car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}


# model 8 --

#prediction_for_table <- raster::predict(model6, test[,-c(1,4:8,10)])
prediction_for_table8 <- raster::predict(model8, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                                           select(c(Class, depth, aspect4, tri, tpi)))
#table(observed=test[,-c(2:10)],  predicted=prediction_for_table)

table(observed=test$Class %>%
        car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
      predicted=prediction_for_table8)

# confusion matrix model 8 ----
caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table8)



# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
install.packages("ROCR")
library(ROCR)


# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(model8,
                                    test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                                      select(c(Class, depth, aspect4, tri, tpi)),
                                    type="prob")

# Plot ROC curve ----
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(test$Class %>%
                    car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(test$Class %>%
                          car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
