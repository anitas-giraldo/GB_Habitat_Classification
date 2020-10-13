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
library(fields)
library(ROCR)
library(caret)
library(surfin) # calculate random forest uncertainty
#install.packages("rfinterval")
library(rfinterval)
#install.packages("VSURF")
library(VSURF)
library(geoR)
library(gstat)
#library(elsa)
install.packages("corrplot")
library(corrplot)
library(broman)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir<- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')


### Load data ----
dir(paste(d.dir, "tidy", sep='/'))

df <- read.csv(paste(d.dir, "tidy", "GB_auv_coarse_bathy_filtered_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

p <- stack(paste(s.dir, "predictors_coarse.tif", sep='/'))
namesp <- read.csv(paste(s.dir, "namespredictors.csv", sep='/'))
namesp
names(p) <- namesp[,2]
names(p)
plot(p$depth)
# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb, add=T)

p <- mask(p,gb)

plot(p$depth)
plot(gb, add=T)


## Prepare data ----

# remove unneeded columns ---
names(df)
df2 <- df %>% dplyr::select(Class, depth, slope4, slope8, aspect4, aspect8, tpi, tri, roughness, flowdir)
head(df2)
# change name of class
names(df2)
#colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
df2$Class <- as.factor(df2$Class)
levels(df2$Class) # "total.seagrass" "Turf.algae"     "Unconsolidated"
summary(df2)
head(df2)

## Plot predictors correlations by class -----

# matrix scatterplot of just these 13 variables --
scatterplotMatrix(df2[2:10], col = df2$Class)

plot(df2[2:10], col = df2$Class)
legend("center", 
       legend = levels(df2$Class))


## using corrplot ----

# compute correlation matrix --
C <- cor(df2[2:10], method = "pearson")
head(round(C,2))

# correlogram : visualizing the correlation matrix --
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram#:~:text=Correlogram%20is%20a%20graph%20of%20correlation%20matrix.&text=In%20this%20plot%2C%20correlation%20coefficients,corrplot%20package%20is%20used%20here.
#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients
corrplot(C, method="circle")
corrplot(C, method="pie")
corrplot(C, method="color")
corrplot(C, method="number", type = "upper")
corrplot(C, method="color", type = "lower", order="hclust") #  “hclust” for hierarchical clustering order is used in the following examples

# compute the p-value of correlations --
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(df2[2:10])
head(p.mat[, 1:5])

# customize correlogram --
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(C, method="color", col=col(100),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)



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


mosthighlycorrelated(df2[2:10], 10) # This results in only depth, rough and slope 4 not being correlated above 0.95


## MAKE BETTER PLOT -- TO do still -----






### Get train and test data ----
set.seed(777)
sample <- caTools::sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 74 10
dim(test) # [1] 25 10



# remove class if needed----

train <- train[train$Class != "Other",]
#train <- train[train$Class != "Stony.corals",]
train <- droplevels(train)
summary(train)

#test <- test[test$Class != "Other",]
#test <- test[test$Class != "Stony.corals",]
test <- droplevels(test)
summary(test)

### MODEL 1 ----

levels(train$Class)

### RF - 4 habitat classes ---
# this is using all the habitat classes = 4 in total
# Used all preds
model <- randomForest(Class ~ ., data=train, ntree=1001, proximity=T, mtry=3)
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
# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx$ID <- c('2','1', '3')
xx$value <- c('Turf.algae', 'total.seagrass', 'Unconsolidated')
levels(pred) <- xx


# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(pred)
lp
#class(lp) # trellis

### UP TO HERE ####


# Optimising ntree and mtry ----

# https://github.com/StatQuest/random_forest_demo/blob/master/random_forest_demo.R

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", 'Turf.algae', 'total.seagrass', 'Unconsolidated'), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Turf.algae"], 
          model$err.rate[,"total.seagrass"],
          #model$err.rate[,"Seagrasses"],
          model$err.rate[,"Unconsolidated"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Class ~ ., data=train, mtry=i, ntree=501)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))



model <- randomForest(Class ~ ., 
                      #data=train %>%  mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")),
                      data = train,
                      ntree=501, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))
                      #mtry = 6)

model

## Predict ----
# model -
testp <- raster::predict(ptest, model)


## Plot ----

plot(testp)

#e <- drawExtent()
#testx <- crop(test, e)
lp <- levelplot(testp)
lp

# Validation ----

prediction_for_table1 <- predict(model, test 
                                 # %>% mutate(Class = car::recode(Class, "c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
)
# %>%
# select(c(Class, depth, slope4, roughness)))

caret::confusionMatrix(test$Class, 
                       #%>% car::recode("c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table1)












#### MODEL 2 ----
### RF - 4 habitat classes ---
# Using depth, tri and roughness ---

# remove all the Classes that are not SG or MA
levels(train$Class)

model2 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"))
                         # %>% select(c(Class, depth, tri, roughness)) 
                       , ntree=501, proximity=TRUE)
#model2 <- randomForest(Class ~ ., data=df2, ntree=501, proximity=T, mtry=3)
model2 # this is OOB = 56.94%  for 2001 trees / OOB = 56.94% for 501 trees
model2$importance
model2$classes 
model2$err.rate
model2$predicted
model2$votes
model2$confusion



# Predict ----
ptest <- p
names(ptest)
#ptest <- dropLayer(p, c(2:6,9))

pred <- raster::predict(ptest, model2)


# plot ----
plot(pred)
pred
# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx$ID <- c('1','3', '2')
xx$value <- c('Hard', 'Seagrass', 'Sand')
levels(pred) <- xx

# Basic plot using lattice --
lp <- levelplot(pred)
lp



# Validation ----

prediction_for_table2 <- predict(model2, test 
                                  %>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"))
)
# %>%
# select(c(Class, depth, slope4, roughness)))

caret::confusionMatrix(test$Class 
                       #%>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"),
                       %>% car::recode("'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"),
                       prediction_for_table2)


## Optimising ntree and mtry ----
  
  # https://github.com/StatQuest/random_forest_demo/blob/master/random_forest_demo.R
  
  oob.error.data2 <- data.frame(
    Trees=rep(1:nrow(model2$err.rate), times=4),
    Type=rep(c("OOB", "Hard" ,  "Seagrass"  ,   "Hard"   ), each=nrow(model2$err.rate)),
    Error=c(model2$err.rate[,"OOB"], 
            model2$err.rate[,"Hard"], 
            model2$err.rate[,"Seagrass"],
            model2$err.rate[,"Sand"]))


ggplot(data=oob.error.data2, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:9) {
  temp.model <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"))
                             , mtry=i, ntree=501)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))



model2 <- randomForest(Class ~ ., 
                      data=train %>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'")),
                      #data = train,
                      ntree=501, 
                      proximity=TRUE, 
                      mtry = 3)
                      #mtry=which(oob.values == min(oob.values)))
#mtry = 6)

model2


# Feature selection using VSURF ----

t <- train %>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"))

TrainData <- t[,c(2:10)]
TrainClasses <- t[,1]


rf.def <- VSURF(TrainData, TrainClasses)
plot(rf.def)
summary(rf.def) 
rf.def$varselect.pred # [1] 1 2
head(TrainData) # depth,  slope4






### MODEL 3 ----
### RF - 5 habitat classes ---
# this is using all the habitat classes = 5 in total
# Use only depth and tri

# data --
train3 <- train %>% 
  mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'")) %>% 
  select(c(Class, depth, slope4))

test3 <- train %>% 
  mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'")) %>% 
  select(c(Class, depth, slope4))

model3 <- randomForest(Class ~ ., data=train3 , ntree=501, mtry=1,proximity=TRUE)
model3 
model3$importance


ptest <- p
names(ptest)
ptest <- dropLayer(p, c(3:9))

## Predict ----

pred <- raster::predict(ptest, model3)

## Plot ----

plot(pred)

# fix class levels for plotting --
xx <-levels(pred)[[1]]
xx$ID <- c('1','3', '2')
xx$value <- c('Hard', 'Seagrass', 'Sand')
levels(pred) <- xx

lp <- levelplot(pred)
lp


# Validation ----

prediction_for_table3 <- predict(model3, test3)
                                 #%>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"))
#)
# %>%
# select(c(Class, depth, slope4, roughness)))

caret::confusionMatrix(test3$Class, 
                       #%>% mutate(Class = car::recode(Class, "'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"),
                       #%>% car::recode("'Seagrasses' = 'Seagrass'; c('Consolidated', 'Macroalgae')='Hard'; 'Unconsolidated' ='Sand'"),
                       prediction_for_table3)






### MODEL 4 ----
### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# Using all preds 
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr

model4 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Macroalgae')='Algae'")) %>%
                         select(Class, depth, aspect4, tri, tpi), 
                       ntree=1001, proximity=TRUE, mtry = 3, importance = T)
model4 #  OOB =  15.87%
model4$importance

# Remove predictors if needed --
ptest <- p
names(ptest)
ptest <- dropLayer(p, c(2,3,5,8,9))
names(ptest)

## Predict ----

pred <- raster::predict(ptest, model4)

## Plot ----

plot(pred)
#e <- drawExtent()
#testx <- crop(pred, e)
testx <- pred
plot(testx)



# save prediction ----
writeRaster(testx, paste(o.dir, "GBpred-Coarse-AUV.tif", sep='/'))


##  plot ----

plot(pred)
#e <- drawExtent()
#e <- extent(115.1187, 115.5686 , -33.6169, -33.32534)
#testx <- crop(pred, e)
testx <- pred

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
# https://stat.ethz.ch/pipermail/r-sig-geo/2013-March/017893.html

#pick colors --
#sg <- brocolors("crayons")["Jungle Green"] # "#78dbe2"
#sg <- brocolors("crayons")["Forest Green"] # "#78dbe2"
sg <- brocolors("crayons")["Fern"] # "#78dbe2"
alg <-  brocolors("crayons")["Raw Umber"] # "#1dacd6" 
sand <-  brocolors("crayons")["Unmellow Yellow"] # "#f75394"

# read gb cmr
gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb)


lp <- levelplot(testx, col.regions=c(alg, sg, sand))
lp
class(lp) # trellis

# https://oscarperpinan.github.io/rastervis/FAQ.html

# plot without CMR--
lp2 <- levelplot(testx, col.regions=c(alg, sg, sand), xlab = list("Longitude", fontface = "bold"),
                 
                 ylab = list("Latitude", fontface = "bold"))
lp2
trellis.device(device ="png", filename = paste(p.dir, "AUV-Coarse.png", sep='/'), width = 1000, height = 670, res = 200)
print(lp2)
dev.off()

# with the CMR polygon
lp3 <- levelplot(testx, col.regions=c(alg, sg, sand), xlab = list("Longitude", fontface = "bold"),
                 ylab = list("Latitude", fontface = "bold")) + layer(sp.polygons(gb))
lp3
#print(lp2)
trellis.device(device ="png", filename = paste(p.dir, "AUV-Coarse-CMR.png", sep='/'), width = 1000, height = 670, res = 200)
print(lp3)
dev.off()



#### Validation  model 4  ----

# model 4

#prediction_for_table <- raster::predict(model6, test[,-c(1,4:8,10)])
prediction_for_table4 <- raster::predict(model4, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Macroalgae')='Algae'")) %>%
                                           select(c(Class, depth, aspect4, tri, tpi)))
#table(observed=test[,-c(2:10)],  predicted=prediction_for_table)

table(observed=test$Class %>%
        car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Macroalgae')='Algae'"),
      predicted=prediction_for_table4)

# confusion matrix model 6 ----
caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Macroalgae')='Algae'"),
                       prediction_for_table4)





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



#### Validation  model 4  ----

# model 4

#prediction_for_table <- raster::predict(model6, test[,-c(1,4:8,10)])
prediction_for_table4 <- raster::predict(model4, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae')='Algae'")) %>%
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
