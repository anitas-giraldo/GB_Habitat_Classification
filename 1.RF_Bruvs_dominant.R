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
library(geoR)
library(gstat)
#library(elsa)
#install.packages("corrplot")
library(corrplot)
library(broman)
library(VSURF)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')


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
scatterplotMatrix(df2[2:10], col = df2$Class)

plot(df2[2:10], col = df2$Class)
legend("center", 
       legend = levels(df2$Class))

## using corrplot ----

# compute correlation matrix --
C <- cor(df2[2:10])
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

#save - it doesnt work
#ggsave(paste(p.dir, "BRUV-fine-corr.png", sep='/'), device = "png", width = 6.23, height = 4.18, dpi = 300)

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


b <- bplot(train$aspect8, train$Class)
plot(b)

### Get train and test data ----
set.seed(777)
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

test <- raster::predict(ptest, model3)

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
                       ntree=1001, proximity=TRUE, mtry = 3)
model4 #  OOB = 53.21%
model4$importance

# Remove predictors if needed --
#ptest <- p
#names(ptest)
#ptest <- dropLayer(p, c(9))

## Predict ----

test <- raster::predict(p, model4)

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


### MODEL 5 ----
### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# Using all preds 
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr

model5 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(-flowdir), 
                       ntree=501, proximity=TRUE, mtry = 3)
model5 #  OOB = 53.21%
model5$importance

# Remove predictors if needed --
ptest <- p
names(ptest)
ptest <- dropLayer(p, c(9))

## Predict ----

test <- raster::predict(ptest, model5)

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


### MODEL 6 and 7 ----
### RF - 3 habitat classes : unvegetated, seagrass, macroalgae ---
# Using all preds 
# to manipulate factors: https://stackoverflow.com/questions/35088812/combine-sets-of-factors-in-a-dataframe-with-dplyr

model6 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(c(Class, depth, slope4, roughness)), 
                       ntree=2001, proximity=TRUE, mtry = 3, importance=TRUE)
model6 #  OOB = 55.05%
model6$importance
varImpPlot(model6)

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

TrainData <- t[,c(2,3,9)]
TrainClasses <- t[,1]

model7 <- caret::train(TrainData, TrainClasses, # Class is a function of the variables we decided to include
               #data = train, # Use the train data frame as the training data
               #preProcess = c("center", "scale"),
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        search = 'random')) # Use 5 folds for cross-validation

model7
model7$finalModel
#model7$importance
v<- varImp(model7, scale =F)
v
varImp(model7)
plot(v, top = 9)

# AREA UNDER THE CURVE --
roc_imp <- filterVarImp(x = train[, 2:10], y = train$Class)
roc_imp


# Remove predictors if needed --
ptest <- p
names(p)
ptest <- dropLayer(p, c(3:7,9))
ptest2 <- dropLayer(p, c(3:5,9))
names(ptest2)

## Predict ----

test <- raster::predict(ptest, model6)

test2 <- raster::predict(ptest, model7)

## Plot ----

plot(test)
#e <- drawExtent()
e <- extent(115.1187, 115.5686 , -33.6169, -33.32534)
testx <- crop(test, e)
plot(testx)

plot(test2)
e <- drawExtent()
test2 <- crop(test2, e)
plot(test2)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
# https://stat.ethz.ch/pipermail/r-sig-geo/2013-March/017893.html

#pick colors --
sg <- brocolors("crayons")["Jungle Green"] # "#78dbe2"
sg <- brocolors("crayons")["Forest Green"] # "#78dbe2"
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
lp2 <- levelplot(testx, col.regions=c(alg, sg, sand), xlab = list("Longitude", fontface = "bold"),
                 ylab = list("Latitude", fontface = "bold"))
# with the gb polygon
lp2 <- levelplot(testx, col.regions=c(alg, sg, sand), xlab = list("Longitude", fontface = "bold"),
              ylab = list("Latitude", fontface = "bold")) + layer(sp.polygons(gb))
lp2
#print(lp2)
trellis.device(device ="png", filename = paste(p.dir, "Bruv-fine.png", sep='/'), width = 1000, height = 670, res = 200)
print(lp2)
dev.off()



#### Model 8 #### 

model8 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(c(Class, depth, slope4, aspect4, tpi, flowdir)), 
                       ntree=2001, proximity=TRUE, mtry = 3, importance=TRUE)
model8 #  OOB = 58.72%
model8$importance
varImpPlot(model8)


### Model 9 #### 
model9 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(c(Class, depth, roughness, aspect4, tpi, flowdir)), 
                       ntree=2001, proximity=TRUE, mtry = 3, importance=TRUE)
model9 #  OOB =  55.05%
model9$importance
varImpPlot(model9)

### Model 10 #### 
model10 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                         select(c(Class, depth, tri, aspect4, tpi, flowdir)), 
                       ntree=2001, proximity=TRUE, mtry = 3, importance=TRUE)
model10 #  OOB =   54.13%
model10$importance
varImpPlot(model10)


### Model 11 #### 
model11 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                          select(c(Class, depth, tri, aspect4, tpi)), 
                        ntree=2001, proximity=TRUE, mtry = 3, importance=TRUE)
model11 #  OOB =   50.46%
model11$importance
varImpPlot(model11)



# predict ----

# Remove predictors if needed --
ptest <- p
names(p)
ptest <- dropLayer(p, c(2,3,5,8,9))
names(ptest)


pred.m11 <- raster::predict(ptest, model11)


#  plot ----

plot(pred.m11)
#e <- drawExtent()
e <- extent(115.1187, 115.5686 , -33.6169, -33.32534)
testx <- crop(pred.m11, e)
plot(testx)

# save prediction ---
writeRaster(testx, paste(o.dir, "GBpred-Fine-Bruvs.tif", sep='/'))

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
# with the CMR polygon
lp2 <- levelplot(testx, col.regions=c(alg, sg, sand), xlab = list("Longitude", fontface = "bold"),
                 ylab = list("Latitude", fontface = "bold")) + layer(sp.polygons(gb))
lp2
#print(lp2)
trellis.device(device ="png", filename = paste(p.dir, "Bruv-fine-CMR.png", sep='/'), width = 1000, height = 670, res = 200)
print(lp2)
dev.off()




### Model 12 #### 
model12 <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                          select(c(Class, tri)), 
                        ntree=2001, proximity=TRUE,  importance=TRUE)
model12 #  OOB =   51.38%
model12$importance
varImpPlot(model12)

# predict ----

# Remove predictors if needed --
ptest <- p
names(p)
ptest <- dropLayer(p, c(1:6,8,9))
names(ptest)


pred.m12 <- raster::predict(ptest, model12)


#  plot ----

plot(pred.m12)
#e <- drawExtent()
e <- extent(115.1187, 115.5686 , -33.6169, -33.32534)
testx <- crop(pred.m12, e)
plot(testx)

# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html
# https://stat.ethz.ch/pipermail/r-sig-geo/2013-March/017893.html

#pick colors --
sg <- brocolors("crayons")["Jungle Green"] # "#78dbe2"
sg <- brocolors("crayons")["Forest Green"] # "#78dbe2"
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
# with the CMR polygon
lp2 <- levelplot(testx, col.regions=c(alg, sg, sand), xlab = list("Longitude", fontface = "bold"),
                 ylab = list("Latitude", fontface = "bold")) + layer(sp.polygons(gb))
lp2
#print(lp2)
trellis.device(device ="png", filename = paste(p.dir, "Bruv-fine-CMR.png", sep='/'), width = 1000, height = 670, res = 200)
print(lp2)
dev.off()





#### Validation set assessment model 6: looking at confusion matrix ----

#prediction_for_table <- raster::predict(model6, test[,-c(1,4:8,10)])
prediction_for_table6 <- raster::predict(model11, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                                          select(c(Class, depth, aspect4, tpi, tri)))
#table(observed=test[,-c(2:10)],  predicted=prediction_for_table)

table(observed=test$Class %>%
        car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
      predicted=prediction_for_table6)

# confusion matrix
caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table6)



# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
install.packages("ROCR")
library(ROCR)


# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(model11,
                                    test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                                      select(c(Class, depth, aspect4, tpi, tri)),
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


# Confusion matrix Model 7 ----

prediction_for_table7 <- raster::predict(model7, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")) %>%
                                          select(c(Class, depth, slope4, roughness)))

caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table7)


#   #   #   #     #     #     #     #

####        VARIOGRAM       ###### this hasn't work yet
# https://stats.idre.ucla.edu/r/faq/how-do-i-generate-a-variogram-for-spatial-data-in-r/
# https://www.aspexit.com/en/implementing-variograms-in-r/
# https://cran.r-project.org/web/packages/elsa/vignettes/elsa.html

df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
names(df)
# rename column 
names(df)[names(df) == "Max_if_2_habitats_have_same"] <- "class"
names(df)

#dataset is a dataframe (a table) with three columns: the longitude (x), the latitude (y) and the variable of interest
# need to convert the classes to numeric
# https://www.researchgate.net/post/What_could_be_the_most_appropriate_approach_for_applying_spatial_interpolation_to_categorical_variables

str(df)
class <- levels(df$class)
class.no <- c("1", "2", "3", "4", "5")

class.df <- cbind(class.no, class)
class.df
class.df <- as.data.frame(class.df)

df2 <- merge(df, class.df, by = "class")
head(df2)
str(df2)
df2$class.no <- as.numeric(df2$class.no)

# transform df in to spatial points data frame ----
coordinates(df2) <- ~longitude+latitude

variog3 <- variogram(class.no~1, df2, cutoff = 0.5, width = 0.02)
plot(variog3)

# fit a semivariogram model to the data ----
v.fit <- fit.variogram(variog1, vgm("Exp"))


v <- variog(df2, max.dist = 0.5)





### # Feature selection using VSURF ----
train

t <- train %>% mutate(Class = car::recode(Class, "c('Unconsolidated', 'Consolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
head(t)

TrainData <- t[,c(2:10)]
TrainClasses <- t[,1]


rf.def <- VSURF(TrainData, TrainClasses)
plot(rf.def)
summary(rf.def) 
rf.def$varselect.pred # [1] 7 : TRI
rf.def$varselect.thres 
head(TrainData) # 7 : TRI
