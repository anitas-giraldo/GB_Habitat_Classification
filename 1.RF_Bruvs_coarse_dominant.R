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

df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_coarse_bathy_habitat_dominant_broad.csv", sep='/'))
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
df2 <- df[,c(5:14)]
head(df2)
# change name of class
names(df2)
colnames(df2)[colnames(df2)=="Max_if_2_habitats_have_same"] <- "Class"
names(df2)
str(df2)
levels(df2$Class) # 6 Classes: consolidated, macroalgae, seagrasses, sponges, turf and unconsolidated
summary(df2)
head(df2)

# check for NAs

any(is.na(df2))
which(is.na(df2))

# remove Nas if needed ----
df2 <- na.omit(df2)
any(is.na(df2))


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


mosthighlycorrelated(df2[2:10], 9) # This results in only depth, rough and slope 4 not being correlated above 0.95


## MAKE BETTER PLOT -- TO do still -----


b <- bplot(train$aspect8, train$Class)
plot(b)

### Get train and test data ----
set.seed(777)
sample <- sample.split(df2$flowdir, SplitRatio = 0.75)
train <- subset(df2, sample == TRUE)
test  <-subset(df2, sample == FALSE)
dim(train) # [1] 245  10
dim(test) # [1] 79 10

summary(df2)

# remove 'consolidated', 'sponges'classes ----

train <- train[train$Class != "Sponges",]
train <- train[train$Class != "Consolidated",]
train <- droplevels(train)
summary(train)
levels(train$Class)

test <- test[test$Class != "Sponges",]
test <- test[test$Class != "Consolidated",]
test <- droplevels(test)
summary(test)
levels(test$Class)

### MODEL 1 ----
### RF - 3 habitat classes ---
# unconsolidated, seagrasses and algae (turf + macroalgae)
# this is using all the habitat classes = 5 in total
# Used only the preds that were not correlated: depth, slope4, roughness

model <- randomForest(Class ~ ., data=train %>%  mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
                     # %>% select(c(Class, depth, slope4, roughness)) 
                      , ntree=1001, proximity=TRUE)
model #  OOB = 47.33%
model$importance
model$classes

ptest <- p
names(ptest)


## Predict ----

test <- raster::predict(ptest, model)

## Plot ----

plot(test)
#e <- drawExtent()
#testx <- crop(test, e)


# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(test)
lp
class(lp) # trellis


# Optimising ntree and mtry ----

# https://github.com/StatQuest/random_forest_demo/blob/master/random_forest_demo.R

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=4),
  Type=rep(c("OOB", "Unvegetated", "Seagrasses","Algae"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Unvegetated"], 
          model$err.rate[,"Seagrass"],
          model$err.rate[,"Algae"]))


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
                      data=train %>%  mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")),
                      ntree=1001, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))

model

## Predict ----

test <- raster::predict(ptest, model)

## Plot ----

plot(test)
#e <- drawExtent()
#testx <- crop(test, e)


# basic plot using lattice --
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

lp <- levelplot(test)
lp
class(lp) # trellis

# Validation ----

prediction_for_table1 <- predict(model, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")))
                                        # %>%
                                           # select(c(Class, depth, slope4, roughness)))

caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table1)


# Feature selection using VSURF ----

t <- train %>% mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
head(t)

TrainData <- t[,c(2:10)]
TrainClasses <- t[,1]


rf.def <- VSURF(TrainData, TrainClasses)
plot(rf.def)
summary(rf.def) 
rf.def$varselect.pred # [1] 1 7 4 2 6
head(TrainData) # depth, tri, aspect4, slope4, tpi






#### MODEL 2 ----
### RF - 3 habitat classes ---
# try using all predictors ---

# remove all the Classes that are not SG or MA
levels(df2$Class)

model2 <- randomForest(Class ~ ., data=train %>%  mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
                       %>% select(c(Class, depth, slope4, tpi)) 
                       , ntree=1001, mtry=3, proximity=TRUE)
model2 # this is OOB = 56.94%  for 2001 trees / OOB = 56.94% for 501 trees
model2$importance

# Predict ----
names(p)
ptest <- dropLayer(p, c(2,3,5,7:9))
names(ptest)
test <- raster::predict(ptest, model2)

# plot ----
plot(test)

#e <- drawExtent()
testx <- test
plot(testx)

# Basic plot using lattice --
lp <- levelplot(testx)
lp


# Validation ----

prediction_for_table2 <- predict(model2, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")))
# %>%
# select(c(Class, depth, slope4, roughness)))

caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table2)



# Optimising ntree and mtry ----

# https://github.com/StatQuest/random_forest_demo/blob/master/random_forest_demo.R

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model2$err.rate), times=4),
  Type=rep(c("OOB", "Unvegetated", "Seagrasses","Algae"), each=nrow(model$err.rate)),
  Error=c(model2$err.rate[,"OOB"], 
          model2$err.rate[,"Unvegetated"], 
          model2$err.rate[,"Seagrass"],
          model2$err.rate[,"Algae"]))


ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(Class ~ ., data=train %>% mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
                                                              %>% select(c(Class, depth, aspect4, tpi)), 
                             mtry=i, ntree=1001)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))




#### MODEL 3 ----
# Like model two with optimized mtry and ntree --


model3 <- randomForest(Class ~ ., data=train %>%  mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
                       %>% select(c(Class, depth, aspect4, tpi)) 
                       , ntree=1001, mtry=2, proximity=TRUE, importance = TRUE)

model3
model3$importance

## Predict ----

names(p)
ptest <- dropLayer(p, c(2,3,5,7:9))
names(ptest)
pred <- raster::predict(ptest, model3)

#  plot ----

plot(pred)
#e <- drawExtent()
#e <- extent(115.1187, 115.5686 , -33.6169, -33.32534)
#testx <- crop(pred, e)
testx <- pred


# save prediction ---
writeRaster(testx, paste(o.dir, "GBpred-Coarse-Bruvs.tif", sep='/'))


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
trellis.device(device ="png", filename = paste(p.dir, "BRUV-Coarse.png", sep='/'), width = 1000, height = 670, res = 200)
print(lp2)
dev.off()

# with the CMR polygon
lp3 <- levelplot(testx, col.regions=c(alg, sg, sand), xlab = list("Longitude", fontface = "bold"),
                 ylab = list("Latitude", fontface = "bold")) + layer(sp.polygons(gb))
lp3
#print(lp2)
trellis.device(device ="png", filename = paste(p.dir, "BRUV-Coarse-CMR.png", sep='/'), width = 1000, height = 670, res = 200)
print(lp3)
dev.off()

# Validation ----

prediction_for_table3 <- predict(model3, test %>% mutate(Class = car::recode(Class, "c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'")))
# %>%
# select(c(Class, depth, slope4, roughness)))

caret::confusionMatrix(test$Class %>%
                         car::recode("c('Unconsolidated')='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"),
                       prediction_for_table3)


## Now let's create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model3$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=t$Class)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
# ggsave(file="random_forest_mds_plot.pdf")




# Feature selection using VSURF ----

t <- train %>% mutate(Class = car::recode(Class, "'Unconsolidated'='Unvegetated';'Seagrasses' = 'Seagrass'; c('Turf.algae','Macroalgae')='Algae'"))
head(t)

TrainData <- t[,c(2:10)]
TrainClasses <- t[,1]


rf.def <- VSURF(TrainData, TrainClasses)
plot(rf.def)
summary(rf.def) 
rf.def$varselect.pred # [1] 1 7 4 2 6
head(TrainData) # depth, tri, aspect4, slope4, tpi




## Now let's create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=t$Class)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
# ggsave(file="random_forest_mds_plot.pdf")




