###### Script --  CLUSTER ANALYSIS - BRUVs data -   ##############  

# https://www.statmethods.net/advstats/cluster.html
# https://uc-r.github.io/kmeans_clustering


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
library(factoextra)
library(gridExtra)


# Clear memory ----
rm(list=ls())

### Set directories ----
m.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "Y:/GB_Habitat_Classification"
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(m.dir, "plots", sep='/')
o.dir <- paste(m.dir, "outputs", sep='/')


### Load data ----

df <- read.csv(paste(d.dir, "stereo-BRUVs_broad.percent.cover.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
