########################################################

###### Random Forest - BRUVs data - v1.0  ###########


### Load libraries --

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

