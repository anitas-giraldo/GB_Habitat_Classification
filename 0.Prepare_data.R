#################################################

############ Prepare data for RF ################

### Load libraries --

library(ggplot2)
library(cowplot)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- "Y:/GB_Habitat_Classification"
d.dir <- "Y:/GB_Habitat_Classification/data"
s.dir <- "Y:/GB_Habitat_Classification/spatial.data"
p.dir <- "Y:/GB_Habitat_Classification/plots"
o.dir <- "Y:/GB_Habitat_Classification/outputs"


### Load data ----

# Habitat data --
df <- read.csv(paste(d.dir, "tidy", "GBonly_Bruv_broad_dominant.csv", sep='/'))
str(df) # 7 categories