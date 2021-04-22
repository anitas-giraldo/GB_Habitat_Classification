###  PCA ---

### Load libraries ----

library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggthemes)
library(cowplot)

# Clear memory ----
rm(list=ls())

### Set directories ----
w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "spatial_data", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')
o.dir <- paste(w.dir, "outputs", sep='/')


####   ###   ###   High Resolution Shallow Data    ###   ###   ####


####   BRUV    ####

### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_Bruvs_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

df$Max_if_2_habitats_have_same <- as.factor(df$Max_if_2_habitats_have_same)
str(df)

# choose only columns to use ----
df.active <- df[,c(5:14)]
head(df.active)
summary(df.active)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = 1)

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, col.ind = df.active$Max_if_2_habitats_have_same, addEllipses = F)
fviz_pca_biplot(df.pca, col.ind = df.active$Max_if_2_habitats_have_same, repel = FALSE)

## just main classes ----

df2 <- df.active %>%
  mutate(Class = car::recode(Max_if_2_habitats_have_same, "'Unconsolidated'='Unvegetated'; 'Consolidated'='Unvegetated';'Seagrasses' = 'Seagrass';'Turf.algae'='Algae'"))
head(df2)
str(df2)

df.pca <- PCA(df2, graph = TRUE, quali.sup = c(1,11))

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, col.ind = df2$Class, addEllipses = F)
fviz_pca_biplot(df.pca, col.ind = df2$Class, repel = FALSE)

####   Forward towed-video    ####

### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_tv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

df$Max_if_2_habitats_have_same <- as.factor(df$Max_if_2_habitats_have_same)
str(df)

# choose only columns to use ----
df.active <- df[,c(5:14)]
head(df.active)
summary(df.active)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = 1)

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, col.ind = df.active$Max_if_2_habitats_have_same, addEllipses = F)
fviz_pca_biplot(df.pca, col.ind = df.active$Max_if_2_habitats_have_same, repel = FALSE)


####   AUV    ####

### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_auv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

df$Max_if_2_habitats_have_same <- as.factor(df$Max_if_2_habitats_have_same)
str(df)

df$sample[duplicated(df$sample)]

# choose only columns to use ----
df.active <- df[,c(5:14)]
head(df.active)
summary(df.active)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = 1)

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, col.ind = df.active$Max_if_2_habitats_have_same, addEllipses = F)
fviz_pca_biplot(df.pca, 
                geom.ind = "point", # show points only (nbut not "text")
                col.ind = df.active$Max_if_2_habitats_have_same, 
                repel = FALSE) # avoid text overlapping


####   Downward Towed video    ####

### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_dtv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
which(is.na(df))
df <- na.omit(df)
str(df) # 2663 obs

df$dominant <- as.factor(df$dominant)
str(df)

df$sample[duplicated(df$sample)]

# choose only columns to use ----
df.active <- df[,c(10,14:22)]
head(df.active)
summary(df.active)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = 1)

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, geom.ind = "point", col.ind = df.active$dominant, addEllipses = F)
fviz_pca_biplot(df.pca, 
                geom.ind = "point", # show points only (nbut not "text")
                col.ind = df.active$dominant, 
                repel = FALSE) # avoid text overlapping


########################################################################################

## PCA of habitat ----



####   BRUVs    ####

### Load data ----

df <- read.csv(paste(d.dir, 'raw', 'dominant-stereo-BRUVs_broad.percent.cover.csv', sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data


# choose only columns to use ----
df.active <- df[,c(5:11, 13)]
head(df.active)
str(df.active)
names(df.active)
summary(df.active)

df.active$Max_if_2_habitats_have_same <- as.factor(df.active$Max_if_2_habitats_have_same)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = 8)

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, 
             col.ind = df.active$Max_if_2_habitats_have_same, 
             addEllipses = F)
fviz_pca_biplot(df.pca, 
                geom.ind = "point", # show points only (nbut not "text")
                col.ind = df.active$Max_if_2_habitats_have_same, 
                repel = FALSE) # avoid text overlapping



####   Forward towed-video    ####

### Load data ----

df <- read.csv(paste(d.dir, 'raw', 'dominant-towed_broad.percent.cover.csv', sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data


# choose only columns to use ----
df.active <- df[,c(4:10, 12)]
head(df.active)
str(df.active)
names(df.active)
summary(df.active)

df.active$Max_if_2_habitats_have_same <- as.factor(df.active$Max_if_2_habitats_have_same)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = 8)

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, 
             col.ind = df.active$Max_if_2_habitats_have_same, 
             addEllipses = F)
fviz_pca_biplot(df.pca, 
                geom.ind = "point", # show points only (nbut not "text")
                col.ind = df.active$Max_if_2_habitats_have_same, 
                repel = FALSE) # avoid text overlapping





####   AUV    ####

### Load data ----

df <- read.csv(paste(d.dir,  "GB_auv_coarse_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data

# check for duplicates ----
df$sample[duplicated(df$sample)]

# choose only columns to use ----
df.active <- df[,c(2:10, 13)]
head(df.active)
str(df.active)

df.active$Transect.id <- as.factor(df.active$Transect.id)
df.active$dominant <- as.factor(df.active$dominant)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = c(9,10))

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, 
             col.ind = df.active$dominant, 
             addEllipses = F)
fviz_pca_biplot(df.pca, 
                geom.ind = "point", # show points only (nbut not "text")
                col.ind = df.active$dominant, 
                repel = FALSE) # avoid text overlapping


####   Downward Towed video    ####

### Load data ----

df <- read.csv(paste(d.dir, "tidy", "GB_dtv_fine_bathy_habitat_dominant_broad.csv", sep='/'))
head(df)
str(df) # check the factors and the predictors
any(is.na(df)) # check for NA's in the data
which(is.na(df))
df <- na.omit(df)
str(df) # 2663 obs

# choose only columns to use ----
df.active <- df[,c(2:10, 13)]
head(df.active)
str(df.active)
df.active$Transect.id <- as.factor(df.active$Transect.id)
df.active$dominant <- as.factor(df.active$dominant)


df.pca <- PCA(df.active, graph = TRUE, quali.sup = c(9,10))

ind <- get_pca_ind(df.pca)
fviz_pca_ind(df.pca, 
             col.ind = df.active$dominant, 
             addEllipses = F)
fviz_pca_biplot(df.pca, 
                geom.ind = "point", # show points only (nbut not "text")
                col.ind = df.active$dominant, 
                repel = FALSE) # avoid text overlapping
