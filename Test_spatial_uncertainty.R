library(GSIF)
library(geoR)
library(ranger)
demo(meuse, echo=FALSE)


# https://d3amtssd1tejdt.cloudfront.net/2018/26693/1/GeoMLA_README_thengl.pdf




#We can derive buffer distance by using:
grid.dist0 <- GSIF::buffer.dist(meuse["zinc"], meuse.grid[1], as.factor(1:nrow(meuse)))

meuse.grid$SW_occurrence = readGDAL("./RF_vs_kriging/data/meuse/Meuse_GlobalSurfaceWater_occurrence.tif")$b

class(meuse.grid)

grids.spc <-  GSIF::spc(meuse.grid, as.formula("~ SW_occurrence + AHN + ffreq + dist"))


meuse.grid@data
grid.dist0@data


meuse.grid@data$soil
str(meuse.grid)

#Spatial prediction of categorical variable using ranger belongs to classification problems. The target variable contains multiple
#states (3 in this case), but the model follows still the same formulation:

fm.s = as.formula(paste("soil ~ ", paste(names(grid.dist0), collapse="+"), " + SW_occurrence + dist"))
fm.s

#to produce probability maps per soil class, we need to turn the probability=TRUE option:

rm.s <- do.call(cbind, list(meuse@data["soil"], over(meuse["soil"], meuse.grid), over(meuse["soil"], grid.dist0)))
m.s <- ranger(fm.s, rm.s, mtry=22, num.trees=150, seed=1, probability=TRUE, keep.inbag=TRUE)
m.s                           
                                                                                      


###########

# using ranger with towed Video ---

prednames <- names(p)

# set model formula
mod_formula <- as.formula(paste("Class~", paste(names(p), collapse = '+')))

rangermodel1 <- ranger(mod_formula, train, mtry = 3, num.trees = 501, seed = 1, probability = TRUE, oob.error = T, keep.inbag=TRUE)

# OOB  number is rather abstract so we can
# also check what is the actual classification accuracy using hard classes:
m.s0 <- ranger(mod_formula, train, mtry = 3, num.trees = 501, seed = 1)
m.s0

# produce prediction probabilities per class ----
str(pdf) # 42385 obs
pdf2 <- na.omit(pdf)
str(pdf2) # 32346 obs
any(is.na(pdf2))

pred1 <-  predict(rangermodel1, pdf2, type="se")
pred1$predictions
pred1$se

predsall <- cbind(pdf2, pred1$predictions, pred1$se)
head(predsall)
names(predsall) <- c( "x"  , "y","depth" , "slope4", "slope8", "aspect4", "aspect8", "tpi" , "tri", "roughness","flowdir"  
                      ,"Seagrasses","Turf.algae", "Unconsolidated", "Seagrasses_se","Turf.algae_se" , "Unconsolidated_se")

head(predsall)
class(predsall)
names(predsall)
# Plot seagrass predictions
seag <- predsall[,c(1,2,12)]
coordinates(seag) <- ~x+y
gridded(seag) <- TRUE
seagr <- raster(seag)
plot(seagr)
# Plot seagrass se predictions
seagse <- predsall[,c(1,2,15)]
coordinates(seagse) <- ~x+y
gridded(seagse) <- TRUE
seagser1 <- raster(seagse)
plot(seagser1)

# Plot algae predictions
seag <- predsall[,c(1,2,13)]
coordinates(seag) <- ~x+y
gridded(seag) <- TRUE
seagr <- raster(seag)
plot(seagr)
# Plot algae se predictions
seagse <- predsall[,c(1,2,16)]
coordinates(seagse) <- ~x+y
gridded(seagse) <- TRUE
seagser2 <- raster(seagse)
plot(seagser2)

# Plot algae predictions
seag <- predsall[,c(1,2,14)]
coordinates(seag) <- ~x+y
gridded(seag) <- TRUE
seagr <- raster(seag)
plot(seagr)
# Plot algae se predictions
seagse <- predsall[,c(1,2,17)]
coordinates(seagse) <- ~x+y
gridded(seagse) <- TRUE
seagser3 <- raster(seagse)
plot(seagser3)


## SUM SE?

SumSE <- (seagser1 + seagser2 + seagser3)
plot(SumSE)
