############### Plot Habitat Classification Maps ######################

library(raster)
library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
library(sp)
library(rgdal)
library(plyr)
library(maptools)
library(broom)


w.dir <- "//uniwa.uwa.edu.au/userhome/staff1/00093391/Desktop/GitHubAnita/GB_Habitat_Classification"

p.dir <- paste(w.dir, "plots", sep='/')

d.dir <- paste(w.dir, "data", sep='/')

s.dir <- paste(w.dir, "spatial_data", sep ='/')

c.dir <- paste(s.dir, "Class_results", sep ='/')


######### FROM BRUV DATA #####################

### Plot results ----

## Pick the colours --
sand <- brocolors("crayons")["Desert Sand"] # "#efcdb8"
Unclassified <- brocolors("crayons")["Gray"] # "#95918c" 
Green <-  brocolors("crayons")["Sea Green"] # "#93dfb8"
Green1 <-  brocolors("crayons")["Jungle Green"] # "#3bb08f" 
Red <- brocolors("crayons")["Red Orange"] # "#ff5349"
Green2 <-  brocolors("crayons")["Screamin' Green"] # "#76ff7a" 



## Load results for habitat classification ----
# these are the result for habitat classfication using dominant categories and BRUV




## Plot class hab 6 ----

filen <- "Bruv_hab_class_6.tif"

c <- raster(paste(c.dir, filen, sep='/'))


breakpoints <- c(0,1,2,3,4,-999,-1000)
colors <- c("#3bb08f","white","#76ff7a" ,"#efcdb8","#ff5349") # Class overlap, Unclassified, seag, sand, macroalgae, 

plot(c, breaks=breakpoints, col=colors )


## Plot class seagrass 7 ----

peach <- brocolors("crayons")["Peach"] # "#ffcfab"
Unclassified <- brocolors("crayons")["Gray"] # "#95918c" 
Green <-  brocolors("crayons")["Sea Green"] # "#93dfb8"
Green1 <-  brocolors("crayons")["Jungle Green"] # "#3bb08f" 
Red <- brocolors("crayons")["Red Orange"] # "#ff5349"
Green2 <-  brocolors("crayons")["Screamin' Green"] # "#76ff7a" 

filen <- "Bruv_seag_class_7.tif"

c <- raster(paste(c.dir, filen, sep='/'))


breakpoints <- c(0,1,2,3,4,5,6,-999,-1000)

# Class overlap, unclasified, 0% sg, 0-25% sg, 25-50% sg, 50-75% sg, 75-100% sg
colors <- c("red","white","green","brown","dark green","black", "yellow") # Class overlap, Unclassified, seag, sand, macroalgae, 

plot(c, breaks=breakpoints, col=colors )





