############### Plot Habitat Classification Maps ######################

library(raster)
library(broman) # for colors: https://kbroman.files.wordpress.com/2014/05/crayons.png
library(sp)
library(rgdal)
#library(plyr)
library(maptools)
library(broom)
library(rasterVis)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(stats)
library(FactoMineR)
library(factoextra)
library(vegan)
library(dplyr)  # if you load dplyr in here, then rename function will come back with an error
library(tidyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(plotly)


w.dir <- "//uniwa.uwa.edu.au/userhome/staff1/00093391/Desktop/GitHubAnita/GB_Habitat_Classification"

p.dir <- paste(w.dir, "plots", sep='/')

d.dir <- paste(w.dir, "data", sep='/')

s.dir <- paste(w.dir, "spatial_data", sep ='/')

c.dir <- paste(s.dir, "Class_results", sep ='/')



## Read Geo bay shapefile ----

gb <- readOGR(dsn="C:/Users/00093391/Dropbox/UWA/Research Associate/PowAnalysis_for1sParksMeeting/Desktop/shapefiles")
plot(gb)
proj4string(gb) # "+proj=longlat +ellps=GRS80 +no_defs"

######### FROM BRUV DATA #####################

### Plot results ----

## Pick the colours --
sand <- brocolors("crayons")["Desert Sand"] # "#efcdb8"
Unclassified <- brocolors("crayons")["Gray"] # "#95918c" 
Green <-  brocolors("crayons")["Sea Green"] # "#93dfb8"
Green1 <-  brocolors("crayons")["Jungle Green"] # "#3bb08f" 
Red <- brocolors("crayons")["Red Orange"] # "#ff5349"
Green2 <-  brocolors("crayons")["Screamin' Green"] # "#76ff7a" 



###### Classification of different habitat types #######

## Load results for habitat classification ----
# these are the result for habitat classfication using dominant categories and BRUV


## Plot class hab 6 ----

filen <- "Bruv_hab_class_6.tif"

c <- raster(paste(c.dir, filen, sep='/'))


breakpoints <- c(0,1,2,3,4,-999,-1000)
colors <- c("#3bb08f","white","#76ff7a" ,"#efcdb8","#ff5349") # Class overlap, Unclassified, seag, sand, macroalgae, 

plot(c, breaks=breakpoints, col=colors )



### plot using BASE ----

colr <- c("#3bb08f","white","#76ff7a" ,"#efcdb8","#ff5349")

par(mar=c(5,4,5,7)) # set plot limits

breakpoints <- c(0,1,2,3,4,-999,-1000)

# # # # #   When ready to save: --
# 1. Open png file :
#png(paste(p.dir, "GB_hab_class_sup_bruv.png", sep='/'), res = 300, width = 5, height = 5, units = "in")

# 2. Create plot
plot(c, 
     breaks = breakpoints,
     legend = F,
     col = colr,
     main = "Habitat classification - Stereo-BRUV",
     ylab = "", xlab="",
     xlim = c(114.9879, 115.6473), 
     ylim = c(-33.66383, -33.26156),
     #xaxt="none", yaxt="none"
     cex.lab = 1, cex.axis = 1
)
#axis(side = 1, seq(112,116,2),las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
#axis(side = 2,las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
mtext(side = 1, "Longitude", font = 2, line =3)
mtext(side = 2, "Latitude", font = 2, line =2)
plot(gb, add=T)
#text(x=114.6, y= -27.7, "Kalbarri", font =1, cex=0.8)
#text(x=114.9, y= -28.1, "Port Gregory", font =2, cex=0.8)
#text(x=115.4, y= -30.3, "Jurien", font =2, cex=0.8)
legend("left",
  #x = 115.44, y = -33.55, 
       legend = c("Mixed", "Unclassified", "Seagrass", "Sand", "Macroalgae"), 
       #inset = c(0.5,0.5),
       #col = "black",
       fill = colr,
       #pch = c(0), 
       #fill=pal,
       #title = "Zones",
       bty = "n", 
       pt.cex = 1, 
       cex = 0.7, 
       text.width = 0.5,
       text.col = "black", 
       horiz = F,
  inset = c(1,0),
  xpd = TRUE)
       #inset = 0.02)


# 3. Close the file
dev.off()




######## Classification of seagrass percent cover ##########



## Plot class seagrass 7 ----

overlap <- brocolors("crayons")["Aquamarine"] # "#1cac78"
unclass <- brocolors("crayons")["White"] # "#FFFFFF"
zero <-  brocolors("crayons")["Yellow"] # "#ffff99"
sg25 <-  brocolors("crayons")["Inchworm"] # "#b2ec5d"
sg50 <- brocolors("crayons")["Green"] # "#78dbe2"
sg75 <-  brocolors("crayons")["Blue"] # "#1dacd6" 
sg100 <-  brocolors("crayons")["Violet Red"] # "#f75394"

filen <- "Bruv_seag_class_7.tif"

c <- raster(paste(c.dir, filen, sep='/'))


breakpoints <- c(0,1,2,3,4,5,6,-999,-1000)

# Class overlap, unclasified, 0% sg, 0-25% sg, 25-50% sg, 50-75% sg, 75-100% sg
#colors <- c("red","white","green","brown","dark green","black", "yellow") # Class overlap, Unclassified, seag, sand, macroalgae, 

colors2 <- c(overlap, unclass, zero, sg25, sg50, sg75, sg100)

plot(c, breaks=breakpoints, col=colors2 )


### plot using BASE ----

colors2 <- c(overlap, unclass, zero, sg25, sg50, sg75, sg100)

#par(mar=c(5,4,5,7)) # set plot limits

breakpoints <- c(0,1,2,3,4,-999,-1000)

# https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics

# # # # #   When ready to save: --
# 1. Open png file :
#png(paste(p.dir, "GB_sgCover_class_sup_bruv.png", sep='/'), res = 300, width = 5, height = 5, units = "in")

# 2. Create plot
plot(c, 
     breaks = breakpoints,
     legend = F,
     col = colors2,
     main = "Seagrass cover classification - Stereo-BRUV",
     ylab = "", xlab="",
     xlim = c(114.9879, 115.6473), 
     ylim = c(-33.66383, -33.26156),
     #xaxt="none", yaxt="none"
     cex.lab = 1, cex.axis = 1
)
#axis(side = 1, seq(112,116,2),las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
#axis(side = 2,las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
mtext(side = 1, "Longitude", font = 2, line =3)
mtext(side = 2, "Latitude", font = 2, line =2)
plot(gb, add=T)
#text(x=114.6, y= -27.7, "Kalbarri", font =1, cex=0.8)
#text(x=114.9, y= -28.1, "Port Gregory", font =2, cex=0.8)
#text(x=115.4, y= -30.3, "Jurien", font =2, cex=0.8)
legend("left",
  #x = 115.44, y = -33.55, 
       legend = c("Mixed", "Unclassified", "0% seagrass", "25% seagrass", 
                  "50% seagrass", "75% seagrass", "100% seagrass"), 
       #inset = c(0.5,0.5),
       #col = "black",
       fill = colors2,
       #pch = c(0), 
       #fill=pal,
       #title = "Zones",
       bty = "n", 
       pt.cex = 1, 
       cex = 0.7, 
       text.width = 0.5,
       text.col = "black", 
       horiz = F,
      inset = c(1,0),
      xpd = TRUE,)


# 3. Close the file
dev.off()



