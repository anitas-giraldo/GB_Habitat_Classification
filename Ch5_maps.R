### Better plots 

library(rasterVis)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(raster)
library(rgdal)
library(stats)
library(FactoMineR)
library(factoextra)
library(vegan)
library(ggplot2)
library(dplyr)  # if you load dplyr in here, then rename function will come back with an error
library(tidyr)
library(tidyverse)
library(reshape)
library(reshape2)
library(broman) # for colors
library(RColorBrewer)
library(plotly)

# Palettes
# https://data.library.virginia.edu/setting-up-color-palettes-in-r/
# https://quantdev.ssri.psu.edu/tutorials/generating-custom-color-palette-function-r

####   Bathymetry plot   ####
# https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
# https://pjbartlein.github.io/REarthSysSci/rasterVis01.html

# Set color
colr <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')))

#### open bathymetry raster ----
setwd("H:/Working/WorkingCh4/sdms/sdmv13")
bathy <- raster("H:/Working/WorkingCh4/sdms/sdmv13/SpatialData/bathy80.tif")
plot(bathy)

# Set color
colr 

# set legend breaks
cutpts <- c(0,-10,-20,-30,-40,-50,-60,-70,-80)


### Plot using rasterVIs and function levelplot
bmap <- levelplot(bathy,
                  margin = F,    # suppress marginal graphics
                  pretty = T,
                  colorkey =list(
                    space = "right", # plot legend at right
                    labels = list(at= cutpts, font = 2) # legend ticks and labels
                  ),
                  par.settings=list(
                    axis.line=list(col="black") # color axes and legend outline
                  ),
                  scales = list(draw = T), # to draw lat and lon
                  col.regions = colr,  # colour ramp
                  at=seq(-80,0,len=40)) # colour ramp breaks

bmap


### To use ggplot ----
# It takes a while to plot and save these graphs - BUT MAKES PRETTY PLOTS
# https://ggplot2-book.org/scales.html

# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

# need to convert raster into a data frame
bathydf <- as.data.frame(bathy, xy = TRUE)
str(bathydf)

# Plot

p1 <- ggplot() +
  geom_raster(data = bathydf , aes(x = x, y = y, fill = bathy80)) +
  #scale_fill_brewer(palette = brewer.pal(n = 10, name = 'RdYlBu') 
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  scale_fill_gradientn(colours=c("#313695","#FFFFBF","#A50026"), na.value = "white") +
  labs(x = "Latitude", y = "Longitude", fill = "Depth (m)") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) +
  #coord_quickmap() 
  # add masking layers
  coord_equal() 
#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

p1       

ggsave(paste(w.dir, "Plots", "Bathy_map_ggplot.png", sep='/'), plot = p1, device = "png", scale =1, dpi = 320)

#############
####   Bathy classified  plot   ####

#### open present class raster ----
w.dir <- "E:/Working/WorkingCh4/sdms/sdmv13"
bclass <- raster(paste(w.dir, "refugia", "Bathy_class.tif", sep='/'))



# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

cols <- c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00")

bathycol <- c("cyan", "deepskyblue", "cornflowerblue", "dodgerblue4", "midnightblue") 
bathycol2 <- c("darkorange1", "firebrick1", "gold1", "mediumturquoise", "darkcyan")

# Convert raster into a data frame
bclassdf <- as.data.frame(bclass, xy = TRUE)
str(bclassdf)
bclassdf$Bathy_class <- as.factor(bclassdf$Bathy_class)
levels <- c(1,2,3,4)

#Get desired core colours from brewer
cols0 <- brewer.pal(n=length(levels), name="RdYlBu")
#Derive desired break/legend colours from gradient of selected brewer palette
cols1 <- colorRampPalette(cols0, space="rgb")(length(levels))

labelsb <- c("0-15 m", "15-25 m", "25-50 m", "50-80 m", "")

p6 <- ggplot(data = bclassdf , aes(x = x, y = y)) + coord_equal() +
  geom_raster(aes( fill = Bathy_class)) +
  #scale_fill_brewer(palette = brewer.pal(n = 5, name = 'RdYlBu'))+
  scale_fill_manual(values=c("darkorange1", "gold1", "mediumturquoise", "darkcyan"), na.value = "white", labels = labelsb)+
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  #scale_fill_gradientn(colours=cols1, values = levels, na.value = "white", breaks = levels) +
  #scale_discrete_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00"), na.value = "white", aesthetics ="fill") +
  #scale_fill_brewer(palette = "Greens") +
  #scale_fill_discrete(aesthetics = 'fill') +
  labs(x = "Latitude", y = "Longitude", fill = "Depth range") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) 
#coord_quickmap() 
# add masking layers

#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

p6       

ggsave(paste(w.dir, "Plots", "Bathy_class_ggplot.png", sep='/'), plot = p6, device = "png", scale =1, dpi = 320)


#########################################

####   Present plot   ####

#### open present  raster ----
w.dir <- "E:/Working/WorkingCh4/sdms/sdmv13"
pkelp <- raster(paste(w.dir, "Predicted", "e13.62.img", sep='/'))
plot(pkelp) 


# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

# Convert raster into a data frame
pkelpdf <- as.data.frame(pkelp, xy = TRUE)
str(pkelpdf)

p2 <- ggplot() +
  geom_raster(data = pkelpdf , aes(x = x, y = y, fill = e13.62)) +
  #scale_fill_brewer(palette = brewer.pal(n = 10, name = 'RdYlBu') 
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  scale_fill_gradientn(colours=c("#666666", "#999999", "#CCCCCC", "#FFFF99", "#FFFF00", "#FFCC99", "#FFCC33", "#99FF99", "#66FF66", "#00FF00"), na.value = "white") +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) +
  #coord_quickmap() 
  # add masking layers
  coord_equal() 
#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

p2       

ggsave(paste(w.dir, "Plots", "Present_ggplot.png", sep='/'), plot = p2, device = "png", scale =1, dpi = 320)



####   Present classified  plot   ####

#### open present class raster ----
w.dir <- "E:/Working/WorkingCh4/sdms/sdmv13"
pkelpclass <- raster(paste(w.dir, "refugia", "Present_class.tif", sep='/'))

# USING BASIC R PLOT
plot(pkelpclass, xaxt="none", yaxt="none", xlab="",ylab="", cex.axis=0.75) 
axis(1, seq(111.7, 117.5, 10),las=2, font=2,cex.axis=0.8)
axis(2, seq(0,700,100),las=2, font=2)
mtext(side=1, line=2, "Latitude", col="black", font=2, cex=1)
mtext(side=2, line=3, "Longitude", col="black", font=2, cex=1)


# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

cols <- c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00")

# Convert raster into a data frame
pkelpclassdf <- as.data.frame(pkelpclass, xy = TRUE)
str(pkelpclassdf)
pkelpclassdf$Present_class <- as.factor(pkelpclassdf$Present_class)
levels <- c(1,2,3,4,5)

#Get desired core colours from brewer
cols0 <- brewer.pal(n=length(levels), name="RdYlBu")
#Derive desired break/legend colours from gradient of selected brewer palette
cols1 <- colorRampPalette(cols0, space="rgb")(length(levels))

labelsc <- c("Unsuitable", "Low probability", "Medium probability", "High probability", "Very high probability", "")

p3 <- ggplot(data = pkelpclassdf , aes(x = x, y = y)) + coord_equal() +
  geom_raster(aes( fill = Present_class)) +
  #scale_fill_brewer(palette = brewer.pal(n = 5, name = 'RdYlBu'))+
  scale_fill_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00CC00"), na.value = "white", labels = labelsc)+
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  #scale_fill_gradientn(colours=cols1, values = levels, na.value = "white", breaks = levels) +
  #scale_discrete_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00"), na.value = "white", aesthetics ="fill") +
  #scale_fill_brewer(palette = "Greens") +
  #scale_fill_discrete(aesthetics = 'fill') +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) 
#coord_quickmap() 
# add masking layers

#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

p3       

ggsave(paste(w.dir, "Plots", "Present_class_ggplot.png", sep='/'), plot = p3, device = "png", scale =1, dpi = 320)


#############################################################

####   RCP45 plot   ####

#### open present  raster ----
w.dir <- "E:/Working/WorkingCh4/sdms/sdmv13"
pkelp <- raster(paste(w.dir, "Predicted", "e13.62.img", sep='/'))
plot(pkelp) 


# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

# Convert raster into a data frame
pkelpdf <- as.data.frame(pkelp, xy = TRUE)
str(pkelpdf)

p2 <- ggplot() +
  geom_raster(data = pkelpdf , aes(x = x, y = y, fill = e13.62)) +
  #scale_fill_brewer(palette = brewer.pal(n = 10, name = 'RdYlBu') 
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  scale_fill_gradientn(colours=c("#666666", "#999999", "#CCCCCC", "#FFFF99", "#FFFF00", "#FFCC99", "#FFCC33", "#99FF99", "#66FF66", "#00FF00"), na.value = "white") +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) +
  #coord_quickmap() 
  # add masking layers
  coord_equal() 
#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

p2       

ggsave(paste(w.dir, "Plots", "RCP45_map_ggplot.png", sep='/'), plot = p2, device = "png", scale =1, dpi = 320)



####   RCP45 classified  plot   ####

#### open present class raster ----
w.dir <- "E:/Working/WorkingCh4/sdms/sdmv13"
rcp45class <- raster(paste(w.dir, "refugia", "rcp4.5_class.tif", sep='/'))



# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

cols <- c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00")

# Convert raster into a data frame
rcp45classdf <- as.data.frame(rcp45class, xy = TRUE)
str(rcp45classdf)
rcp45classdf$rcp4.5_class <- as.factor(rcp45classdf$rcp4.5_class)
levels <- c(1,2,3,4,5)

#Get desired core colours from brewer
cols0 <- brewer.pal(n=length(levels), name="RdYlBu")
#Derive desired break/legend colours from gradient of selected brewer palette
cols1 <- colorRampPalette(cols0, space="rgb")(length(levels))

labelsc <- c("Unsuitable", "Low probability", "Medium probability", "High probability", "Very high probability", "")

p4 <- ggplot(data = rcp45classdf , aes(x = x, y = y)) + coord_equal() +
  geom_raster(aes( fill = rcp4.5_class)) +
  #scale_fill_brewer(palette = brewer.pal(n = 5, name = 'RdYlBu'))+
  scale_fill_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00CC00"), na.value = "white", labels = labelsc)+
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  #scale_fill_gradientn(colours=cols1, values = levels, na.value = "white", breaks = levels) +
  #scale_discrete_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00"), na.value = "white", aesthetics ="fill") +
  #scale_fill_brewer(palette = "Greens") +
  #scale_fill_discrete(aesthetics = 'fill') +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) 
#coord_quickmap() 
# add masking layers

#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

p4       

ggsave(paste(w.dir, "Plots", "RCP45class_map_ggplot.png", sep='/'), plot = p4, device = "png", scale =1, dpi = 320)


####   RCP85 classified  plot   ####

#### open present class raster ----
w.dir <- "E:/Working/WorkingCh4/sdms/sdmv13"
rcp85class <- raster(paste(w.dir, "refugia", "rcp8.5_class.tif", sep='/'))



# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

cols <- c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00")

# Convert raster into a data frame
rcp85classdf <- as.data.frame(rcp85class, xy = TRUE)
str(rcp85classdf)
rcp85classdf$rcp8.5_class <- as.factor(rcp85classdf$rcp8.5_class)
levels <- c(1,2,3,4,5)

#Get desired core colours from brewer
cols0 <- brewer.pal(n=length(levels), name="RdYlBu")
#Derive desired break/legend colours from gradient of selected brewer palette
cols1 <- colorRampPalette(cols0, space="rgb")(length(levels))

labelsc <- c("Unsuitable", "Low probability", "Medium probability", "High probability", "Very high probability", "")

p5 <- ggplot(data = rcp85classdf , aes(x = x, y = y)) + coord_equal() +
  geom_raster(aes( fill = rcp8.5_class)) +
  #scale_fill_brewer(palette = brewer.pal(n = 5, name = 'RdYlBu'))+
  scale_fill_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00CC00"), na.value = "white", labels = labelsc)+
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  #scale_fill_gradientn(colours=cols1, values = levels, na.value = "white", breaks = levels) +
  #scale_discrete_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00"), na.value = "white", aesthetics ="fill") +
  #scale_fill_brewer(palette = "Greens") +
  #scale_fill_discrete(aesthetics = 'fill') +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) 
#coord_quickmap() 
# add masking layers

#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

p5       

ggsave(paste(w.dir, "Plots", "RCP85class_map_ggplot.png", sep='/'), plot = p5, device = "png", scale =1, dpi = 320)


#### Probability of kelp only geomorphology ####

topo <- raster("E:/Working/WorkingCh4/sdms/sdmv08/predicted/e8t5.img")
plot(topo)


# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

# Convert raster into a data frame
topodf <- as.data.frame(topo, xy = TRUE)
str(topodf)

pt <- ggplot() +
  geom_raster(data = topodf , aes(x = x, y = y, fill = e8t5)) +
  #scale_fill_brewer(palette = brewer.pal(n = 10, name = 'RdYlBu') 
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  scale_fill_gradientn(colours=c("#666666", "#999999", "#CCCCCC", "#FFFF99", "#FFFF00", "#FFCC99", "#FFCC33", "#99FF99", "#66FF66", "#00FF00"), na.value = "white") +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "white", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) +
  #coord_quickmap() 
  # add masking layers
  coord_equal() 
#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

pt       

ggsave(paste(w.dir, "Plots", "Kelp_geomor_ggplot.png", sep='/'), plot = pt, device = "png", scale =1, dpi = 320)


#### Refugia classified map ----
####     ####

#### open present class raster ----
w.dir <- "H:/Working/WorkingCh4/sdms/sdmv13"
ref <- raster(paste(w.dir, "refugia", "refugia_class.tif", sep='/'))



# Color 
# https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
# https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# https://www.r-bloggers.com/palettes-in-r/

brewer.pal(n = 11, name = "RdYlBu") # to get the hexadecimal color specification

cols <- c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00")

# Convert raster into a data frame
refdf <- as.data.frame(ref, xy = TRUE)
str(refdf)
refdf$refugia_class <- as.factor(refdf$refugia_class)
levels <- c(1,2,3,4,5)

#Get desired core colours from brewer
cols0 <- brewer.pal(n=length(levels), name="RdYlBu")
#Derive desired break/legend colours from gradient of selected brewer palette
cols1 <- colorRampPalette(cols0, space="rgb")(length(levels))

#labelsc <- c("Unsuitable", "Low probability", "Medium probability", "High probability", "Very high probability", "")
labelsc <- c("Unsuitable", "Low probability", "Medium probability", "High probability", "Refugia", "")

pr <- ggplot(data = refdf , aes(x = x, y = y)) + coord_equal() +
  geom_raster(aes( fill = refugia_class)) +
  #scale_fill_brewer(palette = brewer.pal(n = 5, name = 'RdYlBu'))+
  scale_fill_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00CC00"), na.value = "white", labels = labelsc)+
  scale_y_continuous(limits = c(-35.8, -21.7), expand = c(0,0)) +
  scale_x_continuous(limits = c(111.7, 117.5), expand = c(0,0)) +
  #scale_fill_gradientn(colours=cols1, values = levels, na.value = "white", breaks = levels) +
  #scale_discrete_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00"), na.value = "white", aesthetics ="fill") +
  #scale_fill_brewer(palette = "Greens") +
  #scale_fill_discrete(aesthetics = 'fill') +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "black", fill = "white"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) 
#coord_quickmap() 
# add masking layers

#coord_cartesian(xlim = c(111.7, 117.5), ylim = c(-36.2, -21.7), clip = "on")

pr       

ggsave(paste(w.dir, "Plots", "ref_class_map.png", sep='/'), plot = pr, device = "png", scale =1, dpi = 320)

#### Refugia classified zoomed map ----
####     ####

#### open present class raster ----
w.dir <- "H:/Working/WorkingCh4/sdms/sdmv13"
ref <- raster(paste(w.dir, "refugia", "refugia_class.tif", sep='/'))
plot(ref)

head(refdf)

cols2 <- c("#999999", "#999999", "#999999", "#999999", "#00FF00")

labelsc <- c("Unsuitable", "Low probability", "Medium probability", "High probability", "Refugia", "")

pr2 <- ggplot(data = refdf , aes(x = x, y = y)) + coord_equal() +
  geom_raster(aes( fill = refugia_class)) +
  #scale_fill_brewer(palette = brewer.pal(n = 5, name = 'RdYlBu'))+
  scale_fill_manual(values=c("#999999", "#999999", "#999999", "#999999", "#00FF00"), na.value = "white", labels = labelsc)+
  scale_y_continuous(limits = c(-32, -27), expand = c(0,0)) +
  scale_x_continuous(limits = c(112.5, 116), expand = c(0,0)) +
  #scale_fill_gradientn(colours=cols1, values = levels, na.value = "white", breaks = levels) +
  #scale_discrete_manual(values=c("#999999", "#FFFF33", "#FFCC33", "#FF9900", "#00FF00"), na.value = "white", aesthetics ="fill") +
  #scale_fill_brewer(palette = "Greens") +
  #scale_fill_discrete(aesthetics = 'fill') +
  labs(x = "Latitude", y = "Longitude", fill = "Probability of kelp") + 
  #xlim(111.7, 117.5) + ylim(-36.2, -21.7) +
  theme(panel.background = element_rect(color = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "black"),
        axis.title.x = element_text(size = 12, face = "bold"), axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) 
#coord_quickmap() 

pr2


### PLot REFUGIA with R base ----

colr <- c("#CCCCCC","#CCCCCC","#CCCCCC","#CCCCCC","#66FF00")

par(mar=c(5,4,5,7)) # set plot limits


plot(ref, 
     legend = F,
     col = colr,
     ylab = "", xlab="",
     xlim = c(113, 116), 
     ylim = c(-32, -27),
     #xaxt="none", yaxt="none"
     cex.lab = 1, cex.axis = 1
)
#axis(side = 1, seq(112,116,2),las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
#axis(side = 2,las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
mtext(side = 1, "Longitude", font = 2, line =3)
mtext(side = 2, "Latitude", font = 2, line =2)
plot(wa, add=T, col = "#ffff99")
text(x=114.6, y= -27.7, "Kalbarri", font =1, cex=0.8)
text(x=114.9, y= -28.1, "Port Gregory", font =2, cex=0.8)
text(x=115.4, y= -30.3, "Jurien", font =2, cex=0.8)
text(x=113.6, y= -27.9, "Abrolhos", font =2, cex=0.8)
text(x=113.6, y= -28.1, "Islands", font =2, cex=0.8)
legend("bottomleft", 
       legend = c("Potential kelp refugia"), 
       col = "#66FF00", 
       pch = c(15), 
       #fill=pal,
       #title = "Zones",
       bty = "n", 
       pt.cex = 1, 
       cex = 0.8, 
       text.width = 0.5,
       text.col = "black", 
       horiz = F , 
       inset = c(0.001, 0.05))

## ZOOM MORE ####

### Plot REFUGIA A with R base ----

colr <- c("#D1D1D1","#D1D1D1","#D1D1D1","#D1D1D1","#008B00")

par(mar=c(5,5,7,2)) # set plot limits


plot(ref, 
     legend = F,
     col = colr,
     ylab = "", xlab="",
     xlim = c(113, 115), 
     ylim = c(-29.5, -27.5),
     #xaxt="none", yaxt="none"
     cex.lab = 1, cex.axis = 1
)
#axis(side = 1, seq(112,116,2),las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
#axis(side = 2,las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
mtext(side = 1, "Longitude", font = 2, line =3)
mtext(side = 2, "Latitude", font = 2, line =2)
plot(wa, xlim = c(113, 116), ylim = c(-29.5, -27), add=T, col = "#ffff99")
text(x=114.4, y= -27.7, "Kalbarri", font =2, cex=0.8)
text(x=114.55, y= -28.1, "Port Gregory", font =2, cex=0.8)
#text(x=115.4, y= -30.3, "Jurien", font =2, cex=0.8)
#text(x=113.6, y= -27.9, "Abrolhos", font =2, cex=0.8)
#text(x=113.6, y= -28.1, "Islands", font =2, cex=0.8)
text(x=113.3, y= -28.4, "Abrolhos", font =2, cex=0.8)
text(x=113.3, y= -28.5, "Islands", font =2, cex=0.8)
legend("bottomleft", 
       legend = c("Potential kelp refugia"), 
       col = "#008B00", 
       pch = c(15), 
       #fill=pal,
       #title = "Zones",
       bty = "n", 
       pt.cex = 1, 
       cex = 0.8, 
       text.width = 0.5,
       text.col = "black", 
       horiz = F , 
       inset = c(0.001, 0.005))


### Plot REFUGIA B with R base ----

colr <- c("#D1D1D1","#D1D1D1","#D1D1D1","#D1D1D1","#008B00")

par(mar=c(5,5,7,2)) # set plot limits


plot(ref, 
     legend = F,
     col = colr,
     ylab = "", xlab="",
     xlim = c(114.7, 116.3), 
     ylim = c(-32.2, -30.5),
     #xaxt="none", yaxt="none"
     cex.lab = 1, cex.axis = 1
)
#axis(side = 1, seq(112,116,2),las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
#axis(side = 2,las=2, font=2,cex.axis=0.8, labels = T, angle = 90)
mtext(side = 1, "Longitude", font = 2, line =3)
mtext(side = 2, "Latitude", font = 2, line =2)
plot(wa, xlim = c(113, 116), ylim = c(-29.5, -27), add=T, col = "#ffff99")
#text(x=114.5, y= -27.7, "Kalbarri", font =2, cex=0.8)
#text(x=114.6, y= -28.1, "Port Gregory", font =2, cex=0.8)
#text(x=115.4, y= -30.3, "Jurien", font =2, cex=0.8)
text(x=115.57, y= -31.05, "Lancelin", font =2, cex=0.8)
text(x=115.84, y= -31.5, "Two Rocks", font =2, cex=0.8)
text(x=116.05, y= -32, "Perth", font =2, cex=0.8)
#text(x=113.3, y= -28.4, "Abrolhos", font =2, cex=0.8)
#text(x=113.3, y= -28.6, "Islands", font =2, cex=0.8)
legend("bottomleft", 
       legend = c("Potential kelp refugia"), 
       col = "#008B00", 
       pch = c(15), 
       #fill=pal,
       #title = "Zones",
       bty = "n", 
       pt.cex = 1, 
       cex = 0.8, 
       text.width = 0.5,
       text.col = "black", 
       horiz = F , 
       inset = c(0.001, 0.005))