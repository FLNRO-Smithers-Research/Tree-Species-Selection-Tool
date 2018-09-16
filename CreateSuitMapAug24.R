###Kiri Daust, Nov 2017###
###This code imports a lat/long grid and a tree suitibility grid###
###and creates a map of suitibility by BGC###

.libPaths("E:/R packages351")
rm(list=ls())

library(scales)
library(MASS)   
library(stats)
library(rgl)
library(RColorBrewer)
library(FNN)
library(igraph)
library(raster)
library(maps)
library(mapdata)
library(maptools)
library(sp) 
library(colorRamps)
library(rgeos)
library(rgdal)
library(foreign)
library(randomForest)
require(tcltk)
library(bindr)
library(lattice)
library(plyr)
library(ggplot2)
library(rasterVis)
library(GISTools)
library(data.table)

wd=tk_choose.dir()
setwd(wd)

changeNames <- function(x, old, new){
  result <- vector("numeric", length(x))
  for (i in 1:length(x)){
    code <- x[i]
    index <- match(code, old)
    result[i] <- new[index]
  }
  return(result)
}


###read in file with suitability by species and year

points <- read.csv("MappingTest.csv") ##output file from grid point submitted to CCISS.Predict. working of old and new species suitability
points <- points[grep("01|101", points$SSCurrent),] ###Use zonal SS
points$NewSuit <- round(points$NewSuit, digits = 0) ###can't have decimals
years <- c(2025,2055,2085)
CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")

###Read shape file outline of BC or choose other base layer
boundary <- readOGR(dsn = "Shapefiles", layer = "BulkleyTSA_BGC_Clipped")
for(k in 1:3){##for each time period
  SuitTable <- points[points$FuturePeriod == years[k],]
  treeList <- "Sx"
  #treeList <- as.character(unique(SuitTable$Spp)) ###species to loop through
  ###loop to create maps for each species in treeList###
    for(j in 1:length(treeList)){
    SuitShort <- subset(SuitTable, SuitTable$Spp == treeList[j]) ##Subset table for one tree species
    
    PredSuit <- SuitShort[,c("Latitude","Longitude","NewSuit")]
    colnames(PredSuit)[3] <- "FutureSuit"
    OrigSuit <- SuitShort[,c("Latitude","Longitude","Suitability")]
    colnames(OrigSuit)[3] <- "CurrentSuit"
    PredSuit$ColCode <- changeNames(PredSuit$FutureSuit, old = c(1,2,3,4),new = c("green","yellow","orangered",NA))
    OrigSuit$ColCode <- changeNames(OrigSuit$CurrentSuit, old = c(1,2,3,4),new = c("green","yellow","orangered",NA))
  
    if (nrow(PredSuit) != 0){
      ###Project from Lat Long to BC Albers
      coordinates(PredSuit) <- c("Longitude","Latitude")
      proj4string(PredSuit) <- CRS("+init=epsg:4326")
      PredSuit <- spTransform(PredSuit, CRS.albers)  # standard albers projection for BC gov't data
      ##Suitibility needs to be a factor
      PredSuit$FutureSuit <- as.factor(PredSuit$FutureSuit)
      
      ##for original suitability
      coordinates(OrigSuit) <- c("Longitude","Latitude")
      proj4string(OrigSuit) <- CRS("+init=epsg:4326")
      OrigSuit <- spTransform(OrigSuit, CRS.albers)  # standard albers projection for BC gov't data
      OrigSuit$CurrentSuit <- as.factor(OrigSuit$CurrentSuit)
      
      OrigSuit2 <- OrigSuit
      OrigSuit2$ColCode[is.na(OrigSuit2$ColCode)] <- "grey"
      
      ##Plot future suit as png with three standard colours
      name2 = paste(treeList[j],years[k],"Map", ".png", sep = "")
      png(name2, width = 8, height = 8, units="in", res = 1000)
      plot(boundary)
      plot(OrigSuit2, col = OrigSuit2$ColCode, pch = 15, cex = 0.08, add = T) ##Current Suit as grey
      plot(PredSuit, col = as.character(PredSuit$ColCode), pch = 15, cex = 0.6, add = T) ###Future suit
      plot(boundary, add = TRUE)
      
      plotExtent <- extent(boundary) 
      xEx <- plotExtent@xmax - 10000 ##adjust these to change legend position
      yEx <- plotExtent@ymax - 1000
      par(xpd=TRUE)
      legend(x = xEx, y = yEx, legend = c("Primary Suitability", "Secondary Suitability","Tertiary Suitability","Becoming Unsuitable","Not Suitable"),
             pch = 22, cex = 1.2, pt.bg = c("green","yellow","red","grey","white"), bty = "n")#, xpd = TRUE
      dev.off()
      
      if(k == 1){###only plot current suit once per species
        name2 = paste(treeList[j],"Current","Map", ".png", sep = "")
        png(name2, width = 8, height = 8, units="in", res = 1000)
        plot(boundary)
        plot(OrigSuit, col = as.character(OrigSuit$ColCode), pch = 15, cex = 0.6, add = T)
        plot(boundary, add = TRUE)
        par(xpd=TRUE)
        legend(x = xEx, y = yEx,legend = c("Primary Suitability", "Secondary Suitability","Tertiary Suitability","Becoming Unsuitable","Not Suitable"),
               pch = 22, cex = 1.2, pt.bg = c("green","yellow","red","grey","white"), bty = "n")#, xpd = TRUE
        dev.off()
      }
      
    }
  }
}

###adds PNG image for each species in each time period to the working directory
###Can be turns into a GIF animation using CreateGifs.R script





##########################################################################################################################3

##Old code########################################

PredSuit$FutureSuit <- ifelse(PredSuit$FutureSuit == 13, 3,
                              ifelse(PredSuit$FutureSuit ==12, 2,
                                     ifelse(PredSuit$FutureSuit == 11, 1, PredSuit$FutureSuit)))

treeList <- as.character(treeList)
PredSuit$FutureSuit <- as.factor(PredSuit$FutureSuit)
temp <- data.frame(table(PredSuit$FutureSuit))
SuitCount <- merge(SuitCount, temp, by.x = "Suit", by.y = "Var1", all = TRUE)
colnames(SuitCount)[j+1] <- treeList[j]

for(j in 1:length(treeList)){
  SuitShort <- subset(SuitTable, SuitTable$Spp == treeList[j]) ##Subset table for one tree species
  SuitShort <- SuitShort[,-3]
  PredSuit <- SuitShort[,c(1,2,5)]
  OrigSuit <- SuitShort[,c(1,2,4)]
  
  ##Use below line if using min suitability in each subzone instead of 01 site series###
  ##SuitShort <- SuitShort[SuitShort$FSuit == ave(SuitShort$FSuit, SuitShort$BGC, FUN = min),] ##convert suitibility to minimum for each subzone
  PredSuit <- subset(PredSuit, PredSuit$FutureSuit != 0)
  OrigSuit <- subset(OrigSuit, OrigSuit$CurrentSuit != 0)
  
  PredSuit$FutureSuit <- ifelse(PredSuit$FutureSuit == 13, 3,
                                ifelse(PredSuit$FutureSuit ==12, 2,
                                       ifelse(PredSuit$FutureSuit == 11, 1, PredSuit$FutureSuit)))
  
  treeList <- as.character(treeList)
  PredSuit$FutureSuit <- as.factor(PredSuit$FutureSuit)
  temp <- data.frame(table(PredSuit$FutureSuit))
  SuitCount <- merge(SuitCount, temp, by.x = "Suit", by.y = "Var1", all = TRUE)
  colnames(SuitCount)[j+1] <- treeList[j]
}

if (nrow(PredSuit) != 0 & nrow(OrigSuit) != 0){
  ###Project from Lat Long to BC Albers
  coordinates(PredSuit) <- c("Longitude","Latitude")
  proj4string(PredSuit) <- CRS("+init=epsg:4326")
  PredSuit <- spTransform(PredSuit, CRS.albers)  # standard albers projection for BC gov't data
  ##Suitibility needs to be a factor
  PredSuit$FutureSuit <- as.factor(PredSuit$FutureSuit)
  
  ##for original suitability
  coordinates(OrigSuit) <- c("Longitude","Latitude")
  proj4string(OrigSuit) <- CRS("+init=epsg:4326")
  OrigSuit <- spTransform(OrigSuit, CRS.albers)  # standard albers projection for BC gov't data
  OrigSuit$CurrentSuit <- as.factor(OrigSuit$CurrentSuit)
  
  
  
  PredSuit$FutureSuit <- ifelse(PredSuit$FutureSuit == 13, 3,
                                ifelse(PredSuit$FutureSuit == 12, 2,
                                       ifelse(PredSuit$FutureSuit == 11, 1, PredSuit$FutureSuit)))
  
  
  ##Plot as png with three standard colours
  name2 = paste(treeList[j],"2085",".png", sep = "")
  png(name2, width = 8, height = 8, units="in", res = 1000)
  plot(BC_out)
  plot(OrigSuit, col = "grey", pch = 15, cex = 0.08, add = TRUE)
  palette(c("Green","Yellow", "orangered"))
  plot(PredSuit, col = PredSuit$FutureSuit, pch = 15, cex = 0.08, add = TRUE)
  legend("topright",legend = c("Primary Suitability", "Secondary Suitability","Tertiary Suitability","Becoming Unsuitable","Not Suitable"),
         pch = 22, cex = 1.2, pt.bg = c("green","yellow","red","grey","white"), bty = "n")
  dev.off()
}
ptime <- system.time({
BEC <- merge.data.frame(BEC, SuitShort, by.x = "BGC", by.y = "Site.Series", all = TRUE)
})
BEC <- BEC[complete.cases(BEC),]

BC_cities <- readShapePoints("MajorCitiest.shp",proj4string = CRS.albers)

combineSuit <- function(x,y,z){
  if(is.na(index)){
    result <- 0
  }else{
    result <- z[index]
  }
  return(result)
}

BEC$Suit2 <- combineSuit(BEC$BGC, SuitShort$BGC, SuitShort$FSuit)
BEC$Suit2 <- lapply(BEC$BGC, FUN = combineSuit(BEC$BGC, SuitShort$BGC, SuitShort$FSuit))

BEC <- read.csv("BGCv10_2k_Clipped.csv", stringsAsFactors = TRUE) ##Read in BGC grid
BEC <- BEC[,c(3,4,7:10)]
BEC$BGC <- gsub(" ","", BEC$ID2) ##remove spaces from BGC labels
BEC <- BEC[,-3]
Original <- Original[, c(1,3,4)]


##Plot as PDF with darker colours where previously didn't exist
name = paste(treeList[j], "2025", "diff", ".pdf", sep = "")
pdf(name)
plot(BC_out)
plot(OrigSuit, col = "grey", pch = 15, cex = 0.1, add = TRUE)
palette(c("Green","Green4", "Yellow", "gold", "orangered", "orangered3"))
plot(PredSuit, col = PredSuit$FutureSuit, pch = 15, cex = 0.1, add = TRUE)
dev.off()

##########################################################################
for(j in 1:length(treeList)){
  SuitShort <- subset(SuitTable, SuitTable$Spp == treeList[j]) ##Subset table for one tree species
  SuitShort <- SuitShort[,-3]
  OrigSuit <- SuitShort[,c(1,2,4)]
  
  ##Use below line if using min suitability in each subzone instead of 01 site series###
  ##SuitShort <- SuitShort[SuitShort$FSuit == ave(SuitShort$FSuit, SuitShort$BGC, FUN = min),] ##convert suitibility to minimum for each subzone
  OrigSuit <- subset(OrigSuit, OrigSuit$CurrentSuit != 0)
  
  if (nrow(OrigSuit) != 0){
    
    ##for original suitability
    coordinates(OrigSuit) <- c("Longitude","Latitude")
    proj4string(OrigSuit) <- CRS("+init=epsg:4326")
    OrigSuit <- spTransform(OrigSuit, CRS.albers)  # standard albers projection for BC gov't data
    OrigSuit$CurrentSuit <- as.factor(OrigSuit$CurrentSuit)
    proj4string(CaribouIDF) <- proj4string(OrigSuit)
    OrigSuitIn <- OrigSuit[!is.na(over(OrigSuit, as(CaribouIDF, "SpatialPolygons"))),]
    
    ##Plot as png with three standard colours
    name2 = paste(treeList[j],"Current","_IDF", ".png", sep = "")
    png(name2, width = 8, height = 8, units="in", res = 1000)
    palette(c("slateblue","violetred","purple"))
    plot(CaribouIDF, border = CaribouIDF$MAP_LABEL, lwd = 3, add = FALSE)
    palette(c("Green","Yellow", "orangered"))
    plot(OrigSuitIn, col = OrigSuitIn$CurrentSuit, pch = 15, cex = 0.5, add = TRUE)
    palette(c("slateblue","violetred","purple"))
    plot(CaribouIDF, border = CaribouIDF$MAP_LABEL, lwd = 3, add = TRUE)
    legend("topright",legend = c("Primary Suitability", "Secondary Suitability","Tertiary Suitability","Becoming Unsuitable","Not Suitable","IDFdk3","IDFdk4","IDFxm"),
           pch = 22, cex = 1.2, pt.bg = c("green","yellow","red","grey","white","slateblue","violetred","purple"), bty = "n")
    dev.off()
  }
}

SuitSave <- PredSuit

PredSuit$FutureSuit <- ifelse(PredSuit$FutureSuit == 13, 4,
                              ifelse(PredSuit$FutureSuit ==12, 4,
                                     ifelse(PredSuit$FutureSuit == 11, 4, PredSuit$FutureSuit)))

###plot with purple where new
name = paste(treeList[j], "2085", "New", ".pdf", sep = "")
pdf(name)
plot(BC_out)
plot(OrigSuit, col = "grey60", pch = 15, cex = 0.1, add = TRUE)
palette(c("Green","Yellow", "orangered", "darkmagenta"))
plot(PredSuit, col = PredSuit$FutureSuit, pch = 15, cex = 0.08, add = TRUE)
dev.off()