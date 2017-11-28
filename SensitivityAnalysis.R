rm(list=ls())
.libPaths("E:/R packages")
require(stringr)
require(plyr)
library(rattle)
library(rpart)
library(reshape2)
library(VSURF)
library(reports)
library(rpart.utils)
require(rfUtilities)
library("parallel")
library("foreach")
library("doParallel")
require("ggplot2")
library(functional)
require(plot3D)
library(dplyr)
library(tcltk)
library(caret)
require(randomForest)
require(ranger)
require (OpenMP)
require (randomForestSRC)
require (tools)
require (countr)

wd=tk_choose.dir()
setwd(wd)

mod1 <- read.csv("BGCv10_100Pt_Ensemble_Predicted5.1.csv", stringsAsFactors = TRUE)
mod2 <- read.csv("BGCv10_100Pt_GCMs_Predicted5.1.csv", stringsAsFactors = TRUE)

mod1 <- mod1[,c(7,8)]
mod2 <- mod2[,c(7,8)]

mod1$BGCNum <- lapply(mod1$BGC, as.numeric)
Predict.freq <- data.frame(CurrBGC = factor, x = factor, freq = numeric)

#First Model
for (i in 1:max(as.numeric(mod1$BGCNum))){
  temp <- subset(mod1, BGCNum == i)
  temp2 <- as.data.frame(count(temp$BGC.pred)) 
  temp2$CurrBGC <- rep(temp[1,1], length(temp2$x))
  Predict.freq <- rbind(Predict.freq, temp2)
}

totalNo <- sum(Predict.freq[Predict.freq$CurrBGC == "CMAun",]$freq)
Predict.freq$Percent <- (Predict.freq$freq/300)*100
##Predict.freq <- Predict.freq[-c(68:134),] ##why are there duplicates??

row.names(Predict.freq) <- paste(Predict.freq$CurrBGC, "P", Predict.freq$x, sep = "_")

##Second model
mod2$BGCNum <- lapply(mod2$BGC, as.numeric)
Predict.freq2 <- data.frame(CurrBGC = factor, x = factor, freq = numeric)

for (i in 1:max(as.numeric(mod2$BGCNum))){
  temp <- subset(mod2, BGCNum == i)
  temp2 <- as.data.frame(count(temp$BGC.pred)) 
  temp2$CurrBGC <- rep(temp[1,1], length(temp2$x))
  Predict.freq2 <- rbind(Predict.freq2, temp2)
}

totalNo <- sum(Predict.freq2[Predict.freq2$CurrBGC == "BAFAun",]$freq)
Predict.freq2$Percent <- (Predict.freq2$freq/4300)*100

row.names(Predict.freq2) <- paste(Predict.freq2$CurrBGC, "P", Predict.freq2$x, sep = "_")

##Merge the two dataframes by row name
finalPercents <- merge(Predict.freq, Predict.freq2, by = 0, all = TRUE)
finalPercents[is.na(finalPercents)] <- 0 #Replace NA with 0
row.names(finalPercents) <- finalPercents$Row.names

finalPercents <- finalPercents[, -c(1:4,6:8,10)]
finalPercents$Diff <- finalPercents$Percent.x - finalPercents$Percent.y

hist(finalPercents$Diff, breaks = 100, col = "hotpink")

d <- density(finalPercents$Diff)
plot(d, xlim = c(-15,15), main = "Diff Ensemble and All GCM BioFinal2")
polygon(d, col = "mediumorchid4", border = "black")
rug(finalPercents$Diff, col = "darkorchid")

##See if CWH is called more in Wang model (with winter precip)
k <- grep("CWH", Predict.freq$x)
WangCWH <- Predict.freq[k,]
k<- grep("CWH", Predict.freq2$x)
KiriCWH <- Predict.freq2[k,]

numCWH <- data.frame(Wang = (1), Kiri = (1))
numCWH$Wang<- sum(WangCWH$freq)
numCWH$Kiri <- sum(KiriCWH$freq)
barplot(numCWH)

##Where is CWH predicted
Predict.freq <- Predict.freq[order(Predict.freq$x),]
KiriCWH <- subset(KiriCWH, KiriCWH$freq > 50)
sum(str_count(KiriCWH$CurrBGC, "CWH"))

WangCWH <- subset(WangCWH, WangCWH$freq > 50)
WangCWH <- WangCWH[order(WangCWH$CurrBGC),]
list <- grep("ESSF", WangCWH$CurrBGC)

temp5.1 <- subset(Predict.freq2, Predict.freq2$CurrBGC == "CWHwh2") 
tempWang <- subset(Predict.freq, Predict.freq$CurrBGC == "CWHwh2")
