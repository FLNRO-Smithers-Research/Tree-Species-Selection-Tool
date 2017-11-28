# Code is designed to predict BGC membership of points using a rf model generated in the Build_BGC_RndFor_Model script
# Best to have the rF model, and the dataset to be predictedf in the same working directory:

## http://www.listendata.com/2014/11/random-forest-with-r.html


#===============================================================================
# Install the following R packages:
# (R Version 3.2.2 64-bit)
#===============================================================================
## install.packages("caret")
## install.packages("rattle")
## install.packages("rpart")
## install.packages("rpart.plot")
## install.packages("plyr")
## install.packages("reshape")
## install.packages("reshape2")
## install.packages("reports")
## install.packages("rfUtilities")
## install.packages ("ggplot2")
## install.packages ("functional")
## install.packages ("plot3D")
## install.packages("dplyr")
## install.packages ("RGtk2")
## install.packages("rChoiceDialogs")
## install.packages("rJava")
install.packages ("data.table")
install.packages(("foreach"))
## install.packages("doBy")
## install.packages ("labdsv")
## install.packages("svDialogs")
install.packages ("doBy")

#===============================================================================
# Step 1: Load R packages:
#===============================================================================

.libPaths("E:/R packages")
require (RGtk2)
require(plyr)  # install plyr from zip file
## install.packages("path/plyr_1.6.tar.gz", repos = NULL)
## install.packages("ggplot2", dependencies=TRUE
## install.packages("rJava", dependencies = TRUE); require(rJava)
require (rChoiceDialogs)
require (data.table)
require(doBy)
require (utils)
require(labdsv)
require(tools )
require(svDialogs)
require(tcltk)
require(randomForest)
require(foreach)
require (dplyr)
require(reshape2)
require (reshape)
library(doParallel)
#===============================================================================
# Step 2: Clear environment and add in previous rF model 
#===============================================================================

rm(list=ls())

# The codes below need to be run only once when you start with a new dataset #

#===============================================================================
# Step 3: Set working directory:
#===============================================================================

wd=tk_choose.dir()
setwd(wd)

#===============================================================================
# Step 4: Load up previously created RF model
#
#  e.g.: BGCv10_2000Pt_Rnd_Normal_1961_1990MSY_RFmodelBio3.Rdata
#===============================================================================

## fmodel <- "N:\\Will MacKenzie\\BGCv10_2000Pt_Rnd_Normal_1961_1990MSY_RFmodelBio3.Rdata"

#fmodel <- "N:\\Will MacKenzie\\Single Tile\\BC_AB_USA_1961_1990MSY_RFmodelBioFinal.Rdata"

fmodel = (file.choose())

setwd(gsub(basename(fmodel),"",fmodel))
fname=basename(file_path_sans_ext(fmodel))
load(fmodel)

###alternate method enter model file name
fname="BGCv10_2000Pt_Rnd_Normal_1961_1990MSY_RFmodelKiriFinal"
load(paste(fname,".Rdata",sep=""))

#===============================================================================
#  Step 5: Load plot climate data set to be predicted
#
#  Expects csv data file with (Model), PlotNo, 
#  BGC, Lat, long, Elev as first 6 (5) columns
# 
#  e.g.:  TreeSpp_10pts_BGC_Normal_1981_2010MSY.csv (single model)
#===============================================================================

#### fplot <- "N:\\Will MacKenzie\\TreeSpp_10pts_BGC_Normal_1981_2010MSY.csv"  ## single model

fplot=(file.choose())

#fplot <- "N:\\Will MacKenzie\\Single Tile\\SBSdwTile20_90GCMs_MSYT.csv"


#load(fplot)# use if a .RDA already exists
#fpath2=paste(wd,"/",fplot,".csv",sep="")

Y1 = read.csv(fplot,stringsAsFactors=F,na.strings=".")  ## First 6 columns of Y1: GCM ID1 ID2 Latitude Longitude Elevation


fplot=basename(file_path_sans_ext(fplot))

Y1 <- Y1[-grep("Ensemble", Y1$GCM),]
Y1 <- Y1[-grep("9999", Y1$Tmax01),]
Y1 <- Y1[grep("2085", Y1$GCM),]

Y1 <- Y1[order(Y1$Latitude, Y1$Longitude),]
Y1.orig <- Y1
Y1$ID1 <- rep(1:250, each = 2)
Y1$ID1 <- rep(1:58578, each = 30)
data.ensemble <- Y1
Y1 <- Y1[1:500,]
Y1 <- Y1[grep("BWBS", Y1$ID2),]


#===============================================================================
# Step 5:  Use for SINGLE model Climate data
#
# Note: See next code block for multiple models ############
#===============================================================================

#colnames(Y1)[1:2]=c("SiteNo","BGC" )
#Y1$PlotNo <- as.integer(seq(from = 1, to = nrow(Y1), by =1))
#attr(Y1, "row.names") <- (Y1$SiteNo)
#Y1$BGC <-gsub(" ", "", Y1$BGC, fixed = TRUE)
#str(Y1$BGC)
#Y1$BGC <- factor(Y1$BGC, levels=unique(Y1$BGC))
#Y1$BGC 
#save(Y1,file=paste(fplot,".Rda",sep=""))
#load (paste(fplot,".Rda",sep=""))

#===============================================================================
##########Use for MULTIPLE futures climate data##################
#####first column contains model/scenario
#===============================================================================

colnames (Y1) [1:3] = c("Model", "SiteNo", "BGC")

Y1$Model <- as.factor (Y1$Model)
Y1$SiteNo <- as.factor(Y1$SiteNo)
Y1$BGC <-gsub(" ", "", Y1$BGC, fixed = TRUE)
Y1$BGC <- as.factor(Y1$BGC)

###save(Y1,file=paste(fplot,".Rda",sep=""))
###load (paste(fplot,".Rda",sep=""))


#===============================================================================
# Step 6: Choose appropriate option below
# 
##### reduce variable to same as code to Build_BGC_RndFor_Model #################
##### generate some addtional variables
#===============================================================================

#####generate some addtional variables
####
Y1$PPT_MJ <- Y1$PPT05 + Y1$PPT06 # MaY/June precip
Y1$PPT_JAS <- Y1$PPT07 + Y1$PPT08 + Y1$PPT09 # July/Aug/Sept precip
Y1$PPT.dormant <- Y1$PPT_at + Y1$PPT_wt # for calculating spring deficit
Y1$CMD.def <- 500 - (Y1$PPT.dormant)# start of growing season deficit original value was 400 but 500 seems better
Y1$CMD.def [Y1$CMD.def < 0] <- 0 #negative values set to zero = no deficit
Y1$CMDMax <- Y1$CMD07
Y1$CMD.total <- Y1$CMD.def + Y1$CMD

##BioFinalKiri
model = "5.1"
VarList = c("AHM", "bFFP","CMD.total","DD5_sp","EMT","Eref_sm","EXT","FFP","MCMT","MSP",
            "PPT_JAS","PPT_MJ","PPT06","SHM","TD","Tmax_sp","Tmin_at","Tmin_sm","Tmin_wt",
            "PAS","CMD.def","CMDMax","eFFP","Eref09","MAT","PPT07","Tmin_sp")
List = c("Model", "SiteNo", "BGC")
Y1save = Y1
Y1$BGC  <- as.factor(Y1$BGC)
Y1.sub=Y1[,names(Y1) %in% c(List,VarList)]

##Wang et al
model = "Wang"
TEMP.list = c("Tmin11","Tmax02","Tmin_sm","Tmax_sp","Tmax_wt","Tmax_sm","Tmax11","Tave_sm","Tmax_at",
              "Tmax10","Tmin_wt","Tmin_02","Tave_sp","Tmax05","Tmax01","Tmin12","Tmin_at","Tmax07",
              "Tmin10","Tmax08","Tmin06","Tmin05","Tmax09","Tmin01")
PPT.list = c("PPT10","PPT06","PPT12","PPT08","PPT_sm","PPT_sp","PPT_05","PPT_09","PPT_at","PPT07",
             "PPT_wt","PPT04","PPT11","PPT01","PPT03","PPT02")
OTHER.list = c("SHM","EMT","PAS")
Variables = c(TEMP.list,PPT.list,OTHER.list)
List = c("Model", "SiteNo", "BGC")
Y1save = Y1
Y1$BGC  <- as.factor(Y1$BGC)
Y1.sub=Y1[,names(Y1) %in% c(List,Variables)]

#### Biologically Interpretable Variables Reduced to 25 highest GINI
model = "BioFinal"
TEMP.list=c("Tmax_sp","Tmin_wt","Tmin_sm","Tmin_at",
            "DD5_sp", "MCMT", "EMT","EXT", "DD5_06", "TD")
PPT.list=c("PPT06", "PPT_sp", "PPT_sm","MSP", "MAP","PAS", "PPT_MJ", "PPT_JAS")
OTHER.list=c("AHM","SHM","bFFP","FFP","CMD07", "CMD.total", "Eref")
ClimateVar=c(TEMP.list,PPT.list,OTHER.list)
List=c("Model", "SiteNo", "BGC", "Latitude", "Longitude","Elevation")
Y1save = Y1
Y1$BGC  <- as.factor(Y1$BGC)
Y1.sub=Y1[,names(Y1) %in% c(List,ClimateVar)]


#str(Y1$BGC)
#names(Y1)
#c(List,ClimateVar)
#ClimateVar
#formula(BGCmodel)
#names(Y1)[names(Y1)=="BGC"]
#Y1.sub = subset(Y1, select=c("BGC",
#                             "Tmax_sp", 
 #                            "Tmin_sm", 
  #                           "MWMT", 
   #                          "MCMT",
    #                         "TD", 
     #                        "MSP", 
      #                       "DD5",
       #                      "FFP",  
        #                     "PAS",
         #                    "EXT", 
          #                   "CMD.total"))

#Y1.sub
#str(Y1.sub)
#Y1.sub$MSP  <- as.numeric(Y1.sub$MSP)    
#Y1.sub$DD5  <- as.numeric(Y1.sub$DD5)     
#Y1.sub$FFP  <- as.numeric(Y1.sub$FFP)     
#Y1.sub$PAS  <- as.numeric(Y1.sub$PAS)     
#str(Y1.sub)
#names(Y1.sub)


#===============================================================================
# Step 8: Predict future BGC from randomForest Model
#              
# ###prepare dataset for randomForest
# #####run this block for SINGLE future model######
#===============================================================================

######################################
formula(BGCmodel)

## factor(Y1$BGC)

Y1.sub$BGC <- gsub(" ", "", Y1.sub$BGC, fixed = TRUE)

Y1.sub$BGC <- factor(Y1.sub$BGC, levels=unique(Y1.sub$BGC))  # new command

Y1.sub$BGC.pred <- predict(BGCmodel, Y1.sub[,-c(1:2)])

## Y1$PlotNo <- attr(Y1, "row.names")

Y1.sub$PlotNo <- attr(Y1.sub, "row.names")
S1 <- subset(Y1, select=c("Model", "SiteNo", "Latitude", "Longitude", "Elevation"))
S2 <- subset(Y1.sub, select=c("BGC", "BGC.pred"))     ## "PlotNo"

Y2.sub <- cbind.data.frame(S1, S2)


Y2.sub <- Y2.sub[,c("Model", "SiteNo", "BGC", "BGC.pred", "Latitude", "Longitude", "Elevation")]

Y2.sub$BGC.pred <- gsub(" ", "", Y2.sub$BGC.pred, fixed = TRUE)

Y2.sub$BGC.pred <- factor(Y2.sub$BGC.pred, levels=unique(Y2.sub$BGC.pred))

levels(Y2.sub$BGC)
levels(Y2.sub$BGC.pred)

nlevels(Y2.sub$BGC)
nlevels(Y2.sub$BGC.pred)



#===============================================================================
# Step 9: write prediction file
#
# write.csv(Y2, file=paste(fplot,"_Predicted", model,".csv",sep=""))
#===============================================================================


head(Y2.sub)
Y2.sub$BGC <- gsub(" ", "", Y2.sub$BGC, fixed = TRUE)

Y2x <- as.character(Y2.sub$Model)
Ystr <- strsplit(Y2x, "_")
Y4 <- matrix(unlist(Ystr), ncol=3, byrow=TRUE)
Y2.sub <- cbind(Y4, Y2.sub)
colnames(Y2.sub)[1:3]=c("GCM","Scenario", "FuturePeriod" )

write.csv(Y2.sub, file=paste(fplot,"_Predicted", model,".csv",sep=""))

Y2.year <- Y2.sub[-c(4,5,8:10)]

Y2.year <- dcast(Y2.year, GCM + Scenario + BGC ~ FuturePeriod)
Y2.year <- Y2.year[order(Y2.year$BGC, Y2.year$`2025`),]
write.csv(Y2.year, file = "Data for Flowchart.csv")


#===============================================================================
# Step 10: Save ..._futureSS.Rda 
#===============================================================================

paste(fplot,"_futureSS",model,".Rda",sep="")

## save(Y2,file=paste(fplot,"_futureSS",model,".Rda",sep=""))
## load (paste(fplot,"_futureSS",model,".Rda",sep=""))

save(Y2.sub,file=paste(fplot,"_futureSS",model,".Rda",sep=""))
load(paste(fplot,"_futureSS",model,".Rda",sep=""))
#load("AllGridTest2_90GCMs_MSYT_futureSSBioFinal.Rda")

#===============================================================================
# Step 11:  Build Site Futures summary
#
#           ## Select a future time period
#           ## load (paste(fplot,"_futureSS",".Rda",sep=""))
#      
#           
#===============================================================================

## SSfuture = "TreeSpp_10pts_BGC_Normal_1981_2010MSY_futureSSBio3.Rda"

#SSfuture = "SBSdwTile20_90GCMs_MSYT_futureSSBioFinal.Rda"

## SSfuture=(file.choose()) 

load(SSfuture)

model= "BioALLfinal"

##Y2$BGC <-gsub(" ", "", Y2$BGC, fixed = TRUE)

##FuturePeriod = "2025" ### in the final we will want to have a loop that generates output for each time period in the data

##Y2.sub$FuturePeriod <- FuturePeriod   ## NEW COMMAND!!  Without this, the code does NOT work!  


####Options to Add
##FuturePeriod = select.list(unique(Y2.sub$FuturePeriod),multiple=TRUE,title="Future Period", graphics=TRUE)
#Scenario = select.list(unique(Y5$Scenario),multiple=TRUE,title="Select Carbon Scenario", graphics=TRUE)
#GCM = select.list(unique(Y5$GCM),multiple=TRUE,title="Select GCMs to use", graphics=TRUE)

##Y3 =Y2 [Y2$FuturePeriod == Future,]
##Y3.sub = Y2.sub[Y2.sub$FuturePeriod == FuturePeriod,]
Y3.sub = Y2.sub
## Y3$Num <- 1

Y3.sub$Num <- 1

Y3.sub$BGC.pred <- gsub(" ", "", Y3.sub$BGC.pred, fixed = TRUE)
save(Y3.sub,file=paste(fplot,"_futureSS1",model,".Rda",sep=""))

paste(fplot,"_futureSS1",model,".Rda",sep="")

# Create Tile field for selecting later on
Y3x <- as.character(Y3.sub$SiteNo)
Ystr3 <- strsplit(Y3x, "-")
Y5 <- matrix(unlist(Ystr3), ncol=2, byrow=TRUE)
Y3.sub <- cbind(Y5, Y3.sub)
colnames(Y3.sub)[1:2]=c("Tile","TileSite")
#===============================================================================
# Step 12: Add future period
#===============================================================================

## SSfuture = "TreeSpp_10pts_BGC_Normal_1981_2010MSY_futureSS1CBST.Rda"
#SSfuture=(file.choose()) 


#load(SSfuture)
#model= "BioFinal"
#Y2.sub$BGC <-gsub(" ", "", Y2.sub$BGC, fixed = TRUE)

#Y2.sub$BGC <- 
#Future = 2025     ## Add future period, e.g., 2025


#Y3.sub = Y2.sub
#Y3.sub$Num <- 1
#Y3.sub$BGC.pred <- gsub(" ", "", Y3.sub$BGC.pred, fixed = TRUE)
#save(Y3.sub,file=paste(fplot,"_futureSS1",model,".Rda",sep=""))

#paste(fplot,"_futureSS1",model,".Rda",sep="")

head(Y3.sub)

#===============================================================================
# Step 13: Build Summarys of BGC prediction ratios
#      
#           
#===============================================================================

## "TreeSpp_10pts_BGC_Normal_1981_2010MSY_futureSS1CBST.Rda"

m <- summaryBy(SiteNo ~ BGC + FuturePeriod, data=Y3.sub, FUN=c(length))

p <- summaryBy(SiteNo ~ BGC + BGC.pred + FuturePeriod, data=Y3.sub,  FUN=c(length))


#m <-ddply(Y3, .(BGC), numcolwise(sum))
#p <-ddply(Y3, .(BGC, BGC.pred), numcolwise(sum))

#BGCratio <- merge(m,p, by.x = "BGC", by.y = "BGC" )
BGCratio <- merge(m,p,by=c("BGC","FuturePeriod"))

BGCratio$FutureRatio <- BGCratio$SiteNo.length.y/BGCratio$SiteNo.length.x

summaryBy(FutureRatio  ~ BGC + FuturePeriod, data=BGCratio, FUN=c(sum)) #### summary to check that equals 1 for each BGC

BGCratio$BGC.pred <- gsub(" ", "", BGCratio$BGC.pred, fixed = TRUE)

BGCratio2 <- BGCratio[, c("BGC", "BGC.pred", "FutureRatio")]

BGCratio2$BGC <- as.character(BGCratio2$BGC)

write.csv(BGCratio2, file=paste(fplot,"_BGCFutures",model,".csv",sep=""))


#===============================================================================
# Step 14: Build equivalent Site Series futures summary 
#          ##########################################################
#          Import and Build Edatopic Grid Overlap
#           
#===============================================================================

edatopename="Edatope_v10"
edatopename2=paste(wd,"/",edatopename,".csv",sep="")
E1 <-read.csv(edatopename2,stringsAsFactors=FALSE,na.strings=".")
#E1 <- E1[, -c(2:5)]

###create list of focal BGCs & edatopic space
e1 <- as.list(unique(BGCratio2$BGC), all.names=FALSE)
edatopic1 <- E1[E1$MergedBGC %in% e1,]

###create list of predicted BGCs and edatopic space
e2 <- as.list (unique(BGCratio2$BGC.pred), all.names=FALSE)
edatopic2 <- E1[E1$MergedBGC %in% e2,]


#################Import and Build Tree species suitability##############
treesuit="TreeSppSuit_v10"
treesuit2=paste(wd,"/",treesuit,".csv",sep="")
S1 <-read.csv(treesuit2,stringsAsFactors=F,na.strings=".")

#===============================================================================
# Step 15: Builds list of all BGCs, Future BGCs, and Site Series
#          
#       
#===============================================================================

BGClist = (unique(BGCratio2$BGC))
#BGClist = unique(BGCratio2$BGC)
#BGCfutures <- Y3.sub[Y3.sub$BGC %in% BGClist, ]
FuturePeriod.list <- as.list(unique(Y2.sub$FuturePeriod))
#FuturePeriod.list <- "2025"
BGCfutures.list <- as.list(unique(Y3.sub$BGC.pred)) ### to use later on to limit the site series
BGCfocalE <- edatopic1[edatopic1$MergedBGC %in% Y3.sub$BGC  ,] ### extracts edatopic space for BGC focal of SiteNo
BGCfutureE <- edatopic2[edatopic2$MergedBGC %in% Y3.sub$BGC.pred  ,] #extracts edatopic info only for predicted BGCs
#SSlist = as.list(unique(BGCfocalE$SS_NoSpace))
#SSlist = "BWBSmw/101"
Y3.sub$SiteNo <- as.character(Y3.sub$SiteNo)
SiteNo.list = as.list(unique(Y3.sub$SiteNo))
Y3.sub1 <-Y3.sub
########Section for when testing area based suitability
    #Tile.list = as.list(unique(Y3.sub$Tile))
    #Tile.pick = select.list(unique(Y3.sub$Tile), multiple=FALSE, title="TILE", graphics=FALSE)
    #Y3.sub1 <- subset(Y3.sub, Tile %in% Tile.pick)
    #SiteNo.list = as.list(unique(Y3.sub1$SiteNo))


#SSlist = unique(BGCfocalE$SS_NoSpace)
#SiteNo.list = list ("49.3124.9002237", "49.3124.9002239")
#SNL=49.3124.9002237

remove.list <- Y3.sub1[!Y3.sub1$BGC %in% edatopic1$MergedBGC,]
list <- unique(remove.list$BGC)
Y3.sub1 <- Y3.sub1[!Y3.sub1$BGC %in% FTSremove$Spp,]

###Set up to run loops in parallel
require(doParallel)
cl = makePSOCKcluster(6)
registerDoParallel(cl)
clusterEvalQ(cl, .libPaths("E:/R packages"))
#===============================================================================
# Step 15: FOREACH LOOP to build tree suitability table by SiteNo
#===============================================================================
SNL <- SiteNo.list
################################################################################
ptime <- system.time({
SiteNo.suit <-  foreach(SNL = SiteNo.list, .combine =rbind) %dopar% {      ##  for each SiteNo in the data
  require(foreach)
  require(reshape)
  require(doBy)
  require(data.table)
  options(warn=2)
  
  cat("===========================================","\n")
  cat(SNL, "\n", "===========================================","\n")
  
  #===============================================================================
  # Step 15: FOREACH LOOP to build Site series equivalents
  #          
  #===============================================================================
  SiteFuture.suit <- foreach(i = FuturePeriod.list, .combine = rbind)  %do% {
    #require(foreach)
  
    Y3.each <- Y3.sub1[Y3.sub1$SiteNo %in% SNL ,] ## extracts data for each site
    Y3.each <- Y3.each[Y3.each$FuturePeriod %in% i ,] ##extracts data for each time period
    Y3.siteno <-as.list(unique (Y3.each$SiteNo))
    Y3.BGC <- as.list(unique(Y3.each$BGC))
    Y3.BGC.pred<- unique(Y3.each$BGC.pred)
    BGCfocalE <- edatopic1[edatopic1$MergedBGC %in% Y3.BGC  , ] ### extracts edatopic space for BGC focal of SiteNo
   BGCfutureE <- edatopic2[edatopic2$MergedBGC %in% Y3.BGC.pred  , ] #extracts edatopic info only for predicted BGCs
    Y3.SSlist = as.list(unique(BGCfocalE$SS_NoSpace))
    
    
    FTS2 <-  foreach(SS = Y3.SSlist, .combine =rbind, .packages = 'doBy','reshape','reshape2') %do% {      ##  for each site series for a SiteNo BGC
      
      ###############################################################
      options(warn=2)
      
      cat("===========================================","\n")
      
      cat(SS, "\n", "===========================================","\n")
      
      ## rm(nsmatrix, suitmatrix)
      
      SSfocal <- BGCfocalE [BGCfocalE $SS_NoSpace %in% SS ,] 
      SSfocallist <- as.list(unique(SSfocal$SS_NoSpace))
      SSfocalBGClist <- as.list(unique(SSfocal$MergedBGC))
      SSfocalE <- as.list(unique(SSfocal$Edatopic))
      ##############################################
      ##select site series only with some edatopic overlap with SSfocal
      SSfutureE <- BGCfutureE[BGCfutureE$Edatopic %in% SSfocalE,]
          ##remove any site series from focal BGC
       SSfutureE2 <-SSfutureE[!SSfutureE$MergedBGC %in% SSfocalBGClist,]
      SSfutureEfocal <-SSfutureE[SSfutureE$SS_NoSpace %in% SSfocallist,]
        ## add back in the focal site series 
      SSfutureEAll <- merge(SSfutureE2,SSfutureEfocal, all=TRUE)
      SSfuturelist <- as.list(unique(SSfutureEAll$SS_NoSpace))
      SSfutureEall <- SSfutureE[SSfutureE$SS_NoSpace %in% SSfuturelist,]
      
      ####entire grid space for local and future possible site series
      focalspace <- summaryBy(MergedBGC~SS_NoSpace, data=SSfocal, id = 'MergedBGC', FUN=c(length))
      colnames(focalspace)[2]=c("SSall")
      
      futurespace <- summaryBy(MergedBGC~SS_NoSpace, data=SSfutureEall, id = 'MergedBGC', FUN=c(length))
      colnames(futurespace)[2]=c("SSall")
      
      ####overlap grid space for future SS
      futurespace2 <- summaryBy(MergedBGC~SS_NoSpace, data=SSfutureEall, id = 'MergedBGC', FUN=c(length))
      colnames(futurespace2)[2]=c("SSoverlap")
      
      #############Generate overlap values of future to focal
      futureSS <- futurespace
      futureSS$commonE <- futurespace2$SSoverlap
      futureSS$revoverlap <- futureSS$commonE/futureSS$SSall
      #############Generate overlap values of focal to future
      futureSS$focaltot <- focalspace$SSall
      futureSS$overlap <- futureSS$commonE/futureSS$focaltot
      ###########site series overlap
      futureSS$alloverlap <- futureSS$overlap*futureSS$revoverlap
      
      ####add BGC probability to FutureSS list
      m2 <- summaryBy(SiteNo~BGC, data=Y3.each, FUN=c(length))
      p2 <- summaryBy(SiteNo~BGC + BGC.pred, data=Y3.each,  FUN=c(length))
      #m <-ddply(Y3, .(BGC), numcolwise(sum))
      #p <-ddply(Y3, .(BGC, BGC.pred), numcolwise(sum))
      BGCratio3 <- merge (m2,p2, by.x = "BGC", by.y = "BGC" )
      BGCratio3$FutureRatio <- BGCratio3$SiteNo.length.y/BGCratio3$SiteNo.length.x
      summaryBy(FutureRatio ~ BGC, data=BGCratio3, FUN=c(sum)) #### summary to check that equals 1 for each BGC
      
      futureSS$BGCratio <- BGCratio3$FutureRatio[match(futureSS$MergedBGC, BGCratio3$BGC.pred)]
      
      ####Calculate the SS ratio
      SSoverlap <- summaryBy(alloverlap~MergedBGC, data=futureSS, id = 'SS_NoSpace', FUN=c(sum))
      futureSS$overlaptot<- SSoverlap$alloverlap.sum[match(futureSS$MergedBGC, SSoverlap$MergedBGC )]
      futureSS$SSratio <- futureSS$alloverlap/futureSS$overlaptot
      summaryBy(SSratio ~ MergedBGC, data=futureSS, FUN=c(sum)) #### for checking that SSratio sums to 100%
      summaryBy(SSratio ~ SS_NoSpace, data=futureSS, FUN=c(sum)) #
      
      
      ####Calculated the overall site series probability
      futureSS$SSprob <- (futureSS$BGCratio * futureSS$SSratio)
      sum(futureSS$SSprob)#### for checking that SSprob sums to 100%
      
      #############################################
      #######################################TREE SPECIES SELECTION########
      ##########################################################
      
      ##############Create tree suitability matrix   ## This is where the problems occur for "CDFmm/10" !!  
      cat("Create tree suitability matrix","\n")
      
      Treefocal <- (S1[S1$Unit %in% SS,]) #### need to add in default 0 suitabilities for species not in focal but in future
      Treefocalsp <- as.list(Treefocal$Spp)
      Treefuture <- S1[S1$Unit %in% SSfuturelist,]
      Treefuture.list <- as.list(unique(Treefuture$Spp))
      
      Treefocaladd <- (Treefuture[!Treefuture$Spp %in% Treefocalsp,])
      
      if (nrow(Treefocaladd)==0) {Treefocaladd <- NULL}
      
      Treefocaladd$BGC <- as.character(Y3.BGC)
      Treefocaladd$Unit <- SS
      Treefocaladd$Suitability <- 10
      Treefocaladdsp <-unique(Treefocaladd)
      Treefocal <- merge(Treefocal,Treefocaladdsp, all=TRUE)#######not merging
      ##add species to Treefocal with zero suitability
      
      Treefuture$FocalSuit <- Treefocal$Suitability[match(Treefuture$Spp, Treefocal$Spp)]
      Treefuture$Offset <- Treefuture$FocalSuit - Treefuture$Suitability
      Treefuture$SSprob <- futureSS$SSprob[match(Treefuture$Unit, futureSS$SS_NoSpace )]
      ####convert to matrix and replace N/A with value=4
      treematrix <- cast(Treefuture, Spp ~ Unit, value='Offset', add.missing=TRUE, fun.aggregate = "max")
      treematrix <- as.data.frame(treematrix)
      is.na(treematrix) <- sapply(treematrix, is.infinite)
      treematrix2 <- replace(treematrix, is.na(treematrix), 99) 
      #######convert back to list for analysis
      treefuturenew <-melt(treematrix2)
      treefuturenew$SSprob <- futureSS$SSprob[match(treefuturenew$variable, futureSS$SS_NoSpace )]
      colnames (treefuturenew) [2:3] = c("SS", "Offset")
      treefuturenew$Offset <- as.factor(treefuturenew$Offset)
      ####Summarize raw trajectory
      futuresuit <- summaryBy(SSprob~ Spp + Offset, data=treefuturenew,  FUN=c(sum))
      futuresuit$SSprob.sum <- round(futuresuit$SSprob.sum, digits=3)
      #######cast back into matrix
      suitmatrix <- cast(futuresuit, Spp ~ Offset, value='SSprob.sum', add.missing=TRUE, fun.aggregate = "sum")
      nsmatrix <- merge(suitmatrix, Treefocal,  all.x = TRUE) ###add in original suitability ranks
      dummymatrix <- c('Unit', 'Spp', 'Suitability' , '0','1','2','-1', '-2', '9', '8', '7', '99')
      missing <- setdiff(dummymatrix, names(nsmatrix))
      nsmatrix[missing] <-0 
      nsmatrix <-nsmatrix[dummymatrix]
      #########sum probability by row to adjust to 100% where missing info on future BGCs - will need to flag where some futures are missing information
      probsum <- sum(nsmatrix [1,c(4:12)])
      nsmatrix [,c(4:12)] <- nsmatrix [,c(4:12)]/probsum
      
      #######################################################################
      ###############Rule set for changes to suitability rankings
      ####create new matrix###
      ####need to add missing columns to nrmatrix if a category is missed
      cat("Rule set for changes to suitability rankings","\n")
      FTS <- nsmatrix #[,c('Unit', 'Spp', 'Suitability' , '0','1','2','-1', '-2', '9', '8', '7', '99')]
      setnames(FTS, old = c('Unit', 'Spp', 'Suitability' , '0','1','2','-1', '-2', '7', '8', '9', '99'), 
               new = c('BGC', 'Spp', 'Suit' , 'Same','Imp1','Imp2','Dec1', 'Dec2', 'A3', 'A2', 'A1', 'Not Suit'))
      FTS$Suit[FTS$Suit == 10]  <- 4
      
      ###########Set Future Suitability Ratings to Current
      cat("Set Future Suitability Ratings to Current","\n")
      FTS$FSuit <- FTS$Suit
      
      ##############Rules for improving suitability
      Rules <- "v1.3" # version of the rule set applied
      cat("Rules for improving suitability", "\n")
      FTS$FSuit <- ifelse(FTS$Same>=.75, FTS$FSuit, 
                          ifelse ((FTS$Imp1 + (FTS$Imp2*2)) >= 1, (FTS$FSuit)-2,  
                                  ifelse ((FTS$Imp1 + (FTS$Imp2*2)) >= .5, (FTS$FSuit)-1,
                                          FTS$FSuit
                                  )
                          )
      )
      
      
      ##############Rules for declining suitability
      cat("Rules for declining suitability","\n")
      FTS$FSuit <-  ifelse (FTS$'Not Suit' >=.5, (FTS$FSuit)+3, # changed to .5 from.4
                            ifelse (FTS$Dec2 >=.5 , (FTS$FSuit)+2,
                                    ifelse (FTS$Dec1 >=.5 , (FTS$FSuit)+1,     
                                            ifelse ((FTS$Dec1 + FTS$Dec2 + FTS$'Not Suit') >= .5, (FTS$FSuit)+2, 
                                                    ifelse ((FTS$Dec1 + FTS$Dec2 +FTS$'Not Suit') >= .25, (FTS$FSuit)+1, # added this rule
                                                    FTS$FSuit
                                            )
                                    )
                            )
                )   
      )
      
      ##################Rules for not suitable
      
      
      ##########Reset suitabilities >=4 to zero and calculate trajectory
      cat("Reset suitabilities >=4 to zero and calculate trajectory", "\n")
      FTS$Suit[FTS$Suit>=4]  <- 0
      FTS$FSuit[FTS$FSuit>=4]  <- 0
      FTS$Tbase <- FTS$Suit - FTS$FSuit
      
      
      ########set trajectory where suitable species becomes unsuitable
      cat("set trajectory where suitable species becomes unsuitable",  "\n")
      FTS$Trajectory <- FTS$Tbase
      FTS$Trajectory <- ifelse (((FTS$Suit == 1) & (FTS$FSuit == 0)), FTS$Trajectory <- '-3',
                                ifelse (((FTS$Suit == 2) & (FTS$FSuit == 0)), FTS$Trajectory <- '-2',  
                                        ifelse (((FTS$Suit == 3) & (FTS$FSuit == 0)), FTS$Trajectory <- '-1',  
                                                FTS$Tbase
                                        )
                                )
      )
      
      
      ########Add in species to new to site series
      cat("Add in species to new to site series", "\n")
      FTS$FSuit <- ifelse (FTS$Suit > 0, FTS$FSuit,
                           ifelse (FTS$'Not Suit'>.4,FTS$FSuit,
                                   ifelse ((FTS$A3 + (FTS$A2*1.5)+ (FTS$A1*2)) > 1.5, FTS$FSuit <- '11' ,
                                           ifelse ((FTS$A3 + (FTS$A2*1.5)+ (FTS$A1*2)) > 1, FTS$FSuit <- '12' ,
                                                   ifelse ((FTS$A3 + (FTS$A2*1.5)+ (FTS$A1*2)) > .5, FTS$FSuit <- '13',    
                                                           FTS$FSuit
                                                   )
                                           )
                                   )
                           )
      )
      
      ######remove species that are not suitable in current or future.
      ##cat("remove species that are not suitable in current or future.",  "\n")
      ##FTS <-FTS2
            #rm(nsmatrix, suitmatrix)
            ##FTSremove <-  (FTS[FTS$Suit == 0 & FTS$FSuit == 0, ])
           ##FTS <- FTS[!FTS$Spp %in% FTSremove$Spp,]
      
      
      FTS$SiteNo  <- as.factor(unique(Y3.each$SiteNo))
      
      FTS$FuturePeriod <- as.character(i)  ## add Futureperiod to FTS data outputed by the inner-most foreach loop
      
      FTS <- as.data.frame(FTS)
     
      
    } #For each Site
  } #For each year
} # for all

})
ptime

#################End of FOREACH LOOP###################
#######################################################



#===============================================================================
# Step 16: format suitability table output 
#         
#===============================================================================

names(FTS2)

## > names(FTS2)
##  [1] "BGC"        "Spp"        "Suit"       "Same"       "Imp1"       "Imp2"       "Dec1"       "Dec2"       "A1"        
## [10] "A2"         "A3"         "Not Suit"   "FSuit"      "Tbase"      "Trajectory"


str(SiteNo.suit)

FTS3 <- SiteNo.suit[ ,c('FuturePeriod', 'SiteNo','BGC', 'Spp', 'Suit', 'FSuit', 'Trajectory', 'Same', 'Imp1', 'Imp2', 'Dec1', 'Dec2', 'A1', 'A2', 'A3', 'Not Suit')]

FTS3

FTS3[, ]

str(FTS3)

setnames(FTS3, old = c('BGC','Suit', 'Imp1', 'Imp2', 'Dec1', 'Dec2', 'FSuit' ), new = c('Site Series','CurrentSuit', 'Improve1', 'Improve2', 'Decline1', 'Decline2', 'FutureSuit' ))

str(FTS3)

#FTS3$Trajectory <- as.numeric(FTS3$Trajectory)
FTS3[, 8:16] <- round(FTS3[, 8:16], digits=2)

SuitforMaps <- FTS3[,3:6]
write.csv(SuitforMaps, file = "PredSuit_AllMods_2085.csv")
write.csv(FTS3, file = "FTS3_2085.csv")

SiteLocation = c("SiteNo","Latitude", "Longitude", "Elevation")
FTS.location = unique(Y1.sub[,names(Y1.sub) %in% c(SiteLocation)])
FTS4 <- merge(FTS.location, FTS3, by="SiteNo")

#===============================================================================
# Step 17: write formatted Tree species suitability table to file
#         
#===============================================================================
##Create examples for Bryce
Suit.table <- FTS3[order(FTS3$`Site Series`,FTS3$Spp, FTS3$FuturePeriod),]
Suit.table <- Suit.table[,c(3,4,1,5,8:16)]
write.csv(Suit.table, file = "Suitability Examples 6 Zones.csv")

fname <- paste(fplot,"_TreeSuitTraject2",model,".csv",sep= "")

## write.csv(FTS3, file=paste(fplot,"_TreeSuitTraject2",model,".csv",sep=""))

#write.csv(FTS4, file=paste(fplot,"_TreeSuitTraject2",model,".csv",sep=""))   ## Need to output FTS4 rather than FTS3! 
write.csv(FTS4, file=paste(Tile.pick,"_TreeSuitTraject2",model,".csv",sep=""))   ## Need to output FTS4 rather than FTS3! 

## FTS4 contains Latitude, Longitude and Elevation, 
## whereas FTS3 does not! 


cat("\n\n","formatted Tree species suitability table is written to following file:", "\n\n",fname, "\n\n")


################################################################################

