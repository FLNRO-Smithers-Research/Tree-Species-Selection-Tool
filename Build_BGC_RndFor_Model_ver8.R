# Ensure that you have working Dataset and BEC Pair Table in your working directory:
# When you start with a new dataset, make sure to follow "When You Have New Dataset" below first:
#set library
.libPaths("E:/R packages")

# Install the following R packages:
install.packages("caret")
install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tcltk")
install.packages("plyr")
install.packages("reshape")
install.packages("reshape2")
install.packages("VSURF")
install.packages("reports")
install.packages("rpart.utils")
install.packages("rfUtilities")
install.packages ("ggplot2")
install.packages ("functional") 
install.packages ("plot3D")
install.packages("dplyr")
install.packages("randomForest")
install.packages("ranger")
install.packages("OpenMP")
install.packages ("randomForestSRC")
# Load R packages:

library(rattle)
library(rpart)
library(rpart.plot)
library(plyr)
library(reshape)
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
######Clear environment and add in dataset############

rm(list=ls())
options(stringsAsFactors = FALSE)
# The codes below need to be run only once when you start with a new dataset #
## Set working directory:
wd=tk_choose.dir()
setwd(wd)

#Use to import multiple datasets
##load 1st climate data set and use file name of dataset as fname
fplot=(file.choose()) 
Y1=read.csv(fplot,stringsAsFactors=F,na.strings=".")
fname=basename(file_path_sans_ext(fplot))
#Load a second file to merge with Y1, repeat this block with additional files
fplot2=(file.choose())
Y2=read.csv(fplot2,stringsAsFactors=F,na.strings=".")
Y1 <- rbind(Y1, Y2)

X1=Y1

#####Alternate method for import 
## Name of ClimateWNA output Model Dataset and Plots to predict Table:
fname="AB2000pts_outputfromWNA"

## Unzip .csv file of a BEC Climate dataset and save and store it in .Rda format:
fpath=paste(wd,"/",fname,".csv",sep="")
X1=read.csv(fpath,stringsAsFactors=FALSE,na.strings=".")
###########################

####modify variable names
colnames(X1)[1]=c("PlotNo")
colnames(X1)[2]=c("BGC")
records <- nrow(X1)
X1$PlotNo <- as.integer(seq(from = 1, to = records, by =1))
attr(X1, "row.names") <- (X1$PlotNo)
X2 <- X1 [, c("PlotNo", "BGC", "Latitude", "Longitude", "Elevation")]
X1=X1[,-c(1,3:5)]
#X1<- as.data.frame(X1)
# Drop
X1$BGC <- as.factor(X1$BGC)

save(X1,file=paste(fname,".Rda",sep=""))

#############################################
############# Reduce to working dataset ######
#############################################

############# Prepare Dataset for Analysis:----
# Load saved RDA Dataset (if .csv not used):
fname="BGCv10_2000Pt_Rnd_Normal_1961_1990MSY"
load(paste(fname,".Rda",sep=""))

#####generate some addtional variables
X1$PPT_MJ <- X1$PPT05 + X1$PPT06 # MaY/June precip
X1$PPT_JAS <- X1$PPT07 + X1$PPT08 + X1$PPT09 # July/Aug/Sept precip
X1$CMDMax <- X1$CMD07
X1$CMD.grow <- X1$CMD05 + X1$CMD06 +X1$CMD07 +X1$CMD08 +X1$CMD09
X1$PPT.dormant <- X1$PPT_at + X1$PPT_wt # for calculating spring deficit
X1$CMD.def <- 500 - (X1$PPT.dormant)# start of growing season deficit original value was 400 but 500 seems better
X1$CMD.def [X1$CMD.def < 0] <- 0 #negative values set to zero = no deficit
X1$CMD.total <- X1$CMD.def + X1$CMD
X1save = X1
model = "Allvariables"
X1=X1save


####################Create reduced datasets -  several options

############Reduce variables to match Wang et al. paper
model = "Wang"
TEMP.list = c("Tmin11","Tmax02","Tmin_sm","Tmax_sp","Tmax_wt","Tmax_sm","Tmax11","Tave_sm","Tmax_at",
              "Tmax10","Tmin_wt","Tmin_02","Tave_sp","Tmax05","Tmax01","Tmin12","Tmin_at","Tmax07",
              "Tmin10","Tmax08","Tmin06","Tmin05","Tmax09","Tmin01")
PPT.list = c("PPT10","PPT06","PPT12","PPT08","PPT_sm","PPT_sp","PPT_05","PPT_09","PPT_at","PPT07",
             "PPT_wt","PPT04","PPT11","PPT01","PPT03","PPT02")
OTHER.list = c("SHM","EMT","PAS")
Variables = c(TEMP.list,PPT.list,OTHER.list)
X1save = X1
List = c("BGC")
X1$BGC  <- as.factor(X1$BGC)
X1 = X1[, names(X1) %in% c(List,Variables)]
#######

#### "BioFinal Updated"--Biologically Interpretable Variables Reduced to 25 highest GINI (Will's Version)
model = "BioFinalupdated"
TEMP.list=c("Tmax_sp","Tmin_wt","Tmin_sm","DD5_sp", "MCMT", "EMT", "DD5_06", "TD", "Tmin01")#,"Tmin_at","EXT"
PPT.list=c("PPT06", "PPT_sp","MSP","PAS", "PPT_JAS", "PPT06")
OTHER.list=c("AHM","FFP", "CMD.total", "Eref", "eFFP")
ClimateVar=c(TEMP.list,PPT.list,OTHER.list)
List=c("BGC")
X1save = X1
X1$BGC  <- as.factor(X1$BGC)
X1=X1[,names(X1) %in% c(List,ClimateVar)]
save(X1,file=paste(fname,"_Dataset", model,".Rda",sep=""))
load(paste(fname,"_Dataset", model,".Rda",sep=""))

#### "BioFinal"--Biologically Interpretable Variables Reduced to 25 highest GINI
model = "BioFinalOld"
TEMP.list=c("Tmax_sp","Tmin_wt","Tmin_sm","Tmin_at",
            "DD5_sp", "MCMT", "EMT","EXT", "DD5_06", "TD")
PPT.list=c("PPT06", "PPT_sp", "PPT_sm","MSP", "MAP","PAS", "PPT_MJ", "PPT_JAS")
OTHER.list=c("AHM","SHM","bFFP","FFP","CMD07", "CMD.total", "Eref")
ClimateVar=c(TEMP.list,PPT.list,OTHER.list)
List=c("BGC")
X1save = X1
X1$BGC  <- as.factor(X1$BGC)
X1=X1[,names(X1) %in% c(List,ClimateVar)]

####################Kiri Final Model###################
model = "KiriFinal"
VarList = c("AHM", "bFFP","CMD.total","DD5_sp","EMT","Eref_sm","EXT","FFP","MCMT","MSP",
            "PPT_JAS","PPT_MJ","PPT06","SHM","TD","Tmax_sp","Tmin_at","Tmin_sm","Tmin_wt",
            "PAS","CMD.def","CMDMax","eFFP","Eref09","MAT","PPT07","Tmin_sp")
##VarList <- readLines("Kiri_FinalVars.csv")
List = c("BGC")
X1save = X1
X1$BGC  <- as.factor(X1$BGC)
X1=X1[,names(X1) %in% c(List,VarList)]

###############Build RandomForest Model#################
#BGCmodel standard random forest with trace
set.seed(123321)
ptm <- proc.time()
BGCmodel <- randomForest(BGC ~ ., data=X1, nodesize = 5, do.trace = 10, ntree=101, na.action=na.fail, importance=TRUE, proximity=FALSE)
proc.time() - ptm

##Save output
print(BGCmodel$confusion, digits=2)
Novel <-outlier(BGCmodel, X1)
write.csv(BGCmodel$proximity, file= paste(fname,"_Proximity",model,".csv",sep=""))
write.csv(BGCmodel$importance, file= paste(fname,"_Importance",model,".csv",sep=""))
write.csv(BGCmodel$err.rate, file= paste(fname,"_Error",model,".csv",sep=""))
write.csv(BGCmodel$confusion, file= paste(fname,"_ConfusionMatrix",model,".csv",sep=""))
write.csv(BGCmodel$confusion[, 'class.error'], file= paste(fname,"_Confusion",model,".csv",sep=""))
VIP <- varImpPlot(BGCmodel, sort=TRUE) 
write.csv(VIP, file= paste(fname,"_OBO",model,".csv",sep=""))
dev.copy(pdf,(paste(model,'VarImpPlot.pdf')))
dev.off()
   #### Save random forest model
file=paste(fname,"_RFmodel",model,".Rdata",sep="")
save(BGCmodel,file=file)

##******OPTIONAL*********##

################Do parallel version to increase speed
set.seed(123321)
coreNo <- makeCluster(detectCores() - 1)
registerDoParallel(coreNo, cores = detectCores() - 1)
Cores <- as.numeric(detectCores()-1)
###runs with all cores - does not produce confusion matrix
ptmcore <- proc.time()
BGCmodel2 <- foreach(ntree=rep(round(150/(Cores)), Cores), .combine=combine, .packages='randomForest') %dopar% {
  randomForest(X1$BGC ~ ., data=X1, ntree=ntree, na.action=na.fail, do.trace = 10, importance=TRUE)
}
proc.time() - ptmcore

###BGCmodel from rfsrc (different option)
options(rf.cores=detectCores(), mc.cores=detectCores())
ptm <- proc.time()
BGCmodel <- rfsrc(BGC ~ ., data=X1 , ntree=11, na.action=c("na.omit"), importance=TRUE)
proc.time() - ptm

# Save Confusion matrix
print(BGCmodel$confusion, digits=2)
write.csv(BGCmodel$err.rate, file= paste(fname,"_ErrorBio2v1000_53",".csv",sep=""))
write.csv(BGCmodel$confusion, file= paste(fname,"_ConfusionMatrixBio2v1000_53",".csv",sep=""))
write.csv(BGCmodel$confusion[, 'class.error'], file= paste(fname,"_ConfusionBio2v1000_53",".csv",sep=""))
varImpPlot(BGCmodel, sort=TRUE, n.var=nrow(BGCmodel$importance))

###"RFCV"--Runs iterations to determine how many variables are required for accurancy
ptm <- proc.time()
BGCmodelVar <- rfcv(trainx = X1[,-1], trainy = X1[,1], scale = "log", step=0.5)
proc.time() - ptm
###graph of decrease in error with number of variables
with(BGCmodelVar, plot(n.var, error.cv, type="o", lwd=2, xlim = c(0,40)))
print(BGCmodelVar$predicted)
save(BGCmodelVar$predicted,file=paste(fname,"_VarImp",".JPEG",sep=""))

##### run the next line of code iteratively to find best mtry number
BGCmodel2 <- tuneRF(X1[,-1], X1[,1], mtryStart=5, ntreeTry=151, stepFactor=1.5, improve=0.05,trace=TRUE, plot=TRUE, doBest=FALSE)

##output model variable stats
print(BGCmodel)
varUsed (BGCmodel, by.tree=FALSE, count=TRUE)
round(importance(BGCmodel),2)
varImpPlot(BGCmodel,sort=T,n.var=min(50,nrow(BGCmodel$importance)))
plot(margin(BGCmodel))

###output locations file with original and new BGC predicted
X2$BGC.predict <- BGCmodel$predicted
write.csv(X2, file= paste(fname,"_TrainingPtsPredicted",".csv",sep=""))

###OPTION: Updates the original assignment of BGC from predict model
X1$BGC <- factor (BGCmodel$predicted)

print (BGCmodel$confusion, digits=2)
write.csv(BGCmodel$confusion, file= "AlpineSubzZone_ConfusionMatrix501.csv")
write.csv(BGCmodel$confusion[, 'class.error'], file= "AlpineSubzone_OverallConfusion501.csv")

save(BGCmodel,file=paste(fname,".Rdata",sep=""))
load(paste(fname,".Rdata",sep=""))
