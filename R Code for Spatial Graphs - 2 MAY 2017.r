#=============================================================================== 
# Set working directory
#===============================================================================

# setwd("N:\\Will MacKenzie\\Single Tile")

setwd("C:\\Users\\Isabella\\Dropbox\\Isabella RScript\\Single Tile")

#===============================================================================
# - Manually specify the name of the .csv file which contains the 
#   Formatted Tree Species Suitability Table 
# - Manually specify the Tile Number (aka TileChoice) 
#===============================================================================

FormattedTreeSpeciesSuitabilityTable <- "92K0031_TreeSuitTraject2BioALLfinal.csv"

TileChoice <- "20"   # What Tile are we working with? 

#===============================================================================
# Read the .csv file which contains the Formatted Tree Species Suitability Table 
#===============================================================================

require(data.table)

PlotData <- fread(FormattedTreeSpeciesSuitabilityTable)

PlotData$V1 <- NULL 

PlotData$Tile <- TileChoice

require(stringr)

if ( sum(str_detect(names(PlotData), "Site\\.Series")) == 0) { 
   names(PlotData)[names(PlotData) %in% "Site Series"] <- "Site.Series" 
} 

names(PlotData)


PlotData <- PlotData[,c("FuturePeriod", "Tile", "SiteNo", "Latitude", "Longitude", "Elevation", 
                        "Site.Series",  "Spp", "CurrentSuit", "FutureSuit",  
                        "Trajectory", "Same", "Improve1", "Improve2", "Decline1", "Decline2",     
                        "A1", "A2", "A3", "Not Suit")]
                        
#===============================================================================
# Examine the first and last records of the Formatted Tree Species Suitability Table 
#===============================================================================

head(PlotData)

tail(PlotData)


unique(PlotData$FuturePeriod)  # Check out unique values for FuturePeriod.

unique(PlotData$FutureSuit)  # Check out unique values for FutureSuit.

sum(is.na(PlotData$FutureSuit)) # Are there any missing values for FutureSuit?
                                # If sum = 0, there are NO missing values.

str(PlotData)

#===============================================================================
# Change variable name for Site Serie to exclude blanc space
#===============================================================================

## names(PlotData)
## names(PlotData)[names(PlotData)=="Site Series"] <- "Site.Series"
## names(PlotData)


#===============================================================================
# Read the TreeSppSuit_v10.csv file into R 
#===============================================================================

TreeSppSuit <- read.csv("TreeSppSuit_v10.csv", as.is=TRUE) 

head(TreeSppSuit)

tail(TreeSppSuit) 

dim(TreeSppSuit)

str(TreeSppSuit)

#===============================================================================
# Function for Producing a Spatial Scatterplot
#===============================================================================

SpatialScatterplot <- function(PlotData, FuturePeriodChoice, TileChoice, SiteSeriesChoice){

   .e = environment()

   PlotData0 <- subset(PlotData, FuturePeriod %in% FuturePeriodChoice)

   PlotData1 <- subset(PlotData0, Tile %in% TileChoice)
   
   PlotData2 <- subset(PlotData1,  Site.Series %in% SiteSeriesChoice)

   ## require(plyr)

   ## nrow(PlotData2)

   ## ddply(PlotData2, .(Latitude, Longitude), nrow)

   ## unique(PlotData2$Spp)

   ## PlotData3 <- subset(PlotData2,   Spp %in% SppChoice)
   ## View(PlotData3)

   require(ggplot2)
   ## require(stringr)

   PlotData2$FutureSuit <- as.character(PlotData2$FutureSuit)

   PlotData2$FutureSuit[PlotData2$FutureSuit=="11"] <- "1"
   PlotData2$FutureSuit[PlotData2$FutureSuit=="12"] <- "2"
   PlotData2$FutureSuit[PlotData2$FutureSuit=="13"] <- "3"

   PlotData2$FutureSuit <- factor(PlotData2$FutureSuit)


   ## g <- vector("list", length(unique(PlotData2$Spp)))

   plot_list <- list()

   for (i in 1:length(unique(PlotData2$Spp))) {

       ## print(i)

       SubsetPlotData2 <- subset(PlotData2, Spp %in% unique(PlotData2$Spp)[i])

       SubsetPlotData2$FutureSuit

       levels(SubsetPlotData2$FutureSuit)

       SubsetPlotData2$FutureSuitDrop <- droplevels(SubsetPlotData2$FutureSuit)

       unique(SubsetPlotData2$FutureSuitDrop)

       ## colours.full <- c("grey", "blue", "red", "green")
       ## colours.levels <- levels(SubsetPlotData2$FutureSuitDrop)

       ## colours.levels

       ## mycolours <- NULL
       ## mycolours[colours.levels %in% "0"] <- "darkgrey"
       ## mycolours[colours.levels  %in% "1"] <- "blue"
       ## mycolours[colours.levels  %in% "2"] <- "red"
       ## mycolours[colours.levels  %in% "3"] <- "green"

       ## mycolours

       SubsetPlotData2$color.codes <- rep(NA, length(SubsetPlotData2$FutureSuitDrop))
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "0"] <- "grey35"    # "grey"
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "1"] <- "#178CCB"   # "blue"
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "2"] <- "#CC0000"   # "red"
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "3"] <- "#90BD31"   # "forestgreen"

       #   cols <- c("FS = 0" = "grey35", "FS = 1" = "#178CCB", "FS = 2" = "#CC0000", "FS = 3" = "#90BD31")



       unique(SubsetPlotData2$FutureSuitDrop)

       unique(SubsetPlotData2$color.codes)

       table(SubsetPlotData2$color.codes)

       SubsetPlotData2Unique <- unique(SubsetPlotData2[, c("FutureSuitDrop", "color.codes")])
       SubsetPlotData2Unique

       require(dplyr)

       SubsetPlotData2Unique <- arrange(SubsetPlotData2Unique, FutureSuitDrop)
       SubsetPlotData2Unique

       ## cat("==========================", "\n")
       ## cat("i=",i,"\n\n")
       ## print(unique(SubsetPlotData2$color.codes))
       ## print(SubsetPlotData2Unique)
       ## cat("==========================", "\n")


       # count the needed levels of a factor
       number <- nrow(SubsetPlotData2Unique)
       ## number

       # repeat the given colors enough times
       # palette <- rep(rainbow(number), length.out = number)
       mypalette <- SubsetPlotData2Unique$color.codes

       cols <- c("0" = "grey35", "1" = "#178CCB", "2" = "#CC0000", "3" = "#90BD31")

       require(grid)
       
       #---
       # extract current suitability from TreeSppSuit on the basis 
       # of the Site Series and Tree Species information   
       
       UniqueSiteSeries <- unique(SubsetPlotData2$Site.Series)
       UniqueSpp <- unique(SubsetPlotData2$Spp)  # unique tree species
       
       CurrentSuitability <- subset(TreeSppSuit, (Unit %in% UniqueSiteSeries) & (Spp %in% UniqueSpp), select=Suitability)
       
       CurrentSuitability <- as.numeric(CurrentSuitability)
       
       CurrentSuitability
       
       if (is.na(CurrentSuitability)) { CurrentSuitability <- 0 }  

       #---
       
       ## Retain only the unique rows of SubsetPlotData2 for plotting purposes 
       
       SubsetPlotData2
       
       UniqueSubsetPlotData2 <- unique(SubsetPlotData2)
       
       #---

       gg <- ggplot(UniqueSubsetPlotData2, aes(Longitude, Latitude), group=FutureSuitDrop, environment=.e) +  # environment=.e
         ## scale_colour_manual(breaks = SubsetPlotData2Unique$FutureSuitDrop, values=SubsetPlotData2Unique$color.codes) + # values=mycolours
         ## scale_colour_manual(values=setNames(SubsetPlotData2Unique$color.codes, SubsetPlotData2Unique$FutureSuitDrop)) +
         ## scale_colour_manual(values=palette) +
         ## discrete_scale(aes(Longitude, Latitude), "manual", palette) +
         ## geom_point(aes(size=Elevation, colour=FutureSuitDrop)) +
         geom_point(aes(colour=FutureSuitDrop), size=2.5, alpha=0.9) + ## position = position_jitter(w = 0.01, h = 0.01)  ## position=position_dodge(width=0.5)
         xlab("Longitude") +
         ylab("Latitude") +
         ggtitle(paste(## "Tile Number:", TileChoice, ",",
                       ## "Site Series:", SiteSeriesChoice, ",",
                       "Tree Species:", unique(SubsetPlotData2$Spp), "=", CurrentSuitability)) +
         theme_bw() +
         ## theme(plot.title=element_text(hjust=0.5, size=14),
         ##      plot.margin= unit(c(1, 1, -1, 1), "lines"))  +
         theme(plot.title = element_text(hjust=0.5, size=10),
                  plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
                  legend.position="top",
                  text = element_text(size=9) ,
                  axis.text.x = element_text(angle=45, hjust=1) # ,
                  ## panel.grid.major = element_blank(),
                  ## panel.grid.minor = element_blank()
                  ) +
         ## modified_scale_colour_manual(values= mypalette, name = "FS")  # "Future\nSuitability"
         scale_colour_manual(values = cols, name="FS")


         gg <- gg + theme(legend.margin=margin(t = -0.1, unit='cm'), legend.key.size=unit(0.1,"cm"))


        ## print(gg)

        plot_list[[i]] <- gg

        ## rm(palette)
        ## rm(number)
        ## rm(SubsetPlotData2)
        ## rm(SubsetPlotData2Unique)

   }

   require(gridExtra)

   ## do.call(grid.arrange, c(plot_list, list(ncol=3,
   ## top=paste("Tile Number:", TileChoice, ",", "Site Series:", SiteSeriesChoice))))
   

   nplots <- length(plot_list)

   if (nplots==1) {myncol <- 1; mynrow <- 1}
   if (nplots==2) {myncol <- 2; mynrow <- 1}
   if (nplots==3) {myncol <- 3; mynrow <- 1}

   if (nplots==4) {myncol <- 2; mynrow <- 2}

   if (nplots==5) {myncol <- 3; mynrow <- 2}
   if (nplots==6) {myncol <- 3; mynrow <- 2}

   if (nplots==7) {myncol <- 3; mynrow <- 3}
   if (nplots==8) {myncol <- 3; mynrow <- 3}
   if (nplots==9) {myncol <- 3; mynrow <- 3}

   if (nplots==10) {myncol <- 4; mynrow <- 3}
   if (nplots==11) {myncol <- 4; mynrow <- 3}
   if (nplots==12) {myncol <- 4; mynrow <- 3}

   if (nplots==13) {myncol <- 4; mynrow <- 4}
   if (nplots==14) {myncol <- 4; mynrow <- 4}
   if (nplots==15) {myncol <- 4; mynrow <- 4}
   if (nplots==16) {myncol <- 4; mynrow <- 4}

   if (nplots==17) {myncol <- 4; mynrow <- 5}
   if (nplots==18) {myncol <- 4; mynrow <- 5}
   if (nplots==19) {myncol <- 4; mynrow <- 5}
   if (nplots==20) {myncol <- 4; mynrow <- 5}

   mywidth <- 2.1*myncol; myheight <- 2.5*mynrow


   res <- list(plots = do.call(grid.arrange, c(plot_list, list(ncol=myncol,
                               top=paste0("Future Period: ", FuturePeriodChoice, "\n ", "Tile Number: ", TileChoice, ", ", "Site Series: ", SiteSeriesChoice)))),
               nplots = nplots,
               width = mywidth,
               height = myheight)

   return(res)


}

## windows()
## SpatialScatterplot(PlotData, FuturePeriodChoice, TileChoice, SiteSeriesChoice)$plots



#====================================================================================================
#  Function for Producing a Spatial Stacked Barplot
#====================================================================================================

## Debug:  
##
## FuturePeriodChoice <- "2025"
## TileChoice <- "20"
## SiteSeriesChoice <- "SBSmk1/03"

SpatialStackedBarplot <- function(PlotData, FuturePeriodChoice, TileChoice, SiteSeriesChoice){

   .e = environment()

   ## Select data for a given choice of Future Period, Tile and Site Series

   PlotData0 <- subset(PlotData, FuturePeriod %in% FuturePeriodChoice)

   PlotData1 <- subset(PlotData0, Tile %in% TileChoice)
    
   PlotData2 <- subset(PlotData1,  Site.Series %in% SiteSeriesChoice)
     
   PlotData2$FutureSuit <- as.character(PlotData2$FutureSuit)

   ## Convert Future Suitability Values from "11" to 1, 
   ## from "12" to "2" and from "13" to "3"

   PlotData2$FutureSuit[PlotData2$FutureSuit=="11"] <- "1"
   PlotData2$FutureSuit[PlotData2$FutureSuit=="12"] <- "2"
   PlotData2$FutureSuit[PlotData2$FutureSuit=="13"] <- "3"

   PlotData2$FutureSuit <- factor(PlotData2$FutureSuit)

   ## Create elevation ranges  

   elevation <- PlotData2$Elevation

   Delta <- diff(range(elevation, na.rm=TRUE))/4
   Min <- min(elevation, na.rm=TRUE) 
   
   
   elevationcut <- NULL 
   elevationcut[elevation >= Min & elevation < Min + Delta] <- paste0("[",round(Min,2),",",round(Min+Delta,2),")")
   elevationcut[elevation >= Min + Delta & elevation < Min + 2*Delta] <- paste0("[",round(Min+Delta,2),",",round(Min+2*Delta,2),")")
   elevationcut[elevation >= Min + 2*Delta & elevation < Min + 3*Delta] <- paste0("[",round(Min+2*Delta,2),",",round(Min+3*Delta,2),")")
   elevationcut[elevation >= Min + 3*Delta & elevation <= Min + 4*Delta] <- paste0("[",round(Min+3*Delta,2),",",round(Min+4*Delta,2),"]") 
   
   elevationcut <- factor(elevationcut, levels=c(paste0("[",round(Min,2),",",round(Min+Delta,2),")"), 
                                                 paste0("[",round(Min+Delta,2),",",round(Min+2*Delta,2),")"), 
                                                 paste0("[",round(Min+2*Delta,2),",",round(Min+3*Delta,2),")"), 
                                                 paste0("[",round(Min+3*Delta,2),",",round(Min+4*Delta,2),"]")))
   
   levels(elevationcut)
     
   PlotData2$ElevationRange <- elevationcut

   head(PlotData2)

   levels(PlotData2$ElevationRange)
   
   ## Find out the number of unique sites across all 4 elevation ranges combined
   
   nrow(PlotData2)

   UniqueSiteNumber <- length(unique(PlotData2$SiteNo))
   
   ## Find out the number of unique sites in each separate elevation range
    
   UniqueSiteNumberE1 <- length(unique(PlotData2$SiteNo[PlotData2$ElevationRange %in% levels(PlotData2$ElevationRange)[1]]))
   UniqueSiteNumberE2 <- length(unique(PlotData2$SiteNo[PlotData2$ElevationRange %in% levels(PlotData2$ElevationRange)[2]]))
   UniqueSiteNumberE3 <- length(unique(PlotData2$SiteNo[PlotData2$ElevationRange %in% levels(PlotData2$ElevationRange)[3]]))
   UniqueSiteNumberE4 <- length(unique(PlotData2$SiteNo[PlotData2$ElevationRange %in% levels(PlotData2$ElevationRange)[4]]))
   
   UniqueSiteNumberE1
   UniqueSiteNumberE2
   UniqueSiteNumberE3
   UniqueSiteNumberE4
   
   ## Rename elevation ranges to E1, E2, E3 and $4, respectively

   PlotData2$ElevationRangeRenamed <- factor(PlotData2$ElevationRange, levels=levels(PlotData2$ElevationRange), labels=paste0("E",1:nlevels(PlotData2$ElevationRange)))
    
   ## Create plot annotation which explains the elevation ranges used

   l1 <- paste0("E",1:nlevels(PlotData2$ElevationRange))
   l2 <-  levels(PlotData2$ElevationRange)
   l1
   l2

   l <- paste0(l1,"=", l2)
   l

   l <- paste(l, collapse=", ")
   l

   l <- paste0("Elevation Ranges:  ",l)
   l

   ## Create spatial stacked barplots for each of the available tree species 

   plot_list <- list()

   m <- NULL
   M <- NULL

   for (i in 1:length(unique(PlotData2$Spp))) {


       ## Extract data corresponding to the i-th tree species 

       SubsetPlotData2 <- subset(PlotData2, Spp %in% unique(PlotData2$Spp)[i])

       SubsetPlotData2$FutureSuit

       levels(SubsetPlotData2$FutureSuit)
       
       ## Drop those Future Suitability values which are NOT represented in the data 
       ## for the i-th tree species  

       SubsetPlotData2$FutureSuitDrop <- droplevels(SubsetPlotData2$FutureSuit)

       ## Set colour coding according to values of Future Suitability

       SubsetPlotData2$color.codes <- rep(NA, length(SubsetPlotData2$FutureSuitDrop))
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "0"] <- "darkgrey"
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "1"] <- "blue"
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "2"] <- "red"
       SubsetPlotData2$color.codes[SubsetPlotData2$FutureSuitDrop %in% "3"] <- "forestgreen"

       table(SubsetPlotData2$color.codes)

       SubsetPlotData2Unique <- unique(SubsetPlotData2[, c("FutureSuitDrop", "color.codes")])
       SubsetPlotData2Unique

       require(dplyr)

       SubsetPlotData2Unique <- arrange(SubsetPlotData2Unique, FutureSuitDrop)
       SubsetPlotData2Unique

       # count the needed levels of a factor
       number <- nrow(SubsetPlotData2Unique)

       # repeat the given colors enough times
       palette <- SubsetPlotData2Unique$color.codes

       ## Eliminate duplicate rows in the SubsetPlotData2
       
       UniqueSubsetPlotData2 <- unique(SubsetPlotData2)
       
       nrow(SubsetPlotData2)
       nrow(UniqueSubsetPlotData2)
       
       t <- table(UniqueSubsetPlotData2$FutureSuit, UniqueSubsetPlotData2$ElevationRangeRenamed)

       t
       
       ## t is a cross-table of Future Suitability (rows) by Elevation Range (columns).
       ## The numbers reported in this cross-table represent number of sites predicted 
       ## to have a specific Future Suitability for each elevation range, given 
       ## a Future Period, Tile Number, Site Series and Tree Species.
       ##   
       ## Example of t: 
       ##    E1 E2 E3 E4
       ##  1 35 74 34  3
       ##  2  0 14 50 16
       ##  3  0  0  0  0


       ## t1 will consist of a column vector of elevation ranges. 
       ## For the example of t provided above, t1 will look like this (in row form): 
       ##  "E1" "E1" "E1" "E2" "E2" "E2" "E3" "E3" "E3" "E4" "E4" "E4"
       ## Note that each elevation range is repeated x times, with x being the number of 
       ## Future Suitability values represented in the rows of t.   

       t1 <- rep(colnames(t), each = nrow(t))

       t1

       ## t2 will consist of a column vector of Future Suitability values. 
       ## For the example of t provided above, t2 will look like this (in row form): 
       ## "1" "2" "3" "1" "2" "3" "1" "2" "3" "1" "2" "3". 
       ## Thus, the suitability values 1, 2, 3 predicted for the sites which 
       ## correspond to a given choice of Future Period, Tile Number, Site Series and Tree Species
       ## are repeated once for each elevation range.  Recall that there are 
       ## 4 elevation ranges, E1, E2, E3 and E4. 
       

       t2 <- rep(rownames(t), ncol(t))

       t2

       ## t3 will list in column format the number of sites in each of the four elevation ranges
       ## which are predicted to have a certain Future Suitability, 
       ## for a given choice of Future Period, Tile Number, Site Series and Tree Species. 
       ## For the example of t provided above, the row form of t3 will look like: 
       ## 35  0  0 74 14  0 34 50  0  3 16  0

       t3 <- array(t)

       t3
       
       ## combine together t1, t2 and t3 to obtain tt
       ## For the example of t considered above, tt will look like this: 
       ##    t1 t2 t3
       ## 1  E1  1 35
       ## 2  E1  2  0
       ## 3  E1  3  0
       ## 4  E2  1 74
       ## 5  E2  2 14
       ## 6  E2  3  0
       ## 7  E3  1 34
       ## 8  E3  2 50
       ## 9  E3  3  0
       ## 10 E4  1  3
       ## 11 E4  2 16
       ## 12 E4  3  0

       tt <- cbind.data.frame(t1, t2, t3)
       
       ## Add a column t3.total which will list the total number of sites "present" in 
       ## each elevation range.
       ## 
       ## Use t3.total as a basis for computing t4.
       ##           
       ## t4 will list in column format the percentage of sites in each of the four elevation ranges
       ## which are predicted to have a certain Future Suitability, 
       ## for a given choice of Future Period, Tile Number, Site Series and Tree Species. 
       ## The percentage is computed with reference to the total number of sites "present" 
       ## in that elevation range, with "presence" determined by counting the unique number of sites
       ## across a given choice of Future Period, Tile Number and Site Series.   

       t3.total <- NULL 
       t3.total[tt$t1 %in% "E1"] <- UniqueSiteNumberE1
       t3.total[tt$t1 %in% "E2"] <- UniqueSiteNumberE2
       t3.total[tt$t1 %in% "E3"] <- UniqueSiteNumberE3
       t3.total[tt$t1 %in% "E4"] <- UniqueSiteNumberE4

       tt$t3.total <- t3.total

       tt$t4 <- tt$t3/tt$t3.total
       
       tt$t4 <- tt$t4*100

       tt 
       
       ## Add colour codes for each Future Suitability value

       tt$color.codes <- rep(NA, nrow(tt))
       tt$color.codes[tt$t2 %in% "0"] <- "grey35"   # "sandybrown"
       tt$color.codes[tt$t2 %in% "1"] <- "#178CCB" # "blue"
       tt$color.codes[tt$t2 %in% "2"] <- "#CC0000" # "red"
       tt$color.codes[tt$t2 %in% "3"] <- "#90BD31" # "forestgreen"

       tt$t2 <- as.character(tt$t2)

       ## Define tall to indicate how many sites achieve a specific Future Suitability value 
       ## across all four elevation ranges combined 
       ## Example of what tall could look like: 
       ##    1   2   3    <- Future Suitability Values
       ##  146  80   0    <- Number of Sites 


       tall <- table(UniqueSubsetPlotData2$FutureSuit)

       ## Define tadd to list the values of t1, t2, t3 and t4 for all four elevation ranges combined, 
       ## separately for each Future Suitability value  
       ## For example: 
       ##        t1 t2  t3       t4
       ##   1 E1-E4  1 146 64.60177
       ##   2 E1-E4  2  80 35.39823
       ##   3 E1-E4  3   0  0.00000


       tadd <- data.frame(t1=rep("E1-E4",length(names(tall))),
                          t2=names(tall),
                          t3=as.numeric(tall),
                          t4=as.numeric(tall)/UniqueSiteNumber*100)

       ## Add colour codes to Future Suitability values listed in tadd 

       tadd$color.codes <- rep(NA, nrow(tadd))
       tadd$color.codes[tadd$t2 %in% "0"] <- "grey35"   # "sandybrown"
       tadd$color.codes[tadd$t2 %in% "1"] <- "#178CCB" # "blue"
       tadd$color.codes[tadd$t2 %in% "2"] <- "#CC0000" # "red"
       tadd$color.codes[tadd$t2 %in% "3"] <- "#90BD31" # "forestgreen"

       tadd$t2 <- as.character(tadd$t2)

       ## tadd$t2[tadd$t2 %in% "0"] <- "FS = 0"
       ## tadd$t2[tadd$t2 %in% "1"] <- "FS = 1"
       ## tadd$t2[tadd$t2 %in% "2"] <- "FS = 2"
       ## tadd$t2[tadd$t2 %in% "3"] <- "FS = 3"

       tt$t3.total <- NULL 

       tt <- rbind.data.frame(tadd, tt)

       tt
       
       ## Example of what tt could look like: 
       ## > tt
       ##       t1 t2  t3        t4 color.codes
       ## 1  E1-E4  1 146  64.60177     #178CCB
       ## 2  E1-E4  2  80  35.39823     #CC0000
       ## 3  E1-E4  3   0   0.00000     #90BD31
       ## 4     E1  1  35 100.00000     #178CCB
       ## 5     E1  2   0   0.00000     #CC0000
       ## 6     E1  3   0   0.00000     #90BD31
       ## 7     E2  1  74  84.09091     #178CCB
       ## 8     E2  2  14  15.90909     #CC0000
       ## 9     E2  3   0   0.00000     #90BD31
       ## 10    E3  1  34  40.47619     #178CCB
       ## 11    E3  2  50  59.52381     #CC0000
       ## 12    E3  3   0   0.00000     #90BD31
       ## 13    E4  1   3  15.78947     #178CCB
       ## 14    E4  2  16  84.21053     #CC0000
       ## 15    E4  3   0   0.00000     #90BD31

         
       cols <- c("0" = "grey35", "1" = "#178CCB", "2" = "#CC0000", "3" = "#90BD31")

       ttsub <- subset(tt, t4 > 0)
       ## data=ttsub

       require(grid)
       require(ggplot2)
          
       # ---
       # extract current suitability from TreeSppSuit on the basis 
       # of the Site Series and Tree Species information   
       
       UniqueSiteSeries <- unique(SubsetPlotData2$Site.Series)
       UniqueSpp <- unique(SubsetPlotData2$Spp)  # unique tree species
       
       CurrentSuitability <- subset(TreeSppSuit, (Unit %in% UniqueSiteSeries) & (Spp %in% UniqueSpp), select=Suitability)
       
       CurrentSuitability <- as.numeric(CurrentSuitability)
       
       CurrentSuitability
       
       if (is.na(CurrentSuitability)) { CurrentSuitability <- 0 }  
       
       #---

       g <- ggplot(data=ttsub, aes(x=t1, y=t4, fill=t2), environment=.e) +  # environment=.e # use t3 for raw counts; use t4 for percentages computed with the row totals
            geom_bar(stat="identity") +  # stat="identity"
            ## facet_wrap(~t1, ncol=2) +
            ggtitle(paste(## "Tile Number:", TileChoice, ",",
                       ## "Site Series:", SiteSeriesChoice, ",",
                       "Tree Species:", unique(SubsetPlotData2$Spp), "=", CurrentSuitability)) +
            labs(x = "Elevation (m)" , y = "% Sites", fill="FS") +
            ## xlab("Elevation") +
            ## ylab("% Sites")+
            theme_bw()  +
            theme(plot.title = element_text(hjust=0.5, size=10),
                  plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
                  legend.position="top",
                  text = element_text(size=9) ,
                  axis.text.x = element_text(angle=45, hjust=1),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()
                  ) +
            ## aes(fill=t2) +
            annotate("text",x=factor(levels(ttsub$t1), levels=levels(ttsub$t1)), y=rep(100,5), 
                    label="", vjust=-1, color="white", size=2.5) +
            scale_x_discrete(limits=levels(ttsub$t1)) + 
            scale_fill_manual(values=cols)
            

       ## print(g)

       ## g <- ggplot(data=tt, aes(x=t2, y=t4, fill=t1)) +   # use t3 for raw counts; use t4 for percentages computed with the row totals
       ## geom_bar(stat="identity", position="dodge")
       ##  theme_bw()

       ## g

       
       g <- g + theme(legend.margin=margin(t = -0.1, unit='cm'), legend.key.size=unit(0.4,"cm"))


       plot_list[[i]] <- g



   }


   require(gridExtra)

   ## m <- min(m)[1]
   ## M <- max(M)[1]

   ## m
   ## M

   ## for (i in 1:length(unique(PlotData2$Spp))) {
   ##     plot_list[[i]] <- plot_list[[i]] + ylim(m,M + (M-m)/20)
   ## }



   nplots <- length(plot_list)

   if (nplots==1) {myncol <- 1; mynrow <- 1}
   if (nplots==2) {myncol <- 2; mynrow <- 1}
   if (nplots==3) {myncol <- 3; mynrow <- 1}

   if (nplots==4) {myncol <- 2; mynrow <- 2}
   
   if (nplots==5) {myncol <- 3; mynrow <- 2}
   if (nplots==6) {myncol <- 3; mynrow <- 2}
   
   if (nplots==7) {myncol <- 3; mynrow <- 3}
   if (nplots==8) {myncol <- 3; mynrow <- 3}
   if (nplots==9) {myncol <- 3; mynrow <- 3}
   
   if (nplots==10) {myncol <- 4; mynrow <- 3}
   if (nplots==11) {myncol <- 4; mynrow <- 3}
   if (nplots==12) {myncol <- 4; mynrow <- 3}
   
   if (nplots==13) {myncol <- 4; mynrow <- 4}
   if (nplots==14) {myncol <- 4; mynrow <- 4}
   if (nplots==15) {myncol <- 4; mynrow <- 4}
   if (nplots==16) {myncol <- 4; mynrow <- 4}
   
   if (nplots==17) {myncol <- 4; mynrow <- 5}
   if (nplots==18) {myncol <- 4; mynrow <- 5}
   if (nplots==19) {myncol <- 4; mynrow <- 5}
   if (nplots==20) {myncol <- 4; mynrow <- 5}
   
   mywidth <- 2.1*myncol; myheight <- 2.5*mynrow


   res <- list(plots = do.call(grid.arrange, c(plot_list, list(ncol=myncol,
                        top=paste0("Future Period: ", FuturePeriodChoice, "\n ", "Tile Number: ", TileChoice, ", ", "Site Series: ", SiteSeriesChoice, "\n", l)))
                        ),
               nplots = nplots,
               width = mywidth,
               height = myheight)
               

   return(res)


}

## windows()
## SpatialStackedBarplot(PlotData, FuturePeriodChoice, TileChoice, SiteSeriesChoice)$plots

#====================================================================================================
# http://stackoverflow.com/questions/31212659/r-function-to-export-currently-active-r-plot-to-powerpoint-word-libreoffice
#===========================================================================

## library(foreach)
## library(doParallel)

## df1 <- PlotData[,c("FuturePeriod","Tile","Site.Series")]
## nrow(df1)
## df2 <- df1[!duplicated(t(apply(df1, 1, sort))),]
## nrow(df2)

## no_cores <- detectCores() - 1

## cl <- makeCluster(no_cores)
## registerDoParallel(cl)


## foreach (i = 1:nrow(df2)) %dopar%  { # nesting operator

for (i in 1:length(unique(PlotData$FuturePeriod))) {

    FuturePeriodChoice <- unique(PlotData$FuturePeriod)[i]

    cat("Future Period:", FuturePeriodChoice, "\n")
    
    ## TileChoice is the same 
    
    ## Start Report
    
    source("startReport.r")

    source("endReport.r")

    require("ReporteRs")

    docx.file <- paste0("Spatial_Plots_","Tile_",TileChoice,"_Future_Period_",FuturePeriodChoice,".docx")

    startReport(template=paste0("Template_ReporteRs.docx"),
            plotwidth=6, plotheight=7)


    PlotData0 <- subset(PlotData, FuturePeriod %in% FuturePeriodChoice)
    PlotData1 <- subset(PlotData0, Tile %in% TileChoice)
    
    
    for (j in 1:length(unique(PlotData1$Site.Series))) {
    
        SiteSeriesChoice <- unique(PlotData1$Site.Series)[j]
    
        cat("Site Series: ", SiteSeriesChoice, "\n\n")
        
        ## doc = addSection(doc, landscape=TRUE)  # ncol=1

        require("gridGraphics")

        #===========================================================================

        figurecaption <- paste0("Future Suitability (FS) as a function of ",
                        "Longitude and Latitude, displayed separately for each of the tree species present in ",
                        "Tile Number ", TileChoice, " for the Site Series ", SiteSeriesChoice, 
                        " corresponding to the Future Period ", FuturePeriodChoice, ". ", 
                        "Given a tree species, possible values of FS consist of 0, 1, 2 or 3. ",
                        "For each tree species, Current Suitability is indicated next to the tree species annotation.")

        tmp <- SpatialScatterplot(PlotData, FuturePeriodChoice, TileChoice, SiteSeriesChoice)

        plots <- tmp$plots

        nplots <- tmp$nplots

        mywidth <- tmp$width
        myheight <- tmp$height


        doc = addPlot(doc,
            fun=grid.draw,
            x=plots,
            width=mywidth, height=myheight,
            pointsize=10)

        ## doc = addPlot(doc,
        ##            fun=grid.draw,
        ##            x=myplot,
        ##            width=7, height=6.5,
        ##            pointsize=10)

        doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

        doc = addParagraph(doc, value = " ")

        #===========================================================================

        figurecaption <- paste0("Percentages of sites predicted to achieve each possible Future Suitability (FS) as a function of elevation range, ",
                        "displayed separately for each of the tree species present in ",
                        "Tile Number ", TileChoice, " for the Site Series ", SiteSeriesChoice, 
                        " corresponding to the Future Period ", FuturePeriodChoice, ". ",
                        "Given a tree species, possible values of FS consist of 0, 1, 2 or 3. ", 
                        "For each tree species, Current Suitability is indicated next to the tree species annotation.")


        ## myplot <- SpatialSideBySideBarplot(Data, TileChoice, SiteSeriesChoice)

        tmp <- SpatialStackedBarplot(PlotData, FuturePeriodChoice, TileChoice, SiteSeriesChoice)

        plots <- tmp$plots

        nplots <- tmp$nplots
        
        mywidth <- tmp$width
        myheight <- tmp$height

        doc = addPlot(doc,
            fun=grid.draw,
            x=plots,
            width=mywidth, height=myheight,
            pointsize=10)

        doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

        rm(tmp)

    } # end for j

    #============================================================================

    endReport(docx.file=docx.file)

}  # end for i

## stopCluster(cl)
