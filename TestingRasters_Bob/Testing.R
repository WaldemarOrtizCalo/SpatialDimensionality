#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-12-23 

# Purpose: This script is being used to test the three different alternatives
# for raster calculations in the spatial dimensionality study.

###############################################################################
#   Library / Functions / Data                                              ####
library(raster)
library(stringr)
library(pbapply)
library(future.apply)
library(foreach)

#      Library                                                              ####
library(raster)
library(stringr)
library(pbapply)
library(future.apply)
library(foreach)
library(ggplot2)

#      Functions                                                            ####
source("2.Scripts/2.Functions/Auto_UTM_ID.R") 
source("2.Scripts/2.Functions/WOC_RasterName.R")
source("2.Scripts/2.Functions/FactorCalculator_Agg.R")
source("2.Scripts/2.Functions/PercentDifferenceFunction.R")
source("2.Scripts/2.Functions/FactorCalculator.R")

#      Data                                                                 ####
RasterFilepaths<-list.files("G:\\RAW_DEM_Tiles",
                            recursive = F,
                            full.names = T,
                            pattern = "dem")

Sampled_Rasters <-sample(RasterFilepaths,100,replace = F)

ZoneList<-read.csv("1.Data\\z.UTM_Reprojections\\DF_UTM_Zones.csv")

###############################################################################
#   Home Range Sampling [Development]                                       ####
#      Sampler 1 [No correcting for Base cell Resolution]                   ####

# Code For Sampler 1
HR_Sampler1 <- function(RasterFilepath,HR_Sizes,sample.size){
  tryCatch({
    
    #### Notes                                                              ####
    # RasterFilepath = Filepath of the raster that will be processed
    # HR_Sizes       = List of the home range sizes that you want to be sampled
    # sample.size    = Quantity of "home ranges" to be extracted
    
    #### Data Import and Transformations                                    ####
    
    # Raster Import And Reprojecting
    Raster<-raster(RasterFilepath)
    Raster_Reprojected <- projectRaster(Raster,
                                        crs = paste0("+proj=utm +zone=",
                                                     Auto_UTM_ID(Raster),
                                                     " +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    # Naming the Raster as DEM to make sure it is consistent
    names(Raster_Reprojected) <- "DEM"
    
    
    ####   Percent Difference in Surface Area Raster                        ####
    
    # Creating the Percent Difference Raster 
    Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected)
    
    # Naming the Raster as PercentDifference to make sure it is consistent
    names(Raster_PDif)<-"PercentDifference" 
    
    ####   TRI Raster                                                       ####
    
    # Creating the TRI Raster
    Raster_TRI <- terrain(Raster_Reprojected,opt="TRI")
    
    # Naming the Raster as TRI to make sure it is consistent
    names(Raster_TRI)<-"TRI" 
    
    ####   Eliminating Zero values from DEM and PercentDifference Raster    ####
    
    # Eliminating zeros from the DEM raster.
    Raster_Reprojected <- overlay(Raster_Reprojected, 
                                  Raster_TRI, 
                                  fun = function(x, y) {
                                    x[is.na(y[])] <- NA
                                    return(x)
                                  })
    
    # Renaming the DEM raster again to make sure its consistent
    names(Raster_Reprojected) <- "DEM"
    
    # Eliminating zeros from the PercentDifference raster.
    Raster_PDif <- overlay(Raster_PDif,
                           Raster_TRI,
                           fun = function(x, y) {
                             x[is.na(y[])] <- NA
                             return(x)
                           })
    
    # Renaming the PercentDifference raster again to make sure its consistent
    names(Raster_PDif)<-"PercentDifference"
    
    ####   Stack                                                            ####

    #   Creating a Stack of the created Rasters
    Stack<-raster::stack(Raster_Reprojected,
                         Raster_TRI,
                         Raster_PDif)
    
    #### Home Range Sampling                                                ####
    
    # Coarsening the Raster Layers and Sampling Home Ranges
    HRs <- foreach(i = 1:length(HR_Sizes), .combine = rbind) %do% {
      
      # Changing Raster Resolutions to simulate Home Ranges
      Random_CellSample<- aggregate(Stack,
                                    fact = c(FactorCalculator(xres(Raster_Reprojected),i),FactorCalculator(yres(Raster_Reprojected),i)),
                                    fun=mean,na.rm=TRUE) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()
      
      # Sampling Home Range Cells
      Random_CellSample <- cbind(Size_Category=paste0(HR_Sizes[i],"km"),Random_CellSample)
      
    }
    
    #### Data Aggregation and Extracting Results                            ####
    
    # Raster Name
    RasterName<-WOC_RasterName(Raster)
    
    # Raster Resolution in Square Meters 
    RasterRes <- prod(res(Raster_Reprojected))
    
    # x Dimensions of Raster Cell
    xres <- xres(Raster_Reprojected)
    
    # y Dimension of Raster Cell 
    yres <- yres(Raster_Reprojected)
    
    # Organizing Results into a Dataframe 
    Final_DF<-cbind(RasterName,RasterRes,xres,yres,HRs)
    
    return(Final_DF)},
    error=function(e) c(NA))
}

# Running Sampler 1 with one Raster
S1 <- HR_Sampler1(RasterFilepath = Sampled_Rasters[1],
                  HR_Sizes       = c(10,100),
                  sample.size    = 5)

#      Sampler 2 [Correcting for Base cell Resolution]                      ####

# Code For Sampler 2
HR_Sampler2 <- function(RasterFilepath,BaseAgg_size,HR_Sizes,sample.size){
  tryCatch({
    
    #### Notes                                                              ####
    # RasterFilepath = Filepath of the raster that will be processed.
    # BaseAgg_size   = The Raster Cell size you want in meters.
    # HR_Sizes       = List of the home range sizes in square km.
    # sample.size    = Quantity of "home ranges" to be extracted.
    
    #### Data Import and Transformations                                    ####
    
    # Raster Import,Reprojecting, and Standardizing Base Cell Size
    Raster<-raster(RasterFilepath)
    Raster_Reprojected <- projectRaster(Raster,
                                        crs = paste0("+proj=utm +zone=",
                                                     Auto_UTM_ID(Raster),
                                                     " +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    Raster_Reprojected <- aggregate(Raster_Reprojected, 
                                    fact = c(FactorCalculator_Agg(xres(Raster_Reprojected),BaseAgg_size),
                                             FactorCalculator_Agg(yres(Raster_Reprojected),BaseAgg_size)))
    
    # Naming the Raster as DEM to make sure it is consistent
    names(Raster_Reprojected) <- "DEM"
    
    
    ####   Percent Difference in Surface Area Raster                        ####
    
    # Creating the Percent Difference Raster 
    Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected)
    
    # Naming the Raster as PercentDifference to make sure it is consistent
    names(Raster_PDif)<-"PercentDifference" 
    
    ####   TRI Raster                                                       ####
    
    # Creating the TRI Raster
    Raster_TRI <- terrain(Raster_Reprojected,opt="TRI")
    
    # Naming the Raster as TRI to make sure it is consistent
    names(Raster_TRI)<-"TRI" 
    
    ####   Eliminating Zero values from DEM and PercentDifference Raster    ####
    
    # Eliminating zeros from the DEM raster.
    Raster_Reprojected <- overlay(Raster_Reprojected, 
                                  Raster_TRI, 
                                  fun = function(x, y) {
                                    x[is.na(y[])] <- NA
                                    return(x)
                                  })
    
    # Renaming the DEM raster again to make sure its consistent
    names(Raster_Reprojected) <- "DEM"
    
    # Eliminating zeros from the PercentDifference raster.
    Raster_PDif <- overlay(Raster_PDif,
                           Raster_TRI,
                           fun = function(x, y) {
                             x[is.na(y[])] <- NA
                             return(x)
                           })
    
    # Renaming the PercentDifference raster again to make sure its consistent
    names(Raster_PDif)<-"PercentDifference"
    
    ####   Stack                                                            ####
    
    #   Creating a Stack of the created Rasters
    Stack<-raster::stack(Raster_Reprojected,
                         Raster_TRI,
                         Raster_PDif)
    
    #### Home Range Sampling                                                ####
    
    # Coarsening the Raster Layers and Sampling Home Ranges
    HRs <- foreach(i = 1:length(HR_Sizes), .combine = rbind) %do% {
      
      # Changing Raster Resolutions to simulate Home Ranges
      Random_CellSample<- aggregate(Stack,
                                    fact = c(FactorCalculator(xres(Raster_Reprojected),i),FactorCalculator(yres(Raster_Reprojected),i)),
                                    fun=mean,na.rm=TRUE) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()
      
      # Sampling Home Range Cells
      Random_CellSample <- cbind(Size_Category=paste0(HR_Sizes[i],"km"),Random_CellSample)
      
    }
    
    #### Data Aggregation and Extracting Results                            ####
    
    # Raster Name
    RasterName<-WOC_RasterName(Raster)
    
    # Raster Resolution in Square Meters 
    RasterRes <- prod(res(Raster_Reprojected))
    
    # x Dimensions of Raster Cell
    xres <- xres(Raster_Reprojected)
    
    # y Dimension of Raster Cell 
    yres <- yres(Raster_Reprojected)
    
    # Organizing Results into a Dataframe 
    Final_DF<-cbind(RasterName,RasterRes,xres,yres,HRs)
    
    return(Final_DF)},
    error=function(e) c(NA))
}


# Running Sampler 2 with one Raster
S2 <- HR_Sampler2(RasterFilepath = Sampled_Rasters[1],
                  BaseAgg_size   = 30,
                  HR_Sizes       = c(10,100),
                  sample.size    = 5)



###############################################################################
#   Home Range Sampling [Running a small sample size]                       ####
#      Sampler 1                                                            ####

# Code For Sampler 1
HR_Sampler1 <- function(RasterFilepath,HR_Sizes,sample.size){
  tryCatch({
    
    #### Notes                                                              ####
    # RasterFilepath = Filepath of the raster that will be processed
    # HR_Sizes       = List of the home range sizes that you want to be sampled
    # sample.size    = Quantity of "home ranges" to be extracted
    
    #### Data Import and Transformations                                    ####
    
    # Raster Import And Reprojecting
    Raster<-raster(RasterFilepath)
    Raster_Reprojected <- projectRaster(Raster,
                                        crs = paste0("+proj=utm +zone=",
                                                     Auto_UTM_ID(Raster),
                                                     " +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    # Naming the Raster as DEM to make sure it is consistent
    names(Raster_Reprojected) <- "DEM"
    
    
    ####   Percent Difference in Surface Area Raster                        ####
    
    # Creating the Percent Difference Raster 
    Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected)
    
    # Naming the Raster as PercentDifference to make sure it is consistent
    names(Raster_PDif)<-"PercentDifference" 
    
    ####   TRI Raster                                                       ####
    
    # Creating the TRI Raster
    Raster_TRI <- terrain(Raster_Reprojected,opt="TRI")
    
    # Naming the Raster as TRI to make sure it is consistent
    names(Raster_TRI)<-"TRI" 
    
    ####   Eliminating Zero values from DEM and PercentDifference Raster    ####
    
    # Eliminating zeros from the DEM raster.
    Raster_Reprojected <- overlay(Raster_Reprojected, 
                                  Raster_TRI, 
                                  fun = function(x, y) {
                                    x[is.na(y[])] <- NA
                                    return(x)
                                  })
    
    # Renaming the DEM raster again to make sure its consistent
    names(Raster_Reprojected) <- "DEM"
    
    # Eliminating zeros from the PercentDifference raster.
    Raster_PDif <- overlay(Raster_PDif,
                           Raster_TRI,
                           fun = function(x, y) {
                             x[is.na(y[])] <- NA
                             return(x)
                           })
    
    # Renaming the PercentDifference raster again to make sure its consistent
    names(Raster_PDif)<-"PercentDifference"
    
    ####   Stack                                                            ####
    
    #   Creating a Stack of the created Rasters
    Stack<-raster::stack(Raster_Reprojected,
                         Raster_TRI,
                         Raster_PDif)
    
    #### Home Range Sampling                                                ####
    
    # Coarsening the Raster Layers and Sampling Home Ranges
    HRs <- foreach(i = 1:length(HR_Sizes), .combine = rbind) %do% {
      
      # Changing Raster Resolutions to simulate Home Ranges
      Random_CellSample<- aggregate(Stack,
                                    fact = c(FactorCalculator(xres(Raster_Reprojected),i),FactorCalculator(yres(Raster_Reprojected),i)),
                                    fun=mean,na.rm=TRUE) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()
      
      # Sampling Home Range Cells
      Random_CellSample <- cbind(Size_Category=paste0(HR_Sizes[i],"km"),Random_CellSample)
      
    }
    
    #### Data Aggregation and Extracting Results                            ####
    
    # Raster Name
    RasterName<-WOC_RasterName(Raster)
    
    # Raster Resolution in Square Meters 
    RasterRes <- prod(res(Raster_Reprojected))
    
    # x Dimensions of Raster Cell
    xres <- xres(Raster_Reprojected)
    
    # y Dimension of Raster Cell 
    yres <- yres(Raster_Reprojected)
    
    # Organizing Results into a Dataframe 
    Final_DF<-cbind(RasterName,RasterRes,xres,yres,HRs)
    
    return(Final_DF)},
    error=function(e) c(NA))
}

# Running Sampler 1 with one Raster
plan("multiprocess", workers = 4)

HR_Sampling1<-future_lapply(Sampled_Rasters,
                            HR_Sampler1,
                            HR_Sizes = c(5,10,100,250),
                            sample.size = 5)

plan("sequential")

# Joining the Samples into one coherent Dataframe

HR_Samples1 <- do.call(rbind,HR_Sampling1)

write.csv(HR_Samples1,"TestingRasters_Bob/HR_Samples1.csv",
          row.names = F)

#      Sampler 2                                                            ####

# Code For Sampler 2
HR_Sampler2 <- function(RasterFilepath,BaseAgg_size,HR_Sizes,sample.size){
  tryCatch({
    
    #### Notes                                                              ####
    # RasterFilepath = Filepath of the raster that will be processed.
    # BaseAgg_size   = The Raster Cell size you want in meters.
    # HR_Sizes       = List of the home range sizes in square km.
    # sample.size    = Quantity of "home ranges" to be extracted.
    
    #### Data Import and Transformations                                    ####
    
    # Raster Import,Reprojecting, and Standardizing Base Cell Size
    Raster<-raster(RasterFilepath)
    Raster_Reprojected <- projectRaster(Raster,
                                        crs = paste0("+proj=utm +zone=",
                                                     Auto_UTM_ID(Raster),
                                                     " +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    Raster_Reprojected <- aggregate(Raster_Reprojected, 
                                    fact = c(FactorCalculator_Agg(xres(Raster_Reprojected),BaseAgg_size),
                                             FactorCalculator_Agg(yres(Raster_Reprojected),BaseAgg_size)))
    
    # Naming the Raster as DEM to make sure it is consistent
    names(Raster_Reprojected) <- "DEM"
    
    
    ####   Percent Difference in Surface Area Raster                        ####
    
    # Creating the Percent Difference Raster 
    Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected)
    
    # Naming the Raster as PercentDifference to make sure it is consistent
    names(Raster_PDif)<-"PercentDifference" 
    
    ####   TRI Raster                                                       ####
    
    # Creating the TRI Raster
    Raster_TRI <- terrain(Raster_Reprojected,opt="TRI")
    
    # Naming the Raster as TRI to make sure it is consistent
    names(Raster_TRI)<-"TRI" 
    
    ####   Eliminating Zero values from DEM and PercentDifference Raster    ####
    
    # Eliminating zeros from the DEM raster.
    Raster_Reprojected <- overlay(Raster_Reprojected, 
                                  Raster_TRI, 
                                  fun = function(x, y) {
                                    x[is.na(y[])] <- NA
                                    return(x)
                                  })
    
    # Renaming the DEM raster again to make sure its consistent
    names(Raster_Reprojected) <- "DEM"
    
    # Eliminating zeros from the PercentDifference raster.
    Raster_PDif <- overlay(Raster_PDif,
                           Raster_TRI,
                           fun = function(x, y) {
                             x[is.na(y[])] <- NA
                             return(x)
                           })
    
    # Renaming the PercentDifference raster again to make sure its consistent
    names(Raster_PDif)<-"PercentDifference"
    
    ####   Stack                                                            ####
    
    #   Creating a Stack of the created Rasters
    Stack<-raster::stack(Raster_Reprojected,
                         Raster_TRI,
                         Raster_PDif)
    
    #### Home Range Sampling                                                ####
    
    # Coarsening the Raster Layers and Sampling Home Ranges
    HRs <- foreach(i = 1:length(HR_Sizes), .combine = rbind) %do% {
      
      # Changing Raster Resolutions to simulate Home Ranges
      Random_CellSample<- aggregate(Stack,
                                    fact = c(FactorCalculator(xres(Raster_Reprojected),i),FactorCalculator(yres(Raster_Reprojected),i)),
                                    fun=mean,na.rm=TRUE) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()
      
      # Sampling Home Range Cells
      Random_CellSample <- cbind(Size_Category=paste0(HR_Sizes[i],"km"),Random_CellSample)
      
    }
    
    #### Data Aggregation and Extracting Results                            ####
    
    # Raster Name
    RasterName<-WOC_RasterName(Raster)
    
    # Raster Resolution in Square Meters 
    RasterRes <- prod(res(Raster_Reprojected))
    
    # x Dimensions of Raster Cell
    xres <- xres(Raster_Reprojected)
    
    # y Dimension of Raster Cell 
    yres <- yres(Raster_Reprojected)
    
    # Organizing Results into a Dataframe 
    Final_DF<-cbind(RasterName,RasterRes,xres,yres,HRs)
    
    return(Final_DF)},
    error=function(e) c(NA))
}


# Running Sampler 2 with one Raster
plan("multiprocess", workers = 4)
HR_Sampling2<-future_lapply(Sampled_Rasters,
                            HR_Sampler2,
                            BaseAgg_size = 30,
                            HR_Sizes = c(5,10,100,250),
                            sample.size = 5)
plan("sequential")

# Joining the Samples into one coherent Dataframe

HR_Samples2 <- do.call(rbind,HR_Sampling2)

write.csv(HR_Samples2,"TestingRasters_Bob/HR_Samples2.csv",
          row.names = F)

###############################################################################
#   Plotting and Exploratory Plots                                          ####
#      Coordinate and Resolution Plots                                      ####
#         HR Sampler 1                                                      ####
#            [Spatial Data]                                                 ####

# Extracting Spatial Data from the Raster names
latNS     <- str_sub(string = HR_Samples1$RasterName,
                     start  = 1,
                     end    = 1)

latCoord  <- str_sub(string = HR_Samples1$RasterName,
                     start  = 2,
                     end    = 3)

longEW    <- str_sub(string = HR_Samples1$RasterName,
                     start  = 4,
                     end    = 4)

longCoord <- str_sub(string = HR_Samples1$RasterName,
                     start  = 5,
                     end    = 7)

# Adding the Spatial Coordinates to the existing Data Frame
HR_Samples1$latNS     <- latNS
HR_Samples1$latCoord  <- latCoord
HR_Samples1$longEW    <- longEW
HR_Samples1$longCoord <- longCoord

#            [Plots]                                                        ####

# Raster Cell Area vs Latitude
ggplot(HR_Samples1,aes(as.numeric(HR_Samples1$latCoord),HR_Samples1$RasterRes))+
  geom_point(aes(color = factor(HR_Samples1$latNS)), size = 2)+
  ggtitle("Raster Area vs. Latidude Coordinate")+
  ylab("Raster Area (m^2)")+
  xlab("Latitude Coordinate")+
  theme_bw()+
  geom_smooth()

# Raster Cell Area vs Longitude
ggplot(HR_Samples1,aes(as.numeric(HR_Samples1$longCoord),HR_Samples1$RasterRes))+
  geom_point(aes(color = factor(HR_Samples1$latNS)), size = 2)+
  ggtitle("Raster Area vs. Longitude Coordinate")+
  ylab("Raster Area (m^2)")+
  xlab("Longitude Coordinate")+
  theme_bw()+
  geom_smooth()

# Latitude vs Raster
ggplot(HR_Samples1,aes(as.numeric(HR_Samples1$latCoord),HR_Samples1$xres))+
  geom_point()+
  ggtitle("Latitude Dim vs. Latidude Coordinate")+
  ylab(" x dimension of Raster Cell (m)")+
  xlab("Latitude Coordinate")+
  theme_bw()+
  geom_smooth()

# Longitude vs Raster
ggplot(HR_Samples1,aes(as.numeric(HR_Samples1$longCoord),HR_Samples1$xres))+
  geom_point()+
  ggtitle("Latitude Dim vs. Latidude Coordinate")+
  ylab(" x dimension of Raster Cell (m)")+
  xlab("Latitude Coordinate")+
  theme_bw()+
  geom_smooth()
  

#         HR Sampler 2                                                      ####
#            [Spatial Data]                                                 ####

# Extracting Spatial Data from the Raster names
latNS     <- str_sub(string = HR_Samples2$RasterName,
                     start  = 1,
                     end    = 1)

latCoord  <- str_sub(string = HR_Samples2$RasterName,
                     start  = 2,
                     end    = 3)

longEW    <- str_sub(string = HR_Samples2$RasterName,
                     start  = 4,
                     end    = 4)

longCoord <- str_sub(string = HR_Samples2$RasterName,
                     start  = 5,
                     end    = 7)

# Adding the Spatial Coordinates to the existing Data Frame
HR_Samples2$latNS     <- latNS
HR_Samples2$latCoord  <- latCoord
HR_Samples2$longEW    <- longEW
HR_Samples2$longCoord <- longCoord

#            [Plots]                                                        ####

# Raster Cell Area vs Latitude
ggplot(HR_Samples2,aes(as.numeric(HR_Samples2$latCoord),HR_Samples2$RasterRes))+
  geom_point(aes(color = factor(HR_Samples2$latNS)), size = 2)+
  ggtitle("Raster Area vs. Latidude Coordinate")+
  ylab("Raster Area (m^2)")+
  xlab("Latitude Coordinate")+
  theme_bw()+
  geom_smooth()

# Raster Cell Area vs Longitude
ggplot(HR_Samples2,aes(as.numeric(HR_Samples2$longCoord),HR_Samples2$RasterRes))+
  geom_point(aes(color = factor(HR_Samples2$latNS)), size = 2)+
  ggtitle("Raster Area vs. Longitude Coordinate")+
  ylab("Raster Area (m^2)")+
  xlab("Longitude Coordinate")+
  theme_bw()+
  geom_smooth()

#         Comparison between sampler 1 and 2                                ####

# Joining the Relevant Data
RasRes1 <- HR_Samples1$RasterRes

RasRes2 <- HR_Samples2$RasterRes

Comparison <- data.frame(Sampler = c(rep(1,2000),rep(2,2000)),
                         RasRes  = c(RasRes1,RasRes2))
# Bar Plot
ggplot(Comparison,aes(as.character(Comparison$Sampler),Comparison$RasRes))+
  geom_boxplot()+
  ggtitle("Raster Cell Surface Area v. HR Sampler")+
  xlab("Home Range Sampler")+
  ylab("Raster Cell Surface Area")+
  theme_bw()

#      Percent Difference and TRI (RAW)                                     ####
#         HR Sampler 1                                                      ####

# General Plot 
ggplot(HR_Samples1, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()

# Faceted Plot 
ggplot(HR_Samples1, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))

#         HR Sampler 2                                                      ####

# Plotting Percent Difference and  TRI 

# General Plot 
ggplot(HR_Samples2, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()

# Faceted Plot 
ggplot(HR_Samples2, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))

#      Percent Difference and TRI (Corrected)                               ####
#         HR Sampler 1                                                      ####

# General Plot 
ggplot(HR_Samples1, aes((TRI/RasterRes),PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()

# Faceted Plot 
ggplot(HR_Samples1, aes((TRI/RasterRes),PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))

#         HR Sampler 2                                                      ####

# Plotting Percent Difference and  TRI 

# General Plot 
ggplot(HR_Samples2, aes((TRI/RasterRes),PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()

# Faceted Plot 
ggplot(HR_Samples2, aes((TRI/RasterRes),PercentDifference))+
  geom_point()+
  ggtitle(" Home Range Sampler 1: Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))












