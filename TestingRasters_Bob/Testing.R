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

##  Library                                                                 ####

##  Functions                                                               ####
source("2.Scripts/2.Functions/Auto_UTM_ID.R") 
source("2.Scripts/2.Functions/WOC_RasterName.R")
source("2.Scripts/2.Functions/FactorCalculator_Agg.R")
source("2.Scripts/2.Functions/PercentDifferenceFunction.R")
source("2.Scripts/2.Functions/FactorCalculator.R")

##  Data                                                                    ####
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
