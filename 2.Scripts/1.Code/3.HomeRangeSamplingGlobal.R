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

# Raster Data
RasterFilepaths<-list.files("G:\\RAW_DEM_Tiles",
                            recursive = F,
                            full.names = T,
                            pattern = "dem")

Sampled_Rasters <-sample(RasterFilepaths,2500,replace = F)

ZoneList<-read.csv("1.Data\\z.UTM_Reprojections\\DF_UTM_Zones.csv")

###############################################################################
#   Home Range Sampling                                                     ####
#      Sampler                                                              ####

# Code For Sampler 
HR_Sampler_Global <- function(RasterFilepath,BaseAgg_size,HR_Sizes){
  tryCatch({
    
    #### Notes                                                              ####
    # RasterFilepath = Filepath of the raster that will be processed.
    # BaseAgg_size   = The Raster Cell size you want in meters.
    # HR_Sizes       = List of the home range sizes in square km.
    
    
    #### Data Import and Transformations                                    ####
    
    # Raster Import,Reprojecting, and Standardizing Base Cell Size
    Raster<-raster(RasterFilepath)
    Raster_Reprojected <- projectRaster(Raster,
                                        crs = paste0("+proj=utm +zone=",
                                                     Auto_UTM_ID(Raster),
                                                     " +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    # Aggregating As close to 30 x 30 
    Raster_Reprojected <- aggregate(Raster_Reprojected, 
                                    fact = c(FactorCalculator_Agg(xres(Raster_Reprojected),BaseAgg_size),
                                             FactorCalculator_Agg(yres(Raster_Reprojected),BaseAgg_size)))
    
    # Resampling The Rasters
    DefaultRes <- Raster_Reprojected
    
    res(DefaultRes) <- BaseAgg_size
    
    Raster_Reprojected <- resample(Raster_Reprojected, DefaultRes, method = "bilinear")
    
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
                                    fun=mean,na.rm=TRUE) %>% as.data.frame()
      
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
    
    # Export
    write.csv(Final_DF,paste0("3.Output/GlobalCSV/", RasterName,".csv"),
              row.names = F)
    },
    error=function(e) c(NA))
}


# Batch 6
plan("multiprocess", workers = 4)

HR_Sampling<-future_lapply(RasterFilepaths[15000:17500],
                           HR_Sampler_Global,
                           BaseAgg_size = 30,
                           HR_Sizes = c(5,10,100,250))
plan("sequential")

# Batch 7
plan("multiprocess", workers = 4)

HR_Sampling<-future_lapply(RasterFilepaths[17501:20000],
                           HR_Sampler_Global,
                           BaseAgg_size = 30,
                           HR_Sizes = c(5,10,100,250))
plan("sequential")

# Batch 8
plan("multiprocess", workers = 4)

HR_Sampling<-future_lapply(RasterFilepaths[20000:length(RasterFilepaths)],
                           HR_Sampler_Global,
                           BaseAgg_size = 30,
                           HR_Sizes = c(5,10,100,250))
plan("sequential")
###############################################################################