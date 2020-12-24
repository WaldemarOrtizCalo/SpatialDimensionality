
#### Library ####
library(raster)
library(stringr)
library(pbapply)
library(future.apply)
library(foreach)

#### Functions ####
source("2.Scripts/2.Functions/Auto_UTM_ID.R") 
source("2.Scripts/2.Functions/WOC_RasterName.R")
source("2.Scripts/2.Functions/FactorCalculator.R")
source("2.Scripts/2.Functions/FactorCalculator_Agg.R")
source("2.Scripts/2.Functions/PercentDifferenceFunction.R")
source("2.Scripts/2.Functions/HR_Sampler.R")
source("2.Scripts/2.Functions/RTile_Summaries.R")

#### Data ####

# Rasters
RasterFilepaths<-list.files("G:\\RAW_DEM_Tiles",
                            recursive = F,
                            full.names = T,
                            pattern = "dem")

Rasters <-sample(RasterFilepaths,10,replace = F)

ZoneList<-read.csv("1.Data\\z.UTM_Reprojections\\DF_UTM_Zones.csv")

#### Sampling ####

# HR Sampler
plan("multiprocess", workers = 4)

HR_Samples<-future_lapply(Rasters,HR_Sampler,BaseAgg_size = 30,HR_Sizes = c(5,10,100,250),sample.size = 5)

plan("sequential")


# Tile Summary

plan("multiprocess", workers = 4)

TileSummaries <- future_lapply(Rasters,RTile_Summaries)

plan("sequential")

# Data Compilation

TileSummariesSample <-do.call(rbind,TileSummaries)

HR_FinalSamples <-do.call(rbind,HR_Samples)

# Data Export

write.csv(TileSummariesSample,
          file = "3.Output/TileSummariesSample.csv",
          row.names = F)

write.csv(HR_FinalSamples,
          file = "3.Output/HR_RawSamples.csv",
          row.names = F)
