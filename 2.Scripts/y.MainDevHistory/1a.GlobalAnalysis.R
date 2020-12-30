#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-10-14 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

##  Library                                                                 ####
library(raster)
library(stringr)
library(pbapply)
library(future.apply)
library(foreach)
##  Functions                                                               ####
source("2.Scripts/2.Functions/Auto_UTM_ID.R") 
source("2.Scripts/2.Functions/WOC_RasterName.R")
source("2.Scripts/2.Functions/FactorCalculator.R")
source("2.Scripts/2.Functions/FactorCalculator_Agg.R")
source("2.Scripts/2.Functions/PercentDifferenceFunction.R")
source("2.Scripts/2.Functions/HR_Sampler.R")
source("2.Scripts/2.Functions/RTile_Summaries.R")
source("2.Scripts/2.Functions/GlobalSummary.R")

##  Data                                                                    ####
RasterFilepaths<-list.files("G:\\RAW_DEM_Tiles",
                            recursive = F,
                            full.names = T,
                            pattern = "dem")

ZoneList<-read.csv("1.Data\\z.UTM_Reprojections\\DF_UTM_Zones.csv")

###############################################################################
#   Code Run                                                                ####

# Start of 19000:23000
print(Sys.time())

plan("multiprocess", workers = 4)

Global <- future_lapply(RasterFilepaths[19000:22912], GlobalSummary,BaseAgg_size = 30, HR_Sizes = c(5,10,100,250))

plan("sequential")

print(Sys.time())