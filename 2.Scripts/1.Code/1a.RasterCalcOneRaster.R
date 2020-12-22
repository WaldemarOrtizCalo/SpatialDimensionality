#### Library ####
library(raster)
library(stringr)

#### Functions ####
source("2.Scripts/2.Functions/Auto_UTM_ID.R")
source("2.Scripts/2.Functions/WOC_RasterName.R")
source("2.Scripts/2.Functions/FactorCalculator.R")
source("2.Scripts/2.Functions/FactorCalculator_Agg.R")
source("2.Scripts/2.Functions/PercentDifferenceFunction.R")

#### Data ####
rpaths<-list.files("1.Data/DEMs",
                       recursive = T,
                       full.names = T,
                       pattern = "dem")

ZoneList<-read.csv("1.Data\\z.UTM_Reprojections\\DF_UTM_Zones.csv")

#### Testing One Raster ####

# 
Raster <-raster(rpaths[3])

sample.size=5
BaseAgg_size <- 30

Raster_Reprojected <- projectRaster(Raster,
                                    crs = paste0("+proj=utm +zone=",Auto_UTM_ID(Raster)," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

Raster_Reprojected_BaseAgg <- aggregate(Raster_Reprojected, 
                                        fact = c(FactorCalculator_Agg(xres(Raster_Reprojected),BaseAgg_size),FactorCalculator_Agg(yres(Raster_Reprojected),BaseAgg_size)))

BaseCell_SA <- prod(res(Raster_Reprojected_BaseAgg))

Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected_BaseAgg)

Raster_TRI <- terrain(Raster_Reprojected_BaseAgg,opt="TRI")

Stack<-raster::stack(Raster_Reprojected,
                     Raster_TRI,
                     Raster_PDif)

Random_CellSample_5km<- aggregate(Stack,
                                  fact = c(FactorCalculator(xres(Raster_Reprojected),5),FactorCalculator(yres(Raster_Reprojected),5)),
                                  fun=mean) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()

Random_CellSample_5km <- cbind(Size_Category="5km",Random_CellSample_5km)

Random_CellSample_250km<- aggregate(Stack,
                                  fact = c(FactorCalculator(xres(Raster_Reprojected),250),FactorCalculator(yres(Raster_Reprojected),250)),
                                  fun=mean) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()

Random_CellSample_250km <- cbind(Size_Category="250km",Random_CellSample_250km)

df<-rbind(Random_CellSample_5km,Random_CellSample_250km)
