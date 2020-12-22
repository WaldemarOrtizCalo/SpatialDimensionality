# Importing the Raster Data 
RasterFilepaths<-list.files("G:\\RAW_DEM_Tiles",
                            recursive = F,
                            full.names = T,
                            pattern = "dem")

ZoneList<-read.csv("1.Data\\z.UTM_Reprojections\\DF_UTM_Zones.csv")



# Things used in the Previous Function
RasterFilepath <- RasterFilepaths[10]
BaseAgg_size <- 30
HR_Sizes <- c(5,10,100,250)
sample.size <- 1

# Function Body
Raster<-raster(RasterFilepath)
Raster_Reprojected <- projectRaster(Raster,
                                    crs = paste0("+proj=utm +zone=",Auto_UTM_ID(Raster)," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

Raster_Reprojected <- aggregate(Raster_Reprojected, fact = c(FactorCalculator_Agg(xres(Raster_Reprojected),BaseAgg_size),FactorCalculator_Agg(yres(Raster_Reprojected),BaseAgg_size)))

names(Raster_Reprojected) <- "DEM"

RasterName<-WOC_RasterName(Raster)
RasterRes <- prod(res(Raster_Reprojected))

# Percent Difference in Surface Area Raster

Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected)

names(Raster_PDif)<-"PercentDifference" # Renaming Raster Layer

# TRI Raster
Raster_TRI <- terrain(Raster_Reprojected,opt="TRI")

names(Raster_TRI)<-"TRI" # Renaming Raster Layer

# Eliminating Zero Values in Other Layers 

Raster_Reprojected <- overlay(Raster_Reprojected, Raster_TRI, fun = function(x, y) {
  x[is.na(y[])] <- NA
  return(x)
})

names(Raster_Reprojected) <- "DEM"

Raster_PDif <- overlay(Raster_PDif, Raster_TRI, fun = function(x, y) {
  x[is.na(y[])] <- NA
  return(x)
})

names(Raster_PDif)<-"PercentDifference"

# Stacks 
Stack<-raster::stack(Raster_Reprojected,
                     Raster_TRI,
                     Raster_PDif)

df <- as.data.frame(Raster_TRI)
min(Stack)

Random_CellSample<- aggregate(Stack,
                              fact = c(FactorCalculator(xres(Raster_Reprojected),5),FactorCalculator(yres(Raster_Reprojected),5)),
                              fun=mean,na.rm=TRUE) 

df <- as.data.frame(cellStats(Random_CellSample,range))

df[1,]

colnames(df) <- paste(colnames(df),"M in", sep = "_")



HRs <- foreach(i = 1:length(HR_Sizes), .combine = rbind) %do% {
  
  Random_CellSample<- aggregate(Stack,
                                fact = c(FactorCalculator(xres(Raster_Reprojected),i),FactorCalculator(yres(Raster_Reprojected),i)),
                                fun=mean,na.rm=TRUE) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()
  
  Random_CellSample <- cbind(Size_Category=paste0(HR_Sizes[i],"km"),Random_CellSample)
  
}



