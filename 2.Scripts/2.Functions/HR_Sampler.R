HR_Sampler <- function(RasterFilepath,BaseAgg_size,HR_Sizes,sample.size){

  #### Notes ####
  # RasterFilepath = Filepath of the raster that will be processed
  # BaseAgg_size   = Size of the Base Raster tiles once standarized
  # HR_Sizes       = List of the home range sizes that you want to be sampled
  # sample.size    = Quantity of "home ranges" to be extracted
  
  #### Function ####
  tryCatch({
    
    #### Data Preparation ####
    
    # Reproject Raster To UTM
    
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
    
    #### Home Range Sampling ####
    HRs <- foreach(i = 1:length(HR_Sizes), .combine = rbind) %do% {
      
      Random_CellSample<- aggregate(Stack,
                                    fact = c(FactorCalculator(xres(Raster_Reprojected),i),FactorCalculator(yres(Raster_Reprojected),i)),
                                    fun=mean,na.rm=TRUE) %>% sampleRandom(size=sample.size,na.rm=T) %>% as.data.frame()
      
      Random_CellSample <- cbind(Size_Category=paste0(HR_Sizes[i],"km"),Random_CellSample)

    }
    
    RasterName<-WOC_RasterName(Raster)
    RasterRes <- prod(res(Raster_Reprojected))
    xres <- xres(Raster_Reprojected)
    yres <- yres(Raster_Reprojected)
    #### Data Export ####
    Final_DF<-cbind(RasterName,RasterRes,xres,yres,HRs)
    
    return(Final_DF)},
    error=function(e) c(NA))
  
}
