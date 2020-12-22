RasterCalculationsAgg<-function(RasterFilepath,BaseAgg_size,sample.size){
  tryCatch({
    
    # Reproject Raster To UTM
    Raster<-raster(RasterFilepath)
    Raster_Reprojected <- projectRaster(Raster,
                                        crs = paste0("+proj=utm +zone=",Auto_UTM_ID(Raster)," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
    
    Raster_Reprojected <- aggregate(Raster_Reprojected, fact = c(FactorCalculator_Agg(xres(Raster_Reprojected),BaseAgg_size),FactorCalculator_Agg(yres(Raster_Reprojected),BaseAgg_size)))
    
    names(Raster_Reprojected) <- "DEM"
    
    RasterName<-WOC_RasterName(Raster)
    RasterRes <- prod(res(Raster_Reprojected))
    
    #### Percent Difference in Surface Area ####
    
    Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected)
    names(Raster_PDif)<-"PercentDifference"
    
    #### Landscape Complexity Variables ####
    
    # TRI
    Raster_TRI <- terrain(Raster_Reprojected,opt="TRI")
    names(Raster_TRI)<-"TRI"
    
    #### Stacks ####
    Stack<-raster::stack(Raster_Reprojected,
                         Raster_TRI,
                         Raster_PDif)
    
    # Base
    Random_CellSample_Base<-as.data.frame(sampleRandom(Stack,size=sample.size)) %>%
      rename_all(function(x) paste0("SA_Base_", x))
    
    # 5km
    Random_CellSample_5km<- aggregate(Stack,
                                      fact = c(FactorCalculator(xres(Raster_Reprojected),5),FactorCalculator(yres(Raster_Reprojected),5)),
                                      fun=mean) %>% 
      sampleRandom(size=sample.size,na.rm=T) %>% 
      as.data.frame() %>% 
      rename_all(function(x) paste0("SA_5km_", x))
    
    # 10 km
    Random_CellSample_10km<- aggregate(Stack,
                                       fact = c(FactorCalculator(xres(Raster_Reprojected),10),FactorCalculator(yres(Raster_Reprojected),10)),
                                       fun=mean) %>% 
      sampleRandom(size=sample.size,na.rm=T) %>% 
      as.data.frame() %>% 
      rename_all(function(x) paste0("SA_10km_", x))
    
    # 100km
    Random_CellSample_100km<- aggregate(Stack,
                                        fact = c(FactorCalculator(xres(Raster_Reprojected),100),FactorCalculator(yres(Raster_Reprojected),100)),
                                        fun=mean) %>% 
      sampleRandom(size=sample.size,na.rm=T) %>% 
      as.data.frame() %>% 
      rename_all(function(x) paste0("SA_100km_", x))
    
    # 250km
    Random_CellSample_250km<- aggregate(Stack,
                                        fact = c(FactorCalculator(xres(Raster_Reprojected),250),FactorCalculator(yres(Raster_Reprojected),250)),
                                        fun=mean) %>% 
      sampleRandom(size=sample.size,na.rm=T) %>% 
      as.data.frame() %>% 
      rename_all(function(x) paste0("SA_250km_", x))
    
    data<-cbind(Random_CellSample_Base,
                Random_CellSample_5km,
                Random_CellSample_10km,
                Random_CellSample_100km,
                Random_CellSample_250km)
    
    RasterName<-WOC_RasterName(Raster)
    RasterRes <- prod(res(Raster_Reprojected))
    
    Final_DF<-cbind(RasterName,RasterRes,data)
    
    
    return(Final_DF)},
    error=function(e) c(NA))
  
}


