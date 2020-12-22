PercentDifferenceFunction <- function(raster){
  
  # Grided
  GridedRaster<- as(raster,"SpatialPixelsDataFrame")
  
  #Raster Resolution
  Res_2D<-prod(res(raster))
  
  SurfaceArea_Raster <- surfaceArea(GridedRaster,byCell=T) %>% raster(layer=1,values =T)
  
  # Percent Difference
  
  PercentDifference_Raster_UTM <- (((SurfaceArea_Raster-Res_2D)/abs(Res_2D))*100) #%>% 
    #writeRaster(filename = paste0("1.Data\\PercentDif_Rasters\\",WOC_RasterName(raster),"_UTM"),
                #format = "GTiff",
                #overwrite = T)
  
  
  #PercentDifference_Raster_WGS <- projectRaster(PercentDifference_Raster_UTM,
                                               # crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                                                #filename = paste0("1.Data\\PercentDif_Rasters\\",WOC_RasterName(raster),"_WGS"),
                                                #format = "GTiff",
                                                #overwrite = T)
  return(PercentDifference_Raster_UTM)
}
