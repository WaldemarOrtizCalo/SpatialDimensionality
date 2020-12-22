RTile_Summaries<- function(RasterFilepaths){
  
  # Import Raster filepath and make it into a Raster object
  Raster <- raster(RasterFilepaths[1])
  
  # 
  Raster_Reprojected <- projectRaster(Raster,
                                      crs = paste0("+proj=utm +zone=",Auto_UTM_ID(Raster)," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
  
  RasterName<-WOC_RasterName(Raster)
  RasterRes <- prod(res(Raster_Reprojected))
  NumberOfCells <- ncell(Raster_Reprojected)
  Tile_SA <- (RasterRes*NumberOfCells)/1000000
  
  df<-data.frame(RasterName = RasterName, 
                 BaseRasterRes = RasterRes, 
                 NumberOfCells = NumberOfCells, 
                 Tile_SA = Tile_SA)
  
  return(df)
}
