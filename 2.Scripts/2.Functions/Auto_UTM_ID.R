Auto_UTM_ID<- function(raster){
  name<-names(raster)
  r_name<-str_extract(name,"\\w\\d\\d\\w\\d\\d\\d")
  
  s<-subset(ZoneList,ZoneList$RasterName == WOC_RasterName(raster))
  ZoneValue <-as.character(s$Zone)
  
  return(ZoneValue)
}
