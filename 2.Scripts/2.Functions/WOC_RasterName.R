WOC_RasterName<-function(raster){
  name<-names(raster)
  r_name<-str_extract(name,"\\w\\d\\d\\w\\d\\d\\d")
  return(r_name)
}
