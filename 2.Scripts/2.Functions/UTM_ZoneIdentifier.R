UTM_ZoneIdentifier<- function(raster) {
  
  # Getting the Number
  extent<-extent(raster)
  
  long_mid<-mean(c(extent[1],extent[2]))
  
  Value<-(floor((long_mid + 180)/6) %% 60) + 1
  
  # North or South
  
  r_N.S.<-names(raster) %>% 
    str_extract("\\w\\d\\d\\w\\d\\d\\d") %>% 
    str_sub(start = 1,end=1)
  
  return(paste0(Value,r_N.S.))
}
