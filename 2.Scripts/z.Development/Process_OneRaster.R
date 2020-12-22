# Picking Out a Random Raster
Raster <- raster(sample(RasterFilepaths,1,replace = F))
plot(Raster)

#### Reprojected Raster ####

Raster_Reprojected <- projectRaster(Raster,
                             crs = paste0("+proj=utm +zone=",Auto_UTM_ID(Raster)," +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
names(Raster_Reprojected) <- "DEM"
plot(Raster_Reprojected)

#### Percent Difference in Surface Area ####

Raster_PDif <- PercentDifferenceFunction(Raster_Reprojected)
names(Raster_PDif)<-"PercentDifference"
plot(PDif_Raster)
#### Landscape Complexity Variables ####

# TRI
Raster_TRI <- terrain(Raster_Reprojected,opt="TRI")
names(Raster_TRI)<-"TRI"
plot(Raster_TRI)

# Slope
Raster_Slope <- terrain(Raster_Reprojected,opt="slope",unit="degrees")
names(Raster_Slope)<-"Slope"
plot(Raster_Slope)

# VRM
Raster_VRM <- vrm(Raster_Reprojected,s=3)

names(Raster_VRM)<-"VRM"
plot(Raster_VRM)

#### Aggregation Process ####


xres(Raster_Reprojected)
Raster_Agg_5km <- aggregate(Raster_Reprojected,
                            fact = c(FactorCalculator(xres(Raster_Reprojected),5),FactorCalculator(yres(Raster_Reprojected),5)),
                            fun=mean)

Raster_Agg_100km <- aggregate(Raster_Reprojected,
                            fact = c(FactorCalculator(xres(Raster_Reprojected),100),FactorCalculator(yres(Raster_Reprojected),100)),
                            fun=mean)

sample<-sampleRandom(Raster_Agg_100km,size = 10,cells=T)
getValues(Raster_Agg_100km,8)

getValues(Raster_Agg_100km,139)

Raster_Agg_100km[105]


sample[1:5]


# Stack Try #
x<-raster::stack(Raster_Reprojected,
                 Raster_Slope,
                 Raster_TRI,
                 Raster_VRM,
                 Raster_PDif)
plot(x)

x_agg<-aggregate(x,fact=300)
plot(x_agg)

x_sample<-as.data.frame(sampleRandom(x_agg,size=10)) %>% 
  rename_all(function(x) paste0("Base_", x))

y_sample<-as.data.frame(sampleRandom(x_agg,size=10))%>% 
  rename_all(function(x) paste0("Base2_", x))

cbind(x_sample,y_sample)

