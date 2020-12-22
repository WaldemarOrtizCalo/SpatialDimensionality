
#### Different Iterations ####

#### All Data ####
R_all <-sample(RasterFilepaths,200,replace = F) %>% lapply(raster)

plan("multiprocess", workers = 3)

df_all<-future_lapply(R_all,RasterCalculations,sample.size = 15)

plan("sequential")

df_all<-do.call(rbind,df_all)

plot_data <- ggplot(df_all,aes((SA_250km_TRI/RasterRes),(SA_250km_PercentDifference/RasterRes)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

#### Groups along the Equator ####

R_EQ <-sample(RasterFilepaths[1:10000],200,replace = F) %>% lapply(raster)

plan("multiprocess", workers = 3)

df_EQ<-future_lapply(R_EQ,RasterCalculations,sample.size = 15)

plan("sequential")

df_EQ<-do.call(rbind,df_EQ)

plot_data <- ggplot(df_EQ,aes(SA_250km_TRI,SA_250km_PercentDifference))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

#### Groups close to Poles ####
R_Poles <-sample(RasterFilepaths[14000:15000],200,replace = F) %>% lapply(raster)

plan("multiprocess", workers = 3)

df_Poles<-future_lapply(R_Poles,RasterCalculations,sample.size = 15)

plan("sequential")

df_Poles<-do.call(rbind,df_Poles)

plot_data <- ggplot(df_Poles,aes(SA_250km_TRI,SA_250km_PercentDifference))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")


#### Trying to do all of them ####
R_big <-sample(RasterFilepaths,5000,replace = F) %>% lapply(raster)

plan("multiprocess", workers = 3)

df_big<-future_lapply(R_all,RasterCalculations,sample.size = 15)

plan("sequential")

df_big<-do.call(rbind,df_big)

plot_data <- ggplot(df_big,aes((SA_250km_TRI/RasterRes),(SA_250km_PercentDifference/RasterRes)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")
#### Trying it with the Agg Before processing ####

R_agg <-sample(RasterFilepaths,200,replace = F) %>% lapply(raster)

plan("multiprocess", workers = 3)

df_agg<-future_lapply(R_agg,RasterCalculationsAgg,90,sample.size = 15)

plan("sequential")

df_agg<-do.call(rbind,df_agg)

plot_data <- ggplot(df_agg,aes((SA_5km_TRI),(SA_5km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

#### Comparing With and Without Agg ####

Rasters <-sample(RasterFilepaths,100,replace = F) %>% lapply(raster)

# Without Size Correction

plan("multiprocess", workers = 3)

df_without<-future_lapply(Rasters,RasterCalculations,sample.size = 15)

plan("sequential")

df_without<-do.call(rbind,df_without)

plot_data <- ggplot(df_without,aes((SA_100km_TRI),(SA_100km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI (Without Agg)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

# With Size Correction

plan("multiprocess", workers = 3)

df_with30<-future_lapply(Rasters,RasterCalculationsAgg,30,sample.size = 15)

plan("sequential")

df_with30<-do.call(rbind,df_with30)

plot_data <- ggplot(df_with30,aes((SA_100km_TRI),(SA_100km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI (With Agg 30m)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")


plan("multiprocess", workers = 3)

df_with60<-future_lapply(Rasters,RasterCalculationsAgg,60,sample.size = 15)

plan("sequential")

df_with60<-do.call(rbind,df_with60)

plot_data <- ggplot(df_with60,aes((SA_100km_TRI),(SA_100km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI (With Agg 60m)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")


plan("multiprocess", workers = 3)

df_with90<-future_lapply(Rasters,RasterCalculationsAgg,90,sample.size = 15)

plan("sequential")

df_with90<-do.call(rbind,df_with90)

plot_data <- ggplot(df_with90,aes((SA_100km_TRI),(SA_100km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI (With Agg 90m)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")


plan("multiprocess", workers = 3)

df_with120<-future_lapply(Rasters,RasterCalculationsAgg,120,sample.size = 15)

plan("sequential")

df_with120<-do.call(rbind,df_with120)

plot_data <- ggplot(df_with120,aes((SA_100km_TRI),(SA_100km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI (With Agg 120m)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

#### GGPLOTS ####
# Normal 
plot_data <- ggplot(df_without,aes((SA_100km_TRI),(SA_100km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI (Without Agg)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

# Raster Resolution Division
plot_data <- ggplot(df_without,aes((SA_5km_TRI/RasterRes),(SA_5km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle(" Surface Area ~ TRI (Without Agg and Raster Resolution Division)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

# 30 agg
plot_data <- ggplot(df_with30,aes((SA_250km_TRI),(SA_250km_PercentDifference)))

plot_data + 
  geom_point() + 
  theme_classic() + 
  ggtitle("Relationship Between Surface Area and TRI (With Agg 30m)") +
  xlab("TRI") + 
  ylab("Percent Difference in Surface Area")

