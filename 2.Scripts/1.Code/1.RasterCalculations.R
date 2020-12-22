#   ____________________________________________________________________________
#   Libraries / Function / Data                                             ####

# Library
library(raster)
library(future.apply)
library(pbapply)
library(stringr)
library(spatialEco)
library(pbapply)
library(dplyr)
library(ggplot2)
library(mgcv)
library(tictoc)

# Functions 
source("2.Scripts\\2.Functions\\Auto_UTM_ID.R")
source("2.Scripts\\2.Functions\\WOC_RasterName.R")
source("2.Scripts\\2.Functions\\UTM_ZoneIdentifier.R")
source("2.Scripts\\2.Functions\\PercentDifferenceFunction.R")
source("2.Scripts\\2.Functions\\FactorCalculator.R")
source("2.Scripts\\2.Functions\\RasterCalculations.R")
source("2.Scripts\\2.Functions\\FactorCalculator_Agg.R")
source("2.Scripts\\z.Development\\RasterCalculationsAgg.R")

# Data

# Rasters
RasterFilepaths<-list.files("G:\\RAW_DEM_Tiles",
                            recursive = F,
                            full.names = T,
                            pattern = "dem")
# ZoneList 

ZoneList<-read.csv("1.Data\\z.UTM_Reprojections\\DF_UTM_Zones.csv")

#   ____________________________________________________________________________
#   Raster Calculations                                                     ####

# Randomly Selecting Rasters
tic()
Rasters <-sample(RasterFilepaths,100,replace = F)

# Function Execution
plan("multiprocess", workers = 3)

df_with30<-future_lapply(Rasters,RasterCalculationsAgg,30,sample.size = 5)

plan("sequential")

# Creating a dataframe
df_Master <-do.call(rbind,df_with30)
toc()

write.csv(df_Master,"3.Output//Master_Data.csv",row.names = F)
#   ____________________________________________________________________________
#   Summary Statistics                                                      ####

# Prepping Individual Size Categories dataframes and eliminating observations
# with 0 DEM values
df_5km <- data.frame("RasterName" = df_Master$RasterName,
                     "RasterRes" =df_Master$RasterRes,
                     "DEM" =df_Master$SA_5km_DEM,
                     "TRI" =df_Master$SA_5km_TRI,
                     "PercentDifference" =df_Master$SA_5km_PercentDifference) %>% 
  subset(DEM > 0 & PercentDifference > 0)


df_10km <- data.frame("RasterName"= df_Master$RasterName,
                      "RasterRes" = df_Master$RasterRes,
                      "DEM" = df_Master$SA_10km_DEM,
                      "TRI" = df_Master$SA_10km_TRI,
                      "PercentDifference" = df_Master$SA_10km_PercentDifference) %>% 
  subset(DEM > 0 & PercentDifference > 0)


df_100km <- data.frame("RasterName"= df_Master$RasterName,
                       "RasterRes" = df_Master$RasterRes,
                       "DEM" = df_Master$SA_100km_DEM,
                       "TRI" = df_Master$SA_100km_TRI,
                       "PercentDifference" = df_Master$SA_100km_PercentDifference) %>% 
  subset(DEM > 0 & PercentDifference > 0)


df_250km <- data.frame("RasterName"= df_Master$RasterName,
                       "RasterRes" = df_Master$RasterRes,
                       "DEM" = df_Master$SA_250km_DEM,
                       "TRI" = df_Master$SA_250km_TRI,
                       "PercentDifference" = df_Master$SA_250km_PercentDifference)%>% 
  subset(DEM > 0 & PercentDifference > 0)


# 5km 
summarise(df_5km,
          SampleSize = n(),
          Mean_PercentDifference = mean(PercentDifference),
          sd_PercentDifference = sd(PercentDifference),
          min_PercentDifference = min(PercentDifference),
          max_PercentDifference = max(PercentDifference))
# 10km 
summarise(df_10km,
          SampleSize = n(),
          Mean_PercentDifference = mean(PercentDifference),
          sd_PercentDifference = sd(PercentDifference),
          min_PercentDifference = min(PercentDifference),
          max_PercentDifference = max(PercentDifference))
# 100km 
summarise(df_100km,
          SampleSize = n(),
          Mean_PercentDifference = mean(PercentDifference),
          sd_PercentDifference = sd(PercentDifference),
          min_PercentDifference = min(PercentDifference),
          max_PercentDifference = max(PercentDifference))
# 250km 
summarise(df_250km,
          SampleSize = n(),
          Mean_PercentDifference = mean(PercentDifference),
          sd_PercentDifference = sd(PercentDifference),
          min_PercentDifference = min(PercentDifference),
          max_PercentDifference = max(PercentDifference))
#   ____________________________________________________________________________
#   GAM Analysis                                                            ####

# GAM and plots

# 5km
gam_5km<- gam(PercentDifference ~ s(TRI),
          data=df_5km,
          method = "REML")

ggplot(df_5km, aes(TRI, PercentDifference)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))+
  theme_bw()+
  xlab("Mean TRI")+
  ylab("Percent Increase in Surface Area Estimate")+
  ggtitle("5 km^2")

# 10km
gam_10km<- gam(PercentDifference ~ s(TRI),
              data=df_10km,
              method = "REML")

ggplot(df_10km, aes(TRI, PercentDifference)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))+
  theme_bw()+
  xlab("Mean TRI")+
  ylab("Percent Increase in Surface Area Estimate")+
  ggtitle("10 km^2")

# 100km
gam_100km<- gam(PercentDifference ~ s(TRI),
              data=df_100km,
              method = "REML")

ggplot(df_100km, aes(TRI, PercentDifference)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))+
  theme_bw()+
  xlab("Mean TRI")+
  ylab("Percent Increase in Surface Area Estimate")+
  ggtitle("100 km^2")

# 250km
gam_250km<- gam(PercentDifference ~ s(TRI),
              data=df_250km,
              method = "REML")

ggplot(df_250km, aes(TRI, PercentDifference)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))+
  theme_bw()+
  xlab("Mean TRI")+
  ylab("Percent Increase in Surface Area Estimate")+
  ggtitle("250 km^2")

predict<-predict.gam(gam_5km,c(5,10,20,30))

summary(gam_5km)
summary(gam_10km)
summary(gam_100km)
summary(gam_250km)



ggplot(df_with30, aes(SA_10km_TRI, SA_10km_PercentDifference)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))+
  theme_bw()+
  xlab("Mean TRI")+
  ylab("Percent Increase in Surface Area Estimate")
