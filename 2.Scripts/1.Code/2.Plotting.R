#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-12-30 

# Purpose: This script is for plotting the relationships

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(ggplot2)
library(stringr)

#      Functions                                                            ####

#      Data                                                                 ####
HomeRangeSamples <- read.csv("3.Output/HomeRangeSamples.csv")

###############################################################################
#   Plotting and Exploratory Plots                                          ####
#     Coordinate and Resolution Plots                                       ####
#       [Spatial Data]                                                      ####

# Extracting Spatial Data from the Raster names
latNS     <- str_sub(string = HomeRangeSamples$RasterName,
                     start  = 1,
                     end    = 1)

latCoord  <- str_sub(string = HomeRangeSamples$RasterName,
                     start  = 2,
                     end    = 3)

longEW    <- str_sub(string = HomeRangeSamples$RasterName,
                     start  = 4,
                     end    = 4)

longCoord <- str_sub(string = HomeRangeSamples$RasterName,
                     start  = 5,
                     end    = 7)

# Adding the Spatial Coordinates to the existing Data Frame
HomeRangeSamples$latNS     <- latNS
HomeRangeSamples$latCoord  <- latCoord
HomeRangeSamples$longEW    <- longEW
HomeRangeSamples$longCoord <- longCoord

#       [Plots]                                                             ####

# Raster Cell Area vs Latitude
ggplot(HomeRangeSamples,aes(as.numeric(HomeRangeSamples$latCoord),HomeRangeSamples$RasterRes))+
  geom_point(aes(color = factor(HomeRangeSamples$latNS)), size = 2)+
  ggtitle("Raster Area vs. Latidude Coordinate")+
  ylab("Raster Area (m^2)")+
  xlab("Latitude Coordinate")+
  theme_bw()+
  geom_smooth()

# Raster Cell Area vs Longitude
ggplot(HomeRangeSamples,aes(as.numeric(HomeRangeSamples$longCoord),HomeRangeSamples$RasterRes))+
  geom_point(aes(color = factor(HomeRangeSamples$latNS)), size = 2)+
  ggtitle("Raster Area vs. Longitude Coordinate")+
  ylab("Raster Area (m^2)")+
  xlab("Longitude Coordinate")+
  theme_bw()+
  geom_smooth()

#     Percent Difference v TRI (RAW)                                        ####
# Plotting Percent Difference and  TRI 

# General Plot 
ggplot(HomeRangeSamples, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle("Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()

# Faceted Plot 
ggplot(HomeRangeSamples, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle("Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))

#     Percent Difference v TRI (Outlier Removal)                            ####
# Plotting Percent Difference and  TRI 

# General Plot 
ggplot(HomeRangeSamples, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle("Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()

# Faceted Plot 
ggplot(HomeRangeSamples, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle("Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))


###############################################################################