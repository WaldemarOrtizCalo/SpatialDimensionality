#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-12-30 

# Purpose: This script is for plotting the relationships

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(ggplot2)
library(stringr)
library(tidyverse)
#      Functions                                                            ####
#      Data                                                                 ####

# General Data
HomeRangeSamples <- read.csv("3.Output/HomeRangeSamples.csv") %>% subset(TRI > 0)
HomeRangeSamples$PercentDifference[HomeRangeSamples$PercentDifference <= 0] <- 0

# Outlier Removal
GenOutlier <- subset(HomeRangeSamples,TRI < 25) %>% subset(TRI > 0)
GenOutlier$Size_Category <- factor(GenOutlier$Size_Category, levels = c("5km","10km","100km","250km"))

###############################################################################
#   Summary Statistics                                                      ####
#      Whole Data                                                           ####

# Grouping Observations and counting 
groups <- GenOutlier %>% group_by(Size_Category) %>% 
  count()

# Grouping Observations and counting 
groups <- GenOutlier %>% 
  group_by(Size_Category) %>% 
  summarise(PercentDifference_Mean = mean(PercentDifference),
            PercentDifference_sd = sd(PercentDifference),
            PercentDifference_min = min(PercentDifference),
            PercentDifference_max = max(PercentDifference),
            TRI_Mean = mean(TRI),
            TRI_sd = sd(TRI))

#      Threshold Data Less Than                                             ####
#        [Whole]                                                            ####

Threshold <- subset(GenOutlier, PercentDifference < 5)

percent_nonsig <- nrow(Threshold)/nrow(GenOutlier)

#        [Per Category]                                                     ####
groups <- Threshold %>% 
  group_by(Size_Category) %>% 
  summarise(PercentDifference_Mean = mean(PercentDifference),
            PercentDifference_sd = sd(PercentDifference),
            PercentDifference_min = min(PercentDifference),
            PercentDifference_max = max(PercentDifference),
            TRI_Mean = mean(TRI),
            TRI_sd = sd(TRI),
            TRI_min = min(TRI),
            TRI_max = max(TRI))

#      Threshold Data More Than                                             ####
#        [Whole]                                                            ####

Threshold <- subset(GenOutlier, PercentDifference > 5)

percent_nonsig <- nrow(Threshold)/nrow(GenOutlier)

#        [Per Category]                                                     ####
groups <- Threshold %>% 
  group_by(Size_Category) %>% 
  summarise(PercentDifference_Mean = mean(PercentDifference),
            PercentDifference_sd = sd(PercentDifference),
            PercentDifference_min = min(PercentDifference),
            PercentDifference_max = max(PercentDifference),
            TRI_Mean = mean(TRI),
            TRI_sd = sd(TRI),
            TRI_min = min(TRI),
            TRI_max = max(TRI))

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
ggplot(GenOutlier, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle("Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()

# Faceted Plot 
ggplot(GenOutlier, aes(TRI,PercentDifference))+
  geom_point()+
  ggtitle("Percent Difference v. TRI")+
  theme_bw()+
  geom_smooth()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))

#     Distributions of Percent Difference                                   ####

# Distribution General
ggplot(GenOutlier) + geom_histogram(aes(PercentDifference))+
  ggtitle("Percent Difference Distribution")+
  xlab("Percent Difference")+
  theme_bw()
  
# Distribution Faceted
ggplot(GenOutlier) + geom_histogram(aes(GenOutlier$PercentDifference))+
  ggtitle("Percent Difference Distribution")+
  xlab("Percent Difference")+
  theme_bw()+
  facet_grid(. ~ factor(Size_Category,levels = c("5km","10km","100km","250km")))

###############################################################################
#   Final Plots                                                             ####
#      [Percent Difference v TRI]                                           ####

# Different Color and Shape
p1 <- ggplot(GenOutlier, aes(TRI,PercentDifference))+
  geom_point(aes(shape = Size_Category,color = Size_Category),size = 3)+
  scale_shape_manual(values=c(15, 16, 17,18))+
  theme_bw()+
  geom_smooth(color = "black")+
  theme(text = element_text(size = 16))+
  xlab("Terrain Ruggedness Index (TRI)")+
  ylab("Percent Difference")+
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73","#CC79A7"))

p1

# Different Color Only (Final)
p2 <- ggplot(GenOutlier, aes(TRI,PercentDifference))+
  geom_point(aes(color = Size_Category),size = 6)+
  theme_bw()+
  geom_smooth(color = "black",size = 3)+
  theme(text = element_text(size = 28))+
  xlab("Terrain Ruggedness Index (TRI)")+
  ylab("Percent Difference")+
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73","#CC79A7"))+
  labs(color='Size Category')

p2 

#      [Saving Plot]                                                        ####
ggsave("3.Output/Figures/PDifTRI.png",
       plot = p2,
       device = "png",
       width = 20,
       height = 11,
       units = "in",
       dpi = 300)
