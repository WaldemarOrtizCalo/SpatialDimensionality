#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2021-06-22 

# Purpose: Making the Map

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(raster)
library(mapview)
library(rnaturalearth)
library(sp)
library(magrittr)
library(ggplot2)
library(RColorBrewer)

#      Functions                                                            ####

#      Data                                                                 ####
tif <- raster("1.Data/Map/TRI_10km.tif")
tif2 <- tif

###############################################################################
#   Raster Details                                                          ####
###############################################################################
#   Raster Map                                                              ####
#      [Raster Prep]                                                        ####

# Setting all Zero Values to NA
tif2[tif2 <= 0] <- NA


#      [Breakpoint identification]                                          ####

# Relative Threshold
threshold <- 425

# Non-significant Values to 1
tif2[tif2 < threshold] <- 1

# Non-significant Values to 2
tif2[tif2 >= threshold] <- 2

# Calculating the Ideal Proportion (18%)
prop <- freq(tif2)

prop[5]/(prop[4]+prop[5])

#      [GGplot Map Draft]                                                   ####

# Making Raster Dataframe
ras_df <- as.data.frame(tif2,xy=T)

# Giving NA values a value
ras_df[is.na(ras_df)] = "NA"

# Making Values categorical
ras_df$TRI_10km <- as.factor(ras_df$TRI_10km)

# Plot
ggplot()+
  geom_raster(aes(x,y,fill=TRI_10km),show.legend = FALSE,data = ras_df)+
  scale_fill_manual(values = c(
    "NA" = "black",
    "1" = "gray76",
    "2" = "darkorchid1"
  ))+
  scale_x_continuous(limits = c(-179.9583,179.9583), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55.95833,83.95833), expand = c(0, 0))

###############################################################################
#   Final Map                                                               ####
#      [Creating Base Raster]                                               ####

# Base 
base <- tif

# NA = 0 
base[base <= 0] <- 0

# Land = 1
base[base > 0] <- 1

# Making Raster Dataframe
base_df <- as.data.frame(base,xy=T)

# Making Values categorical
base_df$TRI_10km <- as.factor(base_df$TRI_10km)

# Plot
ggplot()+
  geom_tile(aes(x,y,fill=TRI_10km),show.legend = FALSE,data = base_df)+
  scale_fill_manual(values = c(
    "0" = "black",
    "1" = "gray76"
  ))+
  scale_x_continuous(limits = c(-179.9583,179.9583), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55.95833,83.95833), expand = c(0, 0))

#      [TRI Layer]                                                          ####

# Base 
tri <- tif

# Below Threshold = NA 
tri[tri <= threshold] <- NA

# Making Raster Dataframe
tri_df <- as.data.frame(tri,xy=T) 

# Making Values categorical
tri_df$TRI_10km <- as.factor(tri_df$TRI_10km)

# Plot
ggplot()+
  geom_raster(aes(x,y,fill=TRI_10km),show.legend = FALSE,data = tri_df)+
  scale_x_continuous(limits = c(-179.9583,179.9583), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55.95833,83.95833), expand = c(0, 0))

#      [Final Plot 4 Class]                                                 ####

# Final Raster
final <- tif

# Reclassifying the Raster
reclass_df <- c(-Inf,0,NA,
                0,425,1,
                425,1500,2,
                1500,3000,3,
                3000,Inf,4)

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

tri_classified <- reclassify(final,
                             reclass_m)

# Making the data a dataframe
final_df <- as.data.frame(tri_classified,xy=T)

# Making Values categorical
final_df$TRI_10km <- as.factor(final_df$TRI_10km)

# Plot

map4 <- ggplot()+
  geom_raster(aes(x,y,fill=TRI_10km),show.legend = FALSE,data = final_df)+
  scale_fill_manual(values = c(
    "1" = "#E8E8E8",
    "2" = "#FF00FF",
    "3" = "#CD00CD",
    "4" = "#9400D3"
  ),na.value="#000000")+
  scale_x_continuous(limits = c(-179.9583,179.9583), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55.95833,83.95833), expand = c(0, 0))

ggsave("3.Output/Figures/WorldMap_4Class.png",
       plot = map4,
       device = "png",
       width = 20,
       height = 11,
       units = "in",
       dpi = 300)

#      [Final Plot 6 Class]                                                 ####

# Final Raster
final <- tif

# Reclassifying the Raster
reclass_df <- c(-Inf,0,NA,
                0,425,1,
                425,1000,2,
                1000,1500,3,
                1500,2000,4,
                2000,2500,5,
                2500,Inf,6)

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

tri_classified <- reclassify(final,
                             reclass_m)

# Making the data a dataframe
final_df <- as.data.frame(tri_classified,xy=T)

# Making Values categorical
final_df$TRI_10km <- as.factor(final_df$TRI_10km)

# Plot

map6 <- ggplot()+
  geom_raster(aes(x,y,fill=TRI_10km),show.legend = FALSE,data = final_df)+
  scale_fill_manual(values = c(
    "1" = "#d1d1d1",
    "2" = "#FF00FF",
    "3" = "#CD00CD",
    "4" = "#A020F0",
    "5" = "#9400D3",
    "6" = "#8A2BE2"
  ),na.value="gray34")+
  scale_x_continuous(limits = c(-179.9583,179.9583), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-55.95833,83.95833), expand = c(0, 0))

map6

ggsave("3.Output/Figures/WorldMap_6Class.png",
       plot = map6,
       device = "png",
       width = 20,
       height = 11,
       units = "in",
       dpi = 300)
