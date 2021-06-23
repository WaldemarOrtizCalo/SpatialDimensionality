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
threshold <- 420

# Non-significant Values to 1
tif2[tif2 < threshold] <- 1

# Non-significant Values to 2
tif2[tif2 >= threshold] <- 2

# Calculating the Ideal Proportion (18%)
prop <- freq(tif2)

prop[5]/(prop[4]+prop[5])

# Calculation Proportion of the Globe
breakpoints <- c(0,1,2)
colors <- c("black","green","red")
plot(tif2,breaks=breakpoints,col=colors)

#      [Mapview Exploration]                                                ####
mapview(tif2)

#      [GGplot Map]                                                          ####

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
  

