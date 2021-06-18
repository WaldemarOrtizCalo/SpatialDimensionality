#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2021-03-21 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(plyr)
library(dplyr)
library(tidyverse)
library(mgcv)
library(future.apply)

#      Functions                                                            ####

#      Data                                                                 ####

#General Data
HomeRangeSamples <- read.csv("3.Output/HomeRangeSamples.csv") %>% subset(TRI > 0)
HomeRangeSamples$PercentDifference[HomeRangeSamples$PercentDifference <= 0] <- 0

# Outlier Removal
GenOutlier <- subset(HomeRangeSamples, TRI < 25) %>% subset(TRI > 0)

# TRI Global
csv_list <-list.files("3.Output/GlobalCSV",
                            recursive = F,
                            full.names = T,
                            pattern = ".csv")


###############################################################################
#   Threshold Formula                                                       ####

model <- gam(PercentDifference ~ TRI, data = GenOutlier)
summary(model)


predictions <- predict.gam(model,data.frame("TRI"=seq(1,30, by = 0.01),
                                            "          PercentDifference")) 

pred <- data.frame("TRI" = seq(1,30, by = 0.01),
                   "PercentDifference" = predictions)


#   For Loop Quantifying Earth                                              ####

# Create Function
TRI_tables <- function(csv_list){
  tryCatch({
  TRI_csv <- csv_list
  
  base <- read_csv(TRI_csv,col_names = T) 
  
  base <- read_csv(TRI_csv,col_names = T)  %>% subset(TRI > 0)
  
  base$Size_Category <- factor(base$Size_Category,levels = c("5km","10km","100km","250km"))
  
  T1 <- subset(base,TRI < 5.23) %>% group_by(Size_Category) %>% tally() 
  
  T2 <- subset(base,TRI > 5.23 & TRI < 10) %>% group_by(Size_Category) %>% tally()
  
  T3 <- subset(base,TRI > 10 & TRI < 15) %>% group_by(Size_Category) %>% tally()
  
  T4 <- subset(base,TRI > 15 & TRI < 20) %>% group_by(Size_Category) %>% tally()
  
  T5 <- subset(base,TRI > 20) %>% group_by(Size_Category) %>% tally()
  
  join <- join_all(list(T1,T2,T3,T4,T5), by='Size_Category', type='left')
  
  final <- cbind("Raster"= str_extract(TRI_csv,"\\w\\d\\d\\w\\d\\d\\d"),
                 join)
  
  colnames(final) <- c("Raster", "Size_Category","T1","T2","T3","T4","T5")
  
  return(final)},
  error=function(e) c(NA))
}

# Run Function

list <- lapply(csv_list, TRI_tables)


# Final Tables RAW and exporting
Final_TRI_tables <-do.call(rbind,list)

write.csv(Final_TRI_tables,"3.Output/RawTRIData_Global.csv")

#      [Final Table]                                                              ####

FinalTable <- read.csv("3.Output/RawTRIData_Global.csv")

# Size Categories
FinalTable5   <- subset(FinalTable, Size_Category == "5km")
FinalTable10  <- subset(FinalTable, Size_Category == "10km")
FinalTable100 <- subset(FinalTable, Size_Category == "100km")
FinalTable250 <- subset(FinalTable, Size_Category == "250km")

#        [T1]                                                              ####

sum(FinalTable5$T1,na.rm = T)
sum(FinalTable10$T1,na.rm = T)
sum(FinalTable100$T1,na.rm = T)
sum(FinalTable250$T1,na.rm = T)

#        [T2]                                                              ####

sum(FinalTable5$T2,na.rm = T)
sum(FinalTable10$T2,na.rm = T)
sum(FinalTable100$T2,na.rm = T)
sum(FinalTable250$T2,na.rm = T)

#        [T3]                                                              ####

sum(FinalTable5$T3,na.rm = T)
sum(FinalTable10$T3,na.rm = T)
sum(FinalTable100$T3,na.rm = T)
sum(FinalTable250$T3,na.rm = T)

#        [T4]                                                              ####

sum(FinalTable5$T4,na.rm = T)
sum(FinalTable10$T4,na.rm = T)
sum(FinalTable100$T4,na.rm = T)
sum(FinalTable250$T4,na.rm = T)

#        [T5]                                                              ####

sum(FinalTable5$T5,na.rm = T)
sum(FinalTable10$T5,na.rm = T)
sum(FinalTable100$T5,na.rm = T)
sum(FinalTable250$T5,na.rm = T)
