#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2021-03-21 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
library(tidyverse)
library(mgcv)

#      Functions                                                            ####

#      Data                                                                 ####

#General Data
HomeRangeSamples <- read.csv("3.Output/HomeRangeSamples.csv") %>% subset(TRI > 0)
HomeRangeSamples$PercentDifference[HomeRangeSamples$PercentDifference <= 0] <- 0

# Outlier Removal
GenOutlier <- subset(HomeRangeSamples, PercentDifference < 30) %>% subset(TRI > 0)

# TRI Global
TRI_csv <-list.files("3.Output/GlobalCSV",
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



TRI_table <- function(CSV_filepaths){
  df <- read_csv(CSV_filepaths,col_names = T) %>% 
    group_by(Size_Category) %>% subset(TRI > 0) %>% arrange(Size_Category) %>% count()
  
  df$Name <- str_extract(CSV_filepaths,"\\w\\d\\d\\w\\d\\d\\d")
  
  return(df)
}

TRI_table(TRI_csv[1])

data <- lapply(TRI_csv[1:3], TRI_table)

do.call(rbind,data)



###############################################################################

base <- read_csv(TRI_csv[1],col_names = T) %>% 
  group_by(Size_Category) %>% subset(TRI > 0)


data.frame(RasterName = str_extract(TRI_csv[1],"\\w\\d\\d\\w\\d\\d\\d"),
           "5km" = count(subset(base$TRI > 0)))



base <- read_csv(TRI_csv[1],col_names = T) %>% 
  group_by(Size_Category) %>% subset(TRI > 0)


read_csv(TRI_csv[1],col_names = T) %>% 
  group_by(Size_Category) %>% subset(TRI > 0) %>% 
  summarise("Nonsig"= count(subset(.,TRI > 0)))


data.frame("Name" = "Raster",
           "NonSig" = count(subset(base,base$TRI < 5.23)),
           "Low" = count(subset(base,base$TRI > 5.23 & base$TRI < 10)))





###############################################################################

base <- read_csv(TRI_csv[1],col_names = T) %>% subset(TRI > 0)

T1 <- subset(base,TRI < 5.23) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T1 = n)

T2 <- subset(base,TRI > 5.23 & TRI < 10) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T2 = n)

T3 <- subset(base,TRI > 10 & TRI < 15) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T3 = n)

T4 <- subset(base,TRI > 15 & TRI < 20) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T4 = n)

T5 <- subset(base,TRI > 20) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T5 = n)

left_join(T1,T2, by = "Size_Category")

t <- do.call(left_join,c(T1,T2), by= "Size_Category")


join <- join_all(list(T1,T2,T3,T4,T5), by='Size_Category', type='left')

final <- cbind("Raster"= str_extract(TRI_csv[1],"\\w\\d\\d\\w\\d\\d\\d"),
               join)


TRI_table <- function(TRI_csv){
  
  base <- read_csv(TRI_csv,col_names = T) %>% subset(TRI > 0)
  
  T1 <- subset(base,TRI < 5.23) %>% group_by(Size_Category) %>% 
    count() %>% 
    rename(Size_Category = Size_Category,T1 = n)
  
  T2 <- subset(base,TRI > 5.23 & TRI < 10) %>% group_by(Size_Category) %>% 
    count() %>% 
    rename(Size_Category = Size_Category,T2 = n)
  
  T3 <- subset(base,TRI > 10 & TRI < 15) %>% group_by(Size_Category) %>% 
    count() %>% 
    rename(Size_Category = Size_Category,T3 = n)
  
  T4 <- subset(base,TRI > 15 & TRI < 20) %>% group_by(Size_Category) %>% 
    count() %>% 
    rename(Size_Category = Size_Category,T4 = n)
  
  T5 <- subset(base,TRI > 20) %>% group_by(Size_Category) %>% 
    count() %>% 
    rename(Size_Category = Size_Category,T5 = n)
  
  join <- join_all(list(T1,T2,T3,T4,T5), by='Size_Category', type='left')
  
  final <- cbind("Raster"= str_extract(TRI_csv[1],"\\w\\d\\d\\w\\d\\d\\d"),
                 join)
  
  return(final)
}

base <- read_csv(TRI_csv[1],col_names = T) %>% subset(TRI > 0)

T1 <- subset(base,TRI < 5.23) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T1 = n)

T2 <- subset(base,TRI > 5.23 & TRI < 10) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T2 = n)

T3 <- subset(base,TRI > 10 & TRI < 15) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T3 = n)

T4 <- subset(base,TRI > 15 & TRI < 20) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T4 = n)

T5 <- subset(base,TRI > 20) %>% group_by(Size_Category) %>% 
  count() %>% 
  rename(Size_Category = Size_Category,T5 = n)

join <- join_all(list(T1,T2,T3,T4,T5), by='Size_Category', type='left')

final <- cbind("Raster"= str_extract(TRI_csv[1],"\\w\\d\\d\\w\\d\\d\\d"),
               join)


