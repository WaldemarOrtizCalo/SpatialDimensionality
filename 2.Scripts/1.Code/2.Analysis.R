#   Script Details                                                          ####

# Author: Waldemar Ortiz-Calo

# Date:2020-10-05 

# Purpose: 

###############################################################################
#   Library / Functions / Data                                              ####

##  Library                                                                 ####
library(mgcv)
library(magrittr)

##  Functions                                                               ####
remove_outlier <- function(df,Col.num){
  
  df <- df
  Col.num <- Col.num
  
  # Upper Quantile
  lower_quant <- quantile(df[,Col.num],.25,names = F)
  # Lower Quantile 
  upper_quant <- quantile(df[,Col.num],.75,names = F)
  
  df_clean <- subset(df, df[,Col.num] >= lower_quant & df[,Col.num] <= upper_quant)
  
  return(df_clean)
}

##  Data                                                                    ####

# Home Range Data Imported and Cleaned
data_Raw <- read.csv("3.Output/HR_RawSamples.csv")
HR_Data <- subset(data_Raw, data_Raw$TRI > 0) %>% remove_outlier(7) 

HR_Data$Size_Category <- factor(HR_Data$Size_Category,
                                c("5km", "10km", "100km", "250km"),
                                ordered = T)

###############################################################################

#   ____________________________________________________________________________
#   Analysis                                                                ####
##  ............................................................................
##  Data Exploration                                                        ####

# Initial Data Plotting
plot(HR_Data$PercentDifference ~ HR_Data$TRI)


##  GAM Model Practice                                                      ####

# Practice Model 
Model_Practice <- gam(PercentDifference ~ TRI, data = HR_Data)
summary(Model_Practice)

# Using Splices
Splines_Practice <- gam(PercentDifference ~ s(TRI, bs="cr"), data = HR_Data)
summary(Splines_Practice)
