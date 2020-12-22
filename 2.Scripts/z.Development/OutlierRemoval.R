

num <- 1:10
num_df <- data.frame(Num1 = 1:100,
                     Num2 = 101:200)

quantile<-quantile(num,.25,names = F)


clean <- subset()


df <- remove_outlier(num_df, num_df$Num2)

remove_outlier <- function(Dataframe, FilterColumn){
  # Data
  df <- df
  
  # Columns to use as filter
  col <- FilterColumn
  
  # Upper Quantile
  lower_quant <- quantile(FilterColumn,.25,names = F)
  # Lower Quantile 
  upper_quant <- quantile(FilterColumn,.75,names = F)
  
  df_clean <- subset(df, FilterColumn >= lower_quant & FilterColumn <= upper_quant)
  
  return(df_clean)
}


remove_outlier <- function(Dataframe, Col.num){
  # Data
  df <- Dataframe
  
  # Upper Quantile
  lower_quant <- quantile(df[Col.num],.25,names = F)
  # Lower Quantile 
  upper_quant <- quantile(df[Col.num],.75,names = F)
  
  df_clean <- subset(df, df[Col.num] >= lower_quant & df[Col.num] <= upper_quant)
  
  return(df_clean)
}

remove_outlier(num_df,Col.num = 2)




rm_out(df,2)

df <- num_df
Col.num <- 2

# Upper Quantile
lower_quant <- quantile(df[,Col.num],.25,names = F)
# Lower Quantile 
upper_quant <- quantile(df[,Col.num],.75,names = F)

df_clean <- subset(df, df[,Col.num] >= lower_quant & df[,Col.num] <= upper_quant)

