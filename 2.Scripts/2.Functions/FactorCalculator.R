FactorCalculator <- function(ResNumber,SquareArea_Kilometers){
  Side_Length<-sqrt(SquareArea_Kilometers)*1000
  
  FactValue <- round(Side_Length/ResNumber)
  
  return(FactValue)
}
