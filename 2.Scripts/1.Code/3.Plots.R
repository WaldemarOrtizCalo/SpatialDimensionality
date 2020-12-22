
#   ____________________________________________________________________________
#   Library / Functions / Data                                              ####
library(ggplot2)
library()

#   ____________________________________________________________________________
#   Percentage of the Earth Chart                                           ####

df <- read.csv("1.Data\\z.TestDataFigures\\TestData_PercentEarth.csv")

df$SizeCategory <- factor(df$SizeCategory, levels = c("5km²","10km²","100km²","250km²"))

plot_data<-ggplot(data = df, aes(x=PercentIncrease, y=PercentEarth)) 

plot_data + geom_line(aes(colour=SizeCategory)) +
  theme_classic() +
  scale_x_continuous("Percent Increase in Surface Area",limits = c(0,50)) +
  scale_y_continuous("Percentage of Earth's Surface Area",limits = c(0,100))
  
