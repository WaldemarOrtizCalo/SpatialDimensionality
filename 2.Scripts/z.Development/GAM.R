library(mgcv)

gam<- gam(SA_100km_PercentDifference ~ s(SA_100km_TRI),
          data=df_with30,
          method = "REML")


ggplot(df_with30, aes(SA_100km_TRI, SA_100km_PercentDifference)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x))+
  theme_bw()+
  xlab("Mean TRI")+
  ylab("Percent Change in Surface Area Estimate")

gam.check(gam)
