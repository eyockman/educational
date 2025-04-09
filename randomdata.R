rm(list=ls())

water=rnorm(100, 0, 0.5)
grubs=water*rnorm(100, 5, 3)+20
t=data.frame(water, grubs) # randomly generated data set

ggplot(t, aes(x=water, y=grubs))+
  geom_point(pch=1, col="blue", size=3)+
  theme_few()+labs(x="",y="")