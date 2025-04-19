rm(list=ls())
library(ggplot2)

frog = read.csv("~/Documents/Coding/educational/test.csv")
head(frog, 10)
colnames(frog)

ggplot(frog, aes(x=species, y=height, color=species)) +
  geom_boxplot() +
  theme_bw()

# compare heights of these three species (using ANOVA)
height_comparison=aov(height~species, data=frog)
summary(height_comparison) # p = 0.0273 , the species are significantly different!

# post hoc test to determine which ones are different (we'll use Tukey's for now)
TukeyHSD(height_comparison)

# output
# $species
#                     diff       lwr      upr     p adj
# raphanus-clarkia    -5.4 -23.92453 13.12453 0.7231981
# trifolium-clarkia  -21.0 -39.52453 -2.47547 0.0265384
# trifolium-raphanus -15.6 -34.12453  2.92453 0.1032118

# tells us that trifolium and clarkia are significantly different, but not any of the other comparisions. This is validated by our boxplot!
# Comparison of means showed that species were significantly different (F= 4.9, df=2, p=0.03). 
# Tukey's Post Hoc revealed that ....
# The mean heights of trifolium and clarkia were significantly different (F, df, p)