rm(list=ls())
library(ggplot2)

data <- read.csv("~/Documents/Coding/hester/tutorial/test.csv")
head(data)

ggplot(data, aes(x=species, y=height, fill=species)) +
  geom_boxplot() +
  theme_bw()

# compare heights of these three species (using ANOVA)
height_comparison <- aov(height~species, data=data)
summary(height_comparison) # p = 0.0273 , the species are significantly different!

# post hoc test to determine which ones are different (we'll use Tukey's for now)
TukeyHSD(height_comparison)

#output
# $species
#                     diff       lwr      upr     p adj
# raphanus-clarkia    -5.4 -23.92453 13.12453 0.7231981
# trifolium-clarkia  -21.0 -39.52453 -2.47547 0.0265384
# trifolium-raphanus -15.6 -34.12453  2.92453 0.1032118

# tells us that trifolium and clarkia are significantly different, but not any of the other comparisions. This is validated by our boxplot!