rm(list=ls()) # clear R's brain
library(ggplot2) # load in ggplot2 package. if you don't have it installed already it will fail
library(ggthemes) # cute themes packages

frog = read.csv("~/Documents/Coding/educational/test.csv") # load in test.csv and call it frog. you can call it whatever you want, I just chose to call it something weird so you would understand you can call it anything
head(frog, 10) # look at first 10 rows
colnames(frog) # look at column names

# visualize your data
ggplot(frog, aes(x=species, y=height, color=species)) + # draw a plot using frog, with species as the x axis, height as the y axis, and color code species
  geom_boxplot() + # draw a box plot using the  specificiations from the last line
  theme_bw() # use a cuter theme

# compare heights of these three species (using ANOVA)
height_comparison=aov(height~species, data=frog) # using frog, are average heights different by species? put this analysis into an object called height_comparisons
summary(height_comparison) # call results of the last line
# p = 0.0273 , the species are significantly different! but this doesn't tell us which ones are different

# post hoc test to determine which ones are different (we'll use Tukey's for now)
TukeyHSD(height_comparison) # compare means of each species directly

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