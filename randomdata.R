rm(list=ls()) # clear R's brain

water=rnorm(100, 0, 0.5) # randomly generate a list of 100 numbers with an average of 0 and a standard deviation of 0.5. Call it "water"
grubs=water*rnorm(100, 5, 3)+20 # randomly generate a list of 100 numbers with an average of 5, std dev of 3, but multiply every number by water and add 20
t=data.frame(water, grubs) # randomly generated data set; put water and grubs together into a data frame

ggplot(data=t, aes(x=water, y=grubs))+ # make a plot using t, with water as x axis and grubs as y
  geom_point(pch=1, # draw points on the plot using my data t mentioned in the last line, but draw points using pch style 1
             col="#4EA699", # make points this color
             size=2)+  # make points this size
  theme_few()+ # use a cuter theme
  labs(x="change in water level (mL)",y="grubs are creatures") # manually label the x and y axis
