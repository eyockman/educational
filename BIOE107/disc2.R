# BIOE107 | Emma Yockman
# discussion 2: Stats and CO2
rm(list=ls()) # clear R's brain

dat=read.csv("https://drive.google.com/uc?export=download&id=1k4AR2obNY4Jt7xHdJpZ2QP2cB9zwVvkd") # loads in data

head(dat,10) # displays (aka prints) first 10 lines of dataset into Console

# Yr_BP_1000 – Time before present, measured in years (negative numbers are in the past)
# CO2_ppm – Concentration of CO2, in parts per million
# Temp_C – Relative Temperature, in degrees Celsius

# 1----
## patterns in plot
par(mar = c(5,5,2,5)) #adjusts margins of plot to make room for right axis
#The next line makes plot of Temp (Temp_C) vs time (Yr_BP_1000) which are columns of the object dat – the dollar symbol tells R to look for columns of dat with these names; pch=19 indicates the symbol type (filled circle), 
#type="b" indicates what type of plot (line vs point or "b" for both)
#xlab and ylab are the x-axis and y-axis labels
plot(dat$Temp_C~dat$Yr_BP_1000,pch=19,type="b",xlab="Time (1000s Years)",ylab="Temp C")

#Add the CO2 time series to the same plot
par(new = T) #add plot on top of current graph
#The next line makes a plot of CO2 vs time; axes=F makes it so it doesn't write over old axes
plot(dat$CO2_ppm~dat$Yr_BP_1000,pch=19,type="b",col="red",axes=F,xlab=NA,ylab=NA)
axis(side = 4) #add axis to right side
mtext(side = 4, line = 3, "CO2 conc. ppm") #add y-axis title for right axis
legend("top",legend=c("Temp","CO2"),pch=19, col=c("black", "red")) #add legend

# 2----
plot(Temp_C~CO2_ppm,data=dat,pch=19) #plot temperature as a function of CO2, with data from dat, and point character 19 (a closed circle) 

r1=lm(Temp_C~CO2_ppm,data=dat) #same format as above… run a linear model of temperature as a function of CO2, from the dat dataset.

## 2a-d check assumptions ----
abline(r1) # linearity
shapiro.test(residuals(r1)) # normality
plot(residuals(r1)~predict(r1)) # homoscedasticity; eyeball test
library(lmtest); bptest(fit) #P-values <0.05 indicate non-constant variance (heteroskedasticity) # homoscedasticity; math test
pacf(residuals(r1)) # independence

# 3 ----
summary(r1)
# probability this relationship is random = 4.24e-10 (pval)
0.5^20
0.5^50
coef(summary(r1))["CO2_ppm","Estimate"] # slope; increase in temp deg C for 1ppm CO2
coef(summary(r1))["CO2_ppm","Estimate"]*100 # increase in 7degC for 100ppm CO2

# look at graph again
plot(Temp_C~CO2_ppm,data=dat,pch=19); abline(r1); abline(v=181, col="blue"); abline(v=280, col="red") # blue = 180 ish, red = 280 ppm
abline(h=-1, col="red"); abline(h=-8.5, col="blue")

#5----
# 5a&b contextualize info
2*coef(summary(r1))["CO2_ppm","Std. Error"] #this calculates 2*SE
coef(summary(r1))["CO2_ppm","Estimate"] - 2*coef(summary(r1))["CO2_ppm","Std. Error"] # lower bound
coef(summary(r1))["CO2_ppm","Estimate"] + 2*coef(summary(r1))["CO2_ppm","Std. Error"] # upper bound
summary(r1)$r.squared # how much variation in temp is explained by CO2?

# 6 ----
#correlation =/= causation
r2=lm(CO2_ppm~ Temp_C,data=dat) # is it the other way around? temp causes increase in CO2? = can't prove it like this
summary(r2)
summary(r1)
