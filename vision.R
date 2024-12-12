rm(list=ls())
library(survival)
library(dplyr)
library(ggplot2)
library(ggfortify)

data("retinopathy", package="survival") # find dataset that is in the survival package
head(retinopathy) # show first few rows so we can see the data, puts it in the environment

# you should see this:
# id laser   eye age     type trt futime status risk
# 1  5 argon  left  28    adult   1  46.23      0    9
# 2  5 argon  left  28    adult   0  46.23      0    9
# 3 14 argon right  12 juvenile   1  42.50      0    8
# 4 14 argon right  12 juvenile   0  31.30      1    6
# 5 16 xenon right   9 juvenile   1  42.27      0   11
# 6 16 xenon right   9 juvenile   0  42.27      0   11

# this dataset is of patients with diabetic retinopathy who are being studied for loss of vision.

# in the context of a "survival curve" we are specifically interested in the columns: "futime" and "status". 
# Time is the same as time to event, or time until death/germination/event/etc
# Status is the same as alive/dead = aka a "CENSOR"

# here the EVENT = loss of vision/blind in an eye
# if we were doing plant survival, EVENT = death.
# if we were doing germination, EVENT = seed germinated

# let's plot some of it to get a feel for this data.

# first lets look at the distribution of the age of our patients. 
ggplot(retinopathy, aes(x = age)) +
  geom_histogram(binwidth = 3, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Patient Age", x = "Age", y = "Frequency")
# you should see that there is a right skew, meaning many patients are on the younger side.

# What about the amount of patients that are blind CURRENTLY?
ggplot(retinopathy, aes(x = status)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Patient Blindness", x = "Vision Status (0=Can see, 1=Blind)", y = "Frequency")
# This isn't a perfect histogram, but we can see as of the most recent census, 
# there are more patients who still have their sight, than those who have lost their vision.

# When we meet I will walk you through how to plot a survival curve, and how to analyze for significant differences. 
# If you're interested, this is what a survival curve might look like.
km_fit <- survfit(Surv(futime, status) ~ trt, data=retinopathy)
autoplot(km_fit,
         title = "Proportion of Patients Experiencing Loss of Vision by Treatment",
         xlab = "Time (Days)",
         ylab= "Proportion of Patients that Can See")+
  scale_y_continuous(limits=c(0,1))
# trt -- 0 = not treated, 1 = treated