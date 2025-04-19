rm(list=ls())
library(survival)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(ggsurvfit)
library(survminer)
library(car)

data("retinopathy", package="survival") # load in dataset that comes with the survival package

head(retinopathy) # look at first few rows

# plot!
fit <- survfit(Surv(futime,status) ~ trt, data=retinopathy) # make a km survival curve that separates by treatment (trt)
ggsurvplot(fit, data=retinopathy, # plot the "fit" object using retinopathy data; using ggsurvplot (ggplot won't work with a survfit object)
           size = 1, # size of lines
           palette = c("#ABC864","#795F9E"), # manually set color palette
           conf.int = TRUE, # put confidence intervals on plot
           legend.labs = c("Control Eye","Treated Eye"), # label the legend based on the treatments
           xlab = "Time (days)", # label x axis
           ylab = "Survival Probability", # label y axis
           ggtheme = theme_bw()) # set theme

# model
# the plot used a Kaplan Meier curve, but we want to do cox proportional hazards, so it's a different function
model <- coxph(Surv(futime, status) ~ trt, data = retinopathy) # make a cox model looking at time to event, status (of event), impacted by treatment in the retinopathy dataset

#is our effect (treatment) significant?
Anova(model, test="LR") # yes! p = 2.246e-06 ***

# interpret the model
print(model) # hazard ratio = 0.46 -> since it's less than 1, that means it DECREASES the risk of event

abs(1-exp(coef(model)))*100 # output should be 54%

#Treatment reduced the risk of going blind by 54%, a highly significant effect (LR Chi-Square = 22.4, N = 394, P = 0.0000022)