rm(list=ls())
library(survival)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(ggsurvfit)
library(survminer)
library(car)

data("retinopathy", package="survival")

head(retinopathy)

# plot!
fit <- survfit(Surv(futime,status) ~ trt, data=retinopathy)
ggsurvplot(fit, data=retinopathy, 
           size = 1,
           palette = c("#ABC864","#795F9E"),
           conf.int = TRUE,
           legend.labs = c("Control Eye","Treated Eye"),
           xlab = "Time (days)",
           ylab = "Survival Probability",
           ggtheme = theme_bw())

# model
model <- coxph(Surv(futime, status) ~ trt, data = retinopathy)

#is our effect significant?
Anova(model, test="LR") # yes!

# interpret the model
print(model) # hazard ratio = 0.46 -> since it's less than 1, that means it DECREASES the risk of event

abs(1-exp(coef(model)))*100 # output should be 54%

#Treatment reduced the risk of going blind by 54%, a highly significant effect (LR Chi-Square = 22.4, N = 394, P = 0.0000022)