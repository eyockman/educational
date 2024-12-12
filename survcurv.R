rm(list=ls())
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)

# tutorial ####
data(cancer)
head(cancer)

cancer2 <- cancer %>% mutate(censor = status-1)
km <- with(cancer2, Surv(time,censor))
head(km,80)

km_fit <- survfit(Surv(time, censor) ~ 1, data=cancer2)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit)

km_sex_fit <- survfit(Surv(time, censor) ~ sex, data=cancer2)
autoplot(km_sex_fit)

cancer3 <- mutate(cancer2, AG = ifelse((age < 60), "LT60", "OV60"),
                  AG = factor(AG),
                  sex = factor(sex,labels=c("male","female")),
                  ph.ecog = factor(ph.ecog,labels=c("asym","symAB","LT50bed","GT50bed")))

km_AG_fit <- survfit(Surv(time, censor) ~ AG + sex, data=cancer3)
autoplot(km_AG_fit)

cox <- coxph(Surv(time, censor) ~ sex + age + meal.cal + wt.loss + inst , data = cancer3)
summary(cox)

cox_fit <- survfit(cox)
autoplot(cox_fit)

aa_fit <-aareg(Surv(time, censor) ~ sex + age +
                 meal.cal + wt.loss + inst , 
               data = cancer3)
aa_fit
autoplot(aa_fit)

# ranger model
r_fit <- ranger(Surv(time, censor) ~ sex + age,
                data = cancer3,
                mtry = 2,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(cancer3)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

vi <- data.frame(sort(round(r_fit$variable.importance, 2), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

# compare all survival curves
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()