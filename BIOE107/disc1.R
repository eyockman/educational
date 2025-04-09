# BIOE107 | Emma Yockman
# discussion 1: intro
rm(list=ls())

# R can do math
2.2*10^3
2.2e3

# assign variables/ aka make objects
c=0.5
M=100
E=0.63
k=8.6*10^-5
t=300
e=2.71828

# do complex equations
MR=c*(M^(3/4))*(e^(-E/(k*t))) # metabolic rate

# return outputs
print(MR)