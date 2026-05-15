############################################################
# Causal Inference and Missing Data
# Term 3 - E slot
# Practical 6; IPTW 
###########################################################

#Packages needed 
library(haven) #To read Stata datasets
library(tableone) #To create Table 1
#library(devtools) #To install a package from github
#install_github("ohines/teffectsR")
library(teffectsR) #To reproduce the Stata teffects results
library(emmeans) #To estimate marginal means and contrasts
library(marginaleffects) #To estimate marginal effects
library(tidyverse)

#Reading the data 
#Change the path to where the data are saved
# Set working directory
setwd("C:/Users/danit/Downloads")

# Confirm location
getwd()
# Read Stata file
dat<-read_dta("RFAcat.dta")
# View first rows
head(dat)
summary(dat)

##Exercise 1: IPTW with confounders + outcome causes, excluding coag
install.packages("PSweight")
library(PSweight)

ps_formula <- rfa ~ agecat + gender + smoke + hospital + nodcat + mets +
  durcat + diacat + primary + position

ATE_iptw <- PSweight(
  ps.formula = ps_formula,
  yname = "dodp",
  data = dat,
  weight = "IPW",
  family = binomial(link = "logit")
)

summary(ATE_iptw)