############################################################
# LSHTM - MSc in Medical Statistics
# Causal Inference and Missing Data
# Term 3 - E slot
# Practical 4: propensity score 

#	R script:			CIMD_pract4_R.R
#	Dataset used:		RFAcat.dta
#	Dataset created:	None
#	Output:				Model estimates on screen
#
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

## exercise 1 ,, table for variables for agecat, nodact, durcat and diacat

table(dat$agecat)
table(dat$nodcat)
table(dat$durcat)
table(dat$diacat)

## exercise 2 Standard multivariable regression

m <- glm(dodp ~ rfa + hospital + diacat + position + agecat + gender + smoke + 
           nodcat + mets + durcat + primary, data = dat, family = binomial)
m <- glm(dodp ~ rfa + hospital + diacat + position + agecat + gender + smoke + 
           nodcat + mets + durcat + primary, data = dat, family = "binomial"
summary(m)



####Standardised mean differences,calculate the standardised mean differences after installing the relevant package. 
#The tableone package provides SMDs:

t1 <- CreateTableOne(
  vars = c("agecat", "gender", "smoke", "hospital", "nodcat", "mets", 
           "durcat", "diacat", "primary", "position", "coag"),
  strata = "rfa",
  factorVars = c("agecat", "gender", "smoke", "hospital", "nodcat", "mets", 
                 "durcat", "diacat", "primary", "position", "coag"),
  smd = TRUE, 
  data = dat
)


print ( t1 , smd = T )




