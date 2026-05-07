############################################################
# LSHTM - MSc in Medical Statistics
# Causal Inference and Missing Data
# Term 3 - E slot
# Practical 4: regression methods for binary outcomes

# Created by Clémence Leyrat

#	R script:			CIMD_pract4_R.R
#	Dataset used:		RFA.dta
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

#Reading the data 
#Change the path to where the data are saved
# Set working directory
setwd("C:/Users/danit/Downloads")

# Confirm location
getwd()

# Read Stata file
dat<-read_dta("rfa.dta")
# View first rows
head(dat)
summary(dat)

#Recoding variables
dat$
dat$rfa<-as.numeric(dat$rfa)
dat$gender<-as.factor(dat$gender)
dat$smoke<-as.factor(dat$smoke)
dat$hospital<-as.factor(dat$hospital)
dat$mets<-as.numeric(dat$mets)
dat$primary<-as.factor(dat$primary)
dat$position<-as.factor(dat$position)
dat$dodp<-as.numeric(dat$dodp)
dat$coag<-as.factor(dat$coag)


##############
#Exercise 1
##############

#Description of continuous variables
summary(dat$age)
summary(dat$duration)
summary(dat$maxdia)
summary(dat$nodules)


#Description of binary variables

table(dat$gender)
table(dat$smoke)
table(dat$hospital)
table(dat$mets)
table(dat$primary)
table(dat$position)
table(dat$coag)


t1<-CreateTableOne(vars=c("age","gender","smoke","hospital","nodules","mets",
                          "duration","maxdia","primary","position","coag"),
                   strata = "rfa", 
                   factorVars = c("gender","smoke","hospital","mets",
                                  "primary","position","coag"), smd=T, data=dat)
print(t1, smd=T)


##############
#Exercise 2
##############

#a) 
m1<-glm(dodp~as.factor(rfa), data=dat, family="binomial")
summary(m1)

#b)
m2<-glm(dodp~as.factor(rfa)+hospital+ maxdia+ position, data=dat, family="binomial")
summary(m2)



###############
#Exercise 6
###############

m3<-teffect(rfa~1,dodp~1,data=dat,method="RA", outcome.family="binomial")
m3

m4<-teffect(rfa~1,dodp~hospital+maxdia+position,
            data=dat,method="RA", outcome.family="binomial")
m4

#With the marginaleffects package
m3b<-glm(dodp~as.factor(rfa), data=dat, family="binomial")
#ATE and CI
marg<-avg_comparisons(m3b, variables = list(rfa = 0:1))
print(marg, digits=6) #Same results
#Potential outcomes
p <- predictions(m3b, newdata = datagrid(rfa = 0:1, grid_type = "counterfactual"))
aggregate(estimate ~ rfa, data = p, FUN = mean)


m4b<-glm(dodp~as.factor(rfa)*(hospital+ maxdia+ position), data=dat, family="binomial")
m4b
#ATE and CI
marg<-avg_comparisons(m4b, variables = list(rfa = 0:1))
print(marg, digits=6) #Same results
#Potential outcomes
p <- predictions(m4b, newdata = datagrid(rfa = 0:1, grid_type = "counterfactual"))
aggregate(estimate ~ rfa, data = p, FUN = mean)


##############
#Exercise 10
##############

m5<-teffect(rfa~1,dodp~hospital+maxdia+position+age+gender+smoke+
              nodules+mets+duration+primary,
            data=dat,method="RA", outcome.family="binomial")
m5

#This can also be obtained using the marginaleffects package as described above

###############
#Exercise 11
###############

m6<-teffect(rfa~1,dodp~hospital+maxdia+position+age+gender+smoke+
              nodules+mets+duration+primary+coag,
            data=dat,method="RA", outcome.family="binomial")
m6



###############
#Exercise 12
###############

m7<-teffect(rfa~1,dodp~1,data=dat,method="RA", outcome.family="binomial",
            treatment.effect="ATT")
m7

m7b<-glm(dodp~rfa, data=dat, family=binomial("identity"))
summary(m7b)

m8<-teffect(rfa~1,dodp~hospital+maxdia+position,
            data=dat,method="RA", outcome.family="binomial",
            treatment.effect="ATT")
m8

m9<-teffect(rfa~1,dodp~hospital+maxdia+position+age+gender+smoke+
              nodules+mets+duration+primary,
            data=dat,method="RA", outcome.family="binomial",
            treatment.effect="ATT")
m9

m10<-teffect(rfa~1,dodp~hospital+maxdia+position+age+gender+smoke+
               nodules+mets+duration+primary+coag,
             data=dat,method="RA", outcome.family="binomial",
             treatment.effect="ATT")
m10

table(dat$coag,dat$rfa)


#With the marginaleffects package
m8b<-glm(dodp~as.factor(rfa)*(hospital+ maxdia+ position), data=dat, family="binomial")

#ATT and CI
marg<-avg_comparisons(m8b, variables = list(rfa = 0:1), newdata=dat[dat$rfa==1,])
print(marg, digits=6) #Same results

#Potential outcomes
newdat<-datagrid(model=m8b, rfa = 0:1, grid_type = "counterfactual", 
                 newdata=dat[dat$rfa==1,])
p <- predictions(m8b, newdata = newdat)
aggregate(estimate ~ rfa, data = p, FUN = mean)


##############
#Exercise 13
############## 

m11<-teffect(rfa~1,dodp~1,data=dat,method="RA", outcome.family="binomial")
m11

P1<-m11$Po.means[[1]][1]
P0<-m11$Po.means[[1]][2]

OR<-(P1/(1-P1))/(P0/(1-P0))
OR

m11b<-glm(dodp~as.factor(rfa), data=dat, family="binomial")
#ATE and CI
marg<-avg_comparisons(m11b, variables = list(rfa = 0:1), comparison="lnoravg")
print(marg, digits=6)
exp(-0.588125)