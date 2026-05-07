############################################################
# LSHTM - MSc in Medical Statistics
# Causal Inference and Missing Data
# Term 3 - Eslot
# Practical 3: regression methods for continuous outcomes
#
# Created by Clemence Leyrat

#	R script:			CIMD-practical3_R.R
#	Dataset used:		cattaneo2.dta
#	Dataset created:	None
#	Output:				Model estimates on screen
#
###########################################################
"C:/Users/danit/Downloads"
###Remove lock folders if needed

unlink("C:/Users/danit/AppData/Local/R/win-library/4.5/00LOCK", 
       recursive = TRUE, force = TRUE)

###Install remotes - easier than devtools
install.packages("remotes")

# 4. Install teffectsR from GitHub
remotes::install_github("ohines/teffectsR")
##5. Load it

install.packages("marginaleffects")
install.packages("teffectsR")

library(haven)
library(tableone)
library(emmeans)
library(marginaleffects)
library(teffectsR)

#Reading the data 
#Change the path to where the data are saved
# Set working directory
setwd("C:/Users/danit/Downloads")

# Confirm location
getwd()

# Read Stata file
dat <- read_dta("cattaneo2.dta")

# View first rows
head(dat)
summary(dat)

##############
#Exercise 1
##############

#Description of continuous variables
summary(dat$bweight)
#Description of binary variables
table(dat$mbsmoke)

t1<-CreateTableOne(vars=c("fbaby", "mmarried", "alcohol", "fedu", "mage"),
                   strata = "mbsmoke", 
                   factorVars = c("fbaby", "mmarried", "alcohol"), data=dat)
print(t1)

##############
#Exercise 2
##############

#a) 
m1<-lm(bweight~as.factor(mbsmoke), data=dat)
summary(m1)

#b)
m2<-lm(bweight~as.factor(mbsmoke)+as.factor(fbaby), data=dat)
summary(m2)


###############
#Exercise 3
###############

t2<-CreateTableOne(vars=c("mbsmoke","bweight"), strata="fbaby", 
                   factorVars=c("mbsmoke"),data=dat)
print(t2)

aggregate(dat$bweight, list(Smoke = dat$mbsmoke), mean)


##############
#Exercise 6
##############

m3<-teffect(mbsmoke~1,bweight~fbaby,data=dat,method="RA")
m3

###############
#Exercise 7
###############

m4<-lm(bweight~fbaby*mbsmoke,data=dat)
summary(m4)

#To get c-specific effects
emm_options(opt.digits = FALSE)
contr<-contrast(emmeans(m4, ~ mbsmoke*fbaby), 
                "revpairwise",by="fbaby")
contr



###############
#Exercise 8
###############

#To calculate the marginal effect we need to know P(fbaby) 
p<-prop.table(table(dat$fbaby))
p
#We also need the effect in each subgroup
c<-summary(contr)$estimate




###############
#Exercise 9
###############

#By hand
marg_hand<-p[1]*c[1]+p[2]*c[2]
marg_hand


#With the marginaleffects package
marg<-avg_comparisons(m4, variables = "mbsmoke")
print(marg, digits=6) #Same results


##############
#Exercise 11
############## 

m5<-lm(bweight~mbsmoke+fbaby+mmarried+alcohol+fedu+mage,data=dat)
summary(m5)

m6<-teffect(mbsmoke~1,bweight~fbaby+mmarried+alcohol+fedu+mage,data=dat,method="RA")
m6


##############
#Exercise 12
############## 

m7<-lm(bweight~fbaby+mmarried+alcohol+fedu+mage,data=dat[dat$mbsmoke==0,])
Y0<-mean(predict(m7,newdata=dat))

m8<-lm(bweight~fbaby+mmarried+alcohol+fedu+mage,data=dat[dat$mbsmoke==1,])
Y1<-mean(predict(m8,newdata=dat))

ATE=Y1-Y0
ATE