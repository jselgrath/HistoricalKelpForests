# -----------------------------------------------------
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
# -----------------------------------------------------
# Q2. Method 2: Maximizing variables (1934–2020)
# -----------------------------------------------------
# Goal: model variance strucutre
# 
# -----------------------------------------------------
# Ran models through with all data - it is autocorrelated. All of the options for dealing with it and including year as a random effect work to eliminate the temporal autocorrelation.
# m4 and 6 both work. using m4 because simpler
# correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),

# -----------------------------------------------------
# setup ########
library(tidyverse); library(RColorBrewer); library(colorspace); library(nlme)  
# -----------------------------------------------------

# -----------------------------------------------------
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/all_var_location_2023_new_var.csv")%>%
  glimpse()


# reduce urchin levels, selet subset of variables so less is lost with na.omit below.
d2<-d1%>%
  dplyr::select(
    year,location,kelp,kelp2,kelp3,kelp_source,kelp_source2,urchin_u2,urchin2,pycno_u2,otter_u,enso_b2,enso_b1_5,enso_b1_5_Mplus,enso1_5:Year,tempc_max_lag1,tempc_max)%>%
  mutate(kelp_source=factor(kelp_source),
         kelp_source2=factor(kelp_source2), 
         enso_b2=factor(enso_b2),
         enso_b1_5=factor(enso_b1_5),
         enso_b1_5_Mplus=factor(enso_b1_5_Mplus),
         predator3=factor(predator3),
         predator3_orig=factor(predator3_orig)
         )%>%
  glimpse()


# remove years with NA var, rescale d4
d3<-na.omit(d2)
d3$kelp4<-as.numeric(scale(d3$kelp3))
d3$kelp5<-as.numeric(scale(d3$kelp))

# remove years >12 years after ENSO - rare but see year after enso 1_5 and M+, rescale d4
d4<-d3%>%
  filter(yr_after_enso2<=10)%>%
  mutate(kelp4=as.numeric(scale(kelp3)),
         kelp5=as.numeric(scale(kelp)))%>%
  glimpse()

range(d4$yr_after_enso2)
range(d4$yr_after_enso1_5)
range(d4$yr_after_enso1_5_Mplus)




# --------------------------------------------------------------
# Modeling #########
 # --------------------------------------------------------------


#===================================================
# START HERE BY BUILDING FULL MODEL AND FIGURING OUT TEMPORAL AUTOCORRELATION STRUCTURE
# Zurr - p184 & Ch5 -
# NOTE: Steps from Ch7.6 p182 in Zurr (Penguins) - stat with this then switch to modeling temporal correlations below since I was not able to model both variance and correlation due to the data

# NOTE: In this model I could model kelp source2 (collapsed kelp source) and year random effects or variance structure, but not both - they are correlated (see gls graphs below)



# ---------------------------------
# STEP 1: LM
m0<-lm(kelp5~predator3*urchin_u2+yr_after_enso1_5+enso_b1_5+tempc_max+year,data=d4) #predator urchin interaction is NS
# does not work with temp data. Works with all versions of ENSO distance
drop1(m0,test="F")

# op<-par(mfrow=c(1,1))
plot(m0, which=c(1))

# outliers from full lm -------------
glimpse(d2[12,]) # 1958 - first year with different conditions
glimpse(d2[14,]) #1959
glimpse(d2[48,]) #1992 - 

# STEP 2: GLS  model --------------------------------------
# refit full model with gls & validation graphs ----------------------
m1.gls<-gls(kelp5~predator3*urchin_u2+yr_after_enso1_5+enso_b1_5+tempc_max+year,data=d4)
r1<-resid(m1.gls)


# GLS validation graphs - plot residuals vs explanatory variable -------------------------
op<-par(mfrow=c(2,3))
boxplot(r1~d4$predator3_orig,main="Predator Abundance"); abline(0,0)
boxplot(r1~d4$urchin2,main="Purple Urchin Abundance"); abline(0,0)
boxplot(r1~d4$yr_after_enso1_5,main="Enso Distance 1.5 M+"); abline(0,0)
boxplot(r1~d4$year,main="Year"); abline(0,0)
boxplot(r1~d4$kelp_source2,main="Kelp_Source2"); abline(0,0)



# STEP 3: lme model --------------------------------------
# can model kelp source OR year, not both unless because their residuals are correlated

# year
m0.lme<-lme(kelp5~predator3*urchin_u2+yr_after_enso1_5+enso_b1_5+year,data=d4,
            random=~1|year, method="REML") 

# STEP 4: CHOOSE VARIANCE STRUCTURE
# =================================================
# try corAr1 & ARMA
# use varIdent for groups
# use varComb to combine these two types of variance structures
#===================================================

#===================================================
# STEP 1: build full lm
# because kelp_abundance is a mean, the poisson distribution does not work
# later does not converge if too many interactions
fxn1<-formula(kelp5~predator3*urchin_u2+yr_after_enso1_5+enso_b1_5+tempc_max+year,data=d4) #year
m1<-lm(fxn1, data=d4)
summary(m1)
op<-par(mfrow=c(2,2))
plot(m1)


# gls model version -------------------------------------------
m2<-gls(fxn1, data=d4)
summary(m2) 
r2<-resid(m2, type = "normalized") 

# compare residuals with time - high close correlation
op<-par(mfrow=c(1,1))
qplot(d4$year,r2,ylab="residuals", xlab="Year",main="Model 2")
op<-par(mfrow=c(1,1))
acf(r2,na.action = na.pass,
    main="Auto-correlation plot for residuals m2") 

f2<-fitted(m2)
plot(m2) 


qplot(f2,r2,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0)
hist(r2,xlab="Residuals", main = "Model 2")

# GLS validation graphs - plot residuals vs explanatory variable -------------------------
# op<-par(mfrow=c(2,3))
# boxplot(r2~d4$predator3_orig,main="Predator Abundance"); abline(0,0)
# boxplot(r2~d4$urchin2,main="Purple Urchin Abundance"); abline(0,0)
# boxplot(r2~d4$yr_after_enso1_5,main="Enso Distance"); abline(0,0)
# boxplot(r2~d4$year,main="Year"); abline(0,0)
# # boxplot(r1~d4$year,main="Kelp_Source2"); abline(0,0)
# boxplot(r1~d4$enso1_5_lag1,main="Year"); abline(0,0)
bartlett.test(r2,d4$kelp_source2) #  sensitive to non-normality
bartlett.test(r2,d4$predator3)


#===================================================
# time correlation#############
#===================================================
# corAr1  - better
m3<-gls(fxn1,correlation=corAR1(form=~1|year),
         method="REML", data=d4)
summary(m3) 


r3<-resid(m3, type = "normalized")
op<-par(mfrow=c(1,1))
acf(r3,na.action = na.pass,
    main="M3: Auto-correlation plot,\nresiduals GLS")  

f3<-fitted(m3)
plot(m3) 
anova(m2,m3) # 3=better

r3<-r3
qplot(d4$year,r3,ylab="residuals", xlab="Year", color=d4$predator3,main="Model 3")
hist(r3,xlab="Residuals", main = "Model 3")


# GLS validation graphs - plot residuals vs explanatory variable -------------------------
# op<-par(mfrow=c(2,3))
# boxplot(r3~d4$predator3_orig,main="Predator Abundance"); abline(0,0)
# boxplot(r3~d4$urchin2,main="Purple Urchin Abundance"); abline(0,0)
# 
# boxplot(r3~d4$yr_after_enso1_5,main="Enso Distance"); abline(0,0)
# boxplot(r3~d4$year,main="Year"); abline(0,0)
# # boxplot(r1~d4$year,main="Kelp_Source2"); abline(0,0)
# boxplot(r1~d4$enso1_5_lag1,main="Year"); abline(0,0)


#===================================================
# Model 4: ARMA  ######## - also better - looks identical to the previous
m4<-gls(fxn1,correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),
        method="REML", data=d4)


summary(m4) 

r4<-resid(m4, type = "normalized")
op<-par(mfrow=c(1,1))
acf(r4,na.action = na.pass,
    main="Auto-correlation plot\nfor residuals corARMA")  

f4<-fitted(m4)
plot(m4) # 
anova(m4,m2)  #better
anova(m4,m3) #same

qplot(f4,r4,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0) 
hist(r4,xlab="Residuals", main = "Model 4")


qplot(d4$predator3,r4,ylab="residuals", xlab="Predator No.", main="Model 2")+geom_boxplot()
qplot(d4$predator3,r4,ylab="residuals", xlab="Predator No.", main="Model 2")+geom_boxplot()

qplot(d4$year,r4,ylab="residuals", xlab="Year", color=d4$predator3,main="Model 3")



# GLS validation graphs - plot residuals vs explanatory variable -------------------------
# op<-par(mfrow=c(2,3))
# boxplot(r4~d4$predator3_orig,main="Predator Abundance"); abline(0,0)
# boxplot(r4~d4$urchin2,main="Purple Urchin Abundance"); abline(0,0)
# boxplot(r4~d4$yr_after_enso1_5,main="Enso Distance"); abline(0,0)
# boxplot(r4~d4$year,main="Year"); abline(0,0)
# # boxplot(r1~d4$year,main="Kelp_Source2"); abline(0,0)
# boxplot(r1~d4$enso1_5_lag1,main="Year"); abline(0,0)

#===================================================
# Model 5: varIdent + AutoCor 

m5<-  gls(fxn1, 
          correlation=corAR1(form=~1|year),
          varIdent(form=~1|kelp_source2),
          method="REML", data=d4)
summary(m5) 
r5<-resid(m5, type = "normalized")
op<-par(mfrow=c(1,1))
acf(r5,na.action = na.pass,
    main="Auto-correlation plot for residuals\nModel 5") #less good


f5<-fitted(m5)
plot(m5, main = "Model 5") 
anova(m2,m5) # better
anova(m4,m5) # ns

qplot(f5,r5,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0)
hist(r5,xlab="Residuals", main = "Model 5")
qplot(d4$year,r5,ylab="residuals", xlab="Year", color=d4$otter_u,main="Model 5")
# qplot(d4$enso1_5,r5,ylab="residuals",xlab="ENSO 1.5", main="Model 5")+geom_boxplot()+geom_hline(yintercept = 0)


# GLS validation graphs - plot residuals vs explanatory variable -------------------------
# op<-par(mfrow=c(2,3))
# boxplot(r5~d4$predator3_orig,main="Predator Abundance"); abline(0,0)
# boxplot(r5~d4$urchin2,main="Purple Urchin Abundance"); abline(0,0)
# boxplot(r5~d4$yr_after_enso1_5,main="Enso Distance"); abline(0,0)
# boxplot(r5~d4$year,main="Year"); abline(0,0)
# # boxplot(r1~d4$year,main="Kelp_Source2"); abline(0,0)
# boxplot(r1~d4$enso1_5_lag1,main="Year"); abline(0,0)


#===================================================
# Model 6: varIdent + corARMA (autoregressive moving average, Zurr p150)
m6<-gls(fxn1, 
        correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),  
        varIdent(form=~1|kelp_source2),
        method="REML", data=d4)
summary(m6)

r6<-resid(m6, type = "normalized")
op<-par(mfrow=c(1,1))
acf(r6,na.action = na.pass,
    main="Auto-correlation plot for residuals\nModel 6") # similar to m4, m5

f6<-fitted(m6)
plot(m6, main = "Model 6") #same as 5
anova(m2,m6)
anova(m3,m6)
anova(m4,m6) 
anova(m5,m6) # no dif

qplot(f6,r6,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0)
hist(r6,xlab="Residuals", main = "Model 6")
qplot(d4$year,r6,ylab="residuals", xlab="Year", color=d4$otter_u,main="Model 6")

# GLS validation graphs - plot residuals vs explanatory variable -------------------------
# op<-par(mfrow=c(2,3))
# boxplot(r6~d4$predator3_orig,main="Predator Abundance"); abline(0,0)
# boxplot(r6~d4$urchin2,main="Purple Urchin Abundance"); abline(0,0)
# boxplot(r6~d4$yr_after_enso1_5,main="Enso Distance"); abline(0,0)
# boxplot(r6~d4$year,main="Year"); abline(0,0)
# # boxplot(r1~d4$year,main="Kelp_Source2"); abline(0,0)
# boxplot(r1~d4$enso1_5_lag1,main="Year"); abline(0,0)



# Fit  random effects - going to use this to model year as it seems useful
m0.lme<-lme(kelp5~predator3*urchin_u2+yr_after_enso1_5+enso_b1_5+year,data=d4, random=~1|year, method="REML") 


r7<-resid(m0.lme, type = "normalized")
range(r7)
acf(r7,na.action = na.pass,
    main="M LME: Auto-correlation plot,\nresiduals mixed effect")  
# worse, but not terrible
glimpse(d4)

