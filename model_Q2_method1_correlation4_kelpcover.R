# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

# -------------------------------------------------
# Q2. Method 1: Maximizing mapped years (1856–2016)
# -------------------------------------------------

# Goal:    analysis of kelp cover correlation with other variables - setting correlation structure for model

# Methods: Zurr 2010, Ch5: Mixed Effects Modeling for Nested Data
# Urchins, Pycno, Temp, urchins not in model because not available for early years
# using location3 data to account for variance at locations and gradual return of otters to regions
#===================================================

# Best correlation structure= m6<-gls(fxn1, 
# correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),  
# varIdent(form=~1|location3),
# method="REML", data=d2)

# m4,m5,m6 all similar
#===================================================

# setup ########################################
library(tidyverse); library(RColorBrewer);  library(colorspace) 
library(arm);library(lme4);  library(nlme); library(car);

# output:     ./results/alldata_kelpcover_otterpurple_location3.csv
#             ./results/alldata_kelpcover_otterpurple_location3_enso.csv
#             ./results/alldata_kelpcover_otterpurple_location3_temp.csv
#             ./results/alldata_kelpabundance_otterpurple_location3.csv
#===================================================
rm(list=ls())
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")
dateToday=Sys.Date()

# originally used the data:kelp_map_allsources_area_location3.csv , but switched so that more years with kelp-enso-otters
# now switched to alldata_kelpmap_location because only 1 line per year/site combo

# kelp percent cover and other variables
# location3 is for accounting for location specific variation in when otters returned. 
d1<-read_csv("./results/all_var_location3_all_yr.csv")%>% # this version of data only has years with kelp maps
  dplyr::select(year,location3,kelp_maxarea_p,otter_u)%>%
  #,pycno_abundance_u2,urchin_abundance_u2,tempc_max,tempc_max_lag1,temp_cv
  arrange(year,location3)%>%
  glimpse()
d1

d1$otter_period<-"Absent/Rare"
d1$otter_period[d1$year>1980]<-"Present" #since no maps between 1930s and 1980s, ok to split this way

# set location values 
# note there is a location version of the data too
d1$location<-d1$location3
d1$location[d1$location=="Monterey Bay" | d1$location=="Monterey Outer" | d1$location=="Carmel" | d1$location=="Carmel Bay" |d1$location=="Monterey Unspecified"| d1$location=="Carmel Outer"] <- "Monterey Peninsula"
d1$location[d1$location=="Big Sur"]<-"Big Sur"
d1$location[d1$location=="Santa Cruz Bay"]<-"Santa Cruz"
d1$location[d1$location=="Santa Cruz Outer"]<-"Santa Cruz"
unique(d1$location)



d1$Location<-d1$location%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")



source("./bin/deets.R")
ggplot(d1,aes(x=year,y=kelp_maxarea_p,color=Location))+geom_point()+deets3+facet_grid()


# models ###########
d2<-d1
str(d2)
d2$Location<-as.factor(d2$Location)
d2$location<-as.factor(d2$location)
d2$location3<-as.factor(d2$location3)




#===================================================
# START HERE BY BUILDING FULL MODEL AND FIGURING OUT TEMPORAL AUTOCORRELATION STRUCTURE
# try corAr1 & ARMA
# use varIdent for groups
# use varComb to combine these two types of variance structures
#===================================================

#===================================================
# STEP 1: build full lm
# because kelp_abundance is a mean, the poisson distribution does not work
# later does not converge if too many interactions

fxn1<-formula(kelp_maxarea_p ~ year+Location+otter_u+otter_period) #was otter abundance u
m1<-lm(fxn1, data=d2)
summary(m1)
# plot(m1)

# gls model version
m2<-gls(fxn1, data=d2)
summary(m2) 
r2<-resid(m2, type = "normalized") 

# compare residuals with time
qplot(d2$year,r2,ylab="residuals", xlab="Year", color=d2$location,main="Model 2")

acf(r2,na.action = na.pass,
    main="Auto-correlation plot for residuals m2") 

f2<-fitted(m2)
plot(m2) #unequal residuals


qplot(f2,r2,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0)
hist(r2,xlab="Residuals", main = "Model 2")

qplot(d1$location3,r2,ylab="residuals", xlab="Location", color=d2$Location,main="Model 2")+geom_boxplot()
qplot(d2$year,r2,ylab="residuals", xlab="Year", color=d2$Location,main="Model 2")

bartlett.test(r2,d2$location3) #  sensitive to non-normality


#===================================================
# time correlation#############
#===================================================
# corAr1 

m3<-gls(fxn1,correlation=corAR1(form=~1|location3/year), ## was Location
        method="REML", data=d2)
summary(m3) 
r3g<-resid(m3, type = "normalized")
acf(r3g,na.action = na.pass,
    main="Auto-correlation plot for residuals GLS") 

f3<-fitted(m3)
plot(m3) 
anova(m2,m3) # 1

r3<-r3g
qplot(f3,r3,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0) #ok
hist(r3,xlab="Residuals", main = "Model 3") # large variance
qplot(d2$Location,r3,ylab="residuals",xlab="Location", main="Model 3", fill=d2$Location)+geom_boxplot()+geom_boxplot()
qplot(d2$year,r3,ylab="residuals",xlab="Year", main="Model 3")+geom_hline(yintercept = 0)

#===================================================
# Model 4: ARMA  ########
# THIS IS BETTER

m4<-gls(fxn1,correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),
        method="REML", data=d2)

summary(m4) 
r4<-resid(m4, type = "normalized")

acf(r4,na.action = na.pass,
    main="Auto-correlation plot for residuals corARMA")  # MUCH better

f4<-fitted(m4)
plot(m4) # 
anova(m4,m2) 
anova(m4,m3) 

qplot(f4,r4,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0) # this looks good

hist(r4,xlab="Residuals", main = "Model 4") #very normal, though a few outliers
qplot(d2$year,r4,ylab="residuals", xlab="Year", color=d2$Location,main="Model 4")
qplot(d2$Location,r4,ylab="residuals",xlab="Location", main="Model 4", fill=d2$Location)+geom_boxplot()+geom_hline(yintercept = 0) #variance fairly even across sites

qplot(d2$location3,r4,ylab="residuals",xlab="Location", main="Model 4", fill=d2$location3)+geom_boxplot()+geom_hline(yintercept = 0) #variance fairly even across sites

#===================================================
# Model 5: varIdent + AutoCor 

m5<-  gls(fxn1, 
        correlation=corAR1(form=~1|year),
        varIdent(form=~1|location3),
        method="REML", data=d2)
summary(m5) 
r5<-resid(m5, type = "normalized")
acf(r5,na.action = na.pass,
    main="Auto-correlation plot for residuals") #less good


f5<-fitted(m5)
plot(m5, main = "Model 5") # much better even than m4
anova(m2,m5) # m5 is much better
anova(m4,m5) # not significantly different

qplot(f5,r5,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0)
hist(r5,xlab="Residuals", main = "Model 5")
qplot(d2$year,r5,ylab="residuals", xlab="Year", color=d2$Location,main="Model 5")
qplot(d2$Location,r5,ylab="residuals",xlab="Location", main="Model 5", fill=d2$Location)+geom_boxplot()+geom_hline(yintercept = 0)

#===================================================
# Model 6: varIdent + corARMA (autoregressive moving average, Zurr p150)
m6<-gls(fxn1, 
        correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),  
        varIdent(form=~1|location3),
        method="REML", data=d2)

summary(m6) 
r6<-resid(m6, type = "normalized")

acf(r6,na.action = na.pass,
    main="Auto-correlation plot for residuals") # similar to m4, m5

f6<-fitted(m6)
plot(m6, main = "Model 6") #same as 5
anova(m4,m6) # m5 & m6 not different

qplot(f6,r6,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0)
hist(r6,xlab="Residuals", main = "Model 6")
qplot(d2$year,r6,ylab="residuals", xlab="Year", color=d2$Location,main="Model 6")
qplot(d2$Location,r6,ylab="residuals",xlab="Location", main="Model 6", fill=d2$Location)+geom_boxplot()+geom_hline(yintercept = 0)

