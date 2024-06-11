# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

#===================================================
# Goal: analysis of kelp cover correlation with other variables through time
# variance structure set in "model_kelpcover_correlation4_otterperiod.R"
# removed 1852 map, due to different kelp symbology

# this works too: correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),
# method="REML"

# for Revisions, updated to calculate estimates of X fold change in kelp cover
#===================================================

#===================================================
# setup ########################################
library(tidyverse); library(RColorBrewer);  library(colorspace) 
 library(nlme); library(MuMIn) # for model select library(lme4); library(arm);
#===================================================

#===================================================
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")
dateToday=Sys.Date()

# kelp percent cover and other variables
# location3 is for accounting for location specific variation in when otters returned. 
# selecting here for variables with long time series and years that correspond to kelp maps
d1_3<-read.csv("./results/all_var_location3_all_yr.csv")%>%
  glimpse()

d1_3$otter_period<-"Absent/Rare"
d1_3$otter_period[d1_3$year>1980]<-"Present" #since no maps between 1930s and 1980s, ok  to split this way

#===================================================
# ORGANIZING #####
#===================================================
# set location values - using location3 data so can include location3 in variance structure 
# location3 is for accounting for location specific variation in when otters returned using mixed effects
d1_3$location<-d1_3$location3
d1_3$location[d1_3$location=="Monterey Bay" | d1_3$location=="Monterey Outer" | d1_3$location=="Carmel" | d1_3$location=="Carmel Bay" |d1_3$location=="Monterey Unspecified"| d1_3$location=="Carmel Outer"] <- "Monterey Peninsula"
d1_3$location[d1_3$location=="Big Sur"]<-"Big Sur"
d1_3$location[d1_3$location=="Santa Cruz Bay"]<-"Santa Cruz"
d1_3$location[d1_3$location=="Santa Cruz Outer"]<-"Santa Cruz"
unique(d1_3$location)

# ===================================================
# set factors 
# ===================================================

d1<-d1_3

# location
d1$Location<-d1$location
d1$Location<-as.factor(d1$Location)
d1$Location<-d1$Location%>%fct_relevel("Monterey Peninsula","Big Sur","Santa Cruz")

# location3
d1$location3<-as.factor(d1$location3)

# otter period
d1$otter_period<-as.factor(d1$otter_period)

# removed earliest map because decided different method that is not comparable (brought up in an internal review comment)
d1<-d1%>%
  filter(year!=1852)
range(d1$year)

glimpse(d1)

d_sum<-d1%>%
  group_by(otter_period,location)%>%
  summarize(
    n=n(),
    year_min=min(year),
    year_max=max(year),
    kelp_u=mean(kelp_maxarea_p),
    kelp_sd=sd(kelp_maxarea_p),
    kelp_sem=kelp_sd/sqrt(n)
  )%>%
  glimpse()
write_csv(d_sum,"./doc/kelpsummary_20230703_loc3.csv")

d_change<-d_sum%>%
  group_by(location)%>%
  summarize(
    change_decline=kelp_u[1]/kelp_u[2],
    change_increase=kelp_u[2]/kelp_u[1])%>%
  glimpse()
  


#===================================================
# set analysis correlation and variance structures ######
#===================================================

corr<-  (correlation=corARMA(c(0.2), form=~1|year, p = 1, q = 0))
varr<-  (weights=varIdent(form=~1|location3))

#===================================================
# Figuring out best fixed effects
#===================================================

# set possible model structures

# all four variables

# three vairables, no enso
fxn1<-formula(kelp_maxarea_p ~ year*Location*otter_period+yr_after_enso1_5)
fxn2<-formula(kelp_maxarea_p ~ year*Location+otter_period+yr_after_enso1_5)
fxn3<-formula(kelp_maxarea_p ~ year+Location*otter_period+yr_after_enso1_5)
fxn4<-formula(kelp_maxarea_p ~ year+Location+otter_period+yr_after_enso1_5)

# two variables, Location, otter_period
fxn5<-formula(kelp_maxarea_p ~ Location*otter_period) 
fxn6<-formula(kelp_maxarea_p ~ Location+otter_period)

# two variables, otter_period, year
fxn7<-formula(kelp_maxarea_p ~ otter_period*Location+yr_after_enso1_5) 
fxn8<-formula(kelp_maxarea_p ~ otter_period*Location+yr_after_enso1_5)

# two variables, Location, year
fxn9<-formula(kelp_maxarea_p ~ year*Location) 
fxn10<-formula(kelp_maxarea_p ~ year+Location)

#single variables
fxn11<-formula(kelp_maxarea_p ~ otter_period)
fxn12<-formula(kelp_maxarea_p ~ Location)
fxn13<-formula(kelp_maxarea_p ~ year)

# intercept
fxn14<-formula(kelp_maxarea_p ~ 1)


#===================================================
# model selection
#===================================================
  # use ML to find optimal fixed component ####
  # zurr p 133-135; ML needed to compare two models with different fixed effects . Crawly p 716
m1<-gls(fxn1,   corr, varr, method="ML", data=d1)
m2<-gls(fxn2,   corr, varr, method="ML", data=d1)
m3<-gls(fxn3,   corr, varr, method="ML", data=d1)
m4<-gls(fxn4,   corr, varr, method="ML", data=d1)
m5<-gls(fxn5,   corr, varr, method="ML", data=d1)
m6<-gls(fxn6,   corr, varr, method="ML", data=d1)
m7<-gls(fxn7,   corr, varr, method="ML", data=d1)
m8<-gls(fxn8,   corr, varr, method="ML", data=d1)
m9<-gls(fxn9,   corr, varr, method="ML", data=d1)
m10<-gls(fxn10, corr, varr, method="ML", data=d1)
m11<-gls(fxn11, corr, varr, method="ML", data=d1)
m12<-gls(fxn12, corr, varr, method="ML", data=d1)
m13<-gls(fxn13, corr, varr, method="ML", data=d1)
m14<-gls(fxn14, corr, varr, method="ML", data=d1)


#===================================================
# select best model
#===================================================
model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14) 
# m5 is the best

plot(m5,main="m5") # kelp_maxarea_p ~ Location*otter_period

#========================================.===========
# save final model and tables ########################
#===================================================
# NOTE: this is different from the models above because using ML. See Zurr p 137
# In some cases where best model is not obvious, results may be slightly diff here...

# Here using automatic ones, but might adjust based on output

#===================================================
# function with final models #############
# Final 5: kelp_maxarea_p ~ Location*otter_period

#5
fxn_final<-formula(kelp_maxarea_p ~ Location*otter_period)
mFinal<-gls(fxn_final,
            corr,varr,
            method="REML", data=d1)

# t statistics not useful when factors have > 2 levels (i.e. Location) Zurr 2009 p91
summary(mFinal)

#anova does sequential testing, good for interactions, less useful for main effects alone
anova(mFinal)
mFinal

fit<-fitted(mFinal)
res<-resid(mFinal,type="normalized")
plot(mFinal,main="Residuals vs. Fitted Vaules")

#===================================================
# save final model
#===================================================
save(mFinal,file="./results/model_kelpcover_yearlocationottersenso.rda")
  
#===================================================
# save anova table
a1<-data.frame(anova(mFinal)) #set as df
a1$var<-row.names(a1); #a1$model<-modl # add identifying variables
row.names(a1)<-c(1:length(a1$p.value)); str(a1) # reset row names
a1
a1$F.value=round(a1$F.value,2)
a1$p.value=round(a1$p.value,2) #
a1$p.value<-as.character(a1$p.value)
a1$p.value[a1$p.value<"0.001"]<-"<0.001"
a1<-a1%>%
  dplyr::select(Variable=var,df=numDF,F.value,p.value)
a1
write_csv(a1,"./doc/table_SI9_model_kelpcover_yearlocationottersenso_anovatable.csv")
  
  
#===================================================
# save coefficents #####
#===================================================
a2<-data.frame(mFinal[4]) 
a2$var<-row.names(a2)
row.names(a2)<-c(1:length(a2$coefficients)) # reset row #s
str(a2)
a2$coefficients<-round(a2$coefficients,3)
a2<-a2%>%
  dplyr::select(Variable=var,coefficients)
a2
write_csv(a2,"./doc/table_SI9_model_kelpcover_yearlocationsottersenso_coefficents.csv")
  
  
#===================================================
rFinal<-resid(mFinal, type = "normalized")
# str(rFinal)
  
fFinal<-fitted(mFinal)
labelNo<-1:length(fFinal)
  
  
#===================================================
# plot autocorrelation and residuals and look for normality and homogenity of variance####
#===================================================
source("./bin/deets.R")
acf(rFinal,na.action = na.pass,
      main="Auto-correlation plot for residuals") 
qplot(fFinal,rFinal,xlab="Fitted Values", ylab = "Residuals")+geom_hline(yintercept = 0)+
geom_text(aes(label=labelNo, hjust=1.5),size=3)

hist(rFinal,xlab="Residuals", main = "Final Model Residuals", nclass=15)

qplot(d1$Location,rFinal,ylab="residuals", xlab="Location", main="Final Model Residuals") +deets3

qplot(d1$year,rFinal,ylab="residuals", xlab="Year", main="Final Model Residuals")
  
