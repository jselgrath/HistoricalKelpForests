# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

#===================================================
# Goal: analysis of kelp cover correlation with other variables through time
# variance structure set in "model_kelpcover_correlation4.R"
# removed 1952 map, due to different kelp symbology


# removing correlation to test if overfitting - best model is the same as with correlation
#===================================================

#===================================================
# setup ########################################
library(tidyverse); library(RColorBrewer);  library(colorspace) 
library(arm);library(lme4); library(nlme); library(MuMIn) # for model select
#===================================================

#===================================================
rm(list=ls())
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
dateToday=Sys.Date()

# kelp percent cover and other variables
# location3 is for accounting for location specific variation in when otters returned. 
# selecting here for variables with long time series and years that correspond to kelp maps
d1_3<-read_csv("./results/alldata_kelpmap_location3.csv")%>% # this version of data only has years with kelp maps
  dplyr::select(year,location3,kelp_area_p,enso,otter_abundance_u2)%>%
  arrange(year,location3)
d1_3
unique(d1_3$location3)

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
d1$Location<-d1$Location%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")

# location3
d1$location3<-as.factor(d1$location3)

# otter period
d1$otter_period<-as.factor(d1$otter_period)

# removed earliest map because decided different method that is not comparable (brought up in an internal review comment)
d1<-d1%>%
  filter(year!=1852)

#===================================================
# Figuring out best fixed effects
#===================================================

# set possible model structures

# all four variables
# fxn0<-formula(kelp_area_p ~ year*Location*otter_period*enso) # singluar fit - does not work
fxn1<-formula(kelp_area_p ~ year*Location*otter_period+enso) 
fxn2<-formula(kelp_area_p ~ year*Location+otter_period*enso)
fxn3<-formula(kelp_area_p ~ year+Location*otter_period*enso) 
fxn4<-formula(kelp_area_p ~ year*Location+otter_period+enso)
fxn5<-formula(kelp_area_p ~ year+Location+otter_period*enso)
fxn6<-formula(kelp_area_p ~ year+Location*otter_period+enso) 
fxn7<-formula(kelp_area_p ~ year+Location+otter_period+enso)

# three vairables, no enso
fxn8<-formula(kelp_area_p ~ year*Location*otter_period)
fxn9<-formula(kelp_area_p ~ year*Location+otter_period)
fxn10<-formula(kelp_area_p ~ year+Location*otter_period)
fxn11<-formula(kelp_area_p ~ year+Location+otter_period)

# three vairables, no location
fxn12<-formula(kelp_area_p ~ year*enso*otter_period)
fxn13<-formula(kelp_area_p ~ year*enso+otter_period)
fxn14<-formula(kelp_area_p ~ year+enso*otter_period)
fxn15<-formula(kelp_area_p ~ year+enso+otter_period)

# three vairables, no otter
fxn12<-formula(kelp_area_p ~ year*enso*Location)
fxn13<-formula(kelp_area_p ~ year*enso+Location)
fxn14<-formula(kelp_area_p ~ year+enso*Location)
fxn15<-formula(kelp_area_p ~ year+enso+Location)

# three vairables, no year
fxn16<-formula(kelp_area_p ~ otter_period*enso*Location)
fxn17<-formula(kelp_area_p ~ otter_period*enso+Location)
fxn18<-formula(kelp_area_p ~ otter_period+enso*Location)
fxn19<-formula(kelp_area_p ~ otter_period+enso+Location)

# two variables, Location, enso
fxn20<-formula(kelp_area_p ~ Location*enso) 
fxn21<-formula(kelp_area_p ~ Location+enso)

# two variables, Location, otter_period
fxn22<-formula(kelp_area_p ~ Location*otter_period) 
fxn23<-formula(kelp_area_p ~ Location+otter_period)

# two variables, enso, otter_period 
fxn24<-formula(kelp_area_p ~ enso*otter_period) 
fxn23<-formula(kelp_area_p ~ enso+otter_period)

# two variables, enso, year
fxn25<-formula(kelp_area_p ~ enso*year) 
fxn26<-formula(kelp_area_p ~ enso+year)

# two variables, otter_period, year
fxn27<-formula(kelp_area_p ~ year*otter_period) 
fxn28<-formula(kelp_area_p ~ year+otter_period)

# two variables, Location, year
fxn27<-formula(kelp_area_p ~ year*Location) 
fxn28<-formula(kelp_area_p ~ year+Location)

#single variables
fxn29<-formula(kelp_area_p ~ otter_period)
fxn30<-formula(kelp_area_p ~ enso)
fxn31<-formula(kelp_area_p ~ Location)
fxn32<-formula(kelp_area_p ~ year)

# intercept
fxn33<-formula(kelp_area_p ~ 1)


#===================================================
# model selection
#===================================================
  # use ML to find optimal fixed component ####
  # zurr p 133-135; ML needed to compare two models with different fixed effects . Crawly p 716
m1<-gls(fxn1,  method="ML", data=d1)
m2<-gls(fxn2,  method="ML", data=d1)
m3<-gls(fxn3,  method="ML", data=d1)
m4<-gls(fxn4,  method="ML", data=d1)
m5<-gls(fxn5,  method="ML", data=d1)
m6<-gls(fxn6,  method="ML", data=d1)
m7<-gls(fxn7,  method="ML", data=d1)
m8<-gls(fxn8,  method="ML", data=d1)
m9<-gls(fxn9,  method="ML", data=d1)
m10<-gls(fxn10,method="ML", data=d1)
m11<-gls(fxn11,method="ML", data=d1)
m12<-gls(fxn12,method="ML", data=d1)
m13<-gls(fxn13,method="ML", data=d1)
m14<-gls(fxn14,method="ML", data=d1)
m15<-gls(fxn15,method="ML", data=d1)
m16<-gls(fxn16,method="ML", data=d1)
m17<-gls(fxn17,method="ML", data=d1)
m18<-gls(fxn18,method="ML", data=d1)
m19<-gls(fxn19,method="ML", data=d1)
m20<-gls(fxn20,method="ML", data=d1)
m21<-gls(fxn21,method="ML", data=d1)
m22<-gls(fxn22,method="ML", data=d1)
m23<-gls(fxn23,method="ML", data=d1)
m24<-gls(fxn24,method="ML", data=d1)
m25<-gls(fxn25,method="ML", data=d1)
m26<-gls(fxn26,method="ML", data=d1)
m27<-gls(fxn27,method="ML", data=d1)
m28<-gls(fxn28,method="ML", data=d1)
m29<-gls(fxn29,method="ML", data=d1)
m30<-gls(fxn30,method="ML", data=d1)
m31<-gls(fxn31,method="ML", data=d1)
m32<-gls(fxn32,method="ML", data=d1)
m33<-gls(fxn33,method="ML", data=d1)

#===================================================
# select best model
#===================================================
model.sel(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32) 
# m22 is the best

plot(m22,main="m22") # kelp_area_p ~ Location*otter_period (same as with correlation structure)

#========================================.===========
# save final model and tables ########################
#===================================================
# NOTE: this is different from the models above because using ML. See Zurr p 137
# In some cases where best model is not obvious, results may be slightly diff here...

# Here using automatic ones, but might adjust based on output

#===================================================
# function with final models #############
# Final 22: kelp_area_p ~ Location*otter_period

#22
fxn_final<-formula(kelp_area_p ~ Location*otter_period)
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
save(mFinal,file="./results/model_kelpcover_yearlocationottersenso_nocorr_varr.rda")
  
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
write_csv(a1,"./doc/model_kelpcover_yearlocationottersenso_anovatable_nocorr_varr.csv")
  
  
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
write_csv(a2,"./doc/model_kelpcover_yearlocationsottersenso_coefficents_nocorr_varr.csv")
  
  
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
  
