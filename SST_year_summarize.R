# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: summarize temp by year

# ==================================================================================
# DATA INFO 
# ==================================================================================									
# Shore Stations Program - Pacific Grove Surface Temperature Data									
# Pacific Grove, CA   36¡37'13.2"N 121¡54'12.1"W									
# Data provided are subject to revision and were archived 2022-10-24.									
# 									
# ==================================================================================									
# ***PLEASE CITE & REFERENCE THIS PROGRAM AS FOLLOWS***									
# 									
# Carter, Melissa L.; Flick, Reinhard E.; Terrill, Eric; Beckhaus, Elena C.; Martin, Kayla; Fey, Connie L.;			
# Walker, Patricia W.; Largier, John L.; McGowan, John A. (2022). Shore Stations Program, Pacific Grove.						
# In Shore Stations Program Data Archive: Current and Historical Coastal Ocean 									
# Temperature and Salinity Measurements from California Stations. https://doi.org/10.6075/J0V69JF4									
# 									
# Funding for the Shore Stations Program provided by the California Department of Parks and Recreation, 						
# Natural Resources Division, Award# C1670003.									
# 									
# Source: 									
# Shore Stations Program (https://shorestations.ucsd.edu/)									
# Scripps Institution of Oceanography									
# University of California									
# La Jolla, California USA 92093-0218									
# Contact: sioshorestation@gmail.com  									
# 									
# Data are collected by research personnel with Hopkins Marine Station.									
# 									
# ******************* NOTE ********************									
# 0 = good data,									
# 1 = illegible entry,									
# 2 = data differs from other sources, ie. temperature vs. salinity records,									
# 3 = data uncertain,									
# 4 = leaky bottle,									
# 5 = data collected through different program									
# 6 = data collected at different location.									
# Time flags indicate date or time uncertainties.									
# Data flags indicate uncertainties with data.									
# Time of sample collection available for 2003-current data only.									
# Time records in Pacific Standard time zone (PST).									
# **Temperature data for Aug 1, 2009 - Dec 9, 2011 may be off by +/- 0.1 - 0.4 ¡C due to									
#  thermometer issues.  These uncertain data are flagged with 3's in data flag column.***									
# ==================================================================================									
# 									

##################################################

# setup ########################################
library(tidyverse); library(ggplot2)  
rm(list=ls())
################################################
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./data/PacificGrove_TEMP_1919-202112_NoMetadata.csv", guess_max = 40000)%>%
  glimpse()

# view(filter(d1,YEAR==1976))

all_yr<-d1%>%
  mutate(tempc =as.numeric(SURF_TEMP_C))%>%
  filter(TEMP_FLAG!=3)%>%
  summarize(tempc_u=mean(tempc,na.rm=T),
            temp_CV=sd(tempc,na.rm=T)/tempc_u*100,
            temp_max=max(tempc,na.rm=T),
            temp_min=min(tempc,na.rm=T))%>%
  glimpse()
write_csv(all_yr,"./doc/temperature_sums.csv")

# summarize SST by year
# 1976 missing data - fill in with neighboring years mean
temp76<-d1%>%
  filter(YEAR==1975 | YEAR==1977)%>%
  select(SURF_TEMP_C)%>%
  mutate(SURF_TEMP_C=as.numeric(SURF_TEMP_C))%>%
  filter(SURF_TEMP_C>=0)%>%
  mutate(year=1976)%>%
  group_by(year)%>%
  summarize(
      tempc_u=mean(SURF_TEMP_C,na.rm=T), 
      tempc_min=min(SURF_TEMP_C,na.rm=T), 
      tempc_max=(max(SURF_TEMP_C,na.rm=T)), 
      temp_cv= (sd(SURF_TEMP_C,na.rm=T)/tempc_u)*100)%>%

  glimpse()

  
d2<-d1%>%
  mutate(SURF_TEMP_C =as.numeric(SURF_TEMP_C))%>%
  dplyr::select(year=YEAR,month=MONTH,time=TIME,tempc=SURF_TEMP_C,TEMP_FLAG)%>%
  group_by(year)%>%
  summarize(
    tempc_u=mean(tempc,na.rm=T), 
    tempc_min=min(tempc,na.rm=T), 
    tempc_max=(max(tempc,na.rm=T)), 
    temp_cv= (sd(tempc,na.rm=T)/tempc_u)*100)%>% #tempc_med=median(tempc,na.rm=T),
  filter(year!=1976)%>%
  rbind(temp76)%>%
  glimpse()
d2

filter(d2,year==1976)


# lag year
d2$tempc_max_lag1<-lag(d2$tempc_max,n=1L)
d2$temp_cv_lag1<-lag(d2$temp_cv,n=1L)
d2$tempc_max_lag2<-lag(d2$tempc_max,n=2L)
d2$temp_cv_lag2<-lag(d2$temp_cv,n=2L)
d2$tempc_max_lag3<-lag(d2$tempc_max,n=3L)
d2$temp_cv_lag3<-lag(d2$temp_cv,n=3L)

d2
write_csv(d2,"./results/sst_c_year.csv")
