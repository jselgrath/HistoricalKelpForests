# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

# March 4, 2020
##################################################
# Goal: summarize temp by year
##################################################

# setup ########################################
library(tidyverse); library(ggplot2)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp")
d1<-read_csv("./data/PacificGrove_TEMP_1919-201906_noheader.csv", guess_max = 40000)%>%
  select(YEAR:TEMP_FLAG)
glimpse(d1)

# summarize SST by year
d1$SURF_TEMP_C <-as.numeric(d1$SURF_TEMP_C)
d2<-d1%>%
  dplyr::select(year=YEAR,month=MONTH,TIME,tempc=SURF_TEMP_C,TEMP_FLAG)%>%
  group_by(year)%>%
  summarize(
    tempc_u=mean(tempc,na.rm=T), 
    tempc_min=min(tempc,na.rm=T), 
    tempc_max=(max(tempc,na.rm=T)), 
    temp_cv= (sd(tempc,na.rm=T)/tempc_u)*100) #tempc_med=median(tempc,na.rm=T),
d2

# lag year
d2$tempc_max_lag1<-lag(d2$tempc_max,n=1L)
d2$temp_cv_lag1<-lag(d2$temp_cv,n=1L)

d2
write_csv(d2,"./results/sst_c_year.csv")
