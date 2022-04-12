################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of kelp cover using map data

# Note: kelp cover data are proportional (proportional cover)
# Ch5 in Zurr deals with GAMs, ch7 with irregularily spaced data
# in other code I checked covariance structures. all similar, no correlation is best. CorAR is second best.
##################################################
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace); library(mgcv); library(tidymv);library(mgcViz)  
rm(list=ls())

################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/kelp_map_allsources_location_enso.csv")


# setup ########################################
d1$Source<-d1$kelp_source; d1$Source[d1$Source=="Contemporary Data"]<-"Ecological Data"

d1$location<-as.factor(d1$location)
range(d1$kelp_area_p)

# removed earliest map because decided different method that is not comparable (brought up in an internal review comment)
d1<-d1%>%
  filter(year!=1852)

# BUILD GAM MODELS ###############################
# graph
source("./bin/deets.R")

# all sites
m1<-gam(kelp_area_p~
          s(year,by=(as.numeric(location=="Big Sur")))+
          s(year,by=(as.numeric(location=="Monterey Peninsula")))+
          s(year,by=(as.numeric(location=="Santa Cruz")))+
          location,data=d1,method="REML",family=quasibinomial)
summary(m1)
anova(m1)
gam.check(m1)



######################################################
# graphing: https://socviz.co/makeplot.html
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
#


#####################################################
# MODELS WITH YEAR ####################################
######################################################
# Output 101: See Zuur p57

# edf = effective degrees of freedom - 0 - infinity. higher = more non-linear
# deviance explained = R2
# scale est = variance of residuals



######################################################
# santacruz###########################
d1_s<-filter(d1,location=="Santa Cruz")
m1_s<-gam(kelp_area_p~s(year, bs = "cs"),data=d1_s,method="REML",family=quasibinomial)
summary(m1_s)

# graph
ggplot(data=d1_s,aes(x=year,y=kelp_area_p))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#4D0055",fill="#4D0055",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#4D0055")+
  deets9+
  scale_y_continuous("Kelp Canopy\nProportion of Max. Extent", limits=c(-.1,1),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/kelp_gam_santacruz_qb.jpg",height=3,width=6)



# ==============================================
# MONTEREY ###########################
d1_m<-filter(d1,location=="Monterey Peninsula")
m1_m<-gam(kelp_area_p~s(year, bs = "cs"),data=d1_m,method="REML",family=quasibinomial)
summary(m1_m)


ggplot(data=d1_m,aes(x=year,y=kelp_area_p))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#019A95",fill="#019A95",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#019A95")+
  deets9+
  scale_y_continuous("Kelp Canopy\nProportion of Max. Extent", limits=c(-.1,1),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/kelp_gam_monterey_qb.jpg",height=3,width=6)



# BIG SUR #############################################
d1_b<-filter(d1,location=="Big Sur")
m1_b<-gam(kelp_area_p~s(year, bs = "cs"),data=d1_b,method="REML",family=quasibinomial)
summary(m1_b)

ggplot(data=d1_b,aes(x=year,y=kelp_area_p))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#A94824",fill="#A94824",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#A94824")+
  deets9+ #was deets5
  scale_y_continuous("Kelp Canopy\nProportion of Max. Extent", limits=c(-.1,1),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/kelp_gam_bigsur_qb.jpg",height=3,width=6)

