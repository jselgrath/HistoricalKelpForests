################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of urchin cover using all data sources
##################################################
library(tidyverse); library(dplyr);  library(ggplot2);library(RColorBrewer); library(colorspace); library(mgcv); library(tidymv);library(mgcViz)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/urchin_allsources_purple_abundance_location.csv")%>%filter(location!="Northern CA")
# d3<-read_csv("./results/urchin_allsources_purple_abundance_location3.csv")%>%filter(location3!="Northern CA")

unique(d1$location)


# setup ########################################
d1$Source<-d1$urchin_source; d1$Source[d1$Source=="Contemporary Data"]<-"Ecological Data"
# d3$Source<-d3$urchin_source; d3$Source[d3$Source=="Contemporary Data"]<-"Ecological Data"

# set location values
# d3$location<-d3$location3
# d3$location[d3$location=="Monterey Bay" | d3$location=="Monterey Outer" | d3$location=="Carmel" | d3$location=="Carmel Bay" |d3$location=="Monterey Unspecified"| d3$location=="Carmel Outer"] <- "Monterey Peninsula"
# d3$location[d3$location=="Big Sur"]<-"Big Sur"
# d3$location[d3$location=="Santa Cruz Outer"]<-"Santa Cruz"
# d3$location[d3$location=="Santa Cruz Bay"]<-"Santa Cruz"
# unique(d3$location)


d1$location<-as.factor(d1$location)
# d3$location3<-as.factor(d3$location3)
# d3$location<-as.factor(d3$location)


######################################################
# scale 0 to 1 for quasibinomial

# scale to 0-1
range(d1$urchin_abundance_u)
qplot(d1$urchin_abundance_u)

d1$urchin_abundance_u1<-d1$urchin_abundance_u

# set from 0-5 to 0-1 for modeling
d1$urchin_abundance_u<-d1$urchin_abundance_u*.2

range(d1$urchin_abundance_u1)
range(d1$urchin_abundance_u)
qplot(d1$urchin_abundance_u)



# BUILD GAM MODELS ###############################
# graph
source("./bin/deets.R")

# all sites
m1<-gam(urchin_abundance_u~
          s(year,by=(as.numeric(location=="Big Sur")))+
          s(year,by=(as.numeric(location=="Monterey Peninsula")))+
          s(year,by=(as.numeric(location=="Santa Cruz")))+
          location,data=d1,method="REML",family =quasibinomial) 
summary(m1)


######################################################
# The predicted values of the outcome variable are in the column fit, while fit.se reports the standard error of the predicted values.
# https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html

# graphing: https://socviz.co/makeplot.html
# https://mfasiolo.github.io/mgcViz/articles/mgcviz.html
#


#####################################################
# MODELS WITH YEAR ####################################
######################################################
# santa cruz: 4D0055
# montery: 019A95
# big sur: A94824
cols<- c("#4D0055","#019A95","#A94824")

######################################################
# Output 101: See Zuur p57

# edf = effective degrees of freedom - 0 - infinity. higher = more non-linear
# deviance explained = R2
# scale est = variance of residuals
######################################################

######################################################
# santacruz###########################
d1_s<-filter(d1,location=="Santa Cruz")
range(d1_s$year) #1975 2015

m1_s<-gam(urchin_abundance_u~s(year, bs = "cs"),data=d1_s,method="REML",family =quasibinomial)
summary(m1_s)

ggplot(data=d1_s,aes(x=year,y=urchin_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#4D0055",fill="#4D0055",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#4D0055")+
  deets9+
  scale_y_continuous("Purple Urchins\nRelative Abundance", limits=c(-.1,1),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/urchin_gam_santacruz_qb.jpg",height=3,width=6)

######################################################
# MONTEREY ###########################
d1_m<-filter(d1,location=="Monterey Peninsula")
m1_m<-gam(urchin_abundance_u~s(year, bs = "cs"),data=d1_m,method="REML",family =quasibinomial)
summary(m1_m)

ggplot(data=d1_m,aes(x=year,y=urchin_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#019A95",fill="#019A95",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#019A95")+
  deets9+
  scale_y_continuous("Purple Urchins\nRelative Abundance", limits=c(-.1,1),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/urchin_gam_monterey_qb.jpg",height=3,width=6)

# BIG SUR #############################################
d1_b<-filter(d1,location=="Big Sur")
range(d1_b$year)  #1959-2020

m1_b<-gam(urchin_abundance_u~s(year, bs = "cs",k=3),data=d1_b,msethod="REML",family =quasibinomial)
summary(m1_b)

ggplot(data=d1_b,aes(x=year,y=urchin_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=3),colour="#A94824",fill="#A94824",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#A94824")+
  deets9+
  scale_y_continuous("Purple Urchins\nRelative Abundance", limits=c(-.1,1),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/urchin_gam_bigsur_qb_k3.jpg",height=3,width=6)









