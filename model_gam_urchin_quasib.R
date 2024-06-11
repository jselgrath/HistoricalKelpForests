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
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/urchin_allsources_purple_abundance_location.csv")%>%
  filter(location!="Northern CA")%>%
  filter(year>=1820)%>%
  glimpse()


d2<-read_csv("./results/urchin_hoh_purple_abundance_location.csv")%>%
  filter(is.na(abundance_u))%>%
  select(year,location,present)%>%
  glimpse()

d3<-read_csv("./data/urchins_historical_20240308b.csv")%>% # urchins_historical_20230612
  filter(data_type=="PO"&location!="Other")%>%
  select(year,location,present)%>%
  glimpse() 

d4<-d2%>%
  full_join(d3)%>%
  filter(year>1820)%>%
  mutate(present=1)%>%
  glimpse()

unique(d4$location) #using California for all sites

# setup ########################################
d1$Source<-d1$urchin_source; d1$Source[d1$Source=="Contemporary Data"]<-"Ecological Data"
d1$location<-as.factor(d1$location)


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
d4_s<-d4%>%filter(location=="Northern California")%>%
  select(-location)
d1_s<-filter(d1,location=="Santa Cruz")%>%
  full_join(d4_s)%>%
  glimpse()
d1_s2<-filter(d1_s,!is.na(urchin_abundance_u))

range(d1_s$year) #1975 2015

# model
m1_s<-gam(urchin_abundance_u~s(year, bs = "cs",k=6),data=d1_s2,method="REML",family =quasibinomial)
summary(m1_s)
anova(m1_s)

#graph
ggplot(data=d1_s,aes(x=year,y=urchin_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=6),colour="#4D0055",fill="#4D0055",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#4D0055")+
  geom_point(aes(y=urchin_abundance_u),shape = 19, size = 2, alpha = 0.5,colour= "#4D0055")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+
  scale_y_continuous("Purple Urchins\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_g_urchin_gam_santacruz_qb_k6.jpg",height=3,width=6)

######################################################
# MONTEREY ###########################
d4_m<-d4%>%
  select(-location)

d1_m<-filter(d1,location=="Monterey Peninsula")%>%
  full_join(d4_m)%>%
  glimpse()
d1_m2<-filter(d1_m,!is.na(urchin_abundance_u))

# model
m1_m<-gam(urchin_abundance_u~s(year, bs = "cs"),data=d1_m2,method="REML",family =quasibinomial)
summary(m1_m)
anova(m1_m)

#graph
ggplot(data=d1_m,aes(x=year,y=urchin_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#019A95",fill="#019A95",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#019A95")+
  geom_point(aes(y=urchin_abundance_u),shape = 19, size = 2, alpha = 0.5,colour= "#019A95")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+
  scale_y_continuous("Purple Urchins\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_h_urchin_gam_monterey_qb.jpg",height=3,width=6)

# BIG SUR #############################################
d4_b<-d4%>%filter(location=="Northern California")%>%
  select(-location)
d1_b<-filter(d1,location=="Big Sur")%>%
  full_join(d4_b)%>%
  glimpse()
d1_b2<-filter(d1_b,!is.na(urchin_abundance_u))

range(d1_b$year)  #1959-2020

#model
m1_b<-gam(urchin_abundance_u~s(year, bs = "cs",k=4),data=d1_b2,method="REML",family =quasibinomial)
summary(m1_b)
anova(m1_b)

# graph
ggplot(data=d1_b,aes(x=year,y=urchin_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=4),colour="#A94824",fill="#A94824",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#A94824")+
  geom_point(aes(y=urchin_abundance_u),shape = 19, size = 2, alpha = 0.5,colour= "#A94824")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+
  scale_y_continuous("Purple Urchins\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_i_urchin_gam_bigsur_qb_k4.jpg",height=3,width=6)






# Calc stats for table
d1_s%>%select(!present)%>%filter(!is.na(urchin_abundance_u))%>%group_by(location)%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()
d1_m%>%select(!present)%>%filter(!is.na(urchin_abundance_u))%>%group_by(location)%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()
d1_b%>%select(!present)%>%filter(!is.na(urchin_abundance_u))%>%group_by(location)%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()



