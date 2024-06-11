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

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/kelp_map_allsources_area_loc1_2024.csv")%>% 
  mutate(location=as.factor(location))%>%
  glimpse()

# data for presence observations - new June 2023
d3a<-read_csv("./results/kelp_allsources_abundance_location.csv")%>%
  filter(kelp_source!="Archival Maps")%>%
  select(year,location,present)%>%
  filter(location!="California")%>%
  arrange(year)%>%
  glimpse()
unique(d3a$kelp_source)
unique(d3a$location)

yr<-c(1852, "Monterey Peninsula", 1) # removed map so adding in here

d3<-rbind(d3a,yr)%>%
  mutate(year=as.numeric(year), present=as.numeric(present))%>%arrange(year)%>%glimpse()

d4<-d3%>%
  filter(year>1820)%>%
  mutate(present=0.7)%>% # for graphing
  glimpse()
unique(d4$location)
unique(d4$present)

d4%>%
  filter(location=="Big Sur")%>%
  arrange(year)

d4%>%
  filter(location=="Santa Cruz")%>%
  arrange(year)

# calc means for paper
d1%>%
  group_by(location,kelp_source)%>%
  summarize(u=mean(kelp_area_p,na.rm=T),
            sd=sd(kelp_area_p,na.rm=T))

# setup ########################################
d1$Source<-d1$kelp_source; d1$Source[d1$Source=="Contemporary Data"]<-"Ecological Data"
range(d1$kelp_maxarea_p, na.rm=T)
range(d1$kelp_area_p, na.rm=T)

# merge area and presence data
names(d1)
names(d4)


# BUILD GAM MODELS ###############################
# graph
source("./bin/deets.R")

# all sites
m1<-gam(kelp_area_p~ # kelp_maxarea_p~
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

#presence data
d4_s<-d4%>%filter(location=="Santa Cruz")%>%
  select(-location)%>%
  arrange(year)%>%
  glimpse()

# abundance data
d1_s<-filter(d1,location=="Santa Cruz")%>%
  full_join(d4_s)%>%
  unique()%>%
  glimpse()
d1_s2<-filter(d1_s,!is.na(kelp_area_p ))

range(d1_s$year) 
m1_s<-gam(kelp_area_p ~s(year, bs = "cs",k=5),data=d1_s2,method="REML",family=quasibinomial)
summary(m1_s)
anova(m1_s)



# graph
ggplot(data=d1_s,aes(x=year,y=kelp_area_p ))+ # kelp_maxarea_p  OR kelp_area_p
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=5),colour="#4D0055",fill="#4D0055",method.args = list(family = "quasibinomial"))+
  # geom_point(shape = 19, size = 2, alpha = 0.5,colour="#4D0055")+
  geom_point(aes(y=kelp_area_p ),shape = 19, size = 2, alpha = 0.5,colour= "#4D0055")+
  geom_point(aes(y=present),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+
  scale_y_continuous("Kelp Canopy\nProportion of Max. Area", limits=c(-.1,0.7),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_j_kelp_gam_santacruz_qb_b_k5.jpg",height=3,width=6)



# ==============================================
# MONTEREY ###########################
d4_m<-d4%>%
  filter(location=="Monterey Peninsula")%>%
  select(-location)%>%
  arrange(year)%>%
  glimpse() # presence data

d1_m<-filter(d1,location=="Monterey Peninsula")%>% # does not include 1852 map FYI because of different method
  # filter(year>1870)%>% # early years with very low kelp cover - I hypothesize due to ranching
  filter(year!=1989)%>% # remove 1 outlier  # kelp_area_p=0.509139
    full_join(d4_m)%>%
  unique()%>%
  glimpse()

unique(d1_m$location)

d1_m2<-filter(d1_m,!is.na(kelp_area_p )) # for model

range(d1_m$year) 
m1_m<-gam(kelp_area_p ~s(year, bs = "cs"),data=d1_m2,method="REML",family=quasibinomial)
summary(m1_m)
anova(m1_m)

ggplot(data=d1_m,aes(x=year,y=kelp_area_p))+ # kelp_area_p   OR kelp_area_p
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#019A95",fill="#019A95",method.args = list(family = "quasibinomial"))+
  geom_point(aes(y=kelp_area_p),shape = 19, size = 2, alpha = 0.5,colour= "#019A95")+
  geom_point(aes(y=present),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  geom_point(x=1989, y=0.509139,shape = 19, size = 1, alpha = 0.5,colour="#019A95")+ # outlier, not included in the model
  deets9+
  scale_y_continuous("Kelp Canopy\nProportion of Max. Area", limits=c(-.1,0.7),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_k_kelp_gam_monterey_qb2_b.jpg",height=3,width=6)


# BIG SUR #############################################
d4_b<-d4%>%filter(location!="Monterey Peninsula"&location!="Santa Cruz")%>%
  # select(-location)%>%
  glimpse()
range(d4_b$present)

d1_b<-filter(d1,location=="Big Sur")%>%
  # select(-present)%>%
  full_join(d4_b)%>%
  unique()%>% # remove duplicate entries
  glimpse()
range(d1_b$present)

d1_b2<-filter(d1_b,!is.na(kelp_area_p ))%>%arrange(year) # for model

range(d1_b$year) 
m1_b<-gam(kelp_area_p ~s(year, bs = "cs"),data=d1_b2,method="REML",family=quasibinomial)
summary(m1_b)
anova(m1_b)

ggplot(data=d1_b,aes(x=year,y=kelp_area_p ))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#A94824",fill="#A94824",method.args = list(family = "quasibinomial"))+
  # geom_point(shape = 19, size = 2, alpha = 0.5,colour="#A94824")+
  geom_point(aes(y=kelp_area_p ),shape = 19, size = 2, alpha = 0.5,colour= "#A94824")+
  geom_point(aes(y=present,x=year),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+ #was deets5
  scale_y_continuous("Kelp Canopy\nProportion of Max. Area", limits=c(-.1,0.7),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_l_kelp_gam_bigsur_qb2_b.jpg",height=3,width=6)



# Calc stats for table
d1_s%>%select(!present)%>%filter(!is.na(kelp_area_p ))%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()
d1_m%>%select(!present)%>%filter(!is.na(kelp_area_p ))%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()
d1_b%>%select(!present)%>%filter(!is.na(kelp_area_p ))%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()

range(d1_s$year)
range(d1_m$year)
range(d1_b$year)
