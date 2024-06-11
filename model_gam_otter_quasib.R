################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of otter densities using all data sources

# Note: otter data is relative abudance on a 0-1 scale
# Ch5 in Zurr deals with GAMs, ch7 with irregularily spaced data
# note correlation structures did not work. long period with zero abundance
##################################################
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace); library(mgcv); library(tidymv);library(mgcViz);  

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")


d1<-read_csv("./results/otter_allsources_abundance_location.csv")%>%
  filter(year>=1820)%>%
  glimpse()

d2<-read_csv("./data/otter_historical_long_20230702.csv")%>%
  filter(data_type=="PO"&location!="Other")%>%
  select(year,location,present)%>%
  mutate(year=as.numeric(year))%>%
  filter(year>=1820)%>%
  mutate(present=1)%>%
  glimpse() 

d3<-read_csv("./results/otter_hoh_abundance_location.csv")%>%
  filter(is.na(abundance_u))%>%
  mutate(present=1)%>%
  select(year,location,present)%>%
  glimpse()

d4<-d2%>%
  full_join(d3)%>%
  filter(year>1820)%>%
  mutate(present=1)%>%
  glimpse()

unique(d4$location) #using California for all sites



# setup ########################################
d1$Source<-d1$otter_source; d1$Source[d1$Source=="Contemporary Data"]<-"Ecological Data"
d1$location<-as.factor(d1$location)


# BUILD GAM MODELS ###############################
# graph
source("./bin/deets.R")

# all sites
qplot(d1$otter_abundance_u)

m1<-gam(otter_abundance_u~
          s(year,by=(as.numeric(location=="Big Sur")))+
          s(year,by=(as.numeric(location=="Monterey Peninsula")))+
          s(year,by=(as.numeric(location=="Santa Cruz")))+
          location,data=d1,method="REML")
summary(m1) # interactions etc

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

# scale to 0-1
range(d1$otter_abundance_u)
qplot(d1$otter_abundance_u)

d1$otter_abundance_u1<-d1$otter_abundance_u
d1$otter_abundance_u<-d1$otter_abundance_u*.2
range(d1$otter_abundance_u1)
range(d1$otter_abundance_u)
qplot(d1$otter_abundance_u)

# santacruz###########################
d4_s<-d4%>%filter(location=="California"|location=="Santa Cruz")%>%
  filter(!is.na(year))%>%
  select(-location)%>%
  glimpse()
d1_s<-filter(d1,location=="Santa Cruz")%>%
  full_join(d4_s)%>%
  glimpse()
d1_s2<-filter(d1_s,!is.na(otter_abundance_u))


range(d1_s$year) 
m1_s<-gam(otter_abundance_u~s(year, bs = "cs",k=7),data=d1_s2,method="REML", family=quasibinomial)
summary(m1_s)
anova(m1_s)

ggplot(data=d1_s,aes(x=year,y=otter_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=7),colour="#4D0055",fill="#4D0055",method.args = list(family = "quasibinomial"))+
   geom_point(shape = 19, size = 2, alpha = 0.5,colour="#4D0055",shape=1)+
  geom_point(aes(y=otter_abundance_u),shape = 19, size = 2, alpha = 0.5,colour= "#4D0055")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+
  scale_y_continuous("Sea Otters\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))

ggsave("./doc/fig2_a_otter_gam_santacruz_qb_k7.jpg",height=3,width=6)


#=========================================
# MONTEREY ###########################
d4_m<-d4%>%
  filter(location=="California"|location=="Monterey Peninsula")%>%
  filter(!is.na(year))%>%
  select(-location)%>%
  mutate(present=1)%>%
  glimpse()

d1_m<-filter(d1,location=="Monterey Peninsula")%>%
  full_join(d4_m)%>%
  glimpse()
d1_m2<-filter(d1_m,!is.na(otter_abundance_u))


# d1_m<-filter(d1,location=="Monterey Peninsula")
range(d1_m$year) 
m1_m<-gam(otter_abundance_u~s(year, bs = "cs",k=4),data=d1_m2,method="REML", family=quasibinomial())
summary(m1_m)
anova(m1_m)

ggplot(data=d1_m,aes(x=year,y=otter_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=4),colour="#019A95",fill="#019A95",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#019A95")+
  geom_point(aes(y=otter_abundance_u),shape = 19, size = 2, alpha = 0.5,colour= "#019A95")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+
  scale_y_continuous("Sea Otters\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2021),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_b_otter_gam_monterey_qb_k4.jpg",height=3,width=6)



# BIG SUR #############################################
d4_b<-d4%>%filter(location=="California"| location=="Big Sur")%>%
  select(-location)
d1_b<-filter(d1,location=="Big Sur")%>%
  full_join(d4_b)%>%
  glimpse()
d1_b2<-filter(d1_b,!is.na(otter_abundance_u))

range(d1_b$year) 
m1_b<-gam(otter_abundance_u~s(year, bs = "cs",k=7),data=d1_b2,method="REML", family=quasibinomial)
summary(m1_b)
anova(m1_b)
plot(m1_b)

m1_b_p <- predict_gam(m1_b)
m1_b_p

ggplot(data=d1_b,aes(x=year,y=otter_abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=7),colour="#A94824",fill="#A94824",method.args = list(family = "quasibinomial"))+
  # geom_point(shape = 19, size = 2, alpha = 0.5,colour="#A94824")+
  geom_point(aes(y=otter_abundance_u),shape = 19, size = 2, alpha = 0.5,colour= "#A94824")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+
  deets9+
  scale_y_continuous("Sea Otters\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_c_otter_gam_bigsur_qb_k7.jpg",height=3,width=6)



