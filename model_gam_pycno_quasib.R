################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

# June 3, 2020 (Covid, Antirascism Protests)
##################################################
# Goal: to graph estimates of pycno cover using historical and oral history data
# using quasibinomial distribution
##################################################
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace); library(mgcv); library(tidymv);library(mgcViz) ; library(patchwork) 

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/pycno_allsources_abundance_location.csv")%>%
  glimpse()

d2<-read_csv("./data/pycnopodia_historical_long_20240308.csv")%>% #pycnopodia_historical_long_20230612b
  filter(data_type=="PO"&location!="Other")%>%
  select(year,location,present)%>%
  glimpse() 

d3<-read_csv("./results/pycno_hoh_abundance_location1.csv")%>%
  filter(is.na(abundance_u))%>%
  mutate(present=1)%>%
  select(year,location,present)%>%
  glimpse()

d4<-d2%>%
  full_join(d3)%>%
  filter(year>=1820)%>%
  mutate(present=1)%>%
  glimpse()

unique(d4$location) #using California for all sites



# setup ########################################
d1$Source<-d1$source; d1$Source[d1$Source=="Historical Desncriptions"]<-"Archival Data"
d1$location<-as.factor(d1$location)


######################################################
# scale 0 to 1 for quasibinomial

# scale to 0-1
range(d1$abundance_u)
qplot(d1$abundance_u)

d1$abundance_u1<-d1$abundance_u
d1$abundance_u<-d1$abundance_u*.2
range(d1$abundance_u1)
range(d1$abundance_u)
qplot(d1$abundance_u)



# BUILD GAM MODELS ###############################
# graph
source("./bin/deets.R")

# all sites. significant interactions
m1<-gam(abundance_u~
          s(year,by=(as.numeric(location=="Big Sur")))+
          s(year,by=(as.numeric(location=="Monterey Peninsula")))+
          s(year,by=(as.numeric(location=="Santa Cruz")))+
          location,data=d1,method="REML",family = quasibinomial) #, family=quasipoisson)
summary(m1)
anova(m1)
# gam.check(m1)


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
d4_s<-d4%>%filter(location=="California")%>%
  select(-location)
d1_s2<-filter(d1,location=="Santa Cruz")%>%
  filter(year!=2000)%>% #remove outlier
  glimpse()
d1_s<-d1_s2%>%
  full_join(d4_s)%>%
  glimpse()
  
m1_s<-gam(abundance_u~s(year, bs = "cs",k=7),data=d1_s2,method="REML",family = quasibinomial())
summary(m1_s)
anova(m1_s)
gam.check(m1_s)
# plot(m1_b)

#includes negative values because normal distribution?
m1_s_p <- predict_gam(m1_s)
m1_s_p

ggplot(data=d1_s,aes(x=year,y=abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=7),colour="#4D0055",fill="#4D0055",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#4D0055")+
  geom_point(aes(y=present+0.05,x=year),shape = 25, size = 3, alpha = 0.6,fill="#969696")+ 
  deets9+
  scale_y_continuous("Sunflower Stars\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_d_pycno_gam_santacruz_qb.jpg",height=3,width=6)




######################################################
# MONTEREY ###########################
d4_m<-d4%>%
  select(-location)

d1_m<-filter(d1,location=="Monterey Peninsula")%>%
  full_join(d4_m)%>%
  glimpse()
d1_m2<-filter(d1_m,!is.na(abundance_u))

m1_m<-gam(abundance_u~s(year, bs = "cs",k=7),data=d1_m2,method="REML",family = quasibinomial)
summary(m1_m)
anova(m1_m)
gam.check(m1_m)
# plot(m1_m)

m1_m_p <- predict_gam(m1_m)
m1_m_p

sequential_hcl(3,palette="Grays")

# graph includes presence only data --------------------------------------
ggplot(data=d1_m,aes(x=year,y=abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=7),colour="#019A95",fill="#019A95",method.args = list(family = "quasibinomial"))+
  geom_point(aes(y=abundance_u),shape = 19, size = 2, alpha = 0.5,colour="#019A95")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+ #,color="#019A95"
  deets9+
  scale_y_continuous("Sunflower Stars\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_e_pycno_gam_monterey_qb_k7_b.jpg",height=3,width=6)



# BIG SUR #############################################
d4_b<-d4%>%filter(location=="California")%>%
  select(-location)
d1_b<-filter(d1,location=="Big Sur")%>%
  full_join(d4_b)%>%
  glimpse()
d1_b2<-filter(d1_b,!is.na(abundance_u))


m1_b<-gam(abundance_u~s(year, bs = "cs",k=4),data=d1_b2,method="REML",family = quasibinomial)
summary(m1_b)
anova(m1_b)
gam.check(m1_b)
plot(m1_b)


ggplot(data=d1_b,aes(x=year,y=abundance_u))+
  geom_hline(yintercept=0,color="lightgrey")+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs",k=4),colour="#A94824",fill="#A94824",method.args = list(family = "quasibinomial"))+
  geom_point(shape = 19, size = 2, alpha = 0.5,colour="#A94824")+
  geom_point(aes(y=present+0.05),shape = 25, size = 3, alpha = 0.6,fill="#969696")+ 
  deets9+
  scale_y_continuous("Sunflower Stars\nRelative Abundance", limits=c(-.1,1.05),breaks=c(0,.2,.4,.6,.8,1))+
  scale_x_continuous("Year", limits=c(1820,2020),breaks=c(1820,1860,1900,1940,1980,2020))
ggsave("./doc/fig2_f_pycno_gam_bigsur_qb_k4_b.jpg",height=3,width=6)


# Calc stats for table
# d1_s%>%select(!present)%>%filter(!is.na(abundance_u))%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()
# d1_m%>%select(!present)%>%filter(!is.na(abundance_u))%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()
# d1_b%>%select(!present)%>%filter(!is.na(abundance_u))%>%summarize(n=n(),yr_1=min(year),year_2=max(year))%>%glimpse()
# 
# range(d1_s$year)
# range(d1_m$year)
# range(d1_b$year)
