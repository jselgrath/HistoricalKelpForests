# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to combine contemporary pyncnopodia sources
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(dplyr)  

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# REEF CHECK AND PISCO DATA
d1<- read_csv("./results/pycno_RC_density_transect.csv")%>%         #has all location data in it
  dplyr::select(year,organism,location,location3, density_m2,source)
d2<- read_csv("./results/pycno_PISCO_density_transect.csv")%>%         #has all location data in it, only 1 SC survey
  dplyr::select(year,organism,location,location3, density_m2,source)      

d3<-rbind(d1,d2)
d3


##############################
# summarize contemporary PISCO data
# grand mean = multiply mean by number of data points

# summarize mean values by site and year
# see digital p202 of cochranes handbook for systematic reviews of intervientions
# Combining PISCO and ReefCheck Data. Use Program to separate
# summarized by location3

# Summarized by location #############
d4<- d3%>%  
  group_by(year,organism,location, source)%>%
  dplyr::summarise(
    density_u=sum(density_m2)/n(),     #grand mean
    density_sd=sd(density_m2),
    density_n=length(year))%>%
  arrange(year,location)

d4
write_csv(d4,"./results/pycno_contemporary_density_location.csv")


#stats - source = reference here
d4a<-d4%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  glimpse
write_csv(d4a,"./doc/sampsz_pycno_contemp_all_location.csv")

# summarized by location3
d6<- d3%>%  
  group_by(year,location3,organism, source)%>%
  dplyr::summarise(
    density_u=sum(density_m2)/n(),     #grand mean
    density_sd=sd(density_m2),
    density_n=length(year))%>%
  arrange(year,location3)
d6
write_csv(d6,"./results/pycno_contemporary_density_location3.csv")

# summarized by all sites, by year
d7<- d3%>%  
  group_by(year,organism,source)%>%
  dplyr::summarise(
    density_u=sum(density_m2)/n(),     #grand mean
    density_sd=sd(density_m2),
    density_n=length(year))%>%
  arrange(year)
d7

write_csv(d7,"./results/pycno_contemporary_density_allsites.csv")

# SUMMARIZED FOR ALL SITES AND YEARS - for calculating relative density#############
d8<- d3%>%  
  group_by(organism,source)%>%
  dplyr::summarise(
    density_u=sum(density_m2)/n(),     #grand mean
    density_sd=sd(density_m2),
    density_n=length(year))
d8$years<-"all"

d8
write_csv(d8,"./results/pycno_contemporary_density_allsitesyears.csv")


#through 2013 and SSWD
d9<- d3%>%  
  filter(year<=2013)%>%
  group_by(organism,source)%>%
  dplyr::summarise(
    density_u=sum(density_m2)/n(),     #grand mean
    density_sd=sd(density_m2),
    density_n=length(year))
d9$years<-"pre-SSWD"

d9
write_csv(d9,"./doc/pycno_contemporary_density_pre-SSWD.csv")


#post 2013 and SSWD 
d10<- d3%>%  
  filter(year>2013)%>%
  group_by(organism,source)%>%
  dplyr::summarise(
    density_u=sum(density_m2)/n(),     #grand mean
    density_sd=sd(density_m2),
    density_n=length(year))
d10$years<-"post-SSWD"

d10
write_csv(d10,"./doc/pycno_contemporary_density_post-SSWD.csv")


#=========================================================
# CALCULATE RELATIVE ABUNDANCE ##################################
source("./bin/deets.R")
ggplot(d6,aes(x=year,y=density_u,color=organism))+geom_line()+deets3+facet_grid(rows=vars(location3))

ggplot(d6,aes(x=year,y=density_u,color=organism))+geom_point()+deets3


# assign relative abundance values ##############
range(d4$density_u) #0.00000000 0.05156495
range(d6$density_u) #0.00000000 0.06741071


# d8 = density for all sites


# set breaks in abundance based on standard deviations, using pre-SSWD values ################
d0p<-d8
dens_0<-0
dens_1<-(d0p$density_u-(2*d0p$density_sd));dens_1
dens_2<-d0p$density_u-d0p$density_sd;dens_2
dens_3<-d0p$density_u;dens_3
dens_4<-d0p$density_u+d0p$density_sd;dens_4
dens_5<-d0p$density_u+(2*d0p$density_sd);dens_5

# since dens_1 and dens_2 are both negative, set 1 at mean
dens_1a<-d0p$density_u;dens_1

# since dens_1 and dens_2 are both negative, set 2 half way between median (0.98) and 1 sd
dens_2a<-d0p$density_u+(0.5*d0p$density_sd);dens_2

dens_breaks<-c(dens_0,dens_1a,dens_2a,dens_4,dens_5)
dens_breaks


# set abundance ######################
# location
d3_1<-d4 #d3 = combined RC and PISCO densities
d3_1$abundance_u<-9999
d3_1$abundance_u[d3_1$density_u==dens_breaks[1]]<-0
d3_1$abundance_u[d3_1$density_u>dens_breaks[1]]<-1 # greater than 0
d3_1$abundance_u[d3_1$density_u>=dens_breaks[2]]<-2 # between 0 and mean
d3_1$abundance_u[d3_1$density_u>=dens_breaks[3]]<-3 #halfway between mean and 1 sd
d3_1$abundance_u[d3_1$density_u>=dens_breaks[4]]<-4
d3_1$abundance_u[d3_1$density_u>=dens_breaks[5]]<-5
d3_1
tail(d3_1)

ggplot(d3_1,aes(x=year,y=abundance_u,color=location))+geom_smooth()+facet_grid(rows=vars(organism))+deets3
ggplot(d3_1,aes(x=year,y=density_u,color=location))+geom_smooth()+facet_grid(rows=vars(organism))+deets3


#location3
d3_3<-d6
d3_3$abundance_u<-9999
d3_3$abundance_u[d3_3$density_u==dens_breaks[1]]<-0
d3_3$abundance_u[d3_3$density_u>dens_breaks[1]]<-1 # greater than 0
d3_3$abundance_u[d3_3$density_u>=dens_breaks[2]]<-2 # between 0 and mean
d3_3$abundance_u[d3_3$density_u>=dens_breaks[3]]<-3 #halfway between mean and 1 sd
d3_3$abundance_u[d3_3$density_u>=dens_breaks[4]]<-4
d3_3$abundance_u[d3_3$density_u>=dens_breaks[5]]<-5
d3_3
tail(d3_3)


ggplot(d3_3,aes(x=year,y=abundance_u,color=location3))+geom_smooth()+facet_grid(rows=vars(organism))+deets3
ggplot(d3_3,aes(x=year,y=density_u,color=location3))+geom_smooth()+facet_grid(rows=vars(organism))+deets3

write_csv(d3_1,"./results/pycno_contemporary_abundance_location.csv")
write_csv(d3_3,"./results/pycno_contemporary_abundance_location3.csv")

