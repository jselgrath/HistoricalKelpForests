# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University


##################################################
# Goal: to combine PISCO urchin data and micheli data for urchins # this is with the updated data 2020.. .does not include reef check data. Not Yet written
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  

################################################
rm(list=ls())
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")

# mean and sd for all data

# reef check
d1<- read_csv("./results/urchin_RC_density_transect.csv")%>%         #has all location data in it
  filter(genus=="Strongylocentrotus")%>%
  select(year,organism,genus,location,location3, density_m2,source)
d1

# PISCO
d2<- read_csv("./results/urchin_PISCO_location123_bytransect.csv")%>%         #has all location data in it, only 1 SC survey
  filter(genus=="Strongylocentrotus")%>%
  select(year,organism,genus,location,location3, density_m2)%>%
  mutate(source="Contemporary Data")
d2

# Micheli data
d3<-read_csv("./results/urchin_micheli_density_transect.csv")%>%        
  filter(genus=="Strongylocentrotus")%>%
  mutate(source="Contemporary Data")%>%
  select(year,organism,genus,location,location3, density_m2,source) 

d3


# Combine #############
d4<-rbind(d1,d2,d3)%>%
  arrange(year,location)
d4$organism<-"purple urchin"
d4

unique(d4$organism)

write_csv(d4,"./results/purpleurchin_contemporary_density_transect.csv")


################################################################
# Summarized by location #############
d5<- d4%>%  
  group_by(year,location,organism,source,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))%>% 
  arrange(year,location)
d5
write_csv(d5,"./results/urchin_contemporary_density_location.csv")

# graph
source("./bin/deets.R")

ggplot(d5,aes(x=year, y=density_m2_u)) + geom_point()+
  facet_grid(rows=vars(location))+deets4

# summarized by location3
d6<- d4%>%  
  group_by(year,location3,organism,source,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))%>% 
  arrange(year,location3)

ggplot(d6,aes(x=year, y=density_m2_u)) + geom_point()+
  facet_grid(rows=vars(location3))+deets4

d6
write_csv(d6,"./results/urchin_contemporary_density_location3.csv")

# summarized by all sites, by year
d7<- d4%>%  
  group_by(year,organism,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))%>% 
  arrange(year)

d7
write_csv(d7,"./results/urchin_contemporary_density_allsites.csv")

# summarized by all sites & years
d8<- d4%>%  
  group_by(organism,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))

d8
write_csv(d8,"./results/urchin_contemporary_density_allsitesyears.csv")




#################################################################
# CALCULATE RELATIVE ABUNDANCE ##################################
source("./bin/deets.R")
ggplot(d6,aes(x=year,y=density_m2_u,color=genus))+geom_line()+deets3+facet_grid(rows=vars(location3))

ggplot(d5,aes(x=year,y=density_m2_u,color=genus))+geom_point()+deets3+facet_grid(rows=vars(location))
ggplot(d7,aes(x=year,y=density_m2_u,color=genus))+geom_point()+deets3

# assign relative abundance values ##############
range(d5$density_m2_u) #0.00000 24.00658 #location1
range(d6$density_m2_u) #0.00000 31.08819 #location3


# d8 = density for all sites

# set breaks in abundance based on standard deviations ################
d0p<-filter(d8,genus=="Strongylocentrotus")
dens_0<-0
dens_1<-(d0p$density_m2_u-(2*d0p$density_m2_sd));dens_1
dens_2<-d0p$density_m2_u-d0p$density_m2_sd;dens_2
dens_3<-d0p$density_m2_u;dens_3
dens_4<-d0p$density_m2_u+d0p$density_m2_sd;dens_4
dens_5<-d0p$density_m2_u+(2*d0p$density_m2_sd);dens_5

# since dens_1 and dens_2 are both negative, set 1 at mean
dens_1a<-d0p$density_m2_u;dens_1a

# since dens_1 and dens_2 are both negative, set 2 half way between mean  and 1 sd
dens_2a<-d0p$density_m2_u+(0.5*d0p$density_m2_sd);dens_2

dens_breaks<-c(dens_0,dens_1a,dens_2a,dens_4,dens_5)
dens_breaks


# set abundance ######################
# location
d9<-d5
d9$abundance_u<-9999
d9$abundance_u[d9$density_m2_u==dens_breaks[1]]<-0
d9$abundance_u[d9$density_m2_u>dens_breaks[1]]<-1 # greater than 0
d9$abundance_u[d9$density_m2_u>=dens_breaks[2]]<-2 # between 0 and mean
d9$abundance_u[d9$density_m2_u>=dens_breaks[3]]<-3 #halfway between mean and 1 sd
d9$abundance_u[d9$density_m2_u>=dens_breaks[4]]<-4
d9$abundance_u[d9$density_m2_u>=dens_breaks[5]]<-5
d9
tail(d9)

ggplot(d9,aes(x=year,y=density_m2_u,color=location))+geom_smooth()+facet_grid(rows=vars(genus))+deets3



#location3
d10<-d6
d10$abundance_u<-9999
d10$abundance_u[d10$density_m2_u==dens_breaks[1]]<-0
d10$abundance_u[d10$density_m2_u>dens_breaks[1]]<-1 # greater than 0
d10$abundance_u[d10$density_m2_u>=dens_breaks[2]]<-2 # between 0 and mean
d10$abundance_u[d10$density_m2_u>=dens_breaks[3]]<-3 #halfway between median and 1 sd
d10$abundance_u[d10$density_m2_u>=dens_breaks[4]]<-4
d10$abundance_u[d10$density_m2_u>=dens_breaks[5]]<-5
d10
tail(d10)


ggplot(d10,aes(x=year,y=abundance_u,color=location3))+geom_smooth()+facet_grid(rows=vars(genus))+deets3
ggplot(d10,aes(x=year,y=density_m2_u,color=location3))+geom_smooth()+facet_grid(rows=vars(genus))+deets3

write_csv(d9,"./results/urchin_contemporary_abundance_location.csv")
write_csv(d10,"./results/urchin_contemporary_abundance_location3.csv")
