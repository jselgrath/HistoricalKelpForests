# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: organize PISCO pycno data
# originaldata = MLPA_kelpforest_swath.4.csv
# subset location in pycno_PISCO_latlong.R file
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

source("./bin/deets.R")

# summarized PISCO data
# count is pycno per 60m2 transect
##########################################################

d1<-read_csv("./results/pycno_PISCO_location123.csv")%>% #has all location data in it
  mutate(density_m2=count/60)%>% #core RC transects are 30x2
  dplyr::select(year,organism=species,location,location2,location3, density_m2)%>%
  arrange(year,location3)
d1$source<-"Contemporary Data"
d1
write_csv(d1,"./results/pycno_PISCO_density_transect.csv")


# # checking with data that was not selected to drop site/count
#check all replicates are present (ie not missing 0)
range(d1$density_m2)



# unique(d1$site)
# d1%>%filter(site=="TERRACE_UC"| site=="TERRACE_DC" | site=="SANDHILL_UC" |site=="SANDHILL_DC")%>%filter(year==2000)

  
  
##############################
# summarize PISCO reef data
# calculate mean for all sites within a location 

# SUMMARIZED BY LOCATION#############
d3<- d1%>%  
  dplyr::select(year,organism,location,source,density_m2)%>%
  group_by(year,organism,location,source)%>%
  summarize(
    density_n=length(density_m2), # sample size for all locations
    density_m2_u=sum(density_m2)/density_n,     #grand mean
    density_m2_sd=sd(density_m2)
    )%>%
  arrange(year,location)
d3
range(d3$density_n)
d3$density_marginerror<-qnorm(.975)*(d3$density_m2_u/sqrt(d3$density_n))
d3$density_sem<-d3$density_m2_sd/sqrt(d3$density_n)
  # qnorm(.975)*(15000/sqrt(10))

range(d3$density_m2_u)
range(d3$density_marginerror)

ggplot(data=d3,aes(x=year,y=density_m2_u, color=location))+ 
  geom_ribbon(aes(ymin=((density_m2_u-density_marginerror)), ymax=((density_m2_u+density_marginerror)), alpha=0.05))+
  geom_line()+facet_grid(rows=vars(location))

ggplot(d3,aes(x=year, y=density_m2_u,color=density_n)) + geom_point()+
  facet_grid(rows=vars(location))+deets3

write_csv(d3,"./results/pycno_PISCO_density_location.csv")

# graph
source("./bin/deets.R")

ggplot(d3,aes(x=year, y=density_m2_u)) + geom_point()+
  facet_grid(rows=vars(location))+deets4


# SUMMARIZED BY LOCATION3 #############
d5<- d1%>%  
  dplyr::select(year,organism,location3,source,density_m2)%>%
  group_by(year,organism,location3,source)%>%
  summarize(
    density_n=length(density_m2), # sample size for all locations
    density_m2_u=sum(density_m2)/density_n,     #grand mean
    density_m2_sd=sd(density_m2)
  )%>%
  arrange(year,location3)
range(d5$density_n)
d5$density_marginerror<-qnorm(.975)*(d5$density_m2_u/sqrt(d5$density_n))
d5$density_sem<-d5$density_m2_sd/sqrt(d5$density_n)

range(d5$density_m2_u)
range(d5$density_m2_sd)
range(d5$density_marginerror)

ggplot(data=d5,aes(x=year,y=density_m2_u, color=location3))+ 
  geom_ribbon(aes(ymin=((density_m2_u-density_marginerror)), ymax=((density_m2_u+density_marginerror)), alpha=0.05))+
  geom_line()+facet_grid(rows=vars(location3))

write_csv(d5,"./results/pycno_PISCO_density_location3.csv")



# SUMMARIZED FOR ALL SITES AND YEARS - for calculating relative density#############
# NOTE - using similar values calculated for PISCO and Reef Check
d6<- d1%>%  
  dplyr::select(organism,source,density_m2)%>%
  summarize(
    density_n=length(density_m2), # sample size for all locations
    density_m2_u=sum(density_m2)/density_n,     #grand mean
    density_m2_sd=sd(density_m2))

d6$density_marginerror<-qnorm(.975)*(d6$density_m2_u/sqrt(d6$density_n))
d6$density_sem<-d6$density_m2_sd/sqrt(d6$density_n)
d6$years<-"all"
d6

#through 2013 and SSWD
d7<- d1%>%  
  filter(year<=2013)%>%
  dplyr::select(organism,source,density_m2)%>%
  summarize(
    density_n=length(density_m2), # sample size for all locations
    density_m2_u=sum(density_m2)/density_n,     #grand mean
    density_m2_sd=sd(density_m2))

d7$density_marginerror<-qnorm(.975)*(d7$density_m2_u/sqrt(d7$density_n))
d7$density_sem<-d7$density_m2_sd/sqrt(d7$density_n)
d7$years<-"pre-SSWD"
d7

#post 2013 and SSWD 
d8<- d1%>%  
  filter(year>2013)%>%
  dplyr::select(organism,source,density_m2)%>%
  summarize(
    density_n=length(density_m2), # sample size for all locations
    density_m2_u=sum(density_m2)/density_n,     #grand mean
    density_m2_sd=sd(density_m2))

d8$density_marginerror<-qnorm(.975)*(d8$density_m2_u/sqrt(d8$density_n))
d8$density_sem<-d8$density_m2_sd/sqrt(d8$density_n)
d8$years<-"post-SSWD"
d8

d9<-rbind(d6,d7,d8)%>%
  select(years,density_n:density_sem)
d9
write_csv(d9,"./doc/pycno_PISCO_density_pre-postSSWD.csv")
