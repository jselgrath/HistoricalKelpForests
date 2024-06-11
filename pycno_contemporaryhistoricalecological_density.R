# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to combine PISCO and published historical values for pycno densities
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# =======================================
#summarized PISCO and RC data
d1_1<-read_csv("./results/pycno_contemporary_density_location.csv")%>%glimpse()
range(d1_1$year) #1999-2019
d1_3<-read_csv("./results/pycno_contemporary_density_location3.csv")

# mean and sd for all PISCO and RC data
d0<-read_csv("./results/pycno_contemporary_density_allsitesyears.csv")
d0_pre<-read_csv("./data/pycno_contemporary_density_pre-SSWD.csv") # calculate these, but not sure where so moving to data for now
d0_post<-read_csv("./data/pycno_contemporary_density_post-SSWD.csv")

# =======================================
# historical/grey literature numbers
d2_1<-read_csv("./results/pycno_historical_density_location.csv")%>%
  select(year,organism,location,source,density_u,density_sd,density_n)
d2_3<-read_csv("./results/pycno_historical_density_location3.csv")%>%
  select(year,organism,location3,source,density_u,density_sd,density_n)
d2_3

range(d2_1$year) #1975-2002 # I might drop the years that overlap  so that the means are easier to calculate


# Combine #############
d3_1<-rbind(d1_1,d2_1)%>%arrange(year,location)
d3_3<-rbind(d1_3,d2_3)%>%arrange(year,location3)

# graph
source("./bin/deets.R")

ggplot(d3_1,aes(x=year, y=density_u,fill=source)) + geom_point()+
  facet_grid(rows=vars(location))+deets4

# save
write_csv(d3_1,"./results/pycno_contemporary_lit_density_location.csv")
write_csv(d3_3,"./results/pycno_contemporary_lit_density_location3.csv")


# calculate mean values for all ecological data for location ###########
# SUMMARIZED FOR ALL SITES AND YEARS - for calculating relative abundance#############
d6<- d3_1%>%  
  dplyr::select(organism,source,density_u)%>%
  summarize(
    density_n=n(), # sample size for all locations
    density_m2_u=sum(density_u)/density_n,     #grand mean
    density_m2_sd=sd(density_u))

d6$density_marginerror<-qnorm(.975)*(d6$density_m2_u/sqrt(d6$density_n))
d6$density_sem<-d6$density_m2_sd/sqrt(d6$density_n)
d6$years<-"all"
d6

#through 2013 and SSWD
d7<- d3_1%>%  
  filter(year<=2013)%>%
  dplyr::select(organism,source,density_u)%>%
  summarize(
    density_n=n(), # sample size for all locations
    density_m2_u=sum(density_u)/density_n,     #grand mean
    density_m2_sd=sd(density_u))

d7$density_marginerror<-qnorm(.975)*(d7$density_m2_u/sqrt(d7$density_n))
d7$density_sem<-d7$density_m2_sd/sqrt(d7$density_n)
d7$years<-"pre-SSWD"
d7

#post 2013 and SSWD 
d8<- d3_1%>%  
  filter(year>2013)%>%
  dplyr::select(organism,source,density_u)%>%
  summarize(
    density_n=length(density_u), # sample size for all locations
    density_m2_u=sum(density_u)/density_n,     #grand mean
    density_m2_sd=sd(density_u))

d8$density_marginerror<-qnorm(.975)*(d8$density_m2_u/sqrt(d8$density_n))
d8$density_sem<-d8$density_m2_sd/sqrt(d8$density_n)
d8$years<-"post-SSWD"
d8

d9<-rbind(d6,d7,d8)%>%
  select(years,density_n:density_sem)
d9

d10<-rbind(d6,d7,d8)
d10
write_csv(d10,"./doc/pycno_contemporary_lit_density.csv")
