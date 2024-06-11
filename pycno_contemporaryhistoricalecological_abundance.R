# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to set relative abundance values for pycnopodia from historical and contemporary data sources
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# density data for PISCO and historical ecological published values from literature
d1_1<-read_csv("./results/pycno_contemporary_lit_density_location.csv")
d1_3<-read_csv("./results/pycno_contemporary_lit_density_location3.csv")

# mean and sd for all PISCO data
d0<-read_csv("./doc/pycno_contemporary_lit_density.csv")


#################################################################
# CALCULATE RELATIVE ABUNDANCE ##################################
source("./bin/deets.R")
ggplot(d1_1,aes(x=year,y=density_u))+geom_line()+geom_point()+deets3+facet_grid(rows=vars(location))
ggplot(d1_3,aes(x=year,y=density_u))+geom_line()+geom_point()+deets3+facet_grid(rows=vars(location3))

# assign relative abundance values ##############
range(d1_1$density_u) #0.00 0.060
range(d1_3$density_u) #0.00 0.0674


d0 # has mean and sd for all sites from PISCO and RC data and published densities

# use values pre-SSWD
d0a<-d0%>%filter(years=="pre-SSWD")
d0b<-d0%>%filter(years=="post-SSWD")

# set breaks in abundance based on standard deviations ################
# NOTE: These are the SD from all PISCO values with all sites/years pooled
# since dens_1 and dens_2 are both negative, set 1 at mean post sswd
# since dens_1 and dens_2 are both negative, set 2 half way between mean pre and post SSWD
dens_dif<-d0a$density_m2_u-d0b$density_m2_u                          # diff. between pre/post means #0.02734007

dens_0<-0                                                            # 0
dens_1a<-d0b$density_m2_u;dens_1a                                    # mean post-SSWD
dens_2a<-d0a$density_m2_u-(dens_dif/2);dens_2a                       # center of pre/post-SSWD means
dens_3<-d0a$density_m2_u;dens_3                                      # mean value pre-SSWD
dens_4<-d0a$density_m2_u+d0a$density_m2_sd;dens_4                    # mean-post + 1 SD
dens_5<-d0a$density_m2_u+(2*d0a$density_m2_sd);dens_5                # mean-post + 2 SD

# combine values 
dens_breaks<-round(c(dens_0,dens_1a,dens_2a,dens_3,dens_4,dens_5),4)
dens_breaks


# set abundance ######################
# location
d1_1$abundance_u<-9999
d1_1$abundance_u[d1_1$density_u==dens_breaks[1]]<-0
d1_1$abundance_u[d1_1$density_u>dens_breaks[1]]<-1  # greater than 0
d1_1$abundance_u[d1_1$density_u>=dens_breaks[2]]<-2 # between 0 and center of pre/post-SSWD means
d1_1$abundance_u[d1_1$density_u>=dens_breaks[3]]<-3 # mean value pre-SSWD
d1_1$abundance_u[d1_1$density_u>=dens_breaks[4]]<-4 # between mean-post and 1 SD
d1_1$abundance_u[d1_1$density_u>=dens_breaks[5]]<-5 # greater than 1 SD
d1_1
tail(d1_1)

ggplot(d1_1,aes(x=year,y=abundance_u,color=location))+geom_smooth()+deets3
ggplot(d1_1,aes(x=year,y=density_u,color=location))+geom_smooth()+deets3


#location3
d1_3$abundance_u<-9999
d1_3$abundance_u[d1_3$density_u==dens_breaks[1]]<-0
d1_3$abundance_u[d1_3$density_u>dens_breaks[1]]<-1  # greater than 0
d1_3$abundance_u[d1_3$density_u>=dens_breaks[2]]<-2 # between 0 and center of pre/post-SSWD means
d1_3$abundance_u[d1_3$density_u>=dens_breaks[3]]<-3 # mean value pre-SSWD
d1_3$abundance_u[d1_3$density_u>=dens_breaks[4]]<-4 # between mean-post and 1 SD
d1_3$abundance_u[d1_3$density_u>=dens_breaks[5]]<-5 # greater than 1 SD
d1_3
tail(d1_3)

ggplot(d1_3,aes(x=year,y=abundance_u,color=location3))+geom_smooth()+deets3
ggplot(d1_3,aes(x=year,y=density_u,color=location3))+geom_smooth()+deets3

write_csv(d1_1,"./results/pycno_contemporary_abundance_location.csv")
write_csv(d1_3,"./results/pycno_contemporary_abundance_location3.csv")
