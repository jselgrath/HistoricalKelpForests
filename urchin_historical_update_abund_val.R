# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: organize historical urchin data
# set values to match breaks from abc reef data
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
# LOCATION 2 AND 3 ARE THE SAME HERE #############################

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# literature and field notes
d0<-read_csv("./results/urchins_historical_clean.csv")%>%
  glimpse()

# SET RELATIVE ABUNDANCE VALUES #############################


# ---------------------------------------------------
# using breaks based on ABC reef data (pisco & reefcheck)
# < 2/m2 = 1; 2-3/m3=2; 3-7/m2=4; 7-14/m2=4; >14/m2=5

#-------------------------------------------------------------
# Set relative abundance levels to match abc reef breaks (as of June 2, 2020)
# Relative abundance breaks (urchins/m2): 
dens_breaks<-c(0.0000000, 0.9848838, 2.9851820, 4.9854803, 8.9860767)
d2<-d0

# set relative abundance values ######################
# location
d2$abundance1[d2$density_m2==dens_breaks[1]]<-0
d2$abundance1[d2$density_m2>dens_breaks[1]]<-1 # greater than 0
d2$abundance1[d2$density_m2>=dens_breaks[2]]<-2 # between 0 and mean
d2$abundance1[d2$density_m2>=dens_breaks[3]]<-3 #halfway between median and 1 sd
d2$abundance1[d2$density_m2>=dens_breaks[4]]<-4
d2$abundance1[d2$density_m2>=dens_breaks[5]]<-5
d2
tail(d2)

d2$abundance=d2$abundance1
# cbind(d1$abundance1,d2$abundance1) #most cases the same. for a handful of cases d2 version is slightly higher.


# only keep updated abundance value 
d2d<-d2%>%
  dplyr::select(-abundance1)%>%
  glimpse()

# save ------------------------------
write_csv(d2d,"./results/urchins_historical_updated_abund.csv")
