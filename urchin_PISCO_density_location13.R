# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: organize PISCO urchin data
# originaldata = ./data/MLPA_kelpforest_swath.4.csv
# subset location in urchin_PISCO_latlong.R file



# THIS CURRENTLY IS NOT PART OF THE ANALYSIS. AND MAPPING Transensects to data is not capturing all surveyed transects.
# Next time, get unique list of transects from full dataset and join with  survey datafilter(is.na(zone))



##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

source("./bin/deets.R")

# summarized PISCO data
# count is urchins per 60m2 transect
##########################################################

# by species - raw counts
d1<-read_csv("./results/urchin_PISCO_location123_raw.csv")#has all location and transect data in it
d1$source="PISCO"

d1
d1%>%filter(genus=="Mesocentrotus")
d1%>%filter(genus=="Strongylocentrotus")
##############################
# summarize ABC reef data
# grand mean & pooled variance

# summarize mean values by site and year
# Combining PISCO and ReefCheck Data. Use Program to separate

# Calculate density per species by transect
d2<-d1%>%
  group_by(year, location, location3,organism,classcode,site,genus,zone,transect,source)%>%
  reframe(
    density_m2=count/60, #60m2 transects
    tx_n=n())%>%
  ungroup()

d2%>%filter(is.na(location3))


# SUMMARIZED BY LOCATION#############
#calculate mean density etc per LOCATION
d3<-d2%>%
  group_by(year, location, organism,classcode,genus,source)%>%
  reframe(
    density_n=n(),
    density_m2_u=mean(density_m2),
    density_m2_sd=sd(density_m2))%>%
  arrange(year,location,genus)%>%
  mutate(density_marginerror=qnorm(.975)*(density_m2_u/sqrt(density_n)))
d3

range(d3$density_m2_u)
range(d3$density_marginerror)

write_csv(d3,"./results/urchin_PISCO_density_location.csv")



# SUMMARIZED BY LOCATION3 #############
d5<- d2%>%  
  group_by(year, location3, organism,classcode,genus,source)%>%
  reframe(
    density_n=n(),
    density_m2_u=mean(density_m2),
    density_m2_sd=sd(density_m2))%>%
  arrange(year,location3,genus)%>%
  mutate(density_marginerror=qnorm(.975)*(density_m2_u/sqrt(density_n)))
d5

range(d5$density_m2_u)
range(d5$density_m2_sd)
range(d5$density_marginerror)


write_csv(d5,"./results/urchin_PISCO_density_location3.csv")



# SUMMARIZED FOR ALL SITES AND YEARS - for calculating relative abundance#############
d6<- d2%>%  
  group_by(organism, genus,source)%>%
  reframe(
    density_n=n(),
    density_m2_u=mean(density_m2),
    density_m2_sd=sd(density_m2))%>%
  arrange(genus)%>%
  mutate(density_marginerror=qnorm(.975)*(density_m2_u/sqrt(density_n)))
d6


write_csv(d6,"./results/urchin_PISCO_density_u_sd.csv")
