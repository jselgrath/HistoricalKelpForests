# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: analyze kelp map data
##################################################

# setup ########################################
library(tidyverse); library(colorspace)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

# read file of kelp area by year for all methods
d1a<-read_csv("./results/kelp_map_allsources_area_location3.csv") # percent cover data. location3
d1c<-read_csv("./results/kelp_map_allsources_area_location.csv") #

d2mei<- read_csv("./results/enso_yearbysource_mei.csv") #mei
d3<-  read_csv("./results/sst_c_year.csv")
d3


# DATA ORGANIZING ############################

# combine enso data and kelp data ###########
d4a<-merge(d1a,d2mei,by="year"); head(d4a) # location
head(d4a) # for some reason this doubles the rows
d4a<-tibble(unique(d4a))
d4a

d4c<-merge(d1c,d2mei,by="year"); head(d4c) # location
head(d4c) # for some reason this doubles the rows
d4c<-tibble(unique(d4c))
d4c

# check
source("./bin/deets.R")
ggplot(d1a,aes(x=year,y=kelp_area_p,color=location3))+geom_point()+deets3
ggplot(d4a,aes(x=enso_max_nq,y=kelp_area_p,color=location3))+geom_point()+deets3


ggplot(d4a,aes(x=as.factor(enso_max_nq),y=kelp_area_p))+geom_boxplot()+deets3

# MERGE TEMP DATA (D3) AND OTHER DATA ###########
# NOTE: TEmp is a shorter time series with few pre-kelp datapoints
d5a<-merge(d4a,d3)
d5a<-tibble(unique(d5a))
d5a

d5c<-merge(d4c,d3)
d5c<-tibble(unique(d5c))
d5c

# Save files
write_csv(d4a,"./results/kelp_map_allsources_location3_enso.csv")
write_csv(d5a,"./results/kelp_map_allsources_location3_temp.csv")


write_csv(d4c,"./results/kelp_map_allsources_location_enso.csv")
write_csv(d5c,"./results/kelp_map_allsources_location_temp.csv")


# check
source("./bin/deets.R")
ggplot(d5a,aes(x=tempc_u,y=kelp_area_p,color=location3))+geom_point()+deets3
ggplot(d5c,aes(x=tempc_max,y=kelp_area_p,color=location))+geom_point()+deets3
ggplot(d5c,aes(x=temp_cv,y=kelp_area_p,color=location))+geom_point()+deets3


