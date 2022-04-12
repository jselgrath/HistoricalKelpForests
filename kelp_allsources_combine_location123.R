# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
##################################################

# Goal: to integrate all kelp data sources
# this is not for kelp species data
# standardize loction names - no spaces

# setup ########################################
library(tidyverse); library (modelr)
library(scales)
library(ggthemes) #tableau colors
#####################################################
rm(list=ls())
dateToday=Sys.Date()

# oral history data 
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
d1_1<-read_csv("./results/kelp_hoh_abundance_location.csv")
d1_3<-read_csv("./results/kelp_hoh_abundance_location3.csv")

d2_1<-read_csv("./results/kelp_map_allsources_area_location.csv")
d2_3<-read_csv("./results/kelp_map_allsources_area_location3.csv")


# make compatiable with combining ##########

# location
d1a_1<-d1_1%>%
  dplyr::select(year,location,kelp_source=source,kelp_abundance=abundance_max,kelp_abundance_n=abundance_n)
d2a_1<-d2_1%>%
  dplyr::select(year,location,kelp_source,kelp_abundance=kelp_abundance_u)%>% #kelp_abundance_e = equal interval
  mutate(kelp_abundance_n=1)%>%
  arrange(year)
d1a_1; d2a_1
d3_1<-rbind(d1a_1,d2a_1)

# location3
d1a_3<-d1_3%>%
  dplyr::select(year,location3,kelp_source=source,kelp_abundance=abundance_max,kelp_abundance_n=abundance_n)
d2a_3<-d2_3%>%
  dplyr::select(year,location3,kelp_source,kelp_abundance=kelp_abundance_u)%>% #kelp_abundance_e = equal interval
  mutate(kelp_abundance_n=1)%>%
  arrange(year)
d1a_3; d2a_3
d3_3<-rbind(d1a_3,d2a_3)

# standardize location names 
# #note facet grid does not take spaces so removing spaces here
d3_3$location3[d3_3$location3=="Big Sur"] <-"BigSur" 
d3_3$location3[d3_3$location3=="SantaCruz"] <-"SantaCruzBay" 
d3_3$location3[d3_3$location3=="SantaCruz Bay"] <-"SantaCruzBay" 
d3_3$location3[d3_3$location3=="SantaCruz Outer"] <-"SantaCruzOuter" 
d3_3$location3[d3_3$location3=="Santa Cruz Bay"] <-"SantaCruzBay" 
d3_3$location3[d3_3$location3=="Santa Cruz Outer"] <-"SantaCruzOuter" 
d3_3$location3[d3_3$location3=="MontereyCarmel"] <-"CarmelBay" 
d3_3$location3[d3_3$location3=="Carmel Bay"] <-"CarmelBay" 
d3_3$location3[d3_3$location3=="Monterey Outer"] <-"MontereyOuter"
d3_3$location3[d3_3$location3=="Monterey Bay"] <-"MontereyBay"
d3_3$location3[d3_3$location3=="Carmel Outer"] <-"CarmelOuter"
unique(d3_3$location3)

d3_3<-arrange(d3_3,year,location3)
range(d3_3$kelp_abundance)

write_csv(d3_1,"./results/kelp_allsources_abundance_location.csv")
write_csv(d3_3,"./results/kelp_allsources_abundance_location3.csv")
