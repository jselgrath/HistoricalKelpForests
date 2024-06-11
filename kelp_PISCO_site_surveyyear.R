# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to obtain list of surveys done in study area during various years
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(dplyr); library(tidyr)  
# Pisco Data from file uploaded:"PISCO_kelpforest_swath.1.2.csv" 
# avaliable on https://search.dataone.org/view/doi%3A10.6085%2FAA%2FPISCO_kelpforest.1.6
################################################

rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# Year data
d1<-read_csv("./data/PISCO_kelpforest_site_table.1.2.csv")%>%
  filter(campus=="UCSC", method=="SBTL_SWATH")%>%
  dplyr::select(year,site,latitude,longitude,MPA_Name)
d1

# Year data for MLPA review
d2<-read_csv("./data/MLPA_kelpforest_site_table.4.csv")%>%
  filter(campus=="UCSC", method=="SBTL_SWATH_PISCO")%>%
  dplyr::select(year=survey_year,site,latitude,longitude,CA_MPA_Name_Short)
d2


# location data
d3<-read_csv("./data/PISCO_RC_location.csv")%>%
  filter(source=="PISCO")%>%
  dplyr::select(site,location,location2,location3)
d3


# PISCO DATA
d4<-tibble(merge(d1,d3))
d4

# With 2020 data added
d5<-tibble(merge(d2,d3))
d5

write_csv(d4,"./results/PISCO_site_year.csv")
write_csv(d5,"./results/PISCO_MLPA_site_year.csv")