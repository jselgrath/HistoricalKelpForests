# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to integrate hoh and contemporary data

##################################################
library(tidyverse)
library(scales)
library(ggthemes) #tableau colors
#####################################################
rm(list=ls())
dateToday=Sys.Date()


setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

# hoh data
d1<-read_csv("./results/otter_hoh_abundance_location3.csv")%>% # historical, oral history, literature
  arrange(year,location3)
d1


# cdfw data
d2<-read_csv("./results/otter_cdfw_dens_0-30m_location3.csv") # contemporary otter data
d2$source<-"Contemporary Data"
##############################
unique(d1$location3)
unique(d2$location3)



unique(d1$location3)

# make columns match
d1a<-d1%>%
  dplyr::select(year,location3,organism,otter_source=source,otter_abundance_u=abundance_u)

d2a<-d2%>%
  dplyr::select(year,location3,organism,otter_source=source,otter_abundance_u)

# combine data sources
d3<-rbind(d1a,d2a)
d3
unique(d3$location3)
unique(d3$otter_source)

d3$location3[d3$location3=="CarmelOuter"]<-"Carmel Outer"
d3$location3[d3$location3=="MontereyCarmel"]<-"Carmel Bay"
d3$location3[d3$location3=="Carmel"]<-"Carmel Bay"
d3$location3[d3$location3=="MontereyOuter"]<-"Monterey Outer"
d3$location3[d3$location3=="BigSur"]<-"Big Sur"
d3$location3[d3$location3=="MontereyUnspecified"]<-"Monterey Bay"
d3$location3[d3$location3=="MontereyBay"]<-"Monterey Bay"
d3$location3[d3$location3=="SantaCruzInner"]<-"Santa Cruz Bay" #both points from oral histories
d3$location3[d3$location3=="SantaCruz Bay"]<-"Santa Cruz Bay" 
d3$location3[d3$location3=="Santa Cruz Inner"]<-"Santa Cruz Bay" 
d3$location3[d3$location3=="SantaCruz"]<-"Santa Cruz Bay"
d3$location3[d3$location3=="SantaCruzOuter"]<-"Santa Cruz Outer"
d3$location3[d3$location3=="SantaCruz Outer"]<-"Santa Cruz Outer"
unique(d3$location3)

range(d3$otter_abundance_u)

# save combined data
write_csv(d3,"./results/otter_allsources_abundance_location3.csv")

