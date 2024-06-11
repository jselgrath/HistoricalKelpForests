# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to integrate hoh and contemporary data for otters. standardized to 30m depth

##################################################
library(tidyverse)
library(scales)
library(ggthemes) #tableau colors
#####################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# hoh data
d1<-read_csv("./results/otter_hoh_abundance_location.csv")%>% # historical, oral history, literature
  arrange(year,location)
d1
tail(d1)

# cdfw data
d2<-read_csv("./data/otter_cdfw_dens_0-30m_location.csv") # contemporary otter data
d2$source<-"Contemporary Data"
##############################
unique(d1$location)
unique(d2$location)

# make columns match
d1a<-d1%>%
  dplyr::select(year,location,organism,otter_source=source,otter_abundance_u=abundance_u)

d2a<-d2%>%
  dplyr::select(year,location,organism,otter_source=source,otter_abundance_u=abundance_u)


range(d1a$otter_abundance_u)

# combine data sources
d3<-rbind(d1a,d2a)
d3
unique(d3$location)
unique(d3$otter_source)

d3$location[d3$location=="BigSur"]<-"Big Sur"
d3$location[d3$location=="SantaCruz"]<-"Santa Cruz"
d3$location[d3$location=="MontereyPeninsula"]<-"Monterey Peninsula"

# save combined data
write_csv(d3,"./results/otter_allsources_abundance_location.csv")




