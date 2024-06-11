# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to count number of transects surveyed by PISCO in a year per site
##################################################

# setup ########################################
library(tidyverse); library(dplyr); library(ggplot2); library(modelr); library(tidyr)  

################################################
rm(list=ls())
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# data for transect/site/year- 
d1<-read_csv("./data/MLPA_kelpforest_swath.4.csv", guess_max = 250000) %>%
  filter(campus=="UCSC" & method=="SBTL_SWATH_PISCO")%>% 
  select(year,classcode,site:transect,count,depth,size)%>% 
  #survey_year==year in all cases 
  #when include month and day there are 49 more transects. Assuming surveys across multiple days for now
  arrange(year,classcode,site)#%>% unique
ggplot(data=d1,aes(x=year))+ geom_histogram()
d1

range(d1$count) #1 to 5750: NO ZEROS

#transects by site and year
d2<-d1%>%
  select(year,site,zone,tx=transect)%>%
  unique()%>%
  group_by(year,site,zone)%>%
  summarise(
    transect=length(tx)
  )

range(d2$transect) # 1-2 transect

# Create row for each transect
d3<-d2%>%
  filter(transect==2)
d3$transect<-1
d3

d4<-rbind(d2,d3)%>%
  arrange(year,site,zone,transect)
d4

#save
write_csv(d4,"./results/PISCO_MLPA_transects_site_year.csv")