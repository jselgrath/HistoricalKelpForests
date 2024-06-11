# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to join PISCO pycnopodia data with location codes
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(dplyr); library(tidyr)  


# Pisco Data from file uploaded in 2020:	"PISCO_kelpforest_swath.1.2.csv" 
# avaliable on https://search.dataone.org/view/doi%3A10.6085%2FAA%2FPISCO_kelpforest.1.6
################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")


# data for transect/site/year- 
d1<-read_csv("./data/MLPA_kelpforest_swath.4.csv", guess_max = 250000) %>% #PISCO_kelpforest_swath.1.2.csv")%>%
  filter(campus=="UCSC" & method=="SBTL_SWATH_PISCO" & classcode== "PYCHEL")%>% 
  select(year,site,zone,transect,count)%>%  #,depth,size
  arrange(year,site)
d1$transect<-as.numeric(d1$transect)
ggplot(data=d1,aes(x=year))+ geom_histogram()
d1
range(d1$count) #1 to 44: NO ZEROS

# location data # -----------------------------------------
d2<-read_csv("./results/PISCO_MLPA_site_year.csv")%>%
  select(site,location,location2,location3)%>%
  arrange(location,site)%>%
  unique()
d2


#add in zeros # -----------------------------------------
# -  need to  keep transects that have 0 count for pycno
# transects per site data
d3<-read_csv("./results/PISCO_MLPA_transects_site_year.csv")
d3

# combine  counts and all surveys
d4<-d1%>%
  full_join(d3) 

#assign zeros to count 
d4$count[is.na(d4$count)]<-0
d4%>%filter(is.na(count))
d4


#check that zeros were filled in appropriately -----------------------------

# original
d1%>%filter(year==2020,site=="BLUEFISH_DC")%>%group_by(zone,transect)%>%summarize(count2=sum(count))

# with zeros, for both species
d4%>%filter(year==2020,site=="BLUEFISH_DC")%>%group_by(zone,transect)%>%summarize(count2=sum(count))

d4$species<-"sunflower star"

#merge data and location levels, remove sites not in study area -----------------------------------
d6<-d4%>%
  inner_join(d2)%>%
  select(year,site,location,location2,location3,species,count,zone,transect) %>% #size
  arrange(year, location, site, transect, zone)
d6<-tibble(d6);d6

# confirm no NAs
d6%>%filter(is.na(count))
d6%>%filter(is.na(year))

ggplot(data=d6,aes(x=year))+ geom_histogram()

range(d6$count) #0 44

write_csv(d6,"./results/pycno_PISCO_location123.csv")