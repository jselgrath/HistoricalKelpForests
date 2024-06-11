# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to join PISCO urchin data with location codes
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(dplyr); library(tidyr)  

################################################
rm(list=ls())
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# data for transect/site/year- 
d1<-read_csv("./data/MLPA_kelpforest_swath.4.csv", guess_max = 250000) %>% 
  filter(campus=="UCSC" & method=="SBTL_SWATH_PISCO" & (classcode== "STRPURAD" | classcode == "MESFRAAD"))%>% 
  # this selects adults only, not recruits. 
  #Recruits were inconsistently measured in different years (yes = 2010, 2011, and 2014 onward). 
  #classcode=="STRPURREC" | classcode=="MESFRAREC" ) #these codes pull recruits
  # select(year,classcode,site:size)%>% #(survey_year:size)%>%
  group_by(year, classcode, site, zone, transect)%>%
  # summarise(count=sum(count))%>%
  ungroup()%>%# add up counts from different size classes
glimpse()

  range(d1$size,na.rm=T)
  qplot(d1$size)

d1$transect<-as.numeric(d1$transect)
ggplot(data=d1,aes(x=year))+ geom_histogram()
d1
range(d1$count) # NO ZEROS 1 - 10517

d1_s<-d1%>%filter(classcode=="STRPURAD")
d1_m<-d1%>%filter(classcode=="MESFRAAD")


# location data
d2<-read_csv("./results/PISCO_MLPA_site_year.csv")%>%
  select(site,location,location3)%>%
  arrange(location,site)%>%
  unique()
d2


#add in zeros -  need to  keep transects that have 0 count for urchins
# transects per site data
d3<-read_csv("./results/PISCO_MLPA_transects_site_year.csv")
d3

d3b<-left_join(d2,d3)

# combine  counts and all surveys

# red
d4_m<-d3b%>%
  full_join(d1_m)
d4_m%>%filter(is.na(classcode))
d4_m$classcode[is.na(d4_m$classcode)]<-"MESFRAAD"

# purple
d4_s<-d3b%>%
  full_join(d1_s) 
d4_s%>%filter(is.na(classcode))
d4_s$classcode[is.na(d4_s$classcode)]<-"STRPURAD"


# join two species records
d5<-rbind(d4_m,d4_s)%>%
  arrange(year, location, classcode, site, transect, zone)


# assign missing classcodes to giant kelp and bull kelp
# fill in missing years for both species
d5$count[is.na(d5$count)]<-0

#check that zeros were filled in appropriately -----------------------------

# original
d3%>%filter(year==1999,site=="BLUEFISH_DC")
d1%>%filter(year==1999,site=="BLUEFISH_DC")%>%group_by(classcode,zone)%>%reframe(tx=n(),count2=sum(count))

# with zeros, for both species
d5%>%filter(year==1999,site=="BLUEFISH_DC")
d5%>%filter(year==1999,site=="BLUEFISH_DC")%>%group_by(classcode,zone)%>%reframe(tx=n(),count2=sum(count))


# confirm no NAs
d5%>%filter(is.na(count))
d5%>%filter(is.na(year))

ggplot(data=d5,aes(x=year))+ geom_histogram()

range(d5$count) #0 10517
d5$organism<-"urchin"
d5$genus<-"Strongylocentrotus"
d5$genus[d5$classcode=="MESFRAAD"]<-"Mesocentrotus"

# remove unmatched sites
d6<-d5%>%
  filter(!is.na(location3))
d6%>% filter(is.na(location3))

write_csv(d6,"./results/urchin_PISCO_location123_raw.csv")



######################################################################################################################
# summarize by site (this is already done in ABC reef version of the data) ###########################################
# calculate density by transect
d8<-d6%>%
  group_by(year, location, location3,organism,classcode,site,genus,zone,transect)%>%
  reframe(
    density_m2=count/60, #60m2 transects
    tx_n=n())%>%
  ungroup()
d8

#calculate mean density etc per site
d9<-d8%>%
  group_by(year, location, location3,organism,classcode,site,genus)%>%
  reframe(
    Density_m2_u=sum(density_m2)/sum(tx_n),
    Density_sd=sd(density_m2),
    Density_n=n())
d9
d9%>%filter(is.na(location3))

# checking against original data
# original
d1%>%filter(year==1999,site=="BLUEFISH_DC")%>%group_by(classcode,zone)%>%summarize(tx=n(),count2=sum(count))

# with zeros, for both species
d8%>%filter(year==1999,site=="BLUEFISH_DC")
  
write_csv(d8,"./results/urchin_PISCO_location123_bytransect.csv")  
write_csv(d9,"./results/urchin_PISCO_location123.csv") 


  