# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to join PISCO kelp stipe data with location data
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(dplyr); library(lubridate)

################################################
rm(list=ls())
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

#PISCO DATA
d1<-read_csv("./data/MLPA_kelpforest_swath.4.csv", guess_max = 250000)%>% 
  filter(campus=="UCSC"& method=="SBTL_SWATH_PISCO" & (classcode=="MACPYRAD" | classcode=="NERLUE"))%>%
  select(year,classcode,site:size)%>% #(survey_year:size)%>%
  group_by(year, classcode, site, zone, transect)%>%
  summarise(count=sum(count))%>%
  ungroup()# add up counts from different size classes
  
d1$transect<-as.numeric(d1$transect)
d1

range(d1$count) #1 to 1007
ggplot(data=d1,aes(x=year))+ geom_histogram()

d1_m<-d1%>%filter(classcode=="MACPYRAD")
d1_n<-d1%>%filter(classcode=="NERLUE")


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

# macro
d4_m<-d3b%>%
  full_join(d1_m)
d4_m%>%filter(is.na(classcode))
d4_m$classcode[is.na(d4_m$classcode)]<-"MACPYRAD"

# nereo
d4_n<-d3b%>%
  full_join(d1_n) 
d4_n%>%filter(is.na(classcode))
d4_n$classcode[is.na(d4_n$classcode)]<-"NERLUE"


# join two species records
d5<-rbind(d4_m,d4_n)%>%
  arrange(year, location, classcode, site, transect, zone)

# assign missing classcodes to giant kelp and bull kelp
# fill in missing years for both species
d5$count[is.na(d5$count)]<-0
d5
#check that zeros were filled in appropriately -----------------------------

# original
d3%>%filter(year==2020,site=="BLUEFISH_DC")
d1%>%filter(year==2020,site=="BLUEFISH_DC")%>%group_by(classcode,zone)%>%summarize(tx=n(),count2=sum(count))

# with zeros, for both species
d5%>%filter(year==2020,site=="BLUEFISH_DC")
d5%>%filter(year==2020,site=="BLUEFISH_DC")%>%group_by(classcode,zone)%>%summarize(tx=n(),count2=sum(count))


# confirm no NAs
d5%>%filter(is.na(count))
d5%>%filter(is.na(year))

ggplot(data=d5,aes(x=year))+ geom_histogram()

range(d5$count) #0 612
d5$organism<-"kelp"
d5$species<-"bull kelp"
d5$species[d5$classcode=="MACPYRAD"]<-"giant kelp"

# remove unmatched sites
d6<-d5%>%
  filter(!is.na(location3))
d6%>% filter(is.na(location3))
# d2$source<-"PISCO"

write_csv(d6,"./results/kelpspecies_PISCO_location123_raw.csv")

######################################################################################################################
# summarize by site (this is already done in ABC reef version of the data) ###########################################
# calculate density by transect
d8<-d6%>%
  group_by(year, location, location3,organism,classcode,site,species,zone,transect)%>%
  summarise(
    density_m2=count/60, #60m2 transects
    tx_n=n())%>%
  ungroup()
d8

#calculate mean density etc per site
d9<-d8%>%
  group_by(year, location, location3,organism,classcode,site,species)%>%
  summarise(
    Density_m2_u=sum(density_m2)/sum(tx_n),
    Density_sd=sd(density_m2),
    Density_n=n())
d8

# checking against original data
# original
d1%>%filter(year==1999,site=="BLUEFISH_DC")%>%group_by(classcode,zone)%>%summarize(tx=n(),count2=sum(count))

# with zeros, for both species
d9%>%filter(year==1999,site=="BLUEFISH_DC")

write_csv(d8,"./results/kelpspecies_PISCO_location123_bytransect.csv")  
write_csv(d9,"./results/kelpspecies_PISCO_location123.csv")  



# ------------------------------------------------------------
# summarize by site no species data ####################
# ------------------------------------------------------------

# calculate density by transect
d10<-d6%>%
  group_by(year, location, location3,organism,site,zone,transect)%>%
  summarise(
    count2=sum(count), # for both species
    density_m2=count2/60 #60m2 transects
    )%>%
  ungroup()%>%
  glimpse()
d10

#calculate mean density etc per location3
d11<-d10%>%
  group_by(year, location, location3,organism)%>%
  summarise(
    Density_n=n(),
    Density_m2_u=mean(density_m2),
    Density_sd=sd(density_m2))%>%
  glimpse()
d11


#calculate mean density etc per location
d12<-d10%>%
  group_by(year, location, organism)%>%
  summarise(
    Density_n=n(),
    Density_m2_u=mean(density_m2),
    Density_sd=sd(density_m2))%>%
  glimpse()
d12


