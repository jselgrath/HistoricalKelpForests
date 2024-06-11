# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to join Reef check and PISCO kelp survey data
# PISCO - MLPA 2020 data (downloaded 4/3/2022)
# RC - downloaded 2020
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(dplyr)  
rm(list=ls())
################################################
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# REEF CHECK DATA - locations assigned in GIS
# Bull kelp is summarized, but giant kelp is not because it has stipe count information.
# sometimes more than one set of surveys per year
d1<- read_csv("./data/reefcheckCA_algae_location3.csv")%>% 
  filter(Classcode=="bull kelp" | Classcode=="giant kelp")%>%
  dplyr::select(year=Year,date=SurveyDate,site=Site,species=Classcode,count=Amount,Distance,location,location3,transect=Transect)%>%
  arrange(year,date,site,transect,species)
tail(d1)
range(d1$year)
# view(d1%>%filter(Distance<30))
d1$source<-"ReefCheck"
d1$ID<-paste0(d1$date,d1$site,d1$transect)


# set transect length to 30m when observations were 0
d1a<-d1%>% mutate(Distance=if_else(is.na(Distance)&count>=0, 30, Distance))
tail(d1a)
# view(d1a)

# check that transects with shorter numbers got picked up manually
# view(d1%>%filter(year==2017,site=="Point Sur"))
# view(d1a%>%filter(Distance<30))

# view(d1a%>%filter(year==2007,site=="Breakwater",transect==3))
d1a$Distance[d1a$date=="31-Aug-07" & d1a$site=="Breakwater" & d1a$transect==3]<-28

# view(d1a%>%filter(year==2007,site=="Carmel River",transect==4))
d1a$Distance[d1a$year==2007 & d1a$site=="Carmel River" & d1a$transect==4]<-25

# view(d1a%>%filter(year==2007,site=="Pescadero",transect==1))
# d1a$Distance[d1a$date=="22-Aug-07" & d1a$site=="Pescadero" & d1a$transect==1]<-8

# view(d1a%>%filter(date=="24-Apr-08",site=="Breakwater",transect==4))
d1a$Distance[d1a$date=="24-Apr-08" & d1a$site=="Breakwater" & d1a$transect==4]<-3

# view(d1a%>%filter(year==2009,site=="Stillwater Cove Monterey",transect==4))
# d1a$Distance[d1a$date=="24-May-09" & d1a$site=="Stillwater Cove Monterey" & d1a$transect==4]<-18

# view(d1a%>%filter(year==2014,site=="Coral St Lucus Pt",transect==1))
# d1a$Distance[d1a$date=="12-Jul-14" & d1a$site=="Coral St Lucus Pt" & d1a$transect==1]<-27

# view(d1a%>%filter(year==2015,site=="Breakwater",transect==4))
# d1a$Distance[d1a$date=="8-Aug-15" & d1a$site=="Breakwater" & d1a$transect==4]<-28

# view(d1a%>%filter(year==2015,site=="Breakwater",transect==5))
# d1a$Distance[d1a$date=="8-Aug-15" & d1a$site=="Breakwater" & d1a$transect==5]<-27

# view(d1a%>%filter(year==2015,site=="Lovers Point",transect==5))
# d1a$Distance[d1a$date=="30-May-15" & d1a$site=="Lovers Point" & d1a$transect==5]<-26

# view(d1a%>%filter(year==2017,site=="Breakwater",transect==5))
# d1a$Distance[d1a$date=="19-May-17" & d1a$site=="Breakwater" & d1a$transect==5]<-29


# view(d1a%>%filter(year==2017,site=="Point Sur",transect==2))
# view(d1a%>%filter(year==2017,site=="Point Sur",transect==3))
# view(d1a%>%filter(year==2017,site=="Point Sur",transect==4))
# view(d1a%>%filter(year==2017,site=="Point Sur",transect==5))
# 
# view(d1a%>%filter(year==2018,site=="Point Sur",transect==6))
# view(d1a%>%filter(year==2019,site=="Point Sur",transect==4))

# view(d1a%>%filter(ID=="24-Apr-08Breakwater6"))
d1a$Distance[d1a$ID=="24-Apr-08Breakwater6"]<-30
d1a$count[d1a$ID=="24-Apr-08Breakwater6"]<-0

# view(d1a%>%filter(ID=="14-Jul-10South Monastery5"))
d1a$Distance[d1a$ID=="14-Jul-10South Monastery5"]<-30
d1a$count[d1a$ID=="14-Jul-10South Monastery5"]<-0

# view(d1a%>%filter(ID=="6-Aug-12Pt. Joe5"))
d1a$Distance[d1a$ID=="6-Aug-12Pt. Joe5"]<-30
d1a$count[d1a$ID=="6-Aug-12Pt. Joe5"]<-0

# view(d1a%>%filter(ID=="16-Sep-13Carmel River6"))
d1a$Distance[d1a$ID=="16-Sep-13Carmel River6"]<-30
d1a$count[d1a$ID=="16-Sep-13Carmel River6"]<-0

# view(d1a%>%filter(ID=="6-May-13Breakwater1"))
d1a$Distance[d1a$ID=="6-May-13Breakwater1"]<-30
d1a$count[d1a$ID=="6-May-13Breakwater1"]<-0

# view(d1a%>%filter(ID=="6-May-13Breakwater5"))
d1a$Distance[d1a$ID=="6-May-13Breakwater5"]<-30
d1a$count[d1a$ID=="6-May-13Breakwater5"]<-0

# view(d1a%>%filter(ID=="6-Jun-15MacAbee3")) #bull kelp missing from this


# -------------------------------------------------
#estimate mean density per transect ##################
# amount/distance x 30 = density for 60m2
# https://search.dataone.org/view/doi%3A10.25494%2FP65K5W

d1b<-complete(d1a,species,nesting(year,ID,site,location,location3,transect),fill=list(count=0))


d1c<-d1b%>%
  mutate(area=Distance*2)%>%
  group_by(year,ID,site,location, location3,transect,species)%>%
  summarize(
    Density_m2=sum(count)/max(area))

d1d<-ungroup(d1c)%>%
  dplyr::select(year,location,location3,species,Density_m2,ID)

# check for NAs
d1d_check<-d1d%>%filter(is.na(Density_m2))
d1d_check

d1d$Density_m2[d1d$ID=="6-Jun-15MacAbee3"]<-0

d1d$organism="kelp"



#============================================================
#PISCO DATA - locations assigned in R ##########
# This is not summarized
d2<- read_csv("./results/kelpspecies_PISCO_location123_bytransect.csv") # PISCO_kelp_location3.csv

d2$transect2<-paste0(d2$zone,d2$transect)
d2$ID<-paste0(d2$year,"-",d2$site,"-",d2$transect2)
d2b<-d2%>%
  select(year,location,location3,species,Density_m2=density_m2,ID,organism)

# combine PISCO and Reef Check Data ##############
names(d1d)
names(d2b)

d3<-rbind(d1d,d2b)


d3$genus<-"Nereocystis"
d3$genus[d3$species=="giant kelp"]<-"Macrocystis"
d3 
range(d3$Density_m2)

# check for NAs
d3%>%filter(is.na(Density_m2))
str(d3)

write_csv(d3,"./results/kelpspecies_PISCO_RC_density_transect.csv")


# estimate mean density per year/location ##############
d5<-d3%>% #d4
  group_by(year,location,organism, genus)%>%
  summarize(
    density_n=length(Density_m2),
    density_m2_u=mean(Density_m2),
    density_m2_sd=
      sd(Density_m2))%>%
  arrange(year,location,genus)
d5

write_csv(d5,"./results/kelpspecies_PISCO_RC_density_location.csv")

#stats 
yrs<-d5%>%
  select(location,year)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  glimpse
