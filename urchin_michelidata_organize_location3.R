# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: organize Fio's unpublished urchin data
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp/")

# Micheli data
d1<-read_csv("./data/urchins_data_Micheli_2002.csv") #was urchins_2002 but same file
d1

d1$year=2002
d1$source="micheli"
d1$organism="urchin"

d4<-d1%>%
  dplyr::select(year,organism,source,site,transect_no,Spurpuratus_m2,Mfranciscanus_m2)%>%
  pivot_longer(Spurpuratus_m2:Mfranciscanus_m2,
               names_to = "genus",values_to="density_m2")
d4

d4$genus<-gsub("Spurpuratus_m2","Strongylocentrotus",d4$genus)
d4$genus<-gsub("Mfranciscanus_m2","Mesocentrotus",d4$genus)
d4  

# add location
d4$location<-"Monterey Peninsula"

# add location2
d4$location2<-"set"
d4$location2[d4$site=="HMS"]<-"Monterey Bay"
d4$location2[d4$site=="Carmel Pt."]<-"Monterey Carmel"
d4$location2[d4$site=="Malpaso"]<-"Monterey Outer" #Carmel South
d4$location2[d4$site=="Pescadero Pt"]<-"Monterey Outer"
d4$location2[d4$site=="Pt. Lobos"]<-"Monterey Outer" #Carmel South
d4$location2[d4$site=="Pt. Pinos"]<-"Monterey Outer"
d4

# location3
d4$location3=d4$location2
d4$location3[d4$site=="Malpaso"]<-"Carmel Outer"
d4$location3[d4$site=="Pt. Lobos"]<-"Carmel Outer"
d4

write_csv(d4,"./results/urchin_micheli_density_transect.csv")

# summarize by site
d5<-d4%>%
  group_by(year,organism,source,site,genus)%>%
  summarize(density_m2_n=length(density_m2),density_m2_u=mean(density_m2),density_m2_sd=sd(density_m2))%>%
  arrange(site,genus)
d5


# summarize by location
d7<-d4%>%
  group_by(year,location,organism,source,genus)%>%
  summarize(density_n=length(density_m2),density_m2_u=mean(density_m2),density_m2_sd=sd(density_m2))%>%
  arrange(location,genus)
d7

# summarize by location3
d8<-d4%>%
  group_by(year,location3,organism,source,genus)%>%
  summarize(density_n=length(density_m2),density_m2_u=mean(density_m2),density_m2_sd=sd(density_m2))%>%
  arrange(location3,genus)
d8

write_csv(d7,"./results/urchin_micheli_location.csv")
write_csv(d8,"./results/urchin_micheli_location3.csv")

