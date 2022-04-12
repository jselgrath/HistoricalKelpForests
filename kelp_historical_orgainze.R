# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
##################################################
# Goal: to summarize estimates of historic kelp
# 
##################################################
library(tidyverse); library(dplyr)
library(scales)

####################################################
rm(list=ls())
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
dateToday=Sys.Date()

d1<-read_csv("./data/kelp_historical_expanded3.csv") # i expanded years manually since not so many...  # v2 has cleaned names that are easier for R to handle # this dataset is integrated with older sources post-wsn 2019
d1

d2<-d1%>% 
  dplyr::select(ID,location,year,nereo.abundance:protected,reference);d2
levels(as.factor(d2$location))
d2$organism<-"kelp"
d2$source<-"Literaure, Journals, Field Notes"

#change location names to modern ones
d2$location<-gsub("Pt Aulon", "Lovers Point", d2$location)
d2$location<-gsub("Almeja Point", "Hopkins", d2$location)
d2$location<-gsub("Mussel Point", "Hopkins", d2$location)
d2$location0<-d2$location


# then group location to broader categories used in kelp maps
levels(as.factor(d2$location))
d2$location2<-d2$location
d2$location2[d2$location2=="Almeja a distance of 2.5 miles to Monterey Harbor"]<-"MontereyBay"
d2$location2[d2$location2=="Aulon Reef"]<-"MontereyBay"
d2$location2[d2$location2=="Carmel Bay"]<-"MontereyCarmel"
d2$location2[d2$location2=="Hopkins to Pt Pinos"]<-"MontereyBay"
d2$location2[d2$location2=="Lovers Point"]<-"MontereyBay"
d2$location2[d2$location2=="Lovers Point to Hopkins"]<-"MontereyBay"
d2$location2[d2$location2=="Lovers Point to Monterey Harbor"]<-"MontereyBay"
d2$location2[d2$location2=="Monterey Pier2"]<-"MontereyBay"
d2$location2[d2$location2=="Mussel point"]<-"MontereyBay"
d2$location2[d2$location2=="Hopkins"]<-"MontereyBay"

d2$location2[d2$location2=="Ocean side of Pt Pinos to Carmel Bay"]<-"MontereyOuter"
d2$location2[d2$location2=="Pescadero Pt"]<-"MontereyOuter"
d2$location2[d2$location2=="Point Joe"]<-"MontereyOuter"
d2$location2[d2$location2=="Pt Lobos to Pt Sur"]<-"Big Sur" #also carmel outer
# decided not to add in extra location for pt sur to cooper point because replacing the observation with map data anyways

d2$location2[d2$location2=="Pt Pinos"]<-"MontereyOuter"
d2$location2[d2$location2=="Pt Sur to Cooper Point"]<-"Big Sur" # near pfieffer campground
d2$location2[d2$location2=="Monterey Peninsula"]<-"MontereyUnspecified" 
d2


# set location3
d2$location3<-d2$location2 #same because no carmel outer level



# set location to match other data sources
d2$location<-d2$location2
d2$location[d2$location=="MontereyBay" |d2$location=="MontereyCarmel" |d2$location=="MontereyUnspecified" |d2$location=="MontereyOuter"]<-"Monterey Peninsula"
unique(d2$location)

# set abundance levels ############
# nereocystsis
levels(as.factor(d2$nereo.abundance))
d2$abundance.nereo2<-NA
d2$abundance.nereo2[d2$nereo.abundance=="high"]<-5
d2$abundance.nereo2[d2$nereo.abundance=="numerous"]<-4
d2$abundance.nereo2[d2$nereo.abundance=="medium-high"]<-4
d2$abundance.nereo2[d2$nereo.abundance=="medium"]<-3
d2$abundance.nereo2[d2$nereo.abundance=="common"]<-3
d2$abundance.nereo2[d2$nereo.abundance=="present"]<-3
d2$abundance.nereo2[d2$nereo.abundance=="low-medium"]<-2
d2$abundance.nereo2[d2$nereo.abundance=="patchy"]<-1
d2$abundance.nereo2[d2$nereo.abundance=="absent"]<-0
levels(as.factor(d2$abundance.nereo2))


#macrocystsis
levels(as.factor(d2$macro.abundance))
d2$abundance.macro2<-NA
d2$abundance.macro2[d2$macro.abundance=="high"]<-5
d2$abundance.macro2[d2$macro.abundance=="numerous"]<-4
d2$abundance.macro2[d2$macro.abundance=="medium-high"]<-4
d2$abundance.macro2[d2$macro.abundance=="medium"]<-3
d2$abundance.macro2[d2$macro.abundance=="common"]<-3
d2$abundance.macro2[d2$macro.abundance=="present"]<-3
d2$abundance.macro2[d2$macro.abundance=="low-medium"]<-2
d2$abundance.macro2[d2$macro.abundance=="patchy"]<-1
d2$abundance.macro2[d2$macro.abundance=="low"]<-1
d2$abundance.macro2[d2$macro.abundance=="absent to rare"]<-1
d2$abundance.macro2[d2$macro.abundance=="absent"]<-0
levels(as.factor(d2$abundance.macro2))


# combine macro nereo columns for plotting ################
d3<-d2%>%
  dplyr::select(year,location,location2,location3,organism, source,abundance.macro2,abundance.nereo2,reference,mixed.b)%>%
  gather(abundance.macro2,abundance.nereo2,key="genus",value="abundance")%>%
  filter(abundance>=0)
d3$genus<-gsub("abundance.", "", d3$genus) # remove extra text
d3$genus<-gsub("macro2", "Macrocystsis", d3$genus) # remove extra text
d3$genus<-gsub("nereo2", "Nereocystsis", d3$genus) # remove extra text
d3

write_csv(d3,"./results/kelp_historical_abundance_raw.csv")



# remove map references (using elsewhere) ##############
d4<-d3%>%
  filter(reference!="McFarland 1911")%>%
  arrange(year)
d4

write_csv(d4,"./results/kelp_historical_abundance_raw_nomapdata.csv")




# summarize by genus, location, reference ######################
# note location2 & location3 are the same here
d5<-d4%>%
  group_by(year,location2,location3,organism,genus,source,reference)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_max=round(max(abundance),1),abundance_n=length(year)) %>%
  arrange(year)
d5
write_csv(d5,"./results/kelp_historical_sp_abundance_location23_reference.csv")

#location
d5b<-d4%>%
  group_by(year,location,organism,genus,source,reference)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_max=round(max(abundance),1),abundance_n=length(year)) %>%
  arrange(year)
d5b
write_csv(d5b,"./results/kelp_historical_sp_abundance_location_reference.csv")

# summarize by species, location ######################
# location23
d6<-d4%>%
  group_by(year,location2,location3,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance,na.rm=T),1), abundance_sd=sd(abundance,na.rm=T),abundance_max=round(max(abundance),1),abundance_n=length(year), reference_n=length(unique(reference))) %>%
  arrange(year)
d6 
write_csv(d6,"./results/kelp_historical_sp_abundance_location23.csv") 

d6b<-d4%>%
  group_by(year,location,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance,na.rm=T),1), abundance_sd=sd(abundance,na.rm=T),abundance_max=round(max(abundance),1),abundance_n=length(year), reference_n=length(unique(reference))) %>%
  arrange(year)
d6b 
write_csv(d6b,"./results/kelp_historical_sp_abundance_location.csv") 

# summarize by location ######################
# location
d7<-d4%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_u=round(mean(abundance,na.rm=T),1), abundance_sd=sd(abundance,na.rm=T),abundance_max=round(max(abundance),1),abundance_n=length(year), reference_n=length(unique(reference))) %>%
  arrange(year)
d7 
write_csv(d7,"./results/kelp_historical_abundance_location.csv") 

# location23
d7b<-d4%>%
  group_by(year,location2,location3,organism,source)%>%
  summarize(abundance_u=round(mean(abundance,na.rm=T),1), abundance_sd=sd(abundance,na.rm=T),abundance_max=round(max(abundance),1),abundance_n=length(year), reference_n=length(unique(reference))) %>%
  arrange(year)
d7b 
write_csv(d7b,"./results/kelp_historical_abundance_location23.csv") 

# summarize by year only ######################
d8<-d4%>%
  group_by(year,organism,source)%>%
  summarize(abundance_u=round(mean(abundance,na.rm=T),1), abundance_sd=sd(abundance,na.rm=T),abundance_max=round(max(abundance),1),abundance_n=length(year), reference_n=length(unique(reference))) %>%
  arrange(year)
d8 
write_csv(d8,"./results/kelp_historical_abundance.csv") 

