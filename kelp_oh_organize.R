# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: estimates of kelp for enso 
##################################################

# setup ########################################
library(tidyverse);library(dplyr)
rm(list=ls())

#################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/")

d1<-read_csv("./OralHistories/results/otter_kelp_oh_subset_long.csv")
d1 # has all 3 location levels

setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")

########################################
# set source
d1$source<-"Oral Histories"

# set location to match other data sources
d1$location0<-d1$location
d1$location<-d1$location2
d1$location[d1$location=="MontereyBay" |d1$location=="MontereyCarmel" |d1$location=="MontereyUnspecified" |d1$location=="MontereyOuter"]<-"Monterey Peninsula"
unique(d1$location)



# summarize at for canopy forming kelp ########################
d2<-d1%>%
  filter(organism=="kelp")%>%
  filter(species=="luetkeana"| species=="pyrifera")%>%
  # dplyr::select(!location)%>%
  filter(abundance>=0)%>% # remove observations where kelp was present, but no abundance was given
  arrange(year,location3,genus)
d2

write_csv(d2,"./results/kelp_oh_abundance_raw.csv")





# Summarize by species & location3 #####################
unique(d2$location3)

d3<-d2%>%
  group_by(year,location3,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_max=round(max(abundance),1),abundance_n=length(year),respondent_n=length(unique(ID2)))%>%
  arrange(year,location3,genus)
d3
tail(d3)

filter(d3,abundance_n!=respondent_n) # sometimes > 1 obsv by respondent
unique(d3$location3)

write_csv(d3,"./results/kelp_oh_sp_abundance_location3.csv")


#test of summing values. to merge with summary over location, not species below. max by species now same as max 
d3a<-d3%>%
  group_by(year,location3,organism,source)%>%
  summarize(abundance_sum=sum(abundance_u))%>% #sum creates values > 5 #, abundance_max2=max(abundance_max)
  arrange(year,location3)
d3a  
tail(d3a)

#########################################################
# Summarize by species & location #####################
unique(d2$location2)

d4d<-d2%>%
  group_by(year,location,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_max=round(max(abundance),1),abundance_n=length(year),respondent_n=length(unique(ID2)))%>%
  arrange(year,location,genus)
d4d
tail(d4d)
unique(d4d$location)
write_csv(d4d,"./results/kelp_oh_sp_abundance_location.csv")

#test of summing values and max by species. to merge with summary over location, not species below
d4e<-d4d%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_sum=sum(abundance_u))%>% #sum creates values > 5 #, abundance_max2=max(abundance_max)
  arrange(year,location)
d4e
tail(d4e)

#########################################################
# Summarize by location3, not species #####################
unique(d2$location3)

d5<-d2%>%
  group_by(year,location3,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_max=round(max(abundance),1),abundance_n=length(year),respondent_n=length(unique(ID2)))%>%
  arrange(year,location3)
d5
tail(d5)
unique(d5$location3)

# merge with max of dominant species. don't need this step because abundance_max and abundance_max2 are the same here and below
d6<-merge(d5,d3a)
d6<-tibble(d6)
# filter(d8,abundance_max!=abundance_max2)

write_csv(d6,"./results/kelp_oh_abundance_location3.csv")

#########################################################
# Summarize by location, not species #####################
unique(d2$location)

d7b<-d2%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_max=round(max(abundance),1),abundance_n=length(year),respondent_n=length(unique(ID2)))%>%
  arrange(year,location)
d7b

# merge with max of dominant species. probably do not need this step
d8b<-merge(d7b,d4d)
d8b<-tibble(d8b)
d8b
tail(d8b)

write_csv(d8b,"./results/kelp_oh_abundance_location.csv")

###########################################
# mean abundance for all sites ################
d9<-d2%>%
  group_by(year,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_max=round(max(abundance),1),abundance_n=length(year),respondent_n=length(unique(ID2)))%>%
  arrange(year)
d9

write_csv(d9,"./results/kelp_oh_abundance.csv")

