# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: estimates of kelp for enso 
# kelp obsv is for three respondents who talked specificaly about ENSO kelp abundance - used to weight their responses over other people who often gave broad statements (e.g. kelp was variable over time vs kelp declined in year XXX)
##################################################

# setup ########################################
library(tidyverse);library(dplyr)
rm(list=ls())


#################################################
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange")

d1<-read_csv("./OralHistories/results/otter_kelp_oh_subset_long.csv")%>%
  filter(organism=="kelp")%>%
  mutate(kelp_obsv=ifelse(ID2=="i.37",1,kelp_obsv))%>% #add fio to this list
  glimpse()
d1 # has all 3 location levels

# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")
########################################
# set source
d1$source<-"Oral Histories"

# set location to match other data sources
d1$location0<-d1$location
d1$location<-d1$location2
d1$location[d1$location=="Monterey Bay" |d1$location=="Carmel" |d1$location=="MontereyUnspecified" |d1$location=="Monterey Outer"]<-"Monterey Peninsula"
unique(d1$location)



# summarize at for canopy forming kelp ########################
unique(d1$genus)

d1_c<-d1%>%
  filter(species=="luetkeana"| species=="pyrifera"|is.na(species))%>%
  # arrange(year,location3,genus)%>%
  select(-location0,-location2)%>%
  # tibble()%>%
  glimpse()
d1_c

unique(d1_c$genus)

write_csv(d1_c,"./results/kelp_oh_abundance_raw.csv")

d1_c%>%filter(year==1997&location3=="Monterey Bay"&kelp_obsv==1) # mean should be 2

# Summarize by species & location3 #####################

#remove unknown species data -------------------------------------------------
d2<-d1_c%>%
  filter(species=="luetkeana"| species=="pyrifera")%>%
  glimpse()

unique(d2$location3)

d3<-d2%>%
  group_by(year,location3,organism,genus,source)%>%
  summarize(
    # prioritise observations of people with specific ENSO comments
            abundance_u= ifelse(max(kelp_obsv==1),mean(abundance[kelp_obsv==1]),mean(abundance)), 
            abundance_sd= ifelse(max(kelp_obsv==1),round(sd(abundance[kelp_obsv==1]),4),round(sd(abundance),4)),
            abundance_max=max(abundance),
            respondent_n=length(unique(ID2)),
            abundance_n=length(year),
            present=max(present))%>% 
  ungroup()%>%
  arrange(year,location3,genus)%>%
  glimpse()

# checking
d3%>%filter(year==1997&location3=="Monterey Bay")
tail(d3)

filter(d3,abundance_n!=respondent_n) # sometimes > 1 obsv by respondent
unique(d3$location3)

write_csv(d3,"./results/kelp_oh_sp_abundance_location3.csv")

#########################################################
# Summarize by species & location #####################

# location, species-----------------
d4d<-d2%>%
  group_by(year,location,organism,genus,source)%>%
  summarize(
    abundance_u= ifelse(max(kelp_obsv==1),mean(abundance[kelp_obsv==1]),mean(abundance)), 
    abundance_sd= ifelse(max(kelp_obsv==1),round(sd(abundance[kelp_obsv==1]),4),round(sd(abundance),4)),
            abundance_max=round(max(abundance),1),
            abundance_n=length(year),
            respondent_n=length(unique(ID2)),
            present=max(present))%>%
  arrange(year,location,genus)%>%
  ungroup()%>%
  glimpse()
d4d
tail(d4d)
unique(d4d$location)
write_csv(d4d,"./results/kelp_oh_sp_abundance_location.csv")



#########################################################
# Summarize by location3, not species ----------------------------
unique(d2$location3)

# location 3----------------
d5<-d1_c%>%
  group_by(ID2,year,organism,location,location3,source,kelp_obsv)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location3,organism,source)%>%
  summarize(
    abundance_u= ifelse(max(kelp_obsv==1),mean(abundance2[kelp_obsv==1]),mean(abundance2)), 
    abundance_sd= ifelse(max(kelp_obsv==1),round(sd(abundance2[kelp_obsv==1]),4),round(sd(abundance2),4)),
    abundance_max=round(max(abundance2),1),
    abundance_n=length(year),
    respondent_n=length(unique(ID2)),
    present=max(present2))%>%
  arrange(year,location3)%>%
  ungroup()%>%
  glimpse()
d5
tail(d5)
unique(d5$location3)


write_csv(d5,"./results/kelp_oh_abundance_location3.csv")

#########################################################
# Summarize by location, not species #####################
unique(d2$location)

# location ---------------------------
d7b<-d1_c%>%
  group_by(ID2,year,organism,location,source,kelp_obsv)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location,organism,source)%>%
  summarize(
    abundance_u= ifelse(max(kelp_obsv==1),mean(abundance2[kelp_obsv==1]),mean(abundance2)), 
    abundance_sd= ifelse(max(kelp_obsv==1),round(sd(abundance2[kelp_obsv==1]),4),round(sd(abundance2),4)),
    abundance_max=round(max(abundance2),1),
    abundance_n=length(year),
    respondent_n=length(unique(ID2)),
    present=max(present2))%>%
  arrange(year,location)%>%
  ungroup()%>%
  glimpse()

d7b%>%
  filter(year>=2016&location=="Monterey Peninsula")

# # merge with max of dominant species. probably do not need this step
# d8b<-merge(d7b,d4d)
# d8b<-tibble(d8b)
# d8b
# tail(d8b)

write_csv(d7b,"./results/kelp_oh_abundance_location.csv")

#stats - source = reference here
d10<-d7b%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  mutate(organism="kelp",data="OH")%>%
  glimpse
write_csv(d10,"./doc/sampsz_kelp_oh_location.csv")





###########################################
# mean abundance for all sites ################
d9<-d1_c%>%
  group_by(ID2,year,organism,location,source,kelp_obsv)%>%
  summarize(present2=max(present), abundance2=mean(abundance),respondent_n=length(unique(ID2)))%>% # remove species duplicates by respondent
  ungroup()%>%
  
  group_by(year,organism,source)%>%
  summarize(
    abundance_u= ifelse(max(kelp_obsv==1),mean(abundance2[kelp_obsv==1]),mean(abundance2)), 
    abundance_sd= ifelse(max(kelp_obsv==1),round(sd(abundance2[kelp_obsv==1]),4),round(sd(abundance2),4)),
    abundance_max=round(max(abundance2),1),
    respondent_n2=sum(respondent_n),
    present=max(present2))%>%
  arrange(year)%>%
  ungroup()%>%
  glimpse()
d9

write_csv(d9,"./results/kelp_oh_abundance.csv")

