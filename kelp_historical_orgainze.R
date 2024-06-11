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
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# dateToday=Sys.Date()

d2<-read_csv("./results/kelp_historical_expanded_clean.csv") %>%
  glimpse()

unique(d2$location)

# combine macro nereo columns for plotting ################
d3<-d2%>%
  select(year,location,location3,organism, source,present,Macrocystis,Nereocystis,mixed_b,author)%>%
  gather(Macrocystis ,Nereocystis,key="genus",value="abundance")%>%
  glimpse()
write_csv(d3,"./results/kelp_historical_sp_abundance_raw.csv")



# summarize by species, location3, author -------------------------
# note location2 & location3 are the same here
d5<-d3%>%
  group_by(year,location3,organism,genus,source,author)%>%
  summarize(
    abundance_u=mean(abundance),
    abundance_sd=sd(abundance,na.rm=T),
    abundance_max=max(abundance),
    abundance_n=n(),
    present=max(present)) %>%
  filter(abundance_u>=0)%>% #only records that mention spp
  ungroup()%>%
  arrange(year)%>%
  glimpse()
d5

d5%>%filter(year>=1944)
d5%>%filter(genus=="Nereocystis")
write_csv(d5,"./results/kelp_historical_sp_abundance_location23_author.csv")

# summarize by species, location, author -------------------------
d5b<-d3%>%
  group_by(year,location,organism,genus,source,author)%>%
  summarize(abundance_u=mean(abundance), abundance_sd=sd(abundance),abundance_max=max(abundance),abundance_n=length(year), present=max(present)) %>%
  filter(abundance_u>=0)%>% #only records that mention spp
  arrange(year)%>%
  ungroup()%>%
  glimpse()
d5b
write_csv(d5b,"./results/kelp_historical_sp_abundance_location_author.csv")




# summarize by species, location #######--------------------------

# location3 -------------
d6<-d3%>%
  group_by(year,location3,organism,genus,source,author)%>%
  summarize(
    abundance=mean(abundance,na.rm=T), # one value per year/author
    n=n())%>%
  group_by(year,location3,organism,genus,source)%>%
  summarize(abundance_u=mean(abundance,na.rm=T), 
            abundance_sd=sd(abundance,na.rm=T),
            abundance_max=max(abundance),
            abundance_n=length(year), 
            author_n=length(unique(author))) %>%
  arrange(year)%>%
  ungroup()%>%glimpse()
d6 
write_csv(d6,"./results/kelp_historical_sp_abundance_location23.csv") 

# location -------------
d6b<-d3%>%
  group_by(year,location,organism,genus,source,author)%>%
  summarize(
    abundance=mean(abundance,na.rm=T), # one value per year/author
    n=n())%>%
  group_by(year,location,organism,genus,source)%>%
  summarize(abundance_u=mean(abundance,na.rm=T), 
            abundance_sd=sd(abundance,na.rm=T),
            abundance_max=max(abundance),
            abundance_n=length(year), 
            author_n=length(unique(author))) %>%
  arrange(year)%>%
  ungroup()%>%
  glimpse()
d6b 
write_csv(d6b,"./results/kelp_historical_sp_abundance_location.csv") 




# summarize by location ######################
# use d2 - general abundance not species specific abundances

# location ------------------
d7<-d2%>%
  group_by(year,location,organism,source,present,author)%>%
  summarize(
    abundance=mean(abundance1,na.rm=T), # one value per year/author
    n=n())%>%
  ungroup()%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_u=mean(abundance,na.rm=T), 
            abundance_sd=sd(abundance,na.rm=T),
            abundance_max=max(abundance),
            abundance_n=length(year), 
            author_n=length(unique(author)),
            present=max(present)) %>%
  ungroup()%>%
  arrange(year)%>%
  glimpse()
d7 
write_csv(d7,"./results/kelp_historical_abundance_location.csv") 


#stats - 
yrs<-d7%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  mutate(organism="kelp",data="HIST")%>%
  glimpse()


write_csv(yrs,"./doc/sampsz_kelp_hist_location.csv")


# location3 -----------------
d7b<-d2%>%
  group_by(year,location3,organism,source,present,author)%>%
  summarize(
    abundance=mean(abundance1,na.rm=T), # one value per year/author
    n=n())%>%
  ungroup()%>%
  group_by(year,location3,organism,source)%>%
  summarize(abundance_u=mean(abundance,na.rm=T), 
            abundance_sd=sd(abundance,na.rm=T),
            abundance_max=max(abundance),
            abundance_n=length(year), 
            author_n=length(unique(author)),
            present=max(present)) %>%
  arrange(year)
d7b 
write_csv(d7b,"./results/kelp_historical_abundance_location23.csv") 



