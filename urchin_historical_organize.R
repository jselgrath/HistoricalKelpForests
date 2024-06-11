# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: organize and summarize historical urchin data
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
# LOCATION 2 AND 3 ARE THE SAME HERE #############################

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# literature and field notes
d1<-read_csv("./results/urchins_historical_updated_abund.csv")%>%
  mutate(organism="urchin")%>% # remove dash which is useful for paper
  glimpse()


# ---------------------------------------
# SUMMARIAZING DATA ################
# ---------------------------------------

# ---------------------------------------
# all urchins ----------------------------
# ---------------------------------------

# Summarized historical data by location & year FOR ALL SPECIES ---------------------------------------
d3<-d1%>%
  group_by(year,location,organism,source,author)%>%
  summarize(abundance=mean(abundance),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year,location,organism,source,present)%>% 
    summarise(abundance_max=max(abundance, na.rm=T),
              abundance_u=mean(abundance, na.rm=T), 
              abundance_sd=sd(abundance, na.rm=T),
              abundance_n=length(year))%>%
  # dplyr::select(year, location, organism,present,abundance_max,abundance_u, abundance_sd,abundance_n,source)%>%
  arrange(year,location)%>% 
  glimpse()
d3

# save summarized at location level
write_csv(d3,"./results/urchin_historical_abundance_location.csv")


# Summarized historical data by location3 & year ####################
d4<-d1%>%
  group_by(year,location3,organism,source,author)%>%
  summarize(abundance=mean(abundance),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year,location3,organism,source,present)%>% 
  arrange(year)%>% 
  summarise(abundance_max=max(abundance, na.rm=T),abundance_u=mean(abundance, na.rm=T), abundance_sd=sd(abundance, na.rm=T),abundance_n=length(year))%>%
  glimpse()


# save summarized at location23 level
write_csv(d4,"./results/urchin_historical_abundance_location3.csv")


# summarize historical data by year #######################
d5<-d1%>%
  group_by(year,organism,source,author)%>%
  summarize(abundance=mean(abundance),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year, organism, source,present)%>%
  arrange(year)%>% 
  summarise(abundance_max=max(abundance, na.rm=T),abundance_u=round(mean(abundance, na.rm=T),1), abundance_sd=round(sd(abundance, na.rm=T)),abundance_n=length(year))%>%
  glimpse()
d5

# save 
write_csv(d5,"./results/urchin_historical_abundance.csv")

 

# ---------------------------------------------------------------
# purple urchins only #########
# ---------------------------------------------------------------
unique(d1$genus)

# Summarized historical data by location & year ####################
d6<-d1%>%
  filter(genus=="Strongylocentrotus" | genus=="unspecified" )%>%
  group_by(year,location,organism,source,author)%>%
  summarize(abundance=mean(abundance),
            present=max(present))%>% # summarize by source for each author
  ungroup()%>%
  group_by(year,location,organism, source,present)%>% 
  summarise(abundance_max=max(abundance, na.rm=T),abundance_u=mean(abundance, na.rm=T), abundance_sd=sd(abundance, na.rm=T),abundance_n=length(year))%>%
  mutate(organism="purple urchin",genus="Strongylocentrotus")%>%
  arrange(year)%>% 
  ungroup()%>%
  glimpse()


# save summarized at location level
write_csv(d6,"./results/urchin_historical_purple_abundance_location.csv")


# Summarized historical data by location3 & year ####################
d6b<-d1%>%
  filter(genus=="Strongylocentrotus" | genus=="unspecified" )%>%
  group_by(year,location3,organism,source,author)%>%
  summarize(abundance=mean(abundance),
            present=max(present))%>% # summarize by source for each author
  ungroup()%>%
  group_by(year,location3,organism, source,present)%>% 
  summarise(abundance_max=max(abundance, na.rm=T),abundance_u=mean(abundance, na.rm=T), abundance_sd=sd(abundance, na.rm=T),abundance_n=length(year))%>%
  mutate(organism="purple urchin",genus="Strongylocentrotus")%>%
  arrange(year)%>% 
  ungroup()%>%
  glimpse()

# save summarized at location level
write_csv(d6b,"./results/urchin_historical_purple_abundance_location3.csv")


#stats - source = reference here-----------------------
yrs<-d6%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  mutate(organism="purple urchin",data="HIST")%>%
  glimpse()

write_csv(yrs,"./doc/urchin_hist_yr_n.csv")


