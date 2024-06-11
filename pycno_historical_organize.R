# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# GOAL: organize historical data 
# GOAL: calculate means for historical descriptions pycnopodia data
##################################################

# setup ########################################
library(tidyverse); library(tidyr)
################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# literature and field notes
d1<-read_csv("./results/pycno_historical_allsources_clean.csv")%>%
  glimpse()

unique(d1$source)

# summary by location
d2<-d1%>%
  group_by(year,organism,location,source,author)%>%
  summarize(abundance=mean(abundance1),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year,location,organism,source,present)%>%
  summarise(abundance_u=mean(abundance, na.rm=T), 
            abundance_sd=sd(abundance, na.rm=T),
            abundance_n=n())%>%
  arrange(year)%>% 
  glimpse()


write_csv(d2,"./results/pycno_historical_location.csv")

#stats - source = reference here
d2a<-d2%>%
  select(location,year)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  mutate(organism="sunfower seastar",data="HIST")%>%
  glimpse

unique(d2$location)
write_csv(d2a,"./doc/sampsz_pycno_hist_all_location.csv")


# summary by location3------------------------------
d2<-d1%>%
  group_by(year,organism,location3,source,author)%>%
  summarize(abundance=mean(abundance1),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year,location3,organism,source,present)%>%
  summarise(abundance_u=mean(abundance, na.rm=T), 
            abundance_sd=sd(abundance, na.rm=T),
            abundance_n=n())%>%
  arrange(year)%>% 
  glimpse()


write_csv(d2,"./results/pycno_historical_location3.csv")



# descriptive data by location & year -----------------------------
d4<-d1%>%
  filter(source=="Archival Data")%>%
  group_by(year,organism,location,source,author)%>%
  summarize(abundance=mean(abundance1),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year,organism,location,source,present)%>% 
  summarise(abundance_u=mean(abundance, na.rm=T), 
            abundance_sd=sd(abundance, na.rm=T),
            abundance_n=n())%>%
  arrange(year)%>% 
  glimpse()
d4

write_csv(d4,"./results/pycno_historicaldescriptions_location.csv")



# descriptive data by location3 & year ####################
d6<-d1%>%
  filter(source=="Archival Data")%>%
  group_by(year,organism,location3,source,author)%>%
  summarize(abundance=mean(abundance1),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year,organism,location3)%>% 
  summarise(abundance_u=mean(abundance, na.rm=T), 
            abundance_sd=sd(abundance, na.rm=T),
            abundance_n=n())%>%
  arrange(year)%>% 
  glimpse()
d6

write_csv(d6,"./results/pycno_historicaldescriptions_location3.csv")


# ------------------------------------------------------------
# historical quant ######
# -------------------------------------------------------------

# Summarized historical quantitative density data by location & year ####################
d3<-d1%>%
  filter(source=="Published Surveys")%>%
  group_by(year,organism,location,source,author)%>%
  summarize(density_m2=mean(density_m2),
            present=max(present))%>% # summarize by source for each location
  ungroup()%>%
  group_by(year,organism,location,source,present)%>% 
  arrange(year)%>% 
  summarise(density_u=mean(density_m2, na.rm=T), 
            density_sd=sd(density_m2, na.rm=T),
            density_n=length(density_m2))%>%
  glimpse()
  
  write_csv(d3,"./results/pycno_historical_density_location.csv")



# historical quantitative density by location3 & year ---------------------
d5<-d1%>%
    filter(source=="Published Surveys")%>%
    group_by(year,organism,location3,source,author)%>%
    summarize(density_m2=mean(density_m2),
              present=max(present))%>% # summarize by source for each location
    ungroup()%>%
  group_by(year,organism,location3,source,present)%>% 
  arrange(year)%>% 
  summarise(density_u=mean(density_m2, na.rm=T), 
            density_sd=sd(density_m2, na.rm=T),
            density_n=length(density_m2))%>%
  dplyr::select(year, organism, location3, density_u, density_sd,density_n,source)%>%
    glimpse()

write_csv(d5,"./results/pycno_historical_density_location3.csv")
