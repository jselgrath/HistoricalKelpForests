# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to organize oral history urchin data for graphing and analyzing
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/")

d1<-read_csv("./OralHistories/results/otter_kelp_oh_subset_long.csv")%>%
  filter(organism=="urchins")%>%
  glimpse()
names(d1)

# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp/")

########################################
# set source
d1$source<-"Oral Histories"

# summarize at location level for all urchins########################
d2<-d1
d2$location0<-d2$location
d2$location<-d2$location2
d2$location2 [d2$location2=="MontereyUnspecified"]<-"MontereyBay"
d2$location [d2$location=="Monterey Outer"|d2$location=="MontereyOuter"|d2$location=="MontereyBay"| d2$location=="Monterey Bay"| d2$location=="MontereyCarmel" | d2$location=="MontereyUnspecified" | d2$location=="Carmel"]<-"Monterey Peninsula"
unique(d2$location)

# -----------------------------------------------------------
# ALL URCHINS ###############################################
#summarize by diff location levels --------------------------

# location
d3_1<-d2%>%
  group_by(ID2,year,organism,location,source)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_u=round(mean(abundance2),1), abundance_sd=sd(abundance2),abundance_max=round(max(abundance2),1),abundance_n=length(year),respondent_n=length(unique(ID2)),present=max(present2))%>%
  arrange(year,location)%>%
  ungroup()%>%
  glimpse()

# location3
d3_3<-d2%>%
  group_by(ID2,year,organism,location3,source)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location3,organism,source)%>%
  summarize(abundance_u=round(mean(abundance2),1), abundance_sd=sd(abundance2),abundance_max=round(max(abundance2),1),abundance_n=length(year),respondent_n=length(unique(ID2)),present=max(present2))%>%
  arrange(year,location3)%>%
  ungroup()%>%
  glimpse()


# --------------------------------------------------
# Purple urchins ###########
# ----------------------------------------------------

# location level for purple urchins----------------------
d4_1<-d2%>%
  filter(genus=="Strongylocentrotus")%>%
  group_by(ID2,year,organism,genus,location,source)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance2),1), abundance_sd=sd(abundance2),abundance_max=round(max(abundance2),1),abundance_n=length(year),respondent_n=length(unique(ID2)),present=max(present2))%>%
  arrange(year,location)%>%
  ungroup()%>%
  glimpse()

#stats - source = reference here
d4_1b<-d4_1%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  mutate(organism="purple urchins",data="OH")%>%
  glimpse

write_csv(d4_1b,"./doc/sampsz_urchin_purple_oh_location.csv")


# location 3 -------------
d4_3<-d2%>%
  filter(genus=="Strongylocentrotus")%>%
  group_by(ID2,year,organism,genus,location3,source)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location3,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance2),1), abundance_sd=sd(abundance2),abundance_max=round(max(abundance2),1),abundance_n=length(year),respondent_n=length(unique(ID2)),present=max(present2))%>%
  arrange(year,location3)%>%
  ungroup()%>%
  glimpse()




# save -------------------------

write_csv(d2,"./results/urchin_oh_all_raw.csv")

write_csv(d3_1,"./results/urchin_oh_all_summarized_location.csv")
write_csv(d4_1,"./results/urchin_oh_purple_summarized_location.csv")
write_csv(d3_3,"./results/urchin_oh_all_summarized_location3.csv")
write_csv(d4_3,"./results/urchin_oh_purple_summarized_location3.csv")
