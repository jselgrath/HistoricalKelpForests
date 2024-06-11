# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to organize oral history pycnopodia data for graphing and analyzing
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/")

d1<-read_csv("./OralHistories/results/otter_kelp_oh_subset_long.csv")
names(d1)

# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

########################################
# set source
d1$source<-"Oral Histories"

# summarize at location level for all pycnopodia########################
d2<-d1%>%
  filter(organism=="sea stars" & genus == "Pycnopodia") #479 sea star records, 107 pycno
d2


d2$location0<-d2$location
d2$location<-d2$location2
d2$location [d2$location=="Monterey Bay"]<-"Monterey Peninsula"
unique(d2$location)

#summarize by diff location levels ----------------------------
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


#stats
d3_1b<-d3_1%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
  yr_1=min(year),
  yr_2=max(year),
  n=n())%>%
  mutate(organism="sunfower seastar",data="OH")%>%
  glimpse()

write_csv(d3_1b,"./doc/sampsz_pycno_oh_location.csv")

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

write_csv(d2,"./results/pycno_oh_raw.csv")

write_csv(d3_1,"./results/pycno_oh_summarized_location.csv")
write_csv(d3_3,"./results/pycno_oh_summarized_location3.csv")


d3_1


# make sure this matches work computer version 20230529
d4<-d3_1%>%
  select(year,location,organism, data_present=present)%>%
  glimpse()

write_csv(d4,"./results/pycno_oh_presence_data.csv")

# graph
source("./bin/deets.R")
ggplot(d3_1,aes(x=year,y=abundance_u,color=location))+geom_smooth()+deets3+ylim(c(0,5))
range(d3_3$abundance_u,na.rm=T)


