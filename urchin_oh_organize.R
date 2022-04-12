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
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/")

d1<-read_csv("./OralHistories/results/otter_kelp_oh_subset_long.csv")
names(d1)

setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")

########################################
# set source
d1$source<-"Oral Histories"

# summarize at location level for all urchins########################
d2<-d1%>%
  filter(organism=="urchins")
d2
d2$location0<-d2$location
# d2$location<-d2$location2
# d2$location2 [d2$location2=="MontereyUnspecified"]<-"MontereyBay"
d2$location [d2$location=="Monterey Outer"|d2$location=="MontereyOuter"|d2$location=="MontereyBay"| d2$location=="Monterey Bay"| d2$location=="MontereyCarmel" | d2$location=="MontereyUnspecified" | d2$location=="Carmel"]<-"Monterey Peninsula"
unique(d2$location0)

#summarize by diff location levels
d3_1<-d2%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))

d3_3<-d2%>%
  group_by(year,location3,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))



# summarize at location level for purple urchins########################
d4_1<-d2%>%
  filter(genus=="Strongylocentrotus")%>%
  group_by(year,location,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))

d4_3<-d2%>%
  filter(genus=="Strongylocentrotus")%>%
  group_by(year,location3,organism,genus,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))


# all urchins all sites combined
d5<-d2%>%
  group_by(year,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))

d5
tail(d5)

write_csv(d2,"./results/urchin_oh_all_raw.csv")

write_csv(d3_1,"./results/urchin_oh_all_summarized_location.csv")
write_csv(d4_1,"./results/urchin_oh_purple_summarized_location.csv")
write_csv(d3_3,"./results/urchin_oh_all_summarized_location3.csv")
write_csv(d4_3,"./results/urchin_oh_purple_summarized_location3.csv")

write_csv(d5,"./results/urchin_oh_all_summarized.csv")