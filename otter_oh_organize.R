# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: oral history otter data
##################################################

# setup ########################################
library(tidyverse);
rm(list=ls())

#################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/")

d1<-read_csv("./OralHistories/results/otter_kelp_oh_subset_long.csv")%>%
  filter(location3!="Other")
names(d1)
unique(d1$location)
unique(d1$location3)

# set wd to kelp project
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1$source<-"Oral Histories"

# summarize at location level for all otters########################
d2<-d1%>%
  filter(organism=="sea otters")

# clean location names -------------------------------------------
unique(d2$location) # not cleaned
d2$location0<-d2$location # save uncleaned locations to another variable name
d2$location<-d2$location2
d2$location[d2$location=="Monterey Bay" | d2$location=="Monterey Outer" | d2$location=="MontereyBay" | d2$location=="MontereyOuter" | d2$location=="MontereyCarmel" | d2$location=="MontereyUnspecified" | d2$location== "Carmel"] <- "Monterey Peninsula"
d2$location[d2$location=="BigSur"]<-"Big Sur"
d2$location[d2$location=="SantaCruz"]<-"Santa Cruz"
          
d2$location3[d2$location3=="MontereyUnspecified"]<-"Monterey Bay"

unique(d2$location) # cleaned



# Summarize by location level ---------------------------------------
# location3




# location3
d3<-d2%>%
  group_by(ID2,year,organism,location3,source)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location3,organism,source)%>%
  summarize(abundance_u=round(mean(abundance2),1), abundance_sd=sd(abundance2),abundance_max=round(max(abundance2),1),abundance_n=length(year),respondent_n=length(unique(ID2)),present=max(present2))%>%
  arrange(year,location3)%>%
  ungroup()%>%
  glimpse()

unique(d3$location3) # cleaned
d3
tail(d3)



# location
d5<-d2%>%
  group_by(ID2,year,organism,location,source)%>%
  summarize(present2=max(present), abundance2=mean(abundance))%>% # remove species duplicates by respondent
  ungroup()%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_u=round(mean(abundance2),1), abundance_sd=sd(abundance2),abundance_max=round(max(abundance2),1),abundance_n=length(year),respondent_n=length(unique(ID2)),present=max(present2))%>%
  arrange(year,location)%>%
  ungroup()%>%
  glimpse()

#stats - source = reference here
d5b<-d5%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  mutate(organism="otters",data="OH")%>%
  glimpse
write_csv(d5b,"./doc/sampsz_otter_oh_location.csv")


write_csv(d2,"./results/otter_oh_raw.csv")
write_csv(d3,"./results/otter_oh_summarized_location3.csv")
write_csv(d5,"./results/otter_oh_summarized_location.csv")

