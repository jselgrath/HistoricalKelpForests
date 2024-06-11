# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

#################################################
# GOAL: to combine oral history, historical, and contemporary data sources
# for all location levels
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# contemporary data
d1_1<-read_csv("./results/pycno_contemporary_abundance_location.csv")%>%
  select(year,location,source,organism,abundance_u,n=density_n)
d1_3<-read_csv("./results/pycno_contemporary_abundance_location3.csv")%>%
  select(year,location3,source,organism,abundance_u,n=density_n)

# historical and oral history data

d2_1<-read_csv("./results/pycno_hoh_abundance_location1.csv")%>%
  select(year,location,source,organism,abundance_u,n=abundance_n)
d2_3<-read_csv("./results/pycno_hoh_abundance_location3.csv")%>%
  select(year,location3,source,organism,abundance_u,n=abundance_n)


names(d1_1)
names(d2_1)

#########################################################
#combine
d3_1<-rbind(d1_1,d2_1); d3_1
d3_3<-rbind(d1_3,d2_3); d3_3

d4_1<-d3_1%>%
  arrange(year,location)
d4_1$location[d4_1$location=="Monterey Bay"]<-"Monterey Peninsula"
d4_1$location[d4_1$location=="MontereyPeninsula"]<-"Monterey Peninsula"
d4_1$location[d4_1$location=="SantaCruz"]<-"Santa Cruz"
d4_1$location[d4_1$location=="BigSur"]<-"Big Sur"
unique(d4_1$location)


d4_3<-d3_3%>%
  arrange(year,location3)
d4_3$location3[d4_3$location3=="MontereyBay"]<-"Monterey Bay"
d4_3$location3[d4_3$location3=="MontereyCarmel"]<-"Carmel Bay"
d4_3$location3[d4_3$location3=="Monterey Carmel"]<-"Carmel Bay"
d4_3$location3[d4_3$location3=="Carmel Bay"]<-"Carmel Bay"
d4_3$location3[d4_3$location3=="Carmel"]<-"Carmel Bay"
d4_3$location3[d4_3$location3=="CarmelOuter"]<-"Carmel Outer"
d4_3$location3[d4_3$location3=="MontereyOuter"]<-"Monterey Outer"
d4_3$location3[d4_3$location3=="MontereyUnspecified"]<-"Monterey Bay"
d4_3$location3[d4_3$location3=="SantaCruz"]<-"Santa Cruz Bay"
d4_3$location3[d4_3$location3=="SantaCruz Outer"]<-"Santa Cruz Outer"
d4_3$location3[d4_3$location3=="BigSur"]<-"Big Sur"
unique(d4_3$location3)

# streamline sources
d4_1$source[d4_1$source=="Contemporary Data"]<-"Ecological Data"
d4_3$source[d4_3$source=="Contemporary Data"]<-"Ecological Data"

# update species names
d4_1<-d4_1%>%
  mutate(organism ="sunflower seastar")%>%glimpse()
d4_3<-d4_3%>%
  mutate(organism="sunflower seastar")%>%glimpse()

#graphs to look at data
# Santa Cruz 2000 is an outlier. In PISCO data
source("./bin/deets.R")

ggplot(d4_1,aes(x=year,y=abundance_u,color=source))+geom_point()+deets3
ggplot(d4_1,aes(x=year,y=abundance_u,color=source))+geom_point()+facet_grid(rows=vars(location))+deets3

ggplot(d4_1,aes(x=year, y=abundance_u,fill=source)) + geom_point()+
  facet_grid(rows=vars(location))+deets4

# save 
write_csv(d4_1,"./results/pycno_allsources_abundance_location.csv")
write_csv(d4_3,"./results/pycno_allsources_abundance_location3.csv")



