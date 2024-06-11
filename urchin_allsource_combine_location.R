# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to combine oral history, historical, and contemporary data sources
# for all location levels
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# contemporary data
d1_1<-read_csv("./results/urchin_contemporary_abundance_location.csv")%>%
  mutate(source="Contemporary Data")%>%
  mutate(present=ifelse(density_m2_u>0,1,0))%>%
  glimpse()

d1_3<-read_csv("./results/urchin_contemporary_abundance_location3.csv")%>%
  mutate(source="Contemporary Data")%>%
  mutate(present=ifelse(density_m2_u>0,1,0))%>%
  glimpse()


#filter contemporary data for purple only
d1p_1<-d1_1%>%
  filter(genus=="Strongylocentrotus")
d1p_3<-d1_3%>%
  filter(genus=="Strongylocentrotus")

# historical and oral history data
d2_1<-read_csv("./results/urchin_hoh_purple_abundance_location.csv")%>%glimpse()
d2_3<-read_csv("./results/urchin_hoh_purple_abundance_location3.csv")

#########################################################
# organize data so formatted the same ###############
# names(d1p_1)
# names(d2_1)

d3_1<-d1p_1%>%
  dplyr::select(year:organism,genus,urchin_source=source,urchin_abundance_u=abundance_u,urchin_abundance_n=density_n,urchin_present=present)

d3_3<-d1p_3%>%
  dplyr::select(year:organism,genus,urchin_source=source,urchin_abundance_u=abundance_u,urchin_abundance_n=density_n,urchin_present=present)

d4_1<-d2_1%>%
  dplyr::select(year:organism,genus,urchin_source=source,urchin_abundance_u=abundance_u,urchin_abundance_n=abundance_n,urchin_present=present)%>%
  glimpse()

d4_3<-d2_3%>%
  dplyr::select(year:organism,genus,urchin_source=source,urchin_abundance_u=abundance_u,urchin_abundance_n=abundance_n,urchin_present=present)%>%
  glimpse()

#combine different datasets
names(d3_3)
names(d4_3)

# location
d5_1<-rbind(d3_1,d4_1)%>%
  arrange(year,location)%>%
  mutate(organism="purple urchin")%>%
  glimpse()
unique(d5_1$location)

# location3
d5_3<-rbind(d3_3,d4_3)%>%
  arrange(year,location3)%>%
  mutate(organism="purple urchin")%>%
  glimpse()
unique(d5_3$location3)



# save 
write_csv(d5_1,"./results/urchin_allsources_purple_abundance_location.csv")
write_csv(d5_3,"./results/urchin_allsources_purple_abundance_location.csv.csv")

source("./bin/deets.R")

ggplot(d5_1,aes(x=year,y=urchin_abundance_u,color=urchin_source))+geom_point()+deets3
ggplot(d5_1,aes(x=year,y=urchin_abundance_u,color=urchin_source))+geom_point()+facet_grid(rows=vars(location))+deets3
