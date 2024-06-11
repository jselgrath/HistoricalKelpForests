# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to integrate oral history and other historical data

##################################################
library(tidyverse)
library(scales)
library(ggthemes) #tableau colors
#####################################################
rm(list=ls())
dateToday=Sys.Date()

# oral history data 
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# location
d1_1<-read_csv("./results/kelp_oh_abundance_location.csv")%>% 
  dplyr::select(year:source,present,abundance_u:abundance_n,reference_n=respondent_n)%>%
  filter(location!="Other")%>%
  glimpse()# reference_n & respondent_n are parallel measures 

d1s_1<-read_csv("./results/kelp_oh_sp_abundance_location.csv")%>% # species data
  filter(location!="Other")%>%
  dplyr::select(year:source,abundance_u:abundance_n,reference_n=respondent_n)%>%
  glimpse()

#location3
d1_3<-read_csv("./results/kelp_oh_abundance_location3.csv")%>% 
  filter(location3!="Other")%>%
  dplyr::select(year:source,present,abundance_u:abundance_n,reference_n=respondent_n)%>%glimpse() # reference_n & respondent_n are parallel measures 

d1s_3<-read_csv("./results/kelp_oh_sp_abundance_location3.csv")%>% # species data
  filter(location3!="Other")%>%
  dplyr::select(year:source,abundance_u:abundance_n,reference_n=respondent_n)%>%glimpse()


# historic data by site ##########
# location
d2_1<-read_csv ("./results/kelp_historical_abundance_location.csv")%>% 
  select(year:source,present,abundance_u:abundance_n,reference_n=author_n)%>%
  filter(location!="Other")%>%
  glimpse()

d2s_1<-read_csv("./results/kelp_historical_sp_abundance_location.csv")%>% 
  select(year:source,abundance_u:abundance_n,reference_n=author_n)%>%
  filter(location!="Other")%>%
  glimpse()

#location23 are the same here
d2_3<-read_csv("./results/kelp_historical_abundance_location23.csv")%>%
  select(year:source,present,abundance_u:abundance_n,reference_n=author_n)%>%
  # filter(location3!="California")%>%
  glimpse()

d2s_3<-read_csv("./results/kelp_historical_sp_abundance_location23.csv")%>%
  select(year:source,abundance_u:abundance_n,reference_n=author_n)%>%
  # filter(location3!="California")%>%
  glimpse()

d1s_1
d2s_1

# combine data sources

d3_1<-rbind(d1_1,d2_1)    #location 
d3s_1<-rbind(d1s_1,d2s_1)

d3_3<-rbind(d1_3,d2_3)    #location3
d3s_3<-rbind(d1s_3,d2s_3)

# check location2&3, change unspecified to monterey bay
unique(d3_1$location)
unique(d3s_1$location)


unique(d3_3$location3)
unique(d3s_3$location3)
d3_3$location3[d3_3$location3=="Monterey Unspecified"]<-"Monterey Bay"
d3s_3$location3[d3s_3$location3=="Monterey Unspecified"]<-"Monterey Bay"


# check location
unique(d3_3$location3)

# check genera spelling
unique(d3s_1$genus)
unique(d3s_1$genus)

# save combined data
write_csv(d3_1,"./results/kelp_hoh_abundance_location.csv")
write_csv(d3s_1,"./results/kelp_hoh_sp_abundance_location.csv")

write_csv(d3_3,"./results/kelp_hoh_abundance_location3.csv")
write_csv(d3s_3,"./results/kelp_hoh_sp_abundance_location3.csv")
