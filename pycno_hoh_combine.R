# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: combine historical and oral history data about pycnopodia
##################################################

# setup ########################################
library(tidyverse); library(ggplot2)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# oral history data
# Note: this was recalibrated to match ecological data 2/28/2021
# Possibly should comine categories 2/3 because unclear that observers could identify differences
d1_1<-read_csv("./results/pycno_oh_summarized_location.csv")%>%
  select(-respondent_n,-abundance_max)%>%glimpse()
d1_3<-read_csv("./results/pycno_oh_summarized_location3.csv")%>%
  select(-respondent_n,-abundance_max)%>%glimpse()

# historical sources ##############
d2_1<-read_csv("./results/pycno_historicaldescriptions_location.csv")%>%
  mutate(present=ifelse(abundance_u>0,1,0),source="Archival Data")%>%
  select(year,location,organism,source,abundance_u,abundance_sd,abundance_n,present)%>%
    glimpse()
  
unique(d2_1$location)
unique(d2_1$source)

d2_3<-read_csv("./results/pycno_historicaldescriptions_location3.csv")%>%
  mutate(present=ifelse(abundance_u>0,1,0),source="Archival Data")%>%
  select(year,location3,organism,source,abundance_u,abundance_sd,abundance_n,present)%>%
  glimpse()
unique(d2_3$location3)

# combine data sources
names(d1_1);names(d2_1)
d3_1<-rbind(d1_1,d2_1)
d3_3<-rbind(d1_3,d2_3)

# save 
write_csv(d3_1,"./results/pycno_hoh_abundance_location1.csv")
write_csv(d3_3,"./results/pycno_hoh_abundance_location3.csv")
