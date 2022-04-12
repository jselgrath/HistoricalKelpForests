# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to integrate historical and oral history otter data
##################################################

# setup ########################################
library(tidyverse)

rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

# d1<-read_csv("./results/otter_oh_raw.csv")
# names(d1)
d1_1<-read_csv("./results/otter_oh_summarized_location.csv")
d1_3<-read_csv("./results/otter_oh_summarized_location3.csv")

# other sources ##############
d2_1<-read_csv("./results/otter_historical_all_location_allyears.csv")%>% 
  dplyr::select(year,location,organism,source,abundance_u=abundance)%>%
  mutate(abundance_sd=NA,abundance_n=1)
unique(d2_1$location)
range(d2_1$abundance_u)

d2_3<-read_csv("./results/otter_historical_all_location3_allyears.csv")%>% 
    dplyr::select(year,location3,organism,source,abundance_u=abundance)%>%
  mutate(abundance_sd=NA,abundance_n=1)
unique(d2_3$location3)
range(d2_3$abundance_u)


# combine data sources
d1_3
d2_3


d3_1<-rbind(d1_1,d2_1)
d3_3<-rbind(d1_3,d2_3)


# save 
write_csv(d3_3,"./results/otter_hoh_abundance_location3.csv")
write_csv(d3_1,"./results/otter_hoh_abundance_location.csv")

