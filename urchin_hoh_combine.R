# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of urchin cover using historical and oral history data
##################################################

# setup ########################################
library(tidyverse); library(ggplot2)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1_1<-read_csv("./results/urchin_oh_purple_summarized_location.csv")%>%
  mutate(abundance_n=respondent_n)%>%
  select(-respondent_n)%>%glimpse()
d1_3<-read_csv("./results/urchin_oh_purple_summarized_location3.csv")%>%
  mutate(abundance_n=respondent_n)%>%
  select(-respondent_n)%>% glimpse()

# d1a<-read_csv("./results/urchin_oh_all_summarized.csv")

d1b_1<-read_csv("./results/urchin_oh_all_summarized_location.csv")%>%
  mutate(abundance_n=respondent_n)%>%
  select(-respondent_n)%>% glimpse()
d1b_3<-read_csv("./results/urchin_oh_all_summarized_location3.csv")%>%
  mutate(abundance_n=respondent_n)%>%
  select(-respondent_n)%>% glimpse()


# other sources ##############
d2_1<-read_csv("./results/urchin_historical_purple_abundance_location.csv")%>%
  dplyr::select(year,location,organism,genus,source,abundance_max,abundance_u,abundance_sd,abundance_n,present)%>%
  glimpse()
unique(d2_1$location)


d2_3<-read_csv("./results/urchin_historical_purple_abundance_location3.csv")%>%
  glimpse()
unique(d2_3$location3)


d2b_1<-read_csv("./results/urchin_historical_abundance_location.csv")

d2b_3<-read_csv("./results/urchin_historical_abundance_location3.csv")


# combine data sources, purple urchins, location3 -----------------------
d3_1<-rbind(d1_1,d2_1)%>%glimpse()
unique(d3_1$location)

names(d1_1)
names(d2_1)


d3_3<-rbind(d1_3,d2_3)%>%
  glimpse()
unique(d3_3$location3)


# summarize by year & location, all urchins #######################
d5_1<-rbind(d1b_1,d2b_1)%>%glimpse()
d5_3<-rbind(d1b_3,d2b_3)%>%glimpse()


# save 
write_csv(d3_1,"./results/urchin_hoh_purple_abundance_location.csv")
write_csv(d3_3,"./results/urchin_hoh_purple_abundance_location3.csv")

write_csv(d5_1,"./results/urchin_hoh_abundance_location.csv")
write_csv(d5_3,"./results/urchin_hoh_abundance_location3.csv")

