# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: organize published urchin data - note  Andrews data in a separate file
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./data/urchins_data_published.csv")%>%
  mutate(location=factor(location),location3=factor(location3))%>%
  arrange(year)%>%
  glimpse()

unique(d1$reference)

# summarize by location
d2<-d1%>%
  group_by(year,location,organism,source,genus)%>%
  summarize(density_n=length(density_m2),density_m2=mean(density_m2),density_m2_sd=sd(density_m2))%>%
  arrange(genus,year,location)%>%
  glimpse()
d2

# summarize by location3
d3<-d1%>%
  group_by(year,location,location3,organism,source,genus)%>%
  summarize(density_n=length(density_m2),density_m2=mean(density_m2),density_m2_sd=sd(density_m2))%>%
  arrange(year,genus,location3)%>%
  glimpse()
d3

write_csv(d2,"./results/urchin_published_location.csv")
write_csv(d3,"./results/urchin_published_location3.csv")
