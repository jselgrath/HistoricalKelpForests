# Jennifer Seltrath
# HMS

# to merge historical sources for all spp
# -----------------------------------------------
library(tidyverse)
# -------------------------------------------
remove(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./data/otter_historical_long_20230702.csv")%>%
  dplyr::select(year,location,organism,source,data_type,abundance=abundance1,author,reference)%>%
  mutate(density_m2=NA)%>%
  dplyr::select(year:abundance,density_m2,author,reference)%>%
  glimpse()

d2<-read_csv("./data/pycnopodia_historical_long_20240308.csv")%>%
  dplyr::select(year,location,organism,source,data_type,abundance=abundance1,density_m2,author,reference)%>%
  glimpse()

d3<-read_csv("./data/urchins_historical_20240308b.csv")%>%
  dplyr::select(year,location,organism,source,data_type,abundance=abundance1,density_m2,author,reference)%>%
  glimpse()

d4<-read_csv("./data/kelp_historical_expanded_20230612.csv")%>%
  dplyr::select(year,location,organism,source,data_type,abundance=abundance1,author,reference)%>%
  mutate(density_m2=NA)%>%
  dplyr::select(year:abundance,density_m2,author,reference)%>%
  glimpse()

d5<-rbind(d1,d2,d3,d4)%>%
  arrange(organism,year,location,reference)%>%
  glimpse()
d5

write_csv(d5,"./doc/TableSI7_2024.csv")
