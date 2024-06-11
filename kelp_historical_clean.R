# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
##################################################
# Goal: to summarize estimates of historic kelp
# 
##################################################
library(tidyverse); library(dplyr)
library(scales)

####################################################
rm(list=ls())
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

dateToday=Sys.Date()

d1<-read_csv("./data/kelp_historical_expanded_20230612.csv")%>% # i expanded years manually since not so many...  # v2 has cleaned names that are easier for R to handle # this dataset is integrated with older sources post-wsn 2019
  glimpse()
d1

d2<-d1%>% 
  select(year:author)%>%
  select(-month,-location_detail,-sampling_unit)%>%
  glimpse()

levels(as.factor(d2$location))


# set abundance levels ############
# Nereocystis

d2$abundance.nereo2<-NA
d2$abundance.nereo2[d2$nereocystis=="high"]<-5
d2$abundance.nereo2[d2$nereocystis=="numerous"]<-4
d2$abundance.nereo2[d2$nereocystis=="medium-high"]<-4
d2$abundance.nereo2[d2$nereocystis=="medium"]<-3
d2$abundance.nereo2[d2$nereocystis=="common"]<-3
d2$abundance.nereo2[d2$nereocystis=="present"]<-3
d2$abundance.nereo2[d2$nereocystis=="low-medium"]<-2
d2$abundance.nereo2[d2$nereocystis=="patchy"]<-1
d2$abundance.nereo2[d2$nereocystis=="absent"]<-0
levels(as.factor(d2$abundance.nereo2))


#Macrocystis
levels(as.factor(d2$macrocystis))
d2$abundance.macro2<-NA
d2$abundance.macro2[d2$macrocystis=="high"]<-5
d2$abundance.macro2[d2$macrocystis=="numerous"]<-4
d2$abundance.macro2[d2$macrocystis=="medium-high"]<-4
d2$abundance.macro2[d2$macrocystis=="medium"]<-3
d2$abundance.macro2[d2$macrocystis=="common"]<-3
d2$abundance.macro2[d2$macrocystis=="present"]<-3
d2$abundance.macro2[d2$macrocystis=="low-medium"]<-2
d2$abundance.macro2[d2$macrocystis=="patchy"]<-1
d2$abundance.macro2[d2$macrocystis=="low"]<-1
d2$abundance.macro2[d2$macrocystis=="absent to rare"]<-1
d2$abundance.macro2[d2$macrocystis=="absent"]<-0
levels(as.factor(d2$abundance.macro2))

glimpse(d2)

d3<-d2%>%
  mutate(abundance.nereo=nereocystis,
         Nereocystis=abundance.nereo2,
         abundance.macro=macrocystis,
         Macrocystis=abundance.macro2)%>%
  select(-abundance.nereo2,-abundance.macro2,-abundance.macro,-abundance.nereo,-nereocystis,-macrocystis)%>%
  arrange(year)%>%
  glimpse()


# save cleaned version
write_csv(d3,"./results/kelp_historical_expanded_clean.csv")

