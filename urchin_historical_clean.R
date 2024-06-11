# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: clean names etc in historical urchin data
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
# LOCATION 2 AND 3 ARE THE SAME HERE #############################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# literature and field notes
# d0<-read_csv("./data/urchins_historical_20230612.csv")%>%
d0<-read_csv("./data/urchins_historical_20240308b.csv")%>%
  glimpse()

d1<-d0%>%
  select(year:author)%>%
  select(-month,-location_detail,-abundance_description,-abundance_quote,-sampling_unit,-note)%>%
  glimpse()

# checking and organizing ------------------------------------------
unique(d1$location3)
unique(d1$location)

#Ricketts so assigned to both Monterey BAy and Monterey Outer.
temp<-filter(d1,location3=="Monterey Unspecified")  
temp$location3[temp$location3=="Monterey Unspecified"]<-"Monterey Bay"

d1$location3[d1$location3=="Monterey Unspecified"]<-"Monterey Outer" 
d1a<-rbind(d1,temp)

unique(d1a$location)
unique(d1a$location3)


#remove rows from N CA - north of Monterey e.g. SF
d2<-d1a%>%
  filter(location!="Northern California")%>%
  glimpse()

# save -----------------------------------------
write_csv(d1,"./results/urchins_historical_clean.csv")

