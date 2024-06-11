# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to organize reef check urchin data
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(dplyr)  

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# REEF CHECK DATA - locations assigned in GIS
d1<- read_csv("./data/reefcheckCA_invert_location3.csv")%>% 
  filter(Classcode=="purple urchin"| Classcode=="red urchin", Amount >=0)%>% #note names cleaned in excel 
  mutate(density_m2=Amount/Distance)%>% #core RC transects are 30x2
  dplyr::select(year=Year,organism=Classcode,location,location2,location3,Transect , Amount,Distance,density_m2)%>%
  arrange(year,location3)
d1$source<-"Contemporary Data"
d1$genus<-"Strongylocentrotus"
d1$genus[d1$organism=="red urchin"]<-"Mesocentrotus"
range(d1$Amount) #includes 0s
range(d1$year)
d1
write_csv(d1,"./results/urchin_RC_density_transect.csv")

##############################
# summarize Reef Check
# grand mean = multiply mean by number of data points

# summarize mean values by site and year
# see digital p202 of cochranes handbook for systematic reviews of intervientions

# Summarized by location #############
d2<- d1%>%  
  group_by(year,location,organism,source,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))%>% 
  arrange(year,location)
d2
write_csv(d2,"./results/urchin_RC_density_location.csv")

# graph
source("./bin/deets.R")

ggplot(d2,aes(x=year, y=density_m2_u)) + geom_point()+
  facet_grid(rows=vars(location))+deets4

# summarized by location3
d4<- d1%>%  
  group_by(year,location3,organism,source,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))%>% 
  arrange(year,location3)

d4
write_csv(d4,"./results/urchin_RC_density_location3.csv")

# summarized by all sites, by year
d5<- d1%>%  
  group_by(year,organism,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))%>% 
  arrange(year)

d5
write_csv(d5,"./results/urchin_RC_density_allsites.csv")

# summarized by all sites & years
d6<- d1%>%  
  group_by(organism,genus)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))

d6
write_csv(d6,"./results/urchin_RC_density_allsitesyears.csv")
