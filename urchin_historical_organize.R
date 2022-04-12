# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: organize historical urchin data
# set values to match breaks from abc reef data
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
# LOCATION 2 AND 3 ARE THE SAME HERE #############################


setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

# literature and field notes
d1a<-read_csv("./data/urchins_historical.csv")
d1<-filter(d1a,abundance3>=0) #remove rows with no abundance information

# OLD decision rules about abundance: now using breaks based on ABC reef data
# < 2/m2 = 1; 2-3/m3=2; 3-7/m2=4; 7-14/m2=4; >14/m2=5

# standardize source
d1$source<-"Literature, Journals, Field Notes"
d1$source[d1$Author=="Andrews"]<-"Ecological Data"
d1$source[d1$Author=="Provin, Bruce"]<-"Ecological Data"
d1$source[d1$Author=="Hewatt"]<-"Ecological Data"
d1$organism<-"urchins"

unique(d1$location)
d1$location[d1$location=="California (note probably not Monterey - sites were northward)"]<-"Northern CA"
d1$location2<-d1$location
d1$location2[d1$location2=="Pacific Grove"|d1$location=="Monterey"]<-"MontereyBay"
d1$location2[d1$location2=="Point Pinos"]<-"MontereyOuter"
d1$location2[d1$location2=="Carmel"]<-"Carmel"
unique(d1$location2)

d1$location [d1$location=="Pacific Grove"|d1$location=="Monterey"| d1$location=="Carmel"]<-"MontereyPeninsula"
d1$location[d1$location=="Point Pinos"]<-"MontereyPeninsula"
unique(d1$locationdetail)

d1<-arrange(d1,year,location)
d1

################################
# Set relative abundance levels to match abc reef breaks (as of June 2, 2020)
# Relative abundance breaks (urchins/m2): 
dens_breaks<-c(0.0000000, 0.9848838, 2.9851820, 4.9854803, 8.9860767)
d2<-d1

# set abundance ######################
# location
d2$abundance3[d2$density_m2==dens_breaks[1]]<-0
d2$abundance3[d2$density_m2>dens_breaks[1]]<-1 # greater than 0
d2$abundance3[d2$density_m2>=dens_breaks[2]]<-2 # between 0 and mean
d2$abundance3[d2$density_m2>=dens_breaks[3]]<-3 #halfway between median and 1 sd
d2$abundance3[d2$density_m2>=dens_breaks[4]]<-4
d2$abundance3[d2$density_m2>=dens_breaks[5]]<-5
d2
tail(d2)

d2$abundance_u=d2$abundance3
# cbind(d1$abundance3,d2$abundance3) #most cases the same. for a handful of cases d2 version is slightly higher.

###########################
# data sources that I feel so so about


#remove stephensons? they visited pacific grove in sept/oct 1947
d2b<-d2%>%
  filter(Author!="Stephenson and Stephenson")

#remove andrews?
d2c<-d2%>%
  filter(Author!="Andrews")

# view(d1)

# only keep updated abundance value
d2d<-d2c%>%
  dplyr::select(-abundance3,-abundance)
write_csv(d2d,"./results/urchins_historical_clean.csv")



# Summarized historical data by location & year ####################
d3<-d2c%>%
  group_by(year,location,source,organism)%>% 
  arrange(year)%>% 
  summarise(abundance_u=mean(abundance3, na.rm=T), abundance_sd=sd(abundance3, na.rm=T),abundance_n=length(year))%>%
  dplyr::select(year, location, organism,abundance_u, abundance_sd,abundance_n,source)
d3

# save summarized at location level
write_csv(d3,"./results/urchin_historical_abundance_location.csv")


# Summarized historical data by location2 & year ####################
d4<-d2c%>%
  group_by(year,location2,source,organism)%>% 
  arrange(year)%>% 
  summarise(abundance_u=mean(abundance3, na.rm=T), abundance_sd=sd(abundance3, na.rm=T),abundance_n=length(year))%>%
  dplyr::select(year, location2, organism,abundance_u, abundance_sd,abundance_n,source)
d4

# save summarized at location23 level
write_csv(d4,"./results/urchin_historical_abundance_location23.csv")


# summarize historical data by year #######################
d5<-d2c%>%
  group_by(year, source,organism)%>%
  arrange(year)%>% 
  summarise(abundance_u=round(mean(abundance3, na.rm=T),1), abundance_sd=round(sd(abundance3, na.rm=T)),abundance_n=length(year))%>%
  dplyr::select(year, organism,abundance_u, abundance_sd,abundance_n,source)
d5

# save 
write_csv(d5,"./results/urchin_historical_abundance.csv")

 
# purple urchins only ########################################
# Summarized historical data by location & year ####################
d6<-d2c%>%filter(species=="Strongylocentrotus_purpuratus" | species=="both" )%>%
  group_by(year,location,source,organism)%>% #,locationdetail Author,
  summarise(abundance_u=mean(abundance3, na.rm=T), abundance_sd=sd(abundance3, na.rm=T),abundance_n=length(year))%>%
  arrange(year)%>% 
  dplyr::select(year, location, organism,abundance_u, abundance_sd,abundance_n,source)
d6$genus<-"Strongylocentrotus"
d6
# save summarized at location level
write_csv(d6,"./results/urchin_historical_purple_abundance_location.csv")


# Summarized historical data by location & year ####################
d7<-d2c%>%filter(species=="Strongylocentrotus_purpuratus" | species=="both" )%>%
  group_by(year,location2,source,organism)%>% #,locationdetail Author,
  summarise(abundance_u=mean(abundance3, na.rm=T), abundance_sd=sd(abundance3, na.rm=T),abundance_n=length(year))%>%
  arrange(year)%>% 
  dplyr::select(year, location2, organism,abundance_u, abundance_sd,abundance_n,source)
d7$genus<-"Strongylocentrotus"
d7

# save summarized at location level
write_csv(d7,"./results/urchin_historical_purple_abundance_location23.csv")
