# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University


##################################################
# Goal: to combine PISCO urchin data and micheli data for urchins # this is with the updated data 2020.. .does not include reef check data. Not Yet written
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# mean and sd for all data

# reef check
d1<- read_csv("./results/urchin_RC_density_transect.csv")%>%         #has all location data in it
  filter(genus=="Strongylocentrotus")%>%
  select(year,organism,genus,location,location3, density_m2,source)%>%
  glimpse()
d1

# PISCO
d2<- read_csv("./results/urchin_PISCO_location123_bytransect.csv")%>%         #has all location data in it, only 1 SC survey
  filter(genus=="Strongylocentrotus")%>%
  select(year,organism,genus,location,location3, density_m2)%>%
  mutate(source="Contemporary Data")%>%
  glimpse()
d2

# Micheli data
d3<-read_csv("./results/urchin_micheli_density_transect.csv")%>%        
  filter(genus=="Strongylocentrotus")%>%
  mutate(source="Contemporary Data")%>%
  select(year,organism,genus,location,location3, density_m2,source) %>%
  glimpse()

d3

# Published data
d4<-read_csv("./results/urchin_published_location3.csv")%>%        
  filter(genus=="Strongylocentrotus")%>%
  mutate(source="Contemporary Data")%>%
  select(year,organism,genus,location,location3, density_m2,source)%>%
  glimpse()
d4

d4_b<-read_csv("./results/urchin_published_location.csv")%>%        
  filter(genus=="Strongylocentrotus")%>%
  mutate(source="Contemporary Data")%>%
  select(year,organism,genus,location, density_m2,source)%>%
  glimpse()
d4_b


# Combine #############
d5<-rbind(d1,d2,d3,d4)%>%
  arrange(year,location)
d5$organism<-"purple urchin"
d5

unique(d5$organism)

write_csv(d5,"./results/purpleurchin_contemporary_density_transect.csv")


################################################################
# Summarized by location #############
d6<- d5%>%  
  group_by(year,location,organism,genus,source)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density per location
    density_m2_sd=sd(density_m2))%>% 
  arrange(year,location)%>%glimpse()
d6
write_csv(d6,"./results/urchin_contemporary_density_location.csv")

#stats - source = reference here
d6%>%
  select(year,location)%>%
  unique()%>%
  group_by(location)%>%
  summarize(
    yr_1=min(year),
    yr_2=max(year),
    n=n())%>%
  glimpse


# graph
source("./bin/deets.R")

ggplot(d6,aes(x=year, y=density_m2_u)) + geom_point()+
  facet_grid(rows=vars(location))+deets4



# summarized by location3 ----------------------------
d7<- d5%>%  
  group_by(year,location3,organism,genus,source)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  
    density_m2_sd=sd(density_m2))%>% 
  arrange(year,location3)

ggplot(d7,aes(x=year, y=density_m2_u)) + geom_point()+
  facet_grid(rows=vars(location3))+deets4

d6
write_csv(d6,"./results/urchin_contemporary_density_location3.csv")

# summarized by all sites, by year
d8<- d5%>%  
  group_by(year,organism,genus,source)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  
    density_m2_sd=sd(density_m2))%>% 
  arrange(year)

d8
write_csv(d8,"./results/urchin_contemporary_density_allsites.csv")

# summarized by all sites & years
d9<- d5%>%  
  group_by(organism,genus,source)%>%
  dplyr::summarise(
    density_n=n(),
    density_m2_u=mean(density_m2),  # mean density of transects per region
    density_m2_sd=sd(density_m2))

d9
write_csv(d9,"./results/urchin_contemporary_density_allsitesyears.csv")




#################################################################
# CALCULATE RELATIVE ABUNDANCE ##################################
source("./bin/deets.R")
ggplot(d5,aes(x=year,y=density_m2_u,color=genus))+geom_line()+deets3+facet_grid(rows=vars(location3))

ggplot(d6,aes(x=year,y=density_m2_u,color=genus))+geom_point()+deets3+facet_grid(rows=vars(location))
ggplot(d8,aes(x=year,y=density_m2_u,color=genus))+geom_point()+deets3





# assign relative abundance values ----------------------------
range(d6$density_m2_u) # 0.00000 30.66667 formerly #0.00000 24.00658 without pub data #location1
range(d7$density_m2_u) # 0.00000 30.66667 formerly # 0.00000 31.08819 #location3


# d9 = density for all sites

# set breaks in abundance based on standard deviations ################
d0p<-filter(d9,genus=="Strongylocentrotus")
dens_0<-0
dens_1<-(d0p$density_m2_u-(2*d0p$density_m2_sd));dens_1
dens_2<-d0p$density_m2_u-d0p$density_m2_sd;dens_2
dens_3<-d0p$density_m2_u;dens_3
dens_4<-d0p$density_m2_u+d0p$density_m2_sd;dens_4
dens_5<-d0p$density_m2_u+(2*d0p$density_m2_sd);dens_5

# since dens_1 and dens_2 are both negative, set 1 at mean
dens_1a<-d0p$density_m2_u;dens_1a

# since dens_1 and dens_2 are both negative, set 2 half way between mean  and 1 sd
dens_2a<-d0p$density_m2_u+(0.5*d0p$density_m2_sd);dens_2

dens_breaks<-c(dens_0,dens_1a,dens_2a,dens_4,dens_5)
dens_breaks


# set abundance ######################
# location
d10<-d6%>%
  mutate(Location=location)%>%
  glimpse()
d10$abundance_u<-9999
d10$abundance_u[d10$density_m2_u==dens_breaks[1]]<-0
d10$abundance_u[d10$density_m2_u>dens_breaks[1]]<-1 # greater than 0
d10$abundance_u[d10$density_m2_u>=dens_breaks[2]]<-2 # between 0 and mean
d10$abundance_u[d10$density_m2_u>=dens_breaks[3]]<-3 #halfway between mean and 1 sd
d10$abundance_u[d10$density_m2_u>=dens_breaks[4]]<-4
d10$abundance_u[d10$density_m2_u>=dens_breaks[5]]<-5
d10
tail(d10)


# graphing ---------------------
# santa cruz: 4D0055
# montery: 019A95
# big sur: A94824
cols<- c("#4D0055","#019A95","#A94824")
colScale<-scale_color_manual(name="Location", values=cols)

# abundance
ggplot(d10,aes(x=year,y=abundance_u,color=Location))+geom_jitter()+geom_jitter(shape = 19, size = 2, alpha = 0.7)+
  xlab("Year")+
  ylab("Purple Urchin\nRelative Abundance")+
  colScale+
  scale_x_continuous(breaks=seq(1970,2020,20))+
  deets3

# density
ggplot(d10,aes(x=year,y=density_m2_u,color=Location))+geom_jitter(shape = 19, size = 2, alpha = 0.7)+
  xlab("Year")+
  ylab("Purple Urchin Density\n no.per m2")+
  scale_x_continuous(breaks=seq(1970,2020,10))+
  scale_y_continuous(breaks=seq(0,35,10))+
  ylim(0,35)+
  colScale+
  deets3

#location3
d11<-d7
d11$abundance_u<-9999
d11$abundance_u[d11$density_m2_u==dens_breaks[1]]<-0
d11$abundance_u[d11$density_m2_u>dens_breaks[1]]<-1 # greater than 0
d11$abundance_u[d11$density_m2_u>=dens_breaks[2]]<-2 # between 0 and mean
d11$abundance_u[d11$density_m2_u>=dens_breaks[3]]<-3 #halfway between median and 1 sd
d11$abundance_u[d11$density_m2_u>=dens_breaks[4]]<-4
d11$abundance_u[d11$density_m2_u>=dens_breaks[5]]<-5
d11
tail(d11)


ggplot(d11,aes(x=year,y=abundance_u,color=location3))+geom_point()+facet_grid(rows=vars(genus))+deets3
ggplot(d11,aes(x=year,y=density_m2_u,color=location3))+geom_point()+facet_grid(rows=vars(genus))+deets3

write_csv(d10,"./results/urchin_contemporary_abundance_location.csv")
write_csv(d11,"./results/urchin_contemporary_abundance_location3.csv")
