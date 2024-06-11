# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University


##################################################
# Goal: graph purple and red urchin abundances
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr)  

################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# data
d1<- read_csv("./results/urchin_red_contemporary_abundance_location.csv")%>%
  glimpse()
d2<- read_csv("./results/urchin_contemporary_abundance_location.csv")%>%
  glimpse()

d3<-rbind(d1,d2)%>%
  mutate(Location=location)%>%
  mutate(Region=location)%>%
  glimpse()

# graphing ---------------------
source("./bin/deets.R")

# santa cruz: 4D0055
# montery: 019A95
# big sur: A94824
cols<- c("#A94824","#019A95", "#4D0055")
colScale<-scale_color_manual(name="Region", values=cols)

# red- abundance
ggplot(d3,aes(x=year,y=abundance_u,color=Region))+geom_jitter()+geom_point(shape = 19, size = 2, alpha = 0.5)+
  xlab("Year")+
  ylab("Red Urchin\nRelative Abundance")+
  # scale_x_continuous(name = "Year",breaks=c(1940,1950,1960,1970,1980,1990,2000,2010,2020), label=c("1940","","1960","","1980","","2000","","2020"),limits = c(1939, 2021))+
  # 
  # scale_x_continuous(breaks=seq(1970,2020,10))+
  scale_y_continuous(breaks=seq(0,5,1))+
  facet_grid(cols = vars(organism))+
  ylim(0,5)+
  colScale+
  deets3

ggsave("./doc/fig_urchin_color_rel_abund.jpg",height=3,width=6)


# density
ggplot(d3,aes(x=year,y=density_m2_u,color=Region))+geom_jitter(shape = 19, size = 2, alpha = 0.7)+
  xlab("Year")+
  ylab("Urchin Density\n no.per m2")+
  xlim(1960,2021)+
  
  scale_x_continuous(name = "Year",breaks=c(1960,1970,1980,1990,2000,2010,2020), label=c("1960","","","1990","","","2020"),limits = c(1960, 2020))+
  
  
  # scale_x_continuous(breaks=seq(1960,2020,20))+
  scale_y_continuous(breaks=seq(0,33,10))+
  facet_grid(cols = vars(organism))+
  ylim(0,33)+
  colScale+
  deets12

ggsave("./doc/figS4_urchin_color_density.jpg",height=2.5,width=6)
