# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph cdfw otter data
##################################################
library(tidyverse)
library(scales)
library(ggthemes) #tableau colors
library(modelr)
#####################################################
rm(list=ls())
dateToday=Sys.Date()

# oral history data 
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d2<-read_csv("./results/otter_cdfw_dens_location3.csv") 
d3<-read_csv("./results/otter_cdfw_dens_0-30m_location3.csv")

range(d3$otter_abundance_u)

# set location values
d3$location<-d3$location3
d3$location[d3$location=="Monterey Bay" | d3$location=="Monterey Outer" | d3$location=="Carmel" | d3$location=="Carmel Bay" |d3$location=="Monterey Unspecified"| d3$location=="Carmel Outer"] <- "Monterey Peninsula"
d3$location[d3$location=="Big Sur"]<-"Big Sur"
d3$location[d3$location=="SantaCruz Outer"]<-"Santa Cruz"
d3$location[d3$location=="SantaCruz Bay"]<-"Santa Cruz"
unique(d3$location)

d3$location3[d3$location3=="SantaCruz Outer"]<-"Santa Cruz Outer"
d3$location3[d3$location3=="SantaCruz Bay"]<-"Santa Cruz Bay"

d3$Location<-d3$location3%>%fct_relevel("Santa Cruz Outer","Santa Cruz Bay","Monterey Bay","Monterey Outer","Carmel Bay", "Carmel Outer","Big Sur")

#graph summarized 
source("./bin/deets.R")
ggplot(d3, aes(x=year, y=otter_abundance_u, color=location3))+geom_jitter(height=.2,width=0,size=2)+
  ylab("Sea Otters\nRelative Abundance")+
  xlab("Year")+deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_abund_location3.jpg",width=7,height=4)

ggplot(d3, aes(x=year, y=popdens_km_u, color=location3))+geom_jitter(height=.2,width=0,size=2)+
  ylab("Sea Otters\nDensity (no./km2)")+
  xlab("Year")+deets3+
  # geom_hline(yintercept=15)+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_density_location3.jpg",width=7,height=4)


ggplot(d3, aes(x=year, y=popdens_km_u, color=Location))+geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"))+
  ylab("Sea Otter Density (no./km2)")+
  xlab("Year")+deets3+
  geom_hline(yintercept=13)+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_density_location3_smooth.jpg",width=7,height=4)

# location with location3 details
ggplot(d3, aes(x=year, y=otter_abundance_u, color=location))+geom_jitter(height=.2,width=0,size=2)+
  ylab("Sea Otters\nRelative Abundance")+
  xlab("Year")+deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_abund_location3_location.jpg",width=7,height=4)

ggplot(d3, aes(x=year, y=popdens_km_u, color=location))+geom_point(size=3)+
  ylab("Sea Otters\nDensity (no./km2)")+
  xlab("Year")+deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_density_location3_location.jpg",width=7,height=4)

# location with location3 details loess
ggplot(d3, aes(x=year, y=otter_abundance_u, color=location))+geom_smooth()+
  ylab("Sea Otters\nRelative Abundance")+
  xlab("Year")+deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_abund_location3_location2.jpg",width=7,height=4)

ggplot(d3, aes(x=year, y=popdens_km_u, color=location))+geom_smooth()+
  ylab("Sea Otters\nDensity (no./km2)")+
  xlab("Year")+deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_density_location3_location2.jpg",width=7,height=4)
