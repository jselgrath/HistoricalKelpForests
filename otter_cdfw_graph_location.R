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

d2<-read_csv("./results/otter_cdfw_dens_location.csv") 
d3<-read_csv("./results/otter_cdfw_dens_0-30m_location.csv")

range(d3$abundance_u)

#graph summarized 
source("./bin/deets.R")
ggplot(d3, aes(x=year, y=abundance_u, color=location))+geom_jitter(size=3)+
  ylab("Sea Otters\nRelative Abundance")+
  xlab("Year")+deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_abund_location.jpg",width=7,height=4)

ggplot(d3, aes(x=year, y=popdens_km_u, color=location))+geom_jitter(size=3)+
  ylab("Sea Otters\nDensity (no./km2)")+
  xlab("Year")+deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_contemp_density_location.jpg",width=7,height=4)

