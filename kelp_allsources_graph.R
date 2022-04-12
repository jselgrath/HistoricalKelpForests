# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph kelp harvest maps and ENSO data

##################################################
library(tidyverse)
library(dplyr)
library(smooth)
library(scales)
library(ggthemes) #tableau colors
library(colorspace)
#####################################################
rm(list=ls())
dateToday=Sys.Date()

setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/kelp_map_allsources_area_location.csv")
d1$Location<-d1$location
d1$Source<-d1$kelp_source
d1$Source[d1$Source=="Contemporary Data"]<-"Ecological Data"


d3<-read_csv("./results/kelp_map_allsources_area_location3.csv")
d3$Location<-fct_relevel(d3$location3,c("Big Sur","Carmel Outer","Carmel Bay","Monterey Outer","Monterey Bay","Santa Cruz Bay","Santa Cruz Outer"))
d3$Source<-d3$kelp_source
levels(d3$Location)

# PLOT ###################
source("./bin/deets.R")

# LOCATION ###########################

#scatterplot year, area, location
ggplot(d1, aes(x=year, y=kelp_area_p, color=Source, shape=Location))+geom_jitter(size=3, height=0.01, width=0)+
  ylab("Kelp Cover: Proportion")+
  xlab("Year")+
  xlim(c(1852,2020))+
  ylim(c(0,1))+
  deets4+
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,5))
ggsave("./doc/Fig_S2D_kelp_maparea_location.jpg",width=7,height=4)

#boxplot area, location
ggplot(d1, aes(x=Location, y=kelp_area_p, fill=Location))+geom_boxplot()+
  ylab("Kelp Cover (Proportion)")+
  xlab("Year")+
  deets3+
  scale_fill_discrete_sequential(palette = "Viridis")
ggsave("./doc/kelp_map_areap_location.jpg",width=7,height=4)

#boxplot area, location, source
ggplot(d1, aes(x=Location, y=kelp_area_p, fill=Source))+geom_boxplot()+
  ylab("Kelp Cover (Proportion)")+
  xlab("Year")+
  deets3+
  scale_fill_discrete_sequential(palette = "Viridis")
ggsave("./doc/kelp_map_areap-source_location.jpg",width=7,height=4)





