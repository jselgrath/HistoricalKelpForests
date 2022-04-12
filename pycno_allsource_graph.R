################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of pycno using all data sources
##################################################
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d1_1<-read_csv("./results/pycno_allsources_abundance_location.csv")
# d1_3<-read_csv("./results/pycno_allsources_abundance_location3.csv")

# # set on 0-1 scale to match analysis
d1_1$abundance_u<-d1_1$abundance_u*.2
range(d1_1$abundance_u)

# setup ########################################
d1_1$source[d1_1$source=="Historical Descriptions"]<-"Archival Data"
# d1_3$source[d1_3$source=="Historical Descriptions"]<-"Archival Data"

d1_1$Source<-d1_1$source
# d1_3$Source<-d1_3$source

d1_1$Source<-factor(d1_1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")

d1_1$Location<-factor(d1_1$location)%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")
# d1_3$Location<-d1_3$location3%>%fct_relevel("Santa Cruz Outer","Santa Cruz Bay","Monterey Bay","Monterey Outer","Carmel Bay", "Carmel Outer","Big Sur")
# levels(d1_3$Location)

# graph ###############
source("./bin/deets.R")

#################
# USING THIS IN PAPER - Figure S2 ##############
# pycno by year, location, and source
ggplot(data=d1_1,aes(x=year,y=abundance_u,color=Source, shape=Location))+geom_jitter(size=3)+ #geom_jitter(size=3, height=0.1)+
  xlab("Year")+
  ylab("Sunflower Seastar: Relative Abundance")+
  xlim(c(1850,2020))+
  ylim(c(0,1))+
  deets4+ #deets5
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,2,5))
# scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/Fig_S2B_pycno_all_abundance_source_location.jpg",height=4,width=7)


##########################
# OTHER GRAPHS

# pycno by year and source - shows data sources track each other
ggplot(data=d1_1,aes(x=year,y=abundance_u,color=Source))+geom_point(size=3)+
  xlab("Year")+
  ylab("Sunflower Seastar\nRelative Abundance")+
  deets6+scale_color_discrete_diverging(palette = "Berlin")
ggsave("./doc/_pycno_all_abundance_source.jpg",height=4,width=7)

# pycno by year and location
ggplot(data=d1_1,aes(x=year,y=abundance_u,color=Location))+geom_point(size=3, position="Jitter")+
  xlab("Year")+
  ylab("Sunflower Seastar\nRelative Abundance (mean)")+
  deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/_pycno_all_abundance_location.jpg",height=3,width=6)

# pycno by year and location
ggplot(data=d1_1,aes(x=year,y=abundance_u,color=Location))+geom_smooth()+
  xlab("Year")+
  ylab("Sunflower Seastar\nRelative Abundance (mean)")+
  deets5+
  scale_color_discrete_sequential(palette = "Viridis")+
  facet_grid(rows=vars(Location))

# pycno by year and location3
# ggplot(data=d1_3,aes(x=year,y=abundance_u,color=Source))+geom_jitter(size=3)+
#   xlab("Year")+
#   ylab("Sunflower Seastar\nRelative Abundance (mean)")+
#   deets5+
#   facet_grid(rows=vars(Location))+
#   scale_color_discrete_sequential(palette = "Viridis")


