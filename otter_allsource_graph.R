################################################
# Jennifer Selgrath
# Monterey Bay Change
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of otter populations using historical and oral history data
##################################################
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d1_1<-read_csv("./results/otter_allsources_abundance_location.csv")

# set on 0-1 scale to match analysis
d1_1$otter_abundance_u<-d1_1$otter_abundance_u*.2
range(d1_1$otter_abundance_u)


# setup ########################################
d1_1$Source<-d1_1$otter_source

d1_1$Source[d1_1$Source=="Contemporary Data"]<-"Ecological Data"

d1_1$Source[d1_1$Source=="Literature, Journals, Field Notes"]<-"Archival Data"

d1_1$Source<-factor(d1_1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")


d1_1$Location<-factor(d1_1$location)%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")

# graph ###############
source("./bin/deets.R")

#################
# version in paper is from source_abundance code
# otters by year, location, and source
ggplot(data=d1_1,aes(x=year,y=otter_abundance_u,color=Source, shape=Location))+geom_point(size=3)+ #geom_point(size=3)+ #geom_jitter(size=3, height=0.1)+
  xlab("Year")+
  ylab("Sea Otters: Relative Abundance")+
  xlim(c(1852,2020))+
  ylim(c(0,1))+
  deets4+ #deets5
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,2,5))
# scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/Fig_S2A_otter_all_abundance_source_location.jpg",height=4,width=7)


##########################
# OTHER GRAPHS
ggplot(d1_1, aes(x=otter_abundance_u))+geom_histogram(bins=12)+
  xlab("Sea Otters\nRelative Abundance (mean)")+
  ylab("Count")+deets #bimodal distribution
ggsave("./doc/otter_all_datastructure_location_bimodal.jpg",height=4,width=6)

# otters by year and source
ggplot(data=d1_1,aes(x=year,y=otter_abundance_u,color=Source))+geom_point(size=3)+
  xlab("Year")+
  ylab("Sea Otters\nRelative Abundance (mean)")+
  deets6+scale_color_discrete_diverging(palette = "Berlin")
ggsave("./doc/otter_all_abundance_source.jpg",height=4,width=7)



# otters by year and location
ggplot(data=d1_1,aes(x=year,y=otter_abundance_u,color=Location))+geom_point(size=3, position="Jitter")+
  xlab("Year")+
  ylab("Sea Otters\nRelative Abundance (mean)")+
  deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_all_abundance_location.jpg",height=3,width=6)
