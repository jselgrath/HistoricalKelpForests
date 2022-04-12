################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of urchin cover using historical and oral history data
##################################################
library(tidyverse); library(ggplot2);library(RColorBrewer); library(colorspace)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d2_1<-read_csv("./results/urchin_allsources_purple_abundance_location.csv")%>%filter(location!="Northern CA")
# d2_3<-read_csv("./results/urchin_allsources_purple_abundance_location3.csv")%>%filter(location3!="Northern CA")

# set on 0-1 scale to match analysis
d2_1$urchin_abundance_u<-d2_1$urchin_abundance_u*.2
range(d2_1$urchin_abundance_u)

# setup ########################################
d2_1$Source<-d2_1$urchin_source
# d2_3$Source<-d2_3$urchin_source

d2_1$Source[d2_1$Source=="Contemporary Data"]<-"Ecological Data"
# d2_3$Source[d2_3$Source=="Contemporary Data"]<-"Ecological Data"

d2_1$Source[d2_1$Source=="Literature, Journals, Field Notes"]<-"Archival Data"
# d2_3$Source[d2_3$Source=="Literature, Journals, Field Notes"]<-"Archival Data"

d2_1$Source<-factor(d2_1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")

d2_1$Location<-d2_1$location

factor()%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")

# d2_3$Location<-d2_3$location3%>%fct_relevel("Santa Cruz Outer","Santa Cruz","Monterey Bay","Monterey Outer","Carmel Bay", "Carmel Outer","Big Sur")

# graph ###############
source("./bin/deets.R")

#################
# USING THIS IN PAPER
# urchins by year, location, and source
d2_1%>%filter(is.na(Location))
ggplot(data=d2_1,aes(x=year,y=urchin_abundance_u,color=Source, shape=Location))+geom_point(size=3)+ #geom_jitter(size=3, height=0.1)+
  xlab("Year")+
  ylab("Purple Urchins: Relative Abundance")+
  xlim(c(1850,2020))+
  ylim(c(0,1))+
  deets4+ #deets5
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,2,5))
ggsave("./doc/Fig_S2C_urchin_all_abundance_source_location.jpg",height=4,width=7)




##########################
# OTHER GRAPHS

# urchins by year and source
ggplot(data=d2_1,aes(x=year,y=urchin_abundance_u,color=Source))+geom_point(size=3)+
  xlab("Year")+
  ylab("Purple Urchins\nRelative Abundance (mean)")+
  deets6+scale_color_discrete_diverging(palette = "Berlin")
ggsave("./doc/_urchin_all_abundance_source.jpg",height=4,width=7)

# urchins by year and location
ggplot(data=d2_1,aes(x=year,y=urchin_abundance_u,color=Location))+geom_point(size=3, position="Jitter")+
  xlab("Year")+
  ylab("Purple Urchins\nRelative Abundance (mean)")+
  deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/_urchin_all_abundance_location.jpg",height=3,width=6)


# urchins by year and location3
# ggplot(data=d2_3,aes(x=year,y=urchin_abundance_u,color=Source))+geom_jitter(size=3)+
#   xlab("Year")+
#   ylab("Purple Urchins\nRelative Abundance (mean)")+
#   deets5+
#   facet_grid(rows=vars(Location))+
#   scale_color_discrete_sequential(palette = "Viridis")
# ggsave("./doc/_urchin_all_abundance_location3.jpg",height=3,width=6)

