# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph kelp data from all sources with otter and enso data. To graph in one figure.

##################################################
library(tidyverse)
library(smooth)
library(scales)
library(ggthemes) #tableau colors
library(colorspace)
#####################################################
rm(list=ls())
dateToday=Sys.Date()


setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/alldata_kelpcover_otter-enso_location3.csv")
d1$Location<-fct_relevel(d1$location3,c("Big Sur","Carmel Outer","Carmel Bay","Monterey Outer","Monterey Bay","Santa Cruz Bay","Santa Cruz Outer"))
# d1$Source<-d1$kelp_source

#binary otter variable
d1$otter_b<-0 #"Absent"
d1$otter_b[d1$otter_abundance_u>0]<-1 #"Present"
# d1$otter_b<-factor(d1$otter_b)

# set location values
d1$location<-d1$location3
d1$location[d1$location=="Monterey Bay" | d1$location=="Monterey Outer" | d1$location=="Carmel" | d1$location=="Carmel Bay" |d1$location=="Monterey Unspecified"| d1$location=="Carmel Outer"] <- "Monterey Peninsula"
d1$location[d1$location=="Big Sur"]<-"Big Sur"
d1$location[d1$location=="Santa Cruz Bay"]<-"Santa Cruz"
d1$location[d1$location=="Santa Cruz Outer"]<-"Santa Cruz"
unique(d1$location)

d2<-d1%>%
  dplyr::select(year,Source,location,location3,kelp_area_p,kelp_abundance,enso_max_nq,otter_abundance_u,otter_b)%>%
  pivot_longer(cols = kelp_area_p:otter_b,names_to = "variable", values_to = "value")
d2$Location<-d2$location


# PLOT ###################
source("./bin/deets.R")


ggplot(d2, aes(x=year, y=value, color=Location))+geom_jitter(size=3, height=0.01, width=0)+ #, shape=Source
  ylab("Value")+
  xlab("Year")+
  facet_grid(rows="variable", scales = "free_y")+
  deets3+
  scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/fig2_timeseries.jpg",width=7,height=4)
