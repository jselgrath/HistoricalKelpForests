# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph kelp data from all sources with otter and enso data

##################################################
library(tidyverse)
library(dplyr)
library(smooth)
library(scales)
library(ggthemes) #tableau colors
library(colorspace)
library(grid)

#####################################################
rm(list=ls())
# dateToday=Sys.Date()

# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# not the original dataset. need to confirm the correct one.
d1<-read_csv("./results/all_var_location3_all_yr.csv")%>%
  glimpse()

#===================================================
# ORGANIZING #####
#===================================================
# set location values - using location3 data so can include location3 in variance structure 
# location3 is for accounting for location specific variation in when otters returned using mixed effects
d1$location<-d1$location3
d1$location[d1$location=="Monterey Bay" | d1$location=="Monterey Outer" | d1$location=="Carmel" | d1$location=="Carmel Bay" |d1$location=="Monterey Unspecified"| d1$location=="Carmel Outer"] <- "Monterey\nPeninsula\n"
d1$location[d1$location=="Big Sur"]<-"Big Sur\n\n"
d1$location[d1$location=="Santa Cruz Bay"]<-"Santa Cruz\n\n"
d1$location[d1$location=="Santa Cruz Outer"]<-"Santa Cruz\n\n"
unique(d1$location)

d1$Location<-factor(d1$location)%>%fct_relevel("Santa Cruz\n\n","Monterey\nPeninsula\n","Big Sur\n\n")

# ===================================================
# set factors 
# ===================================================

d1$Source<-d1$kelp_source
d1$Source[d1$Source=="Contemporary Data"]<-"Ecological Data"
d1$Source[d1$Source=="Archival Maps"]<-"Archival Data"

d1$Source<-factor(d1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data") #includes OH to keep levels consistent



d1$otter_period<-"Absent/Rare"
d1$otter_period[d1$year>1980]<-"Present" #since no maps between 1930s and 1980s, ok  to split this way
d1$otter_period<-as.factor(d1$otter_period)

d1
# PLOT ###################
source("./bin/deets.R")


# LOCATION ###########################

##############################
# USING THESE GRAPHS IN PAPER

# santa cruz: 4D0055
# montery: 019A95
# big sur: A94824
cols<- c("#4D0055","#019A95","#A94824")
#boxplot year, area, otter_b,location
d1$Region<-d1$Location

g1<-ggplot(d1, aes(x=otter_period, y=kelp_area_p, fill=Region))+geom_boxplot(position =  position_dodge2(width = 0.75, preserve = "single"))+
  ylab("Kelp\nProportion of Max. Area")+
  xlab("Sea Otters")+
  deets9+
  scale_fill_manual(values=cols)
  # scale_fill_discrete_sequential(palette = "Viridis", nmax = 5, order = c(5,3,1))
gb1<-ggplot_build(g1) # to add missing level

# gb1$data
gb1$data[[1]]$x[1:2]<-c(0.75,1)
gb1$data[[1]]$xmax[1:2]<-c(0.875,1.125)
gb1$data[[1]]$xmin[1:2]<-c(0.625,0.875)
gb1



tiff("./doc/fig1h_kelpcover_otters_location_20240206.tif",width=7,height=4,units="in",res=500) 
grid.draw(ggplot_gtable(gb1))
dev.off()
