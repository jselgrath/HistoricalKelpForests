# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to explore abc kelp species data
##################################################

# setup ########################################
library(tidyverse); library(ggplot2); library(modelr); library(colorspace)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp/")

# kelpspecies from PISCO and RC data
d1<-read_csv("./results/kelpspecies_PISCO_RC_density_transect.csv")
d1

d2<-d1
d2$Location<-factor(d2$location)%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")


# GRAPH #################
source("./bin/deets.R")

# Colors
# santa cruz: 4D0055
# montery: 019A95
# big sur: A94824
cols<- c("#4D0055","#019A95","#A94824")


# FIGURE S5 ################
ggplot(data=d2,aes(x=year,y=Density_m2,color=Location))+geom_smooth()+ #geom_jitter(height=0,width=0.2,size=3)+
  xlab("Year")+
  ylab("Density")+
  deets4+
  scale_color_manual(values=cols)+
  facet_grid(rows=vars(genus), scales = "free_y")
ggsave("./doc/Fig_S5_kelpspecies_density.jpg",height=4,width=7)



