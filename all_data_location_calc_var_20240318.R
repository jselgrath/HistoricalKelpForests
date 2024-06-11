# -----------------------------------------------------
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
# -----------------------------------------------------
# Goal: calc updated variables
# -----------------------------------------------------

# -----------------------------------------------------
# setup ########
library(tidyverse); library(RColorBrewer); library(colorspace); library(modelr)  

# -----------------------------------------------------
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d0<-read_csv("./results/all_var_location_2023_interpol.csv")%>%
  glimpse()

range(d0$year) #1911-2020

#  create variations of variables#---------------------
# since oral histories are the only 5 kelp values changing 5->4 and only one year with kelp =1 (was contemporary map with ENSO) so changing 1 > 2#--------------
# length(d3$kelp[d3$kelp==1]) # only one

# kelp_source           n -------------------------
# 1 Archival Data         2
# 2 Archival Maps         1
# 3 Contemporary Data    11
# 4 Oral Histories       51


# variations------------------------------
d1<-d0%>%
  
  # combine predator var
  mutate(
    predator=pycno_u2*otter_u, #multiplicative
    predator2=pycno_u2*2+otter_u, #additive
    
    predator3=ifelse(year>=1959&year<=2013,2,1))%>% # 1959 = first year mean otter relative abundance >=2 (fluxes until 1967)
  
  #  relevel predator3
  mutate(
    predator3_orig=factor(predator3,order=T,levels=c(1,2)),
    predator3=if_else(predator3==1,"One Predator","Two Predators"),
    predator3=factor(predator3,order=T,levels=c("One Predator","Two Predators")))%>% 
  
  # predator variable specific to which predator is present
  mutate(
    predator4=if_else(predator3_orig==2,"Both",
         if_else(year>2013,"Sea otter","Sunflower seastar")))%>%
  
  # relevel kelp var
  mutate(
    kelp2=ifelse(kelp==5,4,kelp),
    kelp3=ifelse(kelp2==1,2,kelp2),
    # kelp4=scale(kelp3), #won't save if this is included
    kelp5=ifelse(kelp2==4,3,kelp2))%>%
  
  # combine archival data and oral histories because few data points & not enough archival sources for variance
  mutate(
    kelp_source2=factor(ifelse(kelp_source=="Archival Data","Oral Histories", "Map Data")),
    kelp_source=if_else(kelp_source=="Contemporary Data","Contemporary Maps",kelp_source),
    kelp_source=factor(kelp_source))%>%
  
  # ensos 
  mutate(enso_b2=factor(if_else(enso2==1,"ENSO","non-ENSO")),
         enso_b1_5=factor(if_else(enso1_5==1,"ENSO","non-ENSO")),
         enso_b1_5_Mplus=factor(if_else(enso1_5_Mplus==1,"ENSO","non-ENSO")))%>%
  
  #urchins
  mutate(
    urchin2=if_else(urchin_u2>4,5,urchin_u2))%>%
  
  #urchins - category for graphing
  mutate(
    urchin3=if_else(urchin_u2>=4,"High",
                    if_else(urchin_u2<4&urchin_u2>2,"Medium","Low")),
    urchin3=if_else(is.na(urchin2),"Unknown",urchin3))%>%
  
  # remove early years with incomplete records
  filter(year>=1934)%>%
  
  # add var for graphing
  mutate(
    Source=kelp_source,
    Year=year)%>%
  
  glimpse()

cbind(d1$year,d1$predator3,d1$predator4)

# check mutations
levels(d1$predator3)                                              
levels(d1$predator3_orig) 


glimpse(filter(d1,kelp2==1))
range(d1$kelp2)
range(d1$kelp3)


unique(d1$kelp_source)


# save ---------------------------
write_csv(d1,"./results/all_var_location_2023_new_var.csv")

