# -----------------------------------------------------
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
# -----------------------------------------------------
# Goal: graphing variables, data exploration, checking for correlations etc/.
# -----------------------------------------------------

# -----------------------------------------------------
# setup ########
library(tidyverse); library(RColorBrewer); library(colorspace); library(modelr)  
library(ggrepel); library(dplyr) # for labeling points

# -----------------------------------------------------
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d0<-read_csv("./results/all_var_location_2024_interpol_allyr.csv")%>%
  glimpse()

range(d0$year) #1826-2020


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
    predator3=ifelse(otter_u>=1&year<=2013,2,1))%>% #binary 2 & 1
  
  #  relevel predator3
  mutate(
    predator3_orig=factor(predator3,order=T,levels=c(1,2)),
    predator3=if_else(predator3==1,"One Predator","Two Predators"),
    predator3=factor(predator3,order=T,levels=c("One Predator","Two Predators")))%>% 
 
  # make new predator variable for longer time period 
  mutate(
    predator4=if_else((otter_u>=1|is.na(otter_u))&year<=2013,"Both",
      if_else(year>1960&year<=2013,"Both",
         if_else(year>2013,"Sea otter","Sunflower seastar"))))%>% #1955 = migratory front, could pick later date for higher pop? 1957 or 1959 would be other dates

  # relevel kelp var
  mutate(
    kelp2=ifelse(kelp==5,4,kelp),
    kelp3=ifelse(kelp2==1,2,kelp2),
    kelp4=scale(kelp3),
    kelp5=ifelse(kelp3==4,3,kelp2))%>%
  
  # combine archival data and oral histories because few data points & not enough archival sources for variance
  mutate(
    kelp_source2=factor(ifelse(kelp_source=="Archival Data","Oral Histories", "Map Data")),
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
  # filter(year>=1934)%>%
  
  # add var for graphing
  mutate(
    Source=kelp_source,
    Year=year)%>%
  
  glimpse()

# check mutations
cbind(d1$year,d1$predator3,d1$predator4)

levels(d1$predator3)   
unique(d1$predator4) 
unique(d1$urchin_u2) 
levels(d1$predator3_orig) 
cbind(d1$year,d1$predator4,d1$otter_max,d1$otter_u)
cbind(d1$year,d1$predator4,d1$urchin_u2,d1$urchin_u, d1$urchin_max, d1$urchin3)

glimpse(filter(d1,kelp==1))%>%
  dplyr::select(year,kelp,kelp2, kelp3,kelp4)
glimpse(filter(d1,kelp2==1))
range(d1$kelp2)
range(d1$kelp3)
range(d1$kelp4)

unique(d1$kelp_source)


# reduce urchin levels, selet subset of variables so less is lost with na.omit below.
d2<-d1%>%
  dplyr::select(
    year,location,kelp,kelp2,kelp3,kelp4,kelp5,kelp_source,kelp_source2,urchin_u2,urchin2,pycno_u2,otter_u,enso_b2,enso_b1_5,enso_b1_5_Mplus,enso1_5:Year,predator4)%>%
  glimpse()


# remove years >12 years after ENSO - rare but see year after enso 1_5 and M+
d4<-d2%>%
  filter(yr_after_enso2<=12)



# -----------------------------------
# graphs #################
# -------------------------------------
source("./bin/deets.R")

# years after ENSO -----------------------
# kelp by year after 1.5 & M+ ensos


# kelp 5 -combines 3-4 > 3 -----------------------

# This is the clearest visualization/relationship ###############
# kelp by year after 1.5 & M+ ensos
# d5<-d4%>%
#   filter(year!=1856) # outlier, no effect if remove

ggplot(d4,aes(x=yr_after_enso1_5_Mplus,y=kelp5))+
  geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs"),colour="#019A95",fill="#019A95")+
  geom_jitter(aes(x=yr_after_enso1_5_Mplus,y=kelp5,color=predator4,shape=urchin3),size=3,alpha=.8)+ #,size=urchin_u2, shape=kelp_source
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                    name="Predators")+
  scale_shape_discrete(name  ="Urchin Abundance",
                       breaks=c("Low","Medium","High","Unknown"))+
  # xlab("ENSO Recovery Lag\n(1.5 & M+ ENSOs)")+
  scale_x_continuous(name = "ENSO Recovery Lag",breaks=c(-2,0,2,4,6,8,10,12), label=c("-2","0","2","4","6","8","10","12"),limits = c(-2, 12))+
  # xlim(c(-4,12))+
  ylab("Kelp\nRelative Abundance")+
  deets9+
  geom_text_repel(data=subset(d4, kelp5<=1.5),
            aes(x=yr_after_enso1_5_Mplus,y=kelp5,label=year),max.overlaps=15)

ggsave("./doc/fig3_c_kelp_enso_1_5_Mp+dist_lessthan12yr.tiff",width=8,height=4)

