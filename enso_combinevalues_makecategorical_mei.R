# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

# March 11, 2020
##################################################
# Goal: calculate categorical variable for enso years
##################################################

# setup ########################################
library(tidyverse); library(RColorBrewer); library(readr); library(RcppRoll)
rm(list=ls())

#################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp")

# enso data
d1<-read_csv(file="./data/ensoyearsquinn.csv")
d2<-read_csv(file="./data/ensoyearsgergisfowler.csv")
d3mei<-read_csv(file="./results/enso_yr_offset_mei.csv") #Includes offset to assign a years values from the fall to the following year's surveys
d4<-read_csv(file="./data/kelpenso_sources.csv")
# d4b<-read_csv(file="./data/PacificGrove_TEMP_1919-201906_noheader.csv")

################################################################
# quinn and gergis data################################
levels(as.factor(d1$strength)) #"M"   "M+"  "S"   "S+"  "VS"  "W/M" 
levels(as.factor(d2$strength)) #"E"   "M"   "S"   "VS"  "W"   "W/M"

d1$strengthquinn_c<-d1$strength

# categorical variable for d1
d1$strengthquinn<-0 #N
d1$strengthquinn[d1$strength=="W/M"]<-1
d1$strengthquinn[d1$strength=="M"]<-2
d1$strengthquinn[d1$strength=="M+"]<-2.5
d1$strengthquinn[d1$strength=="S"]<-3
d1$strengthquinn[d1$strength=="S+"]<-3.5
d1$strengthquinn[d1$strength=="VS"]<-4

levels(as.factor(d1$strengthquinn))

# fill in missing years for quinn
d1a<-d1%>%
  complete(year=1806:1987, fill = list(reference = "Quinn 1987", strength = "N", strengthquinn_c = "N",strengthquinn = 0))
d1a
tail(d1a)

# year = 2010:2020 or year = \link{full_seq}(year,1)


# categorical variable for d2
d2
d2$strengthgergis_c<-d2$strength

d2$strengthgergis<-0
d2$strengthgergis[d2$strength=="W"]<-1
d2$strengthgergis[d2$strength=="W/M"]<-1
d2$strengthgergis[d2$strength=="M"]<-2
d2$strengthgergis[d2$strength=="S"]<-3
d2$strengthgergis[d2$strength=="VS"]<-4
d2$strengthgergis[d2$strength=="E"]<-5

levels(as.factor(d2$strengthgergis_c))

d2a<-d2%>%
  complete(year=1806:1987, fill = list(reference = "gergis fowler 2009", strength = "N", strengthgergis_c = "N",strengthgergis = 0))
d2a
tail(d2a)%>%arrange(year)

# simplify and combine
d5<-d1a%>%
  dplyr::select(year,strengthquinn,strengthquinn_c)%>%
  arrange(year)

d6<-d2a%>%
  dplyr::select(year,strengthgergis,strengthgergis_c)

d7<-merge(d5,d6,all=T)%>%
  filter(year>1820)
head(d7)

# set years with no ensos estimated to 0
d7$strengthquinn_c[d7$strengthquinn_c=="N"]<-"None"
d7$strengthgergis_c[d7$strengthgergis_c=="N"]<-"None"


################################################################
# noaa data################################
d7c<-d3mei

# make categories ENSO
d7c$enso_mei_max_c<-"None"
d7c$enso_mei_max_c[d7c$enso_mei_max<1 & 0.5<=d7c$enso_mei_max]<-"W"
d7c$enso_mei_max_c[d7c$enso_mei_max<1.5 & 1<=d7c$enso_mei_max]<-"M"
d7c$enso_mei_max_c[d7c$enso_mei_max<2 & 1.5<=d7c$enso_mei_max]<-"S"
d7c$enso_mei_max_c[d7c$enso_mei_max<2.5 & 2<=d7c$enso_mei_max]<-"VS"
d7c$enso_mei_max_c[2.5<=d7c$enso_mei_max]<-"E"
d7c

d7c$enso_mei_u_c<-"None"
d7c$enso_mei_u_c[d7c$enso_mei_u<1 & 0.5<=d7c$enso_mei_u]<-"W"
d7c$enso_mei_u_c[d7c$enso_mei_u<1.5 & 1<=d7c$enso_mei_u]<-"M"
d7c$enso_mei_u_c[d7c$enso_mei_u<2 & 1.5<=d7c$enso_mei_u]<-"S"
d7c$enso_mei_u_c[d7c$enso_mei_u<2.5 & 2<=d7c$enso_mei_u]<-"VS"
d7c$enso_mei_u_c[2.5<=d7c$enso_mei_u]<-"E"
d7c


# ENSO LAG
d7c$enso2_mei_max_lag1_c<-"None"
d7c$enso2_mei_max_lag1_c[d7c$enso2_mei_max_lag1<1 & 0.5<=d7c$enso2_mei_max_lag1]<-"W"
d7c$enso2_mei_max_lag1_c[d7c$enso2_mei_max_lag1<1.5 & 1<=d7c$enso2_mei_max_lag1]<-"M"
d7c$enso2_mei_max_lag1_c[d7c$enso2_mei_max_lag1<2 & 1.5<=d7c$enso2_mei_max_lag1]<-"S"
d7c$enso2_mei_max_lag1_c[d7c$enso2_mei_max_lag1<2.5 & 2<=d7c$enso2_mei_max_lag1]<-"VS"
d7c$enso2_mei_max_lag1_c[d7c$enso2_mei_max_lag1>=2.5]<-"E"

levels(as.factor(d7c$enso_mei_max_c))

# combine noaa data and other papers
d8<-merge(d7c,d7, all=T)
head(d8)
d8

# rank enso years with number - combining NOAA and Quinn. Pre NOAA no obserervations in ENSO years via Quinn.
d9<-d8
d9$enso_max_nq<-d9$strengthquinn

d9$enso_max_nq[d9$enso_mei_max_c=="W"]<-1 # nq=noaa quinn
d9$enso_max_nq[d9$enso_mei_max_c=="M"]<-2
d9$enso_max_nq[d9$enso_mei_max_c=="S"]<-3
d9$enso_max_nq[d9$enso_mei_max_c=="VS"]<-4
d9$enso_max_nq[d9$enso_mei_max_c=="E"]<-5

d9$source<-"NOAA MEI"
d9$source[d9$year<1871]<-"Quinn"

tibble(d9)

d12<-d9%>%
  dplyr::select(year,source,enso_mei_max, enso2_mei_max, enso2_mei_max_lag1, enso2_mei_max_lag2,enso_mei_max_c, strengthquinn:enso_max_nq) 
d12<-unique(d12)
head(d12)
write_csv(d12,"./results/enso_yearbysource_mei.csv")


# get ENSO years for reference
unique(filter(d12,enso_max_nq>=3))%>% select(year, enso_max_nq)

# ========================================
#merge enso data with kelp years
d10<-merge(d12,d4,all=T,by="year")%>% filter(year>1823); 
head(d10)
tail(d10)
levels(as.factor(d10$datatype.kelp))

d11<-d10%>%
  filter(datatype.kelp == "map" | datatype.kelp == "observation")%>%
  dplyr::select(-enso.long, -source.enso)
head(d11)
# view(d11)

temp<- filter(d11,datatype.kelp!="observation")


write_csv(d11,"./results/enso_kelp_yearbysource_mei.csv")


