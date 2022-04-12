################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: integrate final versions of data for Models

##################################################

# setup ########################################
library(tidyverse); library(RColorBrewer); library(colorspace); library(modelr)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

# PYCNO DATA ###############
# ================================================================================
# this is all data sources integrated: oral histories, historical, contemporary
d1.p<-read_csv("./results/pycno_allsources_abundance_location.csv")%>%
  arrange(year,location)

# clean names to match other datasets
d1.p$pycno_source<-d1.p$source
d1.p$pycno_source[d1.p$pycno_source=="Historical Descriptions"]<-"Archival Data"

# calc mean for all data sources
d2.p<-d1.p%>%
  group_by(year,location)%>%
  dplyr::summarize(pycno_abundance_u2=mean(abundance_u),
                   pycno_abundance_n=sum(n))
d2.p


# URCHIN DATA ##############
# ================================================================================
# focus on purple because that is most historical and oral history information
# this is all data sources integrated: oral histories, historical, contemporary
d1.u<-read_csv("./results/urchin_allsources_purple_abundance_location.csv")%>%
  arrange(year,location)%>%
  filter(location!="Northern CA")%>%
  dplyr::select(-organism,-genus)

unique(d1.u$location)
d1.u$location[d1.u$location=="MontereyPeninsula"]<-"Monterey Peninsula"
d1.u$location[d1.u$location=="BigSur"]<-"Big Sur"
d1.u$location[d1.u$location=="SantaCruz"]<-"Santa Cruz"

d1.u$urchin_source[d1.u$urchin_source=="Literature, Journals, Field Notes"]<-"Archival Data"

d1.u
unique(d1.u$location)

# calc mean for all data sources
d2.u<-d1.u%>%
  group_by(year,location)%>%
  dplyr::summarize(urchin_abundance_u2=mean(urchin_abundance_u),
                   urchin_abundance_n=sum(urchin_abundance_n))
d2.u


# OTTER DATA: Oral histories, historical, cdfw surveys ############
# ================================================================================
# this is all data sources integrated: oral histories, historical, contemporary
d1.o<-read_csv("./results/otter_allsources_abundance_location.csv")%>% 
  dplyr::select(-organism)%>%
  arrange(year,location)
d1.o

#binary otter variable
d1.o$otter_b<-"Absent"
d1.o$otter_b[d1.o$otter_abundance_u>0]<-"Present"
d1.o$otter_b<-factor(d1.o$otter_b)

d1.o$otter_source[d1.o$otter_source=="Literature, Journals, Field Notes"]<-"Archival Data"
d1.o

unique(d1.o$location)
d1.o$location[d1.o$location=="Carmel"]<-"Monterey Peninsula"
unique(d2.u$location)

# calc mean for all data sources
d2.o<-d1.o%>%
  group_by(year,location,otter_b)%>%
  dplyr::summarize(otter_abundance_u2=mean(otter_abundance_u))
d2.o


# KELP COVER/ABUNDANCE DATA  ##########
# ================================================================================

# this is all map data sources integrated measuring kelp percent cover (of kelp)i.e. kp) 
d1.kp<-read_csv("./results/kelp_map_allsources_area_location.csv")%>% # % cover 
  arrange(year,location)%>%
  dplyr::select(year,location,kelp_source,kelp_area_p,kelp_area_relative=kelp_abundance_u)
unique(d1.kp$location)
d1.kp

# KELP ABUNDANCE
# this is all data sources integrated
d1.ka<-read_csv("./results/kelp_allsources_abundance_location.csv")%>% # % cover
  arrange(year,location)
unique(d1.ka$location)
d1.ka

# kelp map data with temp and enso
d1.ke<-read_csv("./results/kelp_map_allsources_location_enso.csv")%>%
  dplyr::select(year,location,kelp_area_p,kelp_area_relative=kelp_abundance_u,kelp_source,enso_max_nq,enso2_mei_max_lag1,enso2_mei_max_lag2)
unique(d1.ke$location)
unique(d1.ke$location)
d1.ke
# 
d1.kt<-read_csv("./results/kelp_map_allsources_location_temp.csv")%>%
  dplyr::select(year,location,kelp_area_p,kelp_area_relative=kelp_abundance_u,enso_max_nq,tempc_u:temp_cv) #,ensolag_max_nq
d1.kt
unique(d1.kt$location)
d1.kt

# ENV DATA #######
# ================================================================================
d1.enso<-read_csv("./results/enso_yearbysource_mei.csv")#("./results/enso_datalong_mei.csv") 
d1.temp<-read_csv("./results/sst_c_year.csv")

# create enso variable manually based on Quinn and NOAA S and greater
d1.enso2<-tibble(c(1820:2020)) 
names(d1.enso2)<-"year"
enso<-tibble(c(# Quinn S or greater years:
  1828,1844,1845,1864,1871,1877, 1878, 1891, 1899, 1900, 1911,1912,1917,1925,1926,1932,1940,1941,1957,1958,1972,1973,1982,1983, 
  #from NOAA. confirm which dataset.
  1997, 1998, 2003, 2007,2015, 2016))
names(enso)<-"year"
enso$enso<-1
d1.enso3<-merge(d1.enso2,enso, all=T)
d1.enso3$enso[is.na(d1.enso3$enso)]<-0
d1.enso3


# ================================================================================
# INTEGRATE FINAL DATA SETS ##########################
# ================================================================================
#pycno and urchins
d0<-tibble(full_join(d2.u,d2.p))%>%
  arrange(year,location)
d0


# kelp map and urchins - sometimes > 1 value for urchins for 1 year/location combo - i think due to fio's surveys & abc reef data
d2<-tibble(full_join(d1.kp,d0, by=c("year","location")))%>%
  arrange(year,location)
d2
unique(d2$location)

#kelp map urchins otters pycno - sometimes >1 data type for 1 year/location combo so more rows
d3<-tibble(full_join(d2,d2.o))%>%
  arrange(year,location)%>%
  filter(year>=1820)
d3
unique(d3$location)

# kelp map urchins otters pycno enso
d4<-tibble(full_join(d3,d1.enso3))%>%
  arrange(year,location)
d4
unique(d4$location)

# kelp map urchins otters pycno enso
d5<-tibble(full_join(d4,d1.temp))%>%
  arrange(year,location)
d5

# drop one misc line with NAs for all values
d5<-d5%>%filter(!is.na(d5$location))
unique(d5$location)


# save all together with NAs where data is missing
write_csv(d5, "./results/alldata_allyear_location.csv")

#years with kelp map data
d5b<-d5%>%
  filter(!is.na(d5$kelp_area_p))
d5b

# Save ################################################
write_csv(d5b, "./results/alldata_kelpmap_location.csv")
