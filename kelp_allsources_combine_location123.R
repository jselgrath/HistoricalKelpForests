# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
##################################################

# Goal: to integrate all kelp data sources
# this is not for kelp species data
# standardize location names - no spaces

# from maps: 
#   kelp_area_ha = area of kelp in map
#   area_ha = total area with kelp IN ANY YEAR in extent of map
#   kelp_area_p = kelp_area_ha/area_ha
#   kelp_area_max = max kelp extent IN A SINGLE YEAR in that map extent
#   kelp_maxarea_p = kelp_area_ha/kelp_area_max
#   kelp_abundance_rel = relative abundance given max area of kelp in all years (kelp in one year, relative to the total area where kelp was ever mapped - proxy for rocky habitat available) #kelp_abundance_rel = equal interval (relative)
#   kelp_abundance_u = relative abundance scaled by the mean so that all values are >= 0
#   kelp_abundance_max = relative abundance given max area of kelp IN ONE YEAR (e.g. kelp in one year relative to year with highest amount of kelp cover) #kelp_abundance_max = equal interval (relative)
#   kelp_abundance_u_max = relative abundance of max area IN ONE YEAR scaled by the mean so that all values are >= 0

# setup ########################################
library(tidyverse); library (modelr)
library(scales)
library(ggthemes) #tableau colors
################################################

rm(list=ls())
dateToday=Sys.Date()

# oral history data --------------------------------------
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1_1<-read_csv("./results/kelp_hoh_abundance_location.csv")%>%glimpse()
d1_3<-read_csv("./results/kelp_hoh_abundance_location3.csv")%>%glimpse()

# map data - using relative values
d2_1<-read_csv("./results/kelp_map_allsources_area_location.csv")%>%glimpse()
d2_3<-read_csv("./results/kelp_map_allsources_area_location3.csv")%>%glimpse()

# make compatible with combining ##########
# location ---------------------------------------

# hoh data
d1a_1<-d1_1%>%
  dplyr::select(year,location,kelp_source=source,present,kelp_abundance=abundance_max,kelp_abundance_n=abundance_n)

# map data
d2_1
d2a_1<-d2_1%>%
  dplyr::select(year,location,kelp_source,kelp_abundance=kelp_abundance_rel,kelp_area_ha)%>% #kelp_abundance_rel = equal interval, proportional data (relative)
  mutate(kelp_abundance_n=1,present=ifelse(kelp_area_ha!=0,1,0))%>%
  select(-kelp_area_ha, year, location, kelp_source,present,kelp_abundance,kelp_abundance_n)%>%
  arrange(year)
d1a_1; d2a_1
d3_1<-rbind(d1a_1,d2a_1)%>%arrange(year)%>%glimpse()
d3_1

# location3---------------------------------------

# hoh data
d1a_3<-d1_3%>%
  dplyr::select(year,location3,kelp_source=source,present,kelp_abundance=abundance_max,kelp_abundance_n=abundance_n)%>%glimpse()

# map data
d2a_3<-d2_3%>%
  dplyr::select(year,location3,kelp_source,kelp_abundance=kelp_abundance_rel,kelp_area_ha)%>% #kelp_abundance_rel = equal interval, proportional data (relative)
  mutate(kelp_abundance_n=1,present=ifelse(kelp_area_ha!=0,1,0))%>%
  select(-kelp_area_ha, year, location3, kelp_source,present,kelp_abundance,kelp_abundance_n)%>%
  arrange(year)%>%glimpse()

d1a_3; d2a_3
d3_3<-rbind(d1a_3,d2a_3)%>%arrange(year)%>%glimpse()
d3_3
unique(d3_3$location3)

# standardize location names 
# #note facet grid does not take spaces so removing spaces here
d3_3$location3[d3_3$location3=="SantaCruz"] <-"Santa Cruz Bay"
d3_3$location3[d3_3$location3=="SantaCruz Bay"] <-"Santa Cruz Bay"
d3_3$location3[d3_3$location3=="SantaCruz Outer"] <-"Santa Cruz Outer"
d3_3$location3[d3_3$location3=="Santa Cruz Bay"] <-"Santa Cruz Bay"
d3_3$location3[d3_3$location3=="Santa Cruz Outer"] <-"Santa Cruz Outer"
d3_3$location3[d3_3$location3=="MontereyCarmel"] <-"Carmel Bay"
d3_3$location3[d3_3$location3=="Carmel Bay"] <-"Carmel Bay"
d3_3$location3[d3_3$location3=="Monterey Outer"] <-"Monterey Outer"
d3_3$location3[d3_3$location3=="Monterey Bay"] <-"Monterey Bay"
d3_3$location3[d3_3$location3=="Carmel Outer"] <-"Carmel Outer"
# unique(d3_3$location3)

d3_3<-arrange(d3_3,year,location3)
range(d3_3$kelp_abundance, na.rm=T)
unique(d3_3$location3)

write_csv(d3_1,"./results/kelp_allsources_abundance_location.csv")
write_csv(d3_3,"./results/kelp_allsources_abundance_location3.csv")
