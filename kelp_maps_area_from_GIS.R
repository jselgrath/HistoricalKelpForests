################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to calculate proportional area of kelp cover from area surveyed using files from GIS
# Data are projected in CA Albers Equal Area

# from maps: 
#   kelp_area_rel = relative abundance given max area of kelp in all years (kelp in one year, relative to the total area where kelp was ever mapped - proxy for rocky habitat available)
#   kelp_area_max = relative abundance given max area of kelp in a single year (e.g. kelp in one year relative to year with highest amount of kelp cover)

# ------------------------------------------------------------------
library(tidyverse); library(dplyr); library(sf); library(ggplot2); library(lubridate); library(units); library(modelr) 
# ----------------------------------------------------------------

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")
rm(list=ls())

# 'canopy only' has removed sub canopy for last 4 years for cdfw data - not an issue in other years
st_layers("./gis/kelp_maps_2024/kelp_shallow_final_canopy_only.gpkg")
st_layers("./gis/kelp_maps_2024/cdfw_all_yr_loc3.gpkg")
st_layers("./gis/kelp_maps_2024/extent_historical_map.gpkg")

# KELP DATA --------------------------------
# Kelp area in year by location3 for historical data (year = maps, so each year = total area of kelp for that year)
d1<-(st_read("./gis/kelp_maps_2024/kelp_shallow_final_canopy_only.gpkg", layer = "kelp_historical_yr_loc3"))%>%
  arrange(year)%>%
  glimpse()

# kelp area in year by location3 for cdfw data (year = year of survey, so each year = total area of kelp for that year)
# this file only uses canopy and not sub-canopy from later years to keep results consistent across time
d2<-(st_read("./gis/kelp_maps_2024/cdfw_all_yr_loc3.gpkg", layer = "cdfw_all_yr_loc3_canopy_only"))%>%   
  st_drop_geometry()%>%
  # dplyr::select(-layer,-path)%>%
  glimpse()

# this is the max are that kelp could have been in in any year to use for CDFW data
d3<-(st_read("./gis/kelp_maps_2024/kelp_shallow_final_canopy_only.gpkg", layer = "area_all_loc3_dissolved_canopy_only"))%>%   
  st_drop_geometry()%>%   
  glimpse()


# AREA SURVEYED DATA -----------------------
# these files (historical data only) are the max area that kelp could have been in, given the area mapped 
d4a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_1852"))%>% 
  dplyr::select(-area_ha)%>%
  glimpse()

d4<-d4a%>%
  dplyr::mutate(area_meters = st_area(d4a), area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()


d5a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_1856"))%>%    
  dplyr::select(-area_ha)%>%
  glimpse()

d5<-d5a%>%
  dplyr::mutate(area_meters = st_area(d5a), area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()

d6a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_1862"))%>%  
  dplyr::select(-area_ha)

d6<-d6a%>%
  dplyr::mutate(area_meters = st_area(d6a), area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()

d7a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_1892"))%>%  
  dplyr::select(-area_ha)

d7<-d7a%>%
  dplyr::mutate(area_meters = st_area(d7a),area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()

d8a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_1910"))%>%  
  dplyr::select(-area_ha)
d8<-d8a%>%
  dplyr::mutate(area_meters = st_area(d8a), area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()

d9a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_1934"))%>%  
  dplyr::select(-area_ha)
d9<-d9a%>%
  dplyr::mutate(area_meters = st_area(d9a), area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()

#1912
d10a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_crandall"))%>%  
  dplyr::select(-area_ha)
d10<-d10a%>%
  dplyr::mutate(area_meters = st_area(d10a), area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()

#1911
d11a<-(st_read("./gis/kelp_maps_2024/extent_historical_map.gpkg", layer = "kelp_max_mcfarland"))%>%  
  dplyr::select(-area_ha)
d11<-d11a%>% 
  dplyr::mutate(area_meters = st_area(d11a), area_ha = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
  st_drop_geometry()%>% 
  glimpse()





# set files for kelp area each year ---------------------------------------------------
y1852<-d1%>% filter(year== 1852)%>% mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()

# to calc manually, if needed. currently area calc in QGIS
# y1852<-y1852%>%
# dplyr::mutate(area_meters = st_area(y1852), area_hectares = area_meters * 0.0001)%>%                  # 1m2 = 0.0001 hectares
# st_drop_geometry()%>% glimpse()

y1856<-d1%>% filter(year== 1856)%>%   mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()
y1862<-d1%>% filter(year== 1862)%>%   mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()
y1892<-d1%>% filter(year== 1892)%>%   mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()
y1910<-d1%>% filter(year== 1910)%>%   mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()
y1911<-d1%>% filter(year== 1911)%>%   mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()
y1912<-d1%>% filter(year== 1912)%>%   mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()
y1934<-d1%>% filter(year== 1934)%>%   mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%   st_drop_geometry()%>% glimpse()

# join kelp and total areas for historical maps --------------------------
# These calcs are based on mapped kelp IN ONE YEAR compared to max area of kelp ever mapped IN ALL YEARS TOGETHER (a proxy for rocky habitat)
# Area of the max is clipped by the extent of the specific historical map/chart 

# d1852<-y1852%>%  
#   right_join(d4)%>%  mutate(year=1852, kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha), kelp_area_p=round(kelp_area_ha/area_ha,3))%>% arrange(location3)%>%   
#   filter(location3!="Monterey Outer")%>% # did not map outer area (only tiny section in map)
#   glimpse()

d1856<-y1856%>%  right_join(d5)%>%  mutate(year=1856, kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha),kelp_area_p=round(kelp_area_ha/area_ha,3))%>%   arrange(location3)%>%   
  filter(location3!="Monterey Outer")%>% # unclear if  mapped outer area
  glimpse()

d1862<-y1862%>%  right_join(d6)%>%  mutate(year=1862, kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha),kelp_area_p=round(kelp_area_ha/area_ha,3))%>%   arrange(location3)%>%   glimpse()

d1892<-y1892%>%  right_join(d7)%>%  mutate(year=1892, kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha),kelp_area_p=round(kelp_area_ha/area_ha,3))%>%   arrange(location3)%>%   
  # filter(location3!="Monterey Outer")%>% # mapped outer area, but removed most of the kelp because it was in a deep area
  glimpse()

d1910<-y1910%>%  right_join(d8)%>%  mutate(year=1910, 
                                           kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha),kelp_area_p=round(kelp_area_ha/area_ha,3))%>%   arrange(location3)%>%glimpse()

d1911<-y1911%>%  right_join(d11)%>%  mutate(year=1911, kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha),kelp_area_p=round(kelp_area_ha/area_ha,3))%>%   arrange(location3)%>%   glimpse()

d1912<-y1912%>%  right_join(d10)%>%  mutate(year=1912, kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha),kelp_area_p=round(kelp_area_ha/area_ha,3))%>%   arrange(location3)%>%   glimpse()

d1934<-y1934%>%  right_join(d9)%>%  mutate(year=1934, kelp_area_ha=if_else(is.na(kelp_area_ha),0,kelp_area_ha),kelp_area_p=round(kelp_area_ha/area_ha,3))%>%   arrange(location3)%>%   glimpse()

# removing 1852, because it used a different mapping method
# join data for all historical maps -----------------
d_hist<-rbind( d1856, d1862,d1892,d1910,d1911,d1912,d1934)%>%
  dplyr::select(-area_meters)%>%
  glimpse()


# CDFW MAPS --------------------------
d20<-d2%>%mutate(kelp_area_ha=area_ha)%>%dplyr::select(-area_ha)%>%glimpse()
d20<-tibble(d20)
d20
d3

d21a<-d20%>%
  full_join(d3)%>% # extent of kelp mapped IN ALL YEARS COMBINED - proxy for CDFW max area
  mutate(kelp_area_p=round(kelp_area_ha/area_ha,3))%>%
  dplyr::select(-area)%>%
  glimpse()
d21a


# join historical and cdfw data
names(d21a)
d21<-d21a%>%
  dplyr::select(-layer,-path)

names(d21)
names(d_hist)

d22<-rbind(d_hist,d21)%>% glimpse()
d22

# removed earliest map because decided different method that is not comparable (brought up in an internal review comment)
d23<-d22%>%
  filter(year!=1852)%>%
  glimpse()
d23

unique(d23$location3)

# update labels
d23$location3[d23$location3=="SantaCruz Bay"]<-"Santa Cruz Bay"
d23$location3[d23$location3=="SantaCruz Outer"]<-"Santa Cruz Outer"
unique(d23$location3)


#remove monterey outer & SC outer for years where they was not mapped (as far as I can tell)
d24<-d23%>%
  filter(year!=1852 | location3!="Monterey Outer")%>%
  filter(year!=1862 | location3!="Monterey Outer")%>%
  filter(year!=1856 | location3!="Monterey Outer")%>%
  filter(year!=1856 | location3!="Santa Cruz Outer")%>%
  glimpse()
d24

# source
d24$kelp_source<-"Archival Maps"
d24$kelp_source[d24$year>1970]<-"Contemporary Data"
unique(d24$kelp_source)
glimpse(d24)

# calc max kelp area in a  single year by region
d_max<-d24%>%
  group_by(location3)%>%
  summarize(
    kelp_area_max=max(kelp_area_ha))%>%
  glimpse()

# calculate proportion of max area based on max area IN A SINGLE YEAR
d25<-d24%>%
  left_join(d_max)%>%
  mutate(kelp_maxarea_p=kelp_area_ha/kelp_area_max)%>%
  glimpse()

d25<-tibble(d25)
d25

##################################################
# LOCATION1 -------------------------------------
d26<-d25%>%
  group_by(location,year,kelp_source)%>%
  summarize(
    kelp_area_ha=sum(kelp_area_ha),
    kelp_area_max=sum(kelp_area_max),
    area_ha=sum(area_ha))%>%
  dplyr::mutate(
    kelp_area_p=drop_units(kelp_area_ha/area_ha),
    kelp_maxarea_p=kelp_area_ha/kelp_area_max)%>%
  arrange(year,location)%>%
  glimpse()
d26

d26%>%group_by(location,year)%>%summarize(n=n())%>%filter(location=="Monterey Peninsula")

# save -----------------------------
write_csv(d25, "./results/kelp_map_allsources_area_loc3_2024.csv")
write_csv(d26, "./results/kelp_map_allsources_area_loc1_2024.csv")

