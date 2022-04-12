# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to set years otters arrived and relative abundances = historical data
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

######################################################
# SET RELATIVE ABUNDANCES #################################

# From CDFG report
d3<-structure(list(
  year=c(1914,1938,1947,1950,1955,1957, 1959,1963,1966,1969,1972,1973,1974,1975),
  NorthernMigrantFront=c("Pt Sur", "Rocky Point","Malpaso Creek","Yankee Point", "Pt Lobos", "Pescadero Pt","Pt Joe", "Otter Pt", "Lovers Pt", "Seaside", "Seaside", "Moss Landing", "Pajaro River", "Sunset State Beach"),
  totalmi2to20fm=c(9,18.4,36.5,46.2,59.2,66.0,74.2,84.7,92.0,105.2,125.2,142.2,144.2,146.7),
  estimatedpop=c(50,310,530,660,800,880,1050,1190,1260,1390,1530,1720,1730,1760)), 
  class = "data.frame", row.names = c(NA, -14L))
d3

# estimated population density: ((no. otters))/mi2) to 20 fathoms = 36.576m ########################
# 1otter/1mi2=1otter/2.589988km2

# convert mi2 to 20 fm to km2 to 20 fm
d3$totalkm2to20fm<-d3$totalmi2to20fm*2.589988110336 #calculatorsoup.com

# convert 20 fm estimates to 30m (raw conversion, not based on coastal area)
# 36.576m = 20fm
# 30m /36.576m (ie 20fm) =0.82021  

# calculate density: otters per mi2 or km2 to 30m
d3$popdens_mi<-round((d3$estimatedpop/d3$totalmi2to20fm)*0.82021,2)
d3$popdens_km<-round((d3$estimatedpop/(d3$totalmi2to20fm*2.58999)*0.82021),2)



#set relative abundance based on mean & sd values from CAFW Surveys.. 
# Note different breaks for different location groupings due to high site variability 
# carrying capacity per km2 originally estimated to be around 5 or a little higher

otter_rel<-c(0.00,2.55,5.10,10.21,15.32); otter_rel # location
otter_rel2<-c(0.00,3.35, 6.70, 12.96, 19.26); otter_rel2 
otter_rel3<-c(0.00, 3.93,  7.86, 14.52, 21.18); otter_rel3 # location3

d3$abundance_u1<-999
d3$abundance_u1[d3$popdens_km==0]<-0 
d3$abundance_u1[d3$popdens_km>0]<-1
d3$abundance_u1[d3$popdens_km>=otter_rel[2]]<-2 
d3$abundance_u1[d3$popdens_km>=otter_rel[3]]<-3
d3$abundance_u1[d3$popdens_km>=otter_rel[4]]<-4
d3$abundance_u1[d3$popdens_km>=otter_rel[5]]<-5

d3$abundance_u2<-999
d3$abundance_u2[d3$popdens_km==0]<-0 
d3$abundance_u2[d3$popdens_km>0]<-1
d3$abundance_u2[d3$popdens_km>=otter_rel[2]]<-2 
d3$abundance_u2[d3$popdens_km>=otter_rel[3]]<-3
d3$abundance_u2[d3$popdens_km>=otter_rel[4]]<-4
d3$abundance_u2[d3$popdens_km>=otter_rel[5]]<-5

d3$abundance_u3<-999
d3$abundance_u3[d3$popdens_km==0]<-0 
d3$abundance_u3[d3$popdens_km>0]<-1
d3$abundance_u3[d3$popdens_km>=otter_rel3[2]]<-2 
d3$abundance_u3[d3$popdens_km>=otter_rel3[3]]<-3
d3$abundance_u3[d3$popdens_km>=otter_rel3[4]]<-4
d3$abundance_u3[d3$popdens_km>=otter_rel3[5]]<-5

d3$source<-"Literature, Journals, Field Notes"
d3$reference<-"CDFG Report 1976"
d3$organism<-"sea otters"
d3

write_csv(d3,"./data/otter_migrantfront.csv")


#######################################################
# same data organized by sites in this study ###########

# SET STANDARD VALUE FOR YEAR ARRIVED.  SELECT CORRECT SUBSET
yr_arr<-tibble(data.frame(location3= c("Big Sur", "Monterey Peninsula", "Santa Cruz"), year=c(1800,1963,1977)))

yr_arr23<-tibble(data.frame(location3= c("Big Sur", "Carmel Outer","Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer"), year=c(1800,1951,1957,1959,1966,1977, 1984)))

#########################################################
# SET PRESENCE ABSENCE FOR SITES COMMONLY USED AS REFERENCES
###########################################################

# SET LOCATION PRESENCE/ABSENCE #################

# LOCATION
d4_1a<-tibble(data.frame(expand_grid(year=c(1800,1938,1947,1951,1955,1957,1959,1963,1966,1969,1972,1973,1974,1975), location= c("Big Sur", "Monterey Peninsula", "Santa Cruz"))))

d4_1b<-tibble(merge(d4_1a,d3))%>%arrange(year,location);d4_1b
d4_1<-dplyr::select(d4_1b,year,location,organism,popdens_km,abundance_u1,source,reference)
d4_1

d4_1$present<-0
d4_1$present[d4_1$location=="Big Sur"]<-1
d4_1$present[d4_1$location=="Monterey Peninsula" & d4_1$year>=yr_arr$year[2]]<-1 
d4_1$present[d4_1$location=="Santa Cruz" & d4_1$year>=yr_arr23$year[3]]<-1 #see redmond & estes

# set abundance == 0 to years pre arrival
d4_1$abundance_u1[d4_1$present==0]<-0
d4_1$popdens_km[d4_1$present==0]<-0

filter(d4_1,location=="Big Sur")
d4_1

write_csv(d4_1,"./results/otter_20thCent_abundance_location.csv")



#######################################################
# SET LOCATION2 PRESENCE/ABSENCE #################

# d4_2a<-tibble(data.frame(expand_grid(year=c(1800,1938,1947,1951,1955,1957,1959,1963,1966,1969,1972,1973,1974,1975), location2= c("Big Sur", "Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer"))))
# 
# 
# d4_2b<-tibble(merge(d4_2a,d3));d4_2b
# d4_2<-dplyr::select(d4_2b,year,location2,organism,popdens_km,abundance_u2,source,reference)
# d4_2
# 
# d4_2$present<-0
# d4_2$present[d4_2$location2=="Big Sur"]<-1
# d4_2$present[d4_2$location2=="Carmel" & d4_2$year>=yr_arr23$year[3]]<-1 # d4_2$year arrived carmel - was 1955 when arrived pt lobos
# d4_2$present[d4_2$location2=="Monterey Outer" & d4_2$year>=yr_arr23$year[4]]<-1 #d4_2$year arrived pt joe
# d4_2$present[d4_2$location2=="Monterey Bay" & d4_2$year>=yr_arr23$year[5]]<-1 # d4_2$year arrived lovers point
# d4_2$present[d4_2$location2=="Santa Cruz Bay" & d4_2$year>=yr_arr23$year[6]]<-1 # d4_2$year arrived SC (or maybe next d4_2$year - see pearse and hines)
# d4_2$present[d4_2$location2=="Santa Cruz Outer" & d4_2$year>=yr_arr23$year[7]]<-1 #see redmond & estes
# 
# # set abundance == 0 to years pre arrival
# d4_2$abundance_u2[d4_2$present==0]<-0
# d4_2$popdens_km[d4_2$present==0]<-0
# 
# filter(d4_2,location2=="Big Sur")
# d4_2
# 
# write_csv(d4_2,"./results/otter_20thCent_abundance_location2.csv")


#######################################################
# SET LOCATION3 PRESENCE/ABSENCE  #####################
d4_3a<-tibble(data.frame(expand_grid(year=c(1800,1938,1947,1951,1955,1957,1959,1963,1966,1969,1972,1973,1974,1975), location3= c("Big Sur", "Carmel Outer","Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer"))))

d4_3b<-tibble(merge(d4_3a,d3));d4_3b
d4_3<-dplyr::select(d4_3b,year,location3,popdens_km,abundance_u3,source,reference)%>%
  arrange(year,location3)
d4_3

d4_3$present<-0
d4_3$present[d4_3$location3=="Big Sur"]<-1
d4_3$present[d4_3$location3=="Carmel Outer" & d4_3$year>=yr_arr23$year[2]]<-1 # d4_3$year arrived yankee point
d4_3$present[d4_3$location3=="Carmel" & d4_3$year>=yr_arr23$year[3]]<-1 # d4_3$year arrived carmel
d4_3$present[d4_3$location3=="Monterey Outer" & d4_3$year>=yr_arr23$year[4]]<-1 #d4_3$year arrived pt joe
d4_3$present[d4_3$location3=="Monterey Bay" & d4_3$year>=yr_arr23$year[5]]<-1 # d4_3$year arrived lovers point
d4_3$present[d4_3$location3=="Santa Cruz Bay" & d4_3$year>=yr_arr23$year[6]]<-1 # d4_3$year arrived SC (or maybe next d4_3$year - see pearse and hines)
d4_3$present[d4_3$location3=="Santa Cruz Outer" & d4_3$year>=yr_arr23$year[7]]<-1 #see redmond & estes

# set abundance and popdens_km to 0 to years pre arrival
d4_3$abundance_u3[d4_3$present==0]<-0
d4_3$popdens_km[d4_3$present==0]<-0

filter(d4_3,location3=="Big Sur")

d4_3$organism<-"sea otters"
d4_3 
tail(d4_3)
write_csv(d4_3,"./results/otter_20thCent_abundance_location3.csv")


#########################################################################
# SET ABUNDANCE FOR ALL YEARS ############################################
#########################################################################
# set relative abundance based on mean & sd values from CAFW Surveys.. see above code 
# Note differnt breaks for different location groupings due to high site variability 
# carrying capacity per km2 originally estimated to be around 5 or a little higher


##################################################
# LOCATION
##################################################
# base info
d8_1<-data.frame(expand_grid(year=c(1914:1975), location= c("Big Sur",  "Monterey Peninsula", "Santa Cruz"),organism="sea otters",source="Literature, Journals, Field Notes",reference="CDFG Report 1976"))

# ABUNDANCE LOCATION ################################
# set abundance == 0 to years pre arrival
d8_1$abundance<-0

d3$abundance_u1

# Big Sur
d8_1$abundance[d8_1$year<1938 & d8_1$location=="Big Sur"]<-d3$abundance_u1[1]
d8_1$abundance[d8_1$year>=1938 & d8_1$location=="Big Sur"]<-d3$abundance_u1[2]
d8_1$abundance[d8_1$year>=1947 & d8_1$location=="Big Sur"]<-d3$abundance_u1[3]
d8_1$abundance[d8_1$year>=1951 & d8_1$location=="Big Sur"]<-d3$abundance_u1[4]
d8_1$abundance[d8_1$year>=1955 & d8_1$location=="Big Sur"]<-d3$abundance_u1[5]
d8_1$abundance[d8_1$year>=1957 & d8_1$location=="Big Sur"]<-d3$abundance_u1[6]
d8_1$abundance[d8_1$year>=1959 & d8_1$location=="Big Sur"]<-d3$abundance_u1[7]
d8_1$abundance[d8_1$year>=1963 & d8_1$location=="Big Sur"]<-d3$abundance_u1[8]
d8_1$abundance[d8_1$year>=1966 & d8_1$location=="Big Sur"]<-d3$abundance_u1[9]
d8_1$abundance[d8_1$year>=1969 & d8_1$location=="Big Sur"]<-d3$abundance_u1[10]
d8_1$abundance[d8_1$year>=1972 & d8_1$location=="Big Sur"]<-d3$abundance_u1[11]
d8_1$abundance[d8_1$year>=1973 & d8_1$location=="Big Sur"]<-d3$abundance_u1[12]
d8_1$abundance[d8_1$year>=1974 & d8_1$location=="Big Sur"]<-d3$abundance_u1[13]
d8_1$abundance[d8_1$year>=1975 & d8_1$location=="Big Sur"]<-d3$abundance_u1[14]


# monterey peninsula
d8_1$abundance[d8_1$year>=1957 & d8_1$location=="Monterey Peninsula"]<-1 # set years to 1 two years prior to front moving in
d8_1$abundance[d8_1$year>=1959 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[7]
d8_1$abundance[d8_1$year>=1963 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[8]
d8_1$abundance[d8_1$year>=1966 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[9]
d8_1$abundance[d8_1$year>=1969 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[10]
d8_1$abundance[d8_1$year>=1972 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[11]
d8_1$abundance[d8_1$year>=1973 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[12]
d8_1$abundance[d8_1$year>=1974 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[13]
d8_1$abundance[d8_1$year>=1975 & d8_1$location=="Monterey Peninsula"]<-d3$abundance_u1[14]


# santa cruz 
# arrived 1977 - after this dataset
d8_1$abundance[d8_1$year>=1975 & d8_1$location=="Santa Cruz"]<-1 # set years to 1 two years prior to front moving in

d8_1$present<-0
d8_1$present[d8_1$abundance>0]<-1

d8_1
tail(d8_1)

write_csv(d8_1,"./results/otter_20thCent_abundance_location_allyr.csv")



###################################################
# LOCATION2 ###########################
###################################################

# location2
# d8_2<-data.frame(expand_grid(year=c(1914:1975), location2= c("Big Sur", "Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer"),organism="sea otters",source="Literature, Journals, Field Notes",reference="CDFG Report 1976"))



# ######################################################
# # PRESENCE ABSENCE LOCATION2 ##################
# d8_2$present<-0
# d8_2$present[d8_2$location2=="Big Sur"]<-1
# d8_2$present[d8_2$location2=="Carmel" & d8_2$year>=yr_arr23$year[3]]<-1 # d8_2$year arrived pt lobos
# d8_2$present[d8_2$location2=="Monterey Outer" & d8_2$year>=yr_arr23$year[4]]<-1 #d8_2$year arrived pt joe
# d8_2$present[d8_2$location2=="Monterey Bay" & d8_2$year>=yr_arr23$year[5]]<-1 # d8_2$year arrived lovers point
# d8_2$present[d8_2$location2=="Santa Cruz Bay" & d8_2$year>=yr_arr23$year[6]]<-1 # d8_2$year arrived SC (or maybe next d8_2$year - see pearse and hines)
# d8_2$present[d8_2$location2=="Santa Cruz Outer" & d8_2$year>=yr_arr23$year[7]]<-1 #see redmond & estes
# 
# # ABUNDANCE LOCATION2 ################################
# # set abundance == 0 to years pre arrival
# d8_2$abundance<-0
# 
# # Big Sur
# d8_2$abundance[d8_2$year<1938 &  d8_2$location2=="Big Sur"]<-d3$abundance_u2[1]
# d8_2$abundance[d8_2$year>=1938 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[2]
# d8_2$abundance[d8_2$year>=1947 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[3]
# d8_2$abundance[d8_2$year>=1951 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[4]
# d8_2$abundance[d8_2$year>=1955 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[5]
# d8_2$abundance[d8_2$year>=1957 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[6]
# d8_2$abundance[d8_2$year>=1959 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[7]
# d8_2$abundance[d8_2$year>=1963 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[8]
# d8_2$abundance[d8_2$year>=1966 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[9]
# d8_2$abundance[d8_2$year>=1969 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[10]
# d8_2$abundance[d8_2$year>=1972 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[11]
# d8_2$abundance[d8_2$year>=1973 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[12]
# d8_2$abundance[d8_2$year>=1974 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[13]
# d8_2$abundance[d8_2$year>=1975 & d8_2$location2=="Big Sur"]<-d3$abundance_u2[14]
# 
# # carmel 
# d8_1$abundance[d8_1$year>=1955 & d8_1$location2=="Carmel"]<-1 # set years to 1 two years prior to front moving in
# d8_2$abundance[d8_2$year>=1957 & d8_2$location2=="Carmel"]<-d3$abundance_u2[6]
# d8_2$abundance[d8_2$year>=1959 & d8_2$location2=="Carmel"]<-d3$abundance_u2[7]
# d8_2$abundance[d8_2$year>=1963 & d8_2$location2=="Carmel"]<-d3$abundance_u2[8]
# d8_2$abundance[d8_2$year>=1966 & d8_2$location2=="Carmel"]<-d3$abundance_u2[9]
# d8_2$abundance[d8_2$year>=1969 & d8_2$location2=="Carmel"]<-d3$abundance_u2[10]
# d8_2$abundance[d8_2$year>=1972 & d8_2$location2=="Carmel"]<-d3$abundance_u2[11]
# d8_2$abundance[d8_2$year>=1973 & d8_2$location2=="Carmel"]<-d3$abundance_u2[12]
# d8_2$abundance[d8_2$year>=1974 & d8_2$location2=="Carmel"]<-d3$abundance_u2[13]
# d8_2$abundance[d8_2$year>=1975 & d8_2$location2=="Carmel"]<-d3$abundance_u2[14]
# 
# # monterey outer
# d8_2$abundance[d8_2$year>=1957 & d8_2$location2=="Monterey Outer"]<-1 # set years to 1 two years prior to front moving in
# d8_2$abundance[d8_2$year>=1959 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[7]
# d8_2$abundance[d8_2$year>=1963 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[8]
# d8_2$abundance[d8_2$year>=1966 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[9]
# d8_2$abundance[d8_2$year>=1969 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[10]
# d8_2$abundance[d8_2$year>=1972 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[11]
# d8_2$abundance[d8_2$year>=1973 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[12]
# d8_2$abundance[d8_2$year>=1974 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[13]
# d8_2$abundance[d8_2$year>=1975 & d8_2$location2=="Monterey Outer"]<-d3$abundance_u2[14]
# 
# # monterey bay
# d8_2$abundance[d8_2$year>=1964 & d8_2$location2=="Monterey Bay"]<-1 # set years to 1 two years prior to front moving in
# d8_2$abundance[d8_2$year>=1966 & d8_2$location2=="Monterey Bay"]<-d3$abundance_u2[9]
# d8_2$abundance[d8_2$year>=1969 & d8_2$location2=="Monterey Bay"]<-d3$abundance_u2[10]
# d8_2$abundance[d8_2$year>=1972 & d8_2$location2=="Monterey Bay"]<-d3$abundance_u2[11]
# d8_2$abundance[d8_2$year>=1973 & d8_2$location2=="Monterey Bay"]<-d3$abundance_u2[12]
# d8_2$abundance[d8_2$year>=1974 & d8_2$location2=="Monterey Bay"]<-d3$abundance_u2[13]
# d8_2$abundance[d8_2$year>=1975 & d8_2$location2=="Monterey Bay"]<-d3$abundance_u2[14]
# 
# # santa cruz 
# d8_2$abundance[d8_2$year>=1975 & d8_2$location2=="Santa Cruz Bay"]<-1 # set years to 1 two years prior to front moving in
# # arrived 1977 - after this dataset
# 
# d8_2$present<-0
# d8_2$present[d8_2$abundance>0]<-1
# 
# d8_2
# tail(d8_2)
# 
# write_csv(d8_2,"./results/otter_20thCent_abundance_location2_allyr.csv")
# 


######################################################
# LOCATION3
#####################################################

#location3
d8_3<-data.frame(expand_grid(year=c(1914:1975), location3= c("Big Sur", "Carmel Outer","Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer"),organism="sea otters",source="Literature, Journals, Field Notes",reference="CDFG Report 1976"))


# PRESENCE ABSENCE LOCATION3 ################
d8_3$present<-0
d8_3$present[d8_3$location3=="Big Sur"]<-1
d8_3$present[d8_3$location3=="Carmel Outer" & d8_3$year>=yr_arr23$year[2]]<-1 # year arrived yankee point
d8_3$present[d8_3$location3=="Carmel" & d8_3$year>=yr_arr23$year[3]]<-1 # year arrived pt lobos
d8_3$present[d8_3$location3=="Monterey Outer" & d8_3$year>=yr_arr23$year[4]]<-1 #year arrived pt joe
d8_3$present[d8_3$location3=="Monterey Bay" & d8_3$year>=yr_arr23$year[5]]<-1 # year arrived lovers point
d8_3$present[d8_3$location3=="Santa Cruz Bay" & d8_3$year>=yr_arr23$year[6]]<-1 # year arrived SC (or maybe next year - see pearse and hines)
d8_3$present[d8_3$location3=="Santa Cruz Outer" & d8_3$year>=yr_arr23$year[7]]<-1 #see redmond & estes


# set abundance == 0 to years pre arrival
d8_3$abundance<-0

# Big Sur
d8_3$abundance[d8_3$year<1938 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[1]
d8_3$abundance[d8_3$year>=1938 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[2]
d8_3$abundance[d8_3$year>=1947 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[3]
d8_3$abundance[d8_3$year>=1951 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[4]
d8_3$abundance[d8_3$year>=1955 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[5]
d8_3$abundance[d8_3$year>=1957 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[6]
d8_3$abundance[d8_3$year>=1959 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[7]
d8_3$abundance[d8_3$year>=1963 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[8]
d8_3$abundance[d8_3$year>=1966 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[9]
d8_3$abundance[d8_3$year>=1969 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[10]
d8_3$abundance[d8_3$year>=1972 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[11]
d8_3$abundance[d8_3$year>=1973 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[12]
d8_3$abundance[d8_3$year>=1974 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[13]
d8_3$abundance[d8_3$year>=1975 & d8_3$location3=="Big Sur"]<-d3$abundance_u3[14]

# carmel outer
d8_3$abundance[d8_3$year>=1949 & d8_3$location3=="Carmel Outer"]<-1 # set years to 1 two years prior to front moving in
d8_3$abundance[d8_3$year>=1951 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[4]
d8_3$abundance[d8_3$year>=1955 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[5]
d8_3$abundance[d8_3$year>=1957 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[6]
d8_3$abundance[d8_3$year>=1959 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[7]
d8_3$abundance[d8_3$year>=1963 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[8]
d8_3$abundance[d8_3$year>=1966 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[9]
d8_3$abundance[d8_3$year>=1969 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[10]
d8_3$abundance[d8_3$year>=1972 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[11]
d8_3$abundance[d8_3$year>=1973 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[12]
d8_3$abundance[d8_3$year>=1974 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[13]
d8_3$abundance[d8_3$year>=1975 & d8_3$location3=="Carmel Outer"]<-d3$abundance_u3[14]

# carmel 
d8_3$abundance[d8_3$year>=1953 & d8_3$location3=="Carmel"]<-1 # set years to 1 two years prior to front moving in
d8_3$abundance[d8_3$year>=1955 & d8_3$location3=="Carmel"]<-d3$abundance_u3[5]
d8_3$abundance[d8_3$year>=1957 & d8_3$location3=="Carmel"]<-d3$abundance_u3[6]
d8_3$abundance[d8_3$year>=1959 & d8_3$location3=="Carmel"]<-d3$abundance_u3[7]
d8_3$abundance[d8_3$year>=1963 & d8_3$location3=="Carmel"]<-d3$abundance_u3[8]
d8_3$abundance[d8_3$year>=1966 & d8_3$location3=="Carmel"]<-d3$abundance_u3[9]
d8_3$abundance[d8_3$year>=1969 & d8_3$location3=="Carmel"]<-d3$abundance_u3[10]
d8_3$abundance[d8_3$year>=1972 & d8_3$location3=="Carmel"]<-d3$abundance_u3[11]
d8_3$abundance[d8_3$year>=1973 & d8_3$location3=="Carmel"]<-d3$abundance_u3[12]
d8_3$abundance[d8_3$year>=1974 & d8_3$location3=="Carmel"]<-d3$abundance_u3[13]
d8_3$abundance[d8_3$year>=1975 & d8_3$location3=="Carmel"]<-d3$abundance_u3[14]

# monterey outer
d8_3$abundance[d8_3$year>=1957 & d8_3$location3=="Monterey Outer"]<-1 # set years to 1 two years prior to front moving in
d8_3$abundance[d8_3$year>=1959 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[7]
d8_3$abundance[d8_3$year>=1963 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[8]
d8_3$abundance[d8_3$year>=1966 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[9]
d8_3$abundance[d8_3$year>=1969 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[10]
d8_3$abundance[d8_3$year>=1972 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[11]
d8_3$abundance[d8_3$year>=1973 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[12]
d8_3$abundance[d8_3$year>=1974 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[13]
d8_3$abundance[d8_3$year>=1975 & d8_3$location3=="Monterey Outer"]<-d3$abundance_u3[14]

# monterey bay
d8_3$abundance[d8_3$year>=1964 & d8_3$location3=="Monterey Bay"]<-1 # set years to 1 two years prior to front moving in
d8_3$abundance[d8_3$year>=1966 & d8_3$location3=="Monterey Bay"]<-d3$abundance_u3[9]
d8_3$abundance[d8_3$year>=1969 & d8_3$location3=="Monterey Bay"]<-d3$abundance_u3[10]
d8_3$abundance[d8_3$year>=1972 & d8_3$location3=="Monterey Bay"]<-d3$abundance_u3[11]
d8_3$abundance[d8_3$year>=1973 & d8_3$location3=="Monterey Bay"]<-d3$abundance_u3[12]
d8_3$abundance[d8_3$year>=1974 & d8_3$location3=="Monterey Bay"]<-d3$abundance_u3[13]
d8_3$abundance[d8_3$year>=1975 & d8_3$location3=="Monterey Bay"]<-d3$abundance_u3[14]

# santa cruz 
# arrived 1977 - after this dataset
d8_3$abundance[d8_3$year>=1975 & d8_3$location3=="Santa Cruz Bay"]<-1 

tail(d8_3)

d8_3$present<-0
d8_3$present[d8_3$abundance>0]<-1

write_csv(d8_3,"./results/otter_20thCent_abundance_location3_allyr.csv")

tail(d8_3)



########################################################################################
# HISTORICAL DATA PRE 1914 ####################################
#########################################################################################
# estimages of historical relative abundance in years in study sites #################
# Not including 1914 because estimated in above from same data, but citing source
# LOCATION
d5_1<-data.frame(expand_grid(year=c(1815,1833,1840), location= c("Big Sur", "Monterey Peninsula", "Santa Cruz")))
d5_1$abundance<-c(rep(4,3),rep(2,3),2,rep(1,2))
d5_1$source<-"Literature, Journals, Field Notes"
d5_1$reference<-"Odgen 1942"
d5_1$present<-1
d5_1$present[d5_1$year==1914&d5_1$location!="Big Sur"]<-0
d5_1$organism<-"sea otters"
d5_1$year
d5_1

# # LOCATION2
# d5_2<-data.frame(expand_grid(year=c(1815,1833,1840), location2= c("Big Sur", "Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer")))
# d5_2$abundance<-c(rep(4,6),rep(2,6),2,rep(1,5))
# d5_2$source<-"Literature, Journals, Field Notes"
# d5_2$reference<-"Odgen 1942"
# d5_2$present<-1
# d5_2$present[d5_2$year==1914&d5_2$location2!="Big Sur"]<-0
# d5_2$organism<-"sea otters"
# d5_2$year
# d5_2

#location3
d5_3<-data.frame(expand_grid(year=c(1815,1833,1840), location3= c("Big Sur", "Carmel Outer","Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer")))
d5_3$abundance<-c(rep(5,7),rep(2,7),2,rep(1,6))
d5_3$source<-"Literature, Journals, Field Notes"
d5_3$reference<-"Odgen 1942"
d5_3$present<-1
d5_3$present[d5_3$year==1914&d5_3$location3!="Big Sur"]<-0
d5_3$organism<-"sea otters"
d5_3


#interpolate years after 1820 #####################
# location
d5_1_fill<-tibble(data.frame(expand_grid(year=c(1815,1820:1914), location= c("Big Sur", "Monterey Peninsula", "Santa Cruz"), organism="sea otters", source="Literature, Journals, Field Notes", reference="Odgen 1942")))
d5_1_fill$present<-0
d5_1_fill$abundance<-c(rep(4,3),
                       rep(3,3),rep(2.9,3),rep(2.8,3),rep(2.7,3),rep(2.6,3),rep(2.5,3),rep(2.4,3),rep(2.3,3),rep(2.2,3),rep(2.1,3),
                       rep(2,3),rep(1.9,3),rep(1.8,3),rep(1.7,3),rep(1.6,3),rep(1.5,3),rep(1.4,3),rep(1.3,3),rep(1,3), 
                       rep(0.7,3),rep(0.4,3),
                       rep(0.1,3),rep(0,219)) #set all to 0
d5_1_fill$abundance[d5_1_fill$location=="Big Sur" & d5_1_fill$year>1841]<-.1 # keep big sur population at 1. 1840 = just after guns widely used
d5_1_fill$present[d5_1_fill$abundance>0]<-1
# view(d5_1_fill)



# # location2
# d5_2_fill<-tibble(data.frame(expand_grid(year=c(1815,1820:1914), location2= c("Big Sur", "Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer"), organism="sea otters", source="Literature, Journals, Field Notes", reference="Odgen 1942")))
# d5_2_fill$present<-0
# d5_2_fill$abundance<-c(rep(4,6),
#                        rep(3,6),rep(2.9,6),rep(2.8,6),rep(2.7,6),rep(2.6,6),rep(2.5,6),rep(2.4,6),rep(2.3,6),rep(2.2,6),rep(2.1,6),
#                        rep(2,6),rep(1.9,6),rep(1.8,6),rep(1.7,6),rep(1.6,6),rep(1.5,6),rep(1.4,6),rep(1.3,6),rep(1,6), 
#                        rep(0.7,6),rep(0.4,6),
#                        rep(0.1,6),rep(0,438))
# d5_2_fill$abundance[d5_2_fill$location2=="Big Sur" & d5_2_fill$year>1841]<-0.1 # keep big sur population at 0.1. 1940 = just after guns widely used
# d5_2_fill$present[d5_2_fill$abundance>0]<-1
# # view(d5_2_fill)

# LOCATION3 ################
d5_3_fill<-tibble(expand_grid(year=c(1815,1820:1914), location3= c("Big Sur", "Carmel Outer","Carmel", "Monterey Outer", "Monterey Bay", "Santa Cruz Bay", "Santa Cruz Outer"), organism="sea otters", source="Literature, Journals, Field Notes", reference="Odgen 1942"))
d5_3_fill$present<-0
d5_3_fill$abundance<-c(rep(4,7),
                       rep(3,7),rep(2.9,7),rep(2.8,7),rep(2.7,7),rep(2.6,7),rep(2.5,7),rep(2.4,7),rep(2.3,7),rep(2.2,7),rep(2.1,7),
                       rep(2,7),rep(1.9,7),rep(1.8,7),rep(1.7,7),rep(1.6,7),rep(1.5,7),rep(1.4,7),rep(1.3,7),rep(1,7), 
                       rep(0.7,7),rep(0.4,7),
                       rep(0.1,7),rep(0,511))
d5_3_fill$abundance[d5_3_fill$location3=="Big Sur" & d5_3_fill$year>1841]<-0.1 # keep big sur population at 1. 1838 = just after guns widely used
d5_3_fill$present[d5_3_fill$abundance>0]<-1
# view(d5_3_fill)

write_csv(d5_1_fill,"./data/otter_historical_location.csv")
# write_csv(d5_2_fill,"./data/otter_historical_location2.csv")
write_csv(d5_3_fill,"./data/otter_historical_location3.csv")



# join historical estimates with estimates from early 20th century ##################
#location
names(d5_1)
names(d4_1)

d4_1b<-d4_1%>%
  dplyr::select(year,location,organism,source,reference,present,abundance=abundance_u1)

d6_1<-rbind(d5_1,d4_1b)%>%
  arrange(year,location)
d6_1
write_csv(d6_1,"./results/otter_historical_all_location.csv")


#location2
# names(d5_2)
# names(d4_2)
# 
# d4_2b<-d4_2%>%
#   dplyr::select(year,location2,organism,source,reference,present,abundance=abundance_u2)
# 
# d6_2<-rbind(d5_2,d4_2b)%>%
#   arrange(year,location2)
# d6_2
# write_csv(d6_2,"./results/otter_historical_all_location2.csv")


#location3
names(d5_3)
names(d4_3)

d4_3b<-d4_3%>%
  dplyr::select(year,location3,organism,source,reference,present,abundance=abundance_u3)

d6_3<-rbind(d5_3_fill,d4_3b)%>%
  arrange(year,location3)
d6_3
write_csv(d6_3,"./results/otter_historical_all_location3.csv")



# join historical estimates with estimates from early 20th century - for all years filled in #########

#location
names(d5_1_fill)
names(d8_1)


d9_1<-rbind(d5_1_fill,d8_1)%>%
  arrange(year)
d9_1
write_csv(d9_1,"./results/otter_historical_all_location_allyears.csv")

#location2
# names(d5_2_fill)
# names(d8_2)
# 
# d9_2<-rbind(d5_2_fill,d8_2)%>%
#   arrange(year)
# d9_2
# write_csv(d9_2,"./results/otter_historical_all_location2_allyears.csv")


#location3
names(d5_3_fill)
names(d8_3)

d9_3<-tibble(rbind(d5_3_fill,d8_3))%>%
  arrange(year)
d9_3
write_csv(d9_3,"./results/otter_historical_all_location3_allyears.csv")

