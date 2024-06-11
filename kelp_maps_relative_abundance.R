################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal:to calculate relative abundance of kelp

# from maps: 
#   kelp_area_rel = relative abundance given max area of kelp in all years (kelp in one year, relative to the total area where kelp was ever mapped - proxy for rocky habitat available)
#   kelp_area_max = relative abundance given max area of kelp in a single year (e.g. kelp in one year relative to year with highest amount of kelp cover)

# ------------------------------------------------------------------
library(tidyverse); library(dplyr); library(sf); library(ggplot2); library(lubridate); library(units); library(modelr)# 
# ----------------------------------------------------------------

# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
# setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/Kelp/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")
rm(list=ls())

d25<-read_csv("./results/kelp_map_allsources_area_loc3_2024.csv")%>% glimpse()
d26<-read_csv("./results/kelp_map_allsources_area_loc1_2024.csv")%>%glimpse()

d25%>%group_by(location,year)%>%summarize(n=n())%>%filter(location=="Monterey Peninsula")
d26%>%group_by(location,year)%>%summarize(n=n())%>%filter(location=="Monterey Peninsula")

########################################
# LOCATION 3 -----------------------

# set relative abundance for kelp cover #############
range(d25$kelp_area_p)
# 0.00 0.695

range(d25$kelp_maxarea_p)
# 0 to 1

# equal interval method - kelp proportion of kelp area############
t1<-seq_range(range(d25$kelp_area_p),5);t1
# 0.0000 0.1375 0.2750 0.4125 0.5500

# proportion of max in any year by loc3
t2<-seq_range(range(d25$kelp_maxarea_p),5);t2
# 0.0000 0.1375 0.2750 0.4125 0.5500


# kelp area
d25$kelp_abundance_rel<-999
d25$kelp_abundance_rel[d25$kelp_area_p==0]<-0
d25$kelp_abundance_rel[d25$kelp_area_p>0]<-1
d25$kelp_abundance_rel[d25$kelp_area_p>=t1[2]]<-2
d25$kelp_abundance_rel[d25$kelp_area_p>=t1[3]]<-3
d25$kelp_abundance_rel[d25$kelp_area_p>=t1[4]]<-4
d25$kelp_abundance_rel[d25$kelp_area_p>=t1[5]]<-5 
d25$kelp_abundance_rel
d25

# kelp max --------------------------
d25$kelp_abundance_max<-999
d25$kelp_abundance_max[d25$kelp_maxarea_p==0]<-0
d25$kelp_abundance_max[d25$kelp_maxarea_p>0]<-1
d25$kelp_abundance_max[d25$kelp_maxarea_p>=t2[2]]<-2
d25$kelp_abundance_max[d25$kelp_maxarea_p>=t2[3]]<-3
d25$kelp_abundance_max[d25$kelp_maxarea_p>=t2[4]]<-4
d25$kelp_abundance_max[d25$kelp_maxarea_p>=t2[5]]<-5 
d25

range(d25$kelp_abundance_max)

# make label clearer
d25<-d25%>%
  mutate(kelp_abundance_max_rel=kelp_abundance_max)%>%
  dplyr::select(-kelp_abundance_max)%>%
  glimpse()




##################################################
# LOCATION1 -------------------------------------

########################################
# set relative abundance for kelp cover #############
d26
range(d26$kelp_area_p)

range(d26$kelp_maxarea_p)
#0 1

# area ------------------------
t4<-seq_range(range(d26$kelp_area_p),5);t4

d26$kelp_abundance_rel<-999 # was abundance_max
d26$kelp_abundance_rel[d26$kelp_area_p==0]<-0
d26$kelp_abundance_rel[d26$kelp_area_p>0]<-1
d26$kelp_abundance_rel[d26$kelp_area_p>=t4[2]]<-2
d26$kelp_abundance_rel[d26$kelp_area_p>=t4[3]]<-3
d26$kelp_abundance_rel[d26$kelp_area_p>=t4[4]]<-4
d26$kelp_abundance_rel[d26$kelp_area_p>=t4[5]]<-5 
d26

range(d26$kelp_abundance_rel)
# 



# max area ---------------------------
t5<-seq_range(range(d26$kelp_maxarea_p),5);t5
# 0.0000000 0.1670925 0.3341850 0.5012775 0.6683700

d26$kelp_abundance_max<-999
d26$kelp_abundance_max[d26$kelp_maxarea_p==0]<-0
d26$kelp_abundance_max[d26$kelp_maxarea_p>0]<-1
d26$kelp_abundance_max[d26$kelp_maxarea_p>=t4[2]]<-2
d26$kelp_abundance_max[d26$kelp_maxarea_p>=t4[3]]<-3
d26$kelp_abundance_max[d26$kelp_maxarea_p>=t4[4]]<-4
d26$kelp_abundance_max[d26$kelp_maxarea_p>=t4[5]]<-5 
d26

range(d26$kelp_abundance_max)

d26
glimpse(d26)

d26%>%group_by(location,year)%>%summarize(n=n())%>%filter(location=="Monterey Peninsula")

# make label clearer
d26<-d26%>%
  mutate(kelp_abundance_max_rel=kelp_abundance_max)%>%
  dplyr::select(-kelp_abundance_max)%>%
  glimpse()

# Save files
write_csv(d25,"./results/kelp_map_allsources_area_location3.csv")
write_csv(d26,"./results/kelp_map_allsources_area_location.csv")

