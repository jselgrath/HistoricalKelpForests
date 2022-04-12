# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

# May 1, 2002 (March 3, 2020)
##################################################
# Goal: to integrate kelp map years and estimate proporational cover of kelp vs total area where kelp was ever mapped. 
# Note: area mapped clipped to the area that each map covered for historical data.
##################################################

# setup ########################################
library(tidyverse); library (modelr)
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

l1<-list.files("./data/kelparea_2020april/", pattern=".csv")
l1

# read historical files
d1a<-read_csv(paste0("./data/kelparea_2020april/",l1[[12]]))
d1b<-read_csv(paste0("./data/kelparea_2020april/",l1[[13]]))
d1c<-read_csv(paste0("./data/kelparea_2020april/",l1[[14]]))
d1d<-read_csv(paste0("./data/kelparea_2020april/",l1[[15]]))
d1e<-read_csv(paste0("./data/kelparea_2020april/",l1[[16]]))
d1f<-read_csv(paste0("./data/kelparea_2020april/",l1[[17]]))
d1g<-read_csv(paste0("./data/kelparea_2020april/",l1[[18]]))
d1h<-read_csv(paste0("./data/kelparea_2020april/",l1[[19]]))

# read cdfw data
d0a<-read_csv(paste0("./data/kelparea_2020april/",l1[[1]]))
d0b<-read_csv(paste0("./data/kelparea_2020april/",l1[[2]]))
d0c<-read_csv(paste0("./data/kelparea_2020april/",l1[[3]]))
d0d<-read_csv(paste0("./data/kelparea_2020april/",l1[[4]]))
d0e<-read_csv(paste0("./data/kelparea_2020april/",l1[[5]]))
d0f<-read_csv(paste0("./data/kelparea_2020april/",l1[[6]]))
d0g<-read_csv(paste0("./data/kelparea_2020april/",l1[[7]]))
d0h<-read_csv(paste0("./data/kelparea_2020april/",l1[[8]]))
d0i<-read_csv(paste0("./data/kelparea_2020april/",l1[[9]]))
d0j<-read_csv(paste0("./data/kelparea_2020april/",l1[[10]]))
d0k<-read_csv(paste0("./data/kelparea_2020april/",l1[[11]]))

#combine map data
l1
d2a<-rbind(d1a,d1b,d1c,d1d,d1e,d1h,d1f,d1g,d0a,d0b,d0c,d0d,d0e,d0f,d0g,d0h,d0i,d0j,d0k)
d2<-d2a%>%
  mutate(kelp_area_ha=Shape_Area/10000)%>%
  dplyr::select(year,kelp_source=method,kelp,location=location_1,location2,location3,kelp_area_ha)
  
#remove monterey outer & SC outer for years where they was not mapped (as far as I can tell)
d3<-d2%>%
  filter(year!=1852 | location3!="Monterey Outer")%>%
  filter(year!=1862 | location3!="Monterey Outer")%>%
  filter(year!=1856 | location3!="Monterey Outer")%>%
  filter(year!=1856 | location3!="SantaCruz Outer")
d3

# standardize location names

# location - 3 levels
d2$location3[d2$location3=="SantaCruz Bay"] <-"Santa Cruz Bay" 
d2$location3[d2$location3=="SantaCruz Outer"] <-"Santa Cruz Outer" 

d3$location3[d3$location3=="SantaCruz Bay"] <-"Santa Cruz Bay" 
d3$location3[d3$location3=="SantaCruz Outer"] <-"Santa Cruz Outer" 

unique(d3$location)

d3$year<-as.numeric(d3$year)
d3$kelp <-as.numeric(d3$kelp)
d3$kelp_area_ha<-as.numeric(d3$kelp_area_ha)
str(d3)

unique(d3$kelp_source)
d3$reference<-d3$kelp_source
d3$kelp_source[d3$kelp_source=="chart"]<- "Archival Maps"
d3$kelp_source[d3$kelp_source=="potash"]<- "Archival Maps"
d3$kelp_source[d3$kelp_source=="cdfw"]<- "Contemporary Data"

d3
# save combined file
write_csv(d3, "./results/kelp_map_allsources_area.csv")


####################################################################
# select location level and fill in missing area values##############

# set kelp to character temporarily
d3b<-d3
d3b$kelp[d3b$kelp==1]<-"present"
d3b$kelp[d3b$kelp==0]<-"absent"



##################################################
# LOCATION - fill in missing area values ######
d4a<-d3b%>%
  dplyr::select(year:kelp,location,kelp_area_ha)%>%
  group_by(year,location, kelp_source,kelp)%>%
  summarize(kelp_area_ha2=(sum(kelp_area_ha))/10000)%>%
  arrange(year,location,kelp)
d4a

# fill in values for missing levels
d4b<-d4a%>%
  pivot_wider(names_from = kelp, values_from = kelp_area_ha2,
              values_fill = list(seen = 0))
d4b$present[is.na(d4b$present)]<-0
d4b$absent[is.na(d4b$absent)]<-0
d4b$kelp_area_p<-round(d4b$present/(d4b$present+d4b$absent),3) # calulate proportion kelp cover
d4b

########################################
# set relative abundance for kelp cover #############
range(d4b$kelp_area_p)

# equal interval method ############
t2<-seq_range(range(d4b$kelp_area_p),5);t2
# 0.0 0.6

d4b$kelp_abundance_e<-999
d4b$kelp_abundance_e[d4b$kelp_area_p==0]<-0
d4b$kelp_abundance_e[d4b$kelp_area_p>0]<-1
d4b$kelp_abundance_e[d4b$kelp_area_p>=t2[2]]<-2
d4b$kelp_abundance_e[d4b$kelp_area_p>=t2[3]]<-3
d4b$kelp_abundance_e[d4b$kelp_area_p>=t2[4]]<-4
d4b$kelp_abundance_e[d4b$kelp_area_p>=t2[5]]<-5 
d4b

range(d4b$kelp_abundance_e)


# standard deviation
range(d4b$kelp_area_p)
kelp_sd<-sd(d4b$kelp_area_p); kelp_sd #0.1450081
kelp_3<-mean(d4b$kelp_area_p); kelp_3 #0.1805
kelp_2<-kelp_3-kelp_sd
kelp_1<-kelp_3-(2*kelp_sd)
kelp_4<-kelp_3+kelp_sd
kelp_5<-kelp_3+(2*kelp_sd)

# kelp_1 is a negative number, so remove from relative ranking
kelp_rel<-c(kelp_1,kelp_2,kelp_3,kelp_4,kelp_5); kelp_rel

d4b$kelp_abundance_u<-999
d4b$kelp_abundance_u[d4b$kelp_area_p==0]<-0
d4b$kelp_abundance_u[d4b$kelp_area_p>0]<-1
d4b$kelp_abundance_u[d4b$kelp_area_p>=kelp_rel[2]]<-2
d4b$kelp_abundance_u[d4b$kelp_area_p>=kelp_rel[3]]<-3
d4b$kelp_abundance_u[d4b$kelp_area_p>=kelp_rel[4]]<-4
d4b$kelp_abundance_u[d4b$kelp_area_p>=kelp_rel[5]]<-5

d4b
range(d4b$kelp_abundance_u)



##################################################
# LOCATION3 - fill in missing area values ##############################
d6a<-d3b%>%
  dplyr::select(year:kelp,location3,kelp_area_ha)%>%
  group_by(year,location3, kelp_source,kelp)%>%
  summarize(kelp_area_ha2=round((sum(kelp_area_ha))/10000,5))%>%
  arrange(year,location3,kelp)
d6a

# fill in values for missing levels
d6b<-d6a%>%
  pivot_wider(names_from = kelp, values_from = kelp_area_ha2,
              values_fill = list(seen = 0))
d6b$present[is.na(d6b$present)]<-0
d6b$absent[is.na(d6b$absent)]<-0
d6b$kelp_area_p<-round(d6b$present/(d6b$present+d6b$absent),5) # calulate proportion kelp cover
d6b

########################################
# set relative abundance for kelp cover #############
range(d6b$kelp_area_p)
# 0.0000000 0.6969697

t4<-seq_range(range(d6b$kelp_area_p),5);t4
# 0.0000100 0.1742499 0.3484898 0.5227298 0.6969697

d6b$kelp_abundance_e<-999
d6b$kelp_abundance_e[d6b$kelp_area_p==0]<-0
d6b$kelp_abundance_e[d6b$kelp_area_p>0]<-1
d6b$kelp_abundance_e[d6b$kelp_area_p>=t4[2]]<-2
d6b$kelp_abundance_e[d6b$kelp_area_p>=t4[3]]<-3
d6b$kelp_abundance_e[d6b$kelp_area_p>=t4[4]]<-4
d6b$kelp_abundance_e[d6b$kelp_area_p>=t4[5]]<-5 
d6b

range(d6b$kelp_abundance_e)


# standard deviation
range(d6b$kelp_area_p)
kelp_sd<-sd(d6b$kelp_area_p); kelp_sd #0.1450081
kelp_3<-mean(d6b$kelp_area_p); kelp_3 #0.1805
kelp_2<-kelp_3-kelp_sd
kelp_1<-kelp_3-(2*kelp_sd)
kelp_4<-kelp_3+kelp_sd
kelp_5<-kelp_3+(2*kelp_sd)

# kelp_1 is a negative number, so remove from relative ranking
kelp_rel<-c(kelp_1,kelp_2,kelp_3,kelp_4,kelp_5); kelp_rel

d6b$kelp_abundance_u<-999
d6b$kelp_abundance_u[d6b$kelp_area_p==0]<-0
d6b$kelp_abundance_u[d6b$kelp_area_p>0]<-1
d6b$kelp_abundance_u[d6b$kelp_area_p>=kelp_rel[2]]<-2
d6b$kelp_abundance_u[d6b$kelp_area_p>=kelp_rel[3]]<-3
d6b$kelp_abundance_u[d6b$kelp_area_p>=kelp_rel[4]]<-4
d6b$kelp_abundance_u[d6b$kelp_area_p>=kelp_rel[5]]<-5

d6b
range(d6b$kelp_abundance_u)

# Save files
write_csv(d4b,"./results/kelp_map_allsources_area_location.csv")
write_csv(d6b,"./results/kelp_map_allsources_area_location3.csv")

