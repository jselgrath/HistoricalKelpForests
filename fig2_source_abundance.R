# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

# May 8, 2020 - shelter in place
##################################################
# Goal: to graph kelp data from all sources with otter and enso data

##################################################
library(tidyverse); library(smooth); library(scales); library(ggthemes) #tableau colors
library(colorspace)
#####################################################
rm(list=ls())
dateToday=Sys.Date()

setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

#==================================
# Otter data ######################
# ===================================
d1_1<-read_csv("./results/otter_allsources_abundance_location.csv")
d1_2<-read_csv("./results/otter_allsources_abundance_location2.csv")
d1_3<-read_csv("./results/otter_allsources_abundance_location3.csv")

range(d1_3$otter_abundance_u)

# setup ########################################
d1_1$Source<-d1_1$otter_source
d1_2$Source<-d1_2$otter_source
d1_3$Source<-d1_3$otter_source

d1_1$Source[d1_1$Source=="Contemporary Data"]<-"Ecological Data"
d1_2$Source[d1_2$Source=="Contemporary Data"]<-"Ecological Data"
d1_3$Source[d1_3$Source=="Contemporary Data"]<-"Ecological Data"

d1_1$Source[d1_1$Source=="Literature, Journals, Field Notes"]<-"Archival Data"
d1_2$Source[d1_2$Source=="Literature, Journals, Field Notes"]<-"Archival Data"
d1_3$Source[d1_3$Source=="Literature, Journals, Field Notes"]<-"Archival Data"

d1_1$Source<-factor(d1_1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")
d1_2$Source<-factor(d1_2$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")
d1_3$Source<-factor(d1_3$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")

d1_1$Location<-factor(d1_1$location)%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")
d1_2$Location<-d1_2$location2%>%fct_relevel("Santa Cruz Outer","Santa Cruz Bay","Monterey Bay","Monterey Outer","Carmel Bay", "Big Sur")
d1_3$Location<-d1_3$location3%>%fct_relevel("Santa Cruz Outer","Santa Cruz Bay","Monterey Bay","Monterey Outer","Carmel Bay", "Carmel Outer","Big Sur")


# set location values
d1_3$location<-d1_3$location3
d1_3$location[d1_3$location=="Monterey Bay" | d1_3$location=="Monterey Outer" | d1_3$location=="Carmel" | d1_3$location=="Carmel Bay" |d1_3$location=="Monterey Unspecified"| d1_3$location=="Carmel Outer"] <- "Monterey Peninsula"
d1_3$location[d1_3$location=="Big Sur"]<-"Big Sur"
d1_3$location[d1_3$location=="Santa Cruz Outer"]<-"Santa Cruz"
d1_3$location[d1_3$location=="Santa Cruz Bay"]<-"Santa Cruz"
unique(d1_3$location)

d1_1$organism<-"Sea Otters"

# =======================================
# Urchin Data
# ======================================
d2_1<-read_csv("./results/urchin_allsources_purple_abundance_location.csv")%>%filter(location!="Northern CA")
d2_2<-read_csv("./results/urchin_allsources_purple_abundance_location2.csv")%>%filter(location2!="Northern CA")
d2_3<-read_csv("./results/urchin_allsources_purple_abundance_location3.csv")%>%filter(location3!="Northern CA")



# setup ########################################
d2_1$Source<-d2_1$urchin_source
d2_2$Source<-d2_2$urchin_source
d2_3$Source<-d2_3$urchin_source

d2_1$Source[d2_1$Source=="Contemporary Data"]<-"Ecological Data"
d2$Source[d2$Source=="Contemporary Data"]<-"Ecological Data"
d2_3$Source[d2_3$Source=="Contemporary Data"]<-"Ecological Data"

d2_1$Source[d2_1$Source=="Literature, Journals, Field Notes"]<-"Archival Data"
d2_2$Source[d2_2$Source=="Literature, Journals, Field Notes"]<-"Archival Data"
d2_3$Source[d2_3$Source=="Literature, Journals, Field Notes"]<-"Archival Data"

d2_1$Source<-factor(d2_1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")

d2_1$Location<-factor(d2_1$location)%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")
d2_2$Location<-d2_2$location2%>%fct_relevel("Santa Cruz Outer","Santa Cruz Bay","Monterey Bay","Monterey Outer","Carmel Bay", "Big Sur")
d2_3$Location<-d2_3$location3%>%fct_relevel("Santa Cruz Outer","Santa Cruz Bay","Monterey Bay","Monterey Outer","Carmel Bay", "Carmel Outer","Big Sur")

d2_1$organism<-"Urchins"

# =======================================
# Kelp Data
# ======================================
d3_1<-read_csv("./results/alldata_kelpcover_otter-enso_location.csv")
d3_1$Source<-d3_1$kelp_source
d3_1$Source[d3_1$Source=="Contemporary Data"]<-"Ecological Data"
d3_1$Source[d3_1$Source=="Archival Maps"]<-"Archival Data"

d3_1$Source<-factor(d3_1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data") #includes OH to keep levels consistent

d3_1$Location<-factor(d3_1$location)%>%fct_relevel("Santa Cruz","Monterey Peninsula","Big Sur")

d3_1$Organism<-"Kelp"

# ======================================
# Combine Data
# ======================================
d1_1a<-d1_1%>%
  select(year,Region=Location,Organism=organism,Source=otter_source,Abundance=otter_abundance_u);d1_1a

d2_1a<-d2_1%>%
  select(year,Region=location,Organism=organism,Source=urchin_source,Abundance=urchin_abundance_u);d2_1a

d3_1a<-d3_1%>%
  select(year,Region=location,Organism,Source=kelp_source,Abundance=kelp_area_p);d3_1a

d4_1<-rbind(d1_1a,d2_1a,d3_1a)
d4_1

# Organism Factor Order
d4_1$Organism<-factor(d4_1$Organism)%>%fct_relevel("Sea Otters","Urchins","Kelp")

# Source Factor Names and Order
d4_1$Source[d4_1$Source=="Contemporary Data"]<-"Ecological Data"
d4_1$Source[d4_1$Source=="Literature, Journals, Field Notes"]<-"Archival Data"
d4_1$Source[d4_1$Source=="Archival Maps"]<-"Archival Data"
d4_1$Source<-factor(d4_1$Source)%>%fct_relevel("Archival Data","Oral Histories","Ecological Data")
unique(d4_1$Source)

unique(d4_1$Abundance)

# ======================================
# PLOT ###################
# ======================================
source("./bin/deets.R") 

#.......................................
# USING THESE GRAPHS IN PAPER
#.......................................

#.......................................
# Combined Graph
#.......................................
ggplot(data=d4_1,aes(x=year,y=Abundance,color=Source, shape=Region))+geom_point(size=3,alpha=0.9)+ #geom_point(size=3)+ #geom_jitter(size=3, height=0.1)+
  scale_x_continuous("Year", limits=c(1850,2020),breaks=c(1850,1900,1950,2000,2020))+
  ylab("Extent (Proportion)           Relative Abundance (mean)          ")+
  facet_grid(rows=vars(Organism), scales = "free")+
  deets5+ #deets5
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,5,2))
ggsave("./doc/Fig2.jpg",height=6,width=7)



#.......................................
# Single Graphs
#.......................................
# =====================
# Otter graph
# =====================
# otters by year, location, and source
ggplot(data=d1_1,aes(x=year,y=otter_abundance_u,color=Source, shape=Location))+geom_point(size=3)+ #geom_point(size=3)+ #geom_jitter(size=3, height=0.1)+
  scale_x_continuous("Year", limits=c(1850,2020),breaks=c(1850,1900,1950,2000,2020))+
  ylab("Sea Otters\nRelative Abundance (mean)")+
  deets4+ #deets5
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,2,5))
# scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/otter_all_abundance_source_location.jpg",height=4,width=7)


# =====================
# Urchin graph
# =====================
ggplot(data=d2_1,aes(x=year,y=urchin_abundance_u,color=Source, shape=Location))+geom_point(size=3)+ #geom_jitter(size=3, height=0.1)+
  scale_x_continuous("Year", limits=c(1850,2020),breaks=c(1850,1900,1950,2000,2020))+
  ylab("Purple Urchins\nRelative Abundance (mean)")+
  deets4+ #deets5
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,2,5))
# scale_color_discrete_sequential(palette = "Viridis")
ggsave("./doc/urchin_all_abundance_source_location.jpg",height=4,width=7)



# =====================
# Kelp graph
# =====================

#boxplot year, area, otter_b,location
ggplot(d3_1, aes(x=otter_b, y=kelp_area_p, fill=Location))+geom_boxplot(position =  position_dodge2(width = 0.75, preserve = "single"))+
  ylab("Kelp Cover (Proportion)")+
  xlab("Sea Otters")+
  deets4+
  scale_fill_discrete_sequential(palette = "Viridis", nmax = 5, order = c(5,3,1))
ggsave("./doc/kelp_map_location-otteru-loc.jpg",width=7,height=4)



#scatterplot source, location
ggplot(d3_1, aes(x=year, y=kelp_area_p, color=Source, shape=Location))+geom_jitter(size=3, height=0.01, width=0)+
  ylab("Kelp Cover (Proportion)")+
  scale_x_continuous("Year", limits=c(1850,2020),breaks=c(1850,1900,1950,2000,2020))+
  deets4+
  scale_color_discrete_sequential(palette = "Plasma", nmax = 5, order = c(4,5))
ggsave("./doc/kelp_map_source_location.jpg",width=7,height=4)


