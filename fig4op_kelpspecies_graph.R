# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph abundance of kelp
# abundance5 = best or abundance4 is same, but uses start value when change in single year
##################################################

# setup ########################################
library(tidyverse); library(RColorBrewer); library(ggplot2); library(colorspace)
rm(list=ls())

#################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/OralHistories")
dateToday=Sys.Date()

# number of respondents in a year
d0<- read_csv(file="./results/respondentsYear.csv")#, header=T, strip.white = T, stringsAsFactors = F)
head(d0)
d0<-filter(d0,year>1938)

# otter kelp story
d1z<-read_csv(file="./results/otter_kelp_oh_subset_long.csv")#, header=T, strip.white = T, stringsAsFactors = F)

levels(as.factor(d1z$organism))
d1<-d1z%>%
  filter(organism=="kelp",species!="NA",species!="menziesii",species!="palmaeformis")%>%arrange(year,ID2)%>%
  filter(year>1938) #remove tim's comments
unique(d1$species)
head(d1)

 
# # check
levels(as.factor(d1$location3))
levels(as.factor(d1$location2))


d1$location3<-factor(d1$location3)%>%fct_relevel("Santa Cruz","Monterey Bay","Monterey Outer","Carmel","Carmel Outer")
d1$location2<-factor(d1$location2)%>%fct_relevel("Santa Cruz","Monterey Bay","Monterey Outer","Carmel")

# ================================
# filter for giant and bull kelp #######
# ================================
# only obsevations for carmel outer in post-otter world, same as carmel
d1b<-d1%>%
  group_by(year,organism,genus,species,location2)%>%
  summarise(
    respondent2.n=length(unique(ID2)),
    abundance.u=mean(abundance))%>%
  arrange(year)
head(d1b)

d1b$Kelp<-"Giant Kelp"
d1b$Kelp[d1b$genus=="Nereocystis"]<-"Bull Kelp"
head(d1b)
d1b$present<-0
d1b$present[d1b$abundance.u>0]<-1


# bull kelp ############
d2<-d1%>%
  filter(organism=="kelp",species=="luetkeana")%>%
  group_by(year,present,location2)%>%
  summarise(
    respondent2.n=length(unique(ID2)),
    abundance.u=mean(abundance)) 
head(d2)
tail(d2)



d3<-d1%>%
  filter(organism=="kelp",species=="luetkeana")%>%
  group_by(year,present,organism,genus,species,location2)%>%
  summarise(
     respondent2.n=length(unique(ID2)),
     abundance.u=mean(abundance))
head(d3)


# remove carmel and santa cruz from location2
d1c<-d1b%>%
  filter(location2!="Santa Cruz" & location2!="Carmel")
unique(d1c$location2)
#########################################
# graphing############
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
source("./bin/deets.R")

#################################
# ===============================
# abundance by year, color = sp monterey bay only: 2x2 grid
# ===============================

d1d<-d1c%>%filter(location2=="Monterey Bay")
ggplot(d1d,aes(x=year,y=abundance.u,color=Kelp))+
  # ENSO LINES
  geom_segment(aes(x = 1844, xend = 1844, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1864, xend = 1864, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1871, xend = 1871, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1877, xend = 1877, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1878, xend = 1878, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1891, xend = 1891, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1899, xend = 1899, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1900, xend = 1900, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1911, xend = 1911, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1912, xend = 1912, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1917, xend = 1917, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1925, xend = 1925, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1926, xend = 1926, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1932, xend = 1932, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1940, xend = 1940, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1941, xend = 1941, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1957, xend = 1957, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1958, xend = 1958, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1972, xend = 1972, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1973, xend = 1973, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1982, xend = 1982, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1983, xend = 1983, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1997, xend = 1997, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1998, xend = 1998, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 2015, xend = 2015, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre",alpha=.8,color="grey") +
  geom_segment(aes(x = 2016, xend = 2016, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey")+
  
  # REST OF GRAPH
  geom_point(size=2)+ #width = 0.1, height = 0,
  scale_y_continuous(name="Relative Abundance (mean)",breaks=c(0,1,2,3,4,5), limits = c(0, 5.2))+
  scale_x_continuous(name = "Year",breaks=c(1940,1950,1960,1970,1980,1990,2000,2010,2020), label=c("1940","","1960","","1980","","2000","","2020"),limits = c(1939, 2020))+
  # facet_grid(cols = vars(location2), rows = vars(Kelp))+
  scale_color_discrete_sequential(palette = "BluGrn", nmax = 5, order = c(5,1))+
  deets4
ggsave("./doc/kelpsp_abundance_montbay_enso2_fig4p.jpg",height=3,width=5)


# ===============================
# abundance by year, color = sp monterey outer only
# ===============================
d1e<-d1c%>%filter(location2=="Monterey Outer")

ggplot(d1e,aes(x=year,y=abundance.u,color=Kelp))+
  # ENSO LINES
  geom_segment(aes(x = 1844, xend = 1844, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1864, xend = 1864, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1871, xend = 1871, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1877, xend = 1877, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1878, xend = 1878, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1891, xend = 1891, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1899, xend = 1899, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1900, xend = 1900, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1911, xend = 1911, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1912, xend = 1912, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1917, xend = 1917, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1925, xend = 1925, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1926, xend = 1926, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1932, xend = 1932, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1940, xend = 1940, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1941, xend = 1941, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1957, xend = 1957, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1958, xend = 1958, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1972, xend = 1972, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1973, xend = 1973, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1982, xend = 1982, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1983, xend = 1983, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1997, xend = 1997, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1998, xend = 1998, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 2015, xend = 2015, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre",alpha=.8,color="grey") +
  geom_segment(aes(x = 2016, xend = 2016, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey")+
  
  # REST OF GRAPH
  geom_point(size=2)+
  scale_y_continuous(name="Relative Abundance (mean)",breaks=c(0,1,2,3,4,5), limits = c(0, 5.2))+
  scale_x_continuous(name = "Year",breaks=c(1940,1950,1960,1970,1980,1990,2000,2010,2020), label=c("1940","","1960","","1980","","2000","","2020"),limits = c(1939, 2020))+
  # facet_grid(cols = vars(location2), rows = vars(Kelp))+
  scale_color_discrete_sequential(palette = "BluGrn", nmax = 5, order = c(5,1))+
  deets4
ggsave("./doc/kelpsp_abundance_montouter_enso2_fig4o.jpg",height=3,width=5)




