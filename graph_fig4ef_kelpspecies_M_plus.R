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
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/OralHistories")

dateToday=Sys.Date()

# number of respondents in a year
d0<- read_csv(file="./results/respondentsYear.csv")%>%glimpse()#, header=T, strip.white = T, stringsAsFactors = F)

d0<-filter(d0,year>1938)

range(d0$year)

# otter kelp story
d1z<-read_csv(file="./results/otter_kelp_oh_subset_long.csv")%>%glimpse()#, header=T, strip.white = T, stringsAsFactors = F)
levels(as.factor(d1z$organism))

# filter for data about macro and nereo in Monterey inner and outer
d1<-d1z%>%
  filter(organism=="kelp",species!="NA",species!="menziesii",species!="palmaeformis")%>%arrange(year,ID2)%>%
  filter(location3!="Santa Cruz" & location3!="Carmel"& location3!="Big Sur")%>% # remove other locations from location3
  filter(year>1938)%>%
  glimpse()#remove tim's comments
unique(d1$species)
head(d1)

d1%>%
  filter(year>1980&location3=="Monterey Outer"& species=="luetkeana")

 
# # check
levels(as.factor(d1$location3))


d1$Kelp<-"Giant Kelp"
d1$Kelp[d1$genus=="Nereocystis"]<-"Bull Kelp"


# make factors
unique(d1$location3)
d1$location3<-factor(d1$location3)%>%fct_relevel("Monterey Bay","Monterey Outer","Carmel Outer")

# ================================
# summarize to mean values #######
# ================================
# only obsevations for carmel outer in post-otter world, same as carmel
d1b<-d1%>%
  group_by(year,organism,genus,species,Kelp,location3)%>%
  summarise(
    respondent2.n=length(unique(ID2)),
    abundance.u=mean(abundance,na.rm=T),
    abundance.sd=sd(abundance,na.rm=T),
    n=n())%>%
  arrange(year)%>%
  ungroup()%>%
  glimpse()
head(d1b)


#########################################
# graphing############
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")
source("./bin/deets.R")


# ===============================
# abundance by year, color = sp monterey bay only: 2x2 grid
# ===============================

d1d<-d1%>%filter(location3=="Monterey Bay")

ggplot(d1d,aes(x=year,y=abundance,color=Kelp))+
  
  # Quinn ENSO years S or greater or MEIv2 > 1.5 
  # M+: 1817,1819,1857,1858,1896,1897,1902,1914,1939,1943,1953,1965
  # S or greater years: 1844,1845,1864,1871,1877, 1878, 1891, 1899, 1900, 1911,1912,1917,1925,1926,1932,1940,1941,1957,1958,1972,1973,1982,1983 Added MHW (2013-2014)
  
  
  # ENSO LINES - M+ or greater or 1.5 or greater or MHW
  geom_segment(aes(x = 1932, xend = 1932, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1939, xend = 1939, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1940, xend = 1940, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1941, xend = 1941, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1943, xend = 1943, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1953, xend = 1953, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1957, xend = 1957, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1958, xend = 1958, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1965, xend = 1965, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1972, xend = 1972, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1973, xend = 1973, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1982, xend = 1982, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1983, xend = 1983, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1987, xend = 1987, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1992, xend = 1992, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1993, xend = 1993, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1997, xend = 1997, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1998, xend = 1998, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 2013, xend = 2013, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2014, xend = 2014, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2015, xend = 2015, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2016, xend = 2016, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey")+
  
  # REST OF GRAPH
  # geom_point(size=2)+
  geom_jitter(size=2, alpha=0.7,width=0.2,height=0.2)+ #width = 0.1, height = 0,
  geom_smooth()+
  scale_y_continuous(name="Relative Abundance (mean)",breaks=c(0,1,2,3,4,5), limits = c(-.50, 5.2))+
  scale_x_continuous(name = "Year",breaks=c(1940,1950,1960,1970,1980,1990,2000,2010,2020), label=c("1940","","1960","","1980","","2000","","2020"),limits = c(1939, 2021))+
  scale_color_discrete_sequential(palette = "BluGrn", nmax = 5, order = c(5,1))+
  deets4
ggsave("./doc/fig4_f_kelpsp_abundance_montbay_enso3.jpg",height=3,width=5)


# ===============================
# abundance by year, color = sp monterey outer only
# ===============================
d1e<-d1%>%filter(location3=="Monterey Outer")

ggplot(d1e,aes(x=year,y=abundance,color=Kelp))+
  # ENSO LINES
    # Quinn ENSO years M+ or greater or MEIv2 > 1.5 
 # M+: 1817,1819,1857,1858,1896,1897,1902,1914,1939,1943,1953,1965
  # S or greater years: 1844,1845,1864,1871,1877, 1878, 1891, 1899, 1900, 1911,1912,1917,1925,1926,1932,1940,1941,1957,1958,1972,1973,1982,1983 Added MHW (2013-2014)

  geom_segment(aes(x = 1932, xend = 1932, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1939, xend = 1939, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1940, xend = 1940, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1941, xend = 1941, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1943, xend = 1943, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1953, xend = 1953, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1957, xend = 1957, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1958, xend = 1958, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1965, xend = 1965, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1972, xend = 1972, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1973, xend = 1973, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1982, xend = 1982, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1983, xend = 1983, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1987, xend = 1987, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1992, xend = 1992, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1993, xend = 1993, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1997, xend = 1997, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1998, xend = 1998, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 2013, xend = 2013, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2014, xend = 2014, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2015, xend = 2015, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2016, xend = 2016, y = 5.2, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey")+
  
  # REST OF GRAPH
  # geom_point(size=2)+
geom_jitter(size=2, alpha=0.7,width=0.2,height=0.2)+ #width = 0.1, height = 0,
geom_smooth()+
  scale_y_continuous(name="Relative Abundance (mean)",breaks=c(0,1,2,3,4,5), limits = c(-.5, 5.2))+
  scale_x_continuous(name = "Year",breaks=c(1940,1950,1960,1970,1980,1990,2000,2010,2020), label=c("1940","","1960","","1980","","2000","","2020"),limits = c(1939, 2021))+
  # facet_grid(cols = vars(location3), rows = vars(Kelp))+
  scale_color_discrete_sequential(palette = "BluGrn", nmax = 5, order = c(5,1))+
  deets4
ggsave("./doc/fig4_e_kelpsp_abundance_montouter_enso3.jpg",height=3,width=5)




