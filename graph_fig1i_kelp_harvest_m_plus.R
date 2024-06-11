# Jennifer Selgrath
# Hopkins Marine Station, Stanford University
# August 21, 2020
#########################################

library(tidyverse)
library(smooth)
library(scales)
library(ggthemes) #tableau colors
library(colorspace)

# -------------------------------
# goal: graph historical kelp harverst data and ENSOs >S+ or >1.5 MEI v2
##############################
# setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

source("./bin/deets.R")
# source("./bin/graph_ensoyears.R")
d1<-read_csv("./data/kelp_harvest_CDFG_2003.csv")%>%
  mutate(kelp_tons2=kelp_tons/1000)%>%
  glimpse()
head(d1)

ggplot(d1,aes(x=year,y=kelp_tons2))+geom_line()+
  ylab("CA Kelp Harvest (1,000 tons)")+
  xlab("Year")+
  xlim(1930,2001)+
  ylim(0,175)+
  deets6+
  # Quinn ENSO years S or greater or MEIv2 > 1.5 
  # M+: 1817,1819,1857,1858,1896,1897,1902,1914,1939,1943,1953,1965
  # S or greater years: 1844,1845,1864,1871,1877, 1878, 1891, 1899, 1900, 1911,1912,1917,1925,1926,1932,1940,1941,1957,1958,1972,1973,1982,1983 Added MHW (2013-2014)
  geom_segment(aes(x = 1932, xend = 1932, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1939, xend = 1939, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1940, xend = 1940, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1941, xend = 1941, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1943, xend = 1943, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1953, xend = 1953, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1957, xend = 1957, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1958, xend = 1958, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1965, xend = 1965, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1972, xend = 1972, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1973, xend = 1973, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1982, xend = 1982, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1983, xend = 1983, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1987, xend = 1987, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1992, xend = 1992, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1993, xend = 1993, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1997, xend = 1997, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 1998, xend = 1998, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre" ,alpha=.8,color="grey") +
  geom_segment(aes(x = 2013, xend = 2013, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2014, xend = 2014, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2015, xend = 2015, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey") +
  geom_segment(aes(x = 2016, xend = 2016, y = 175, yend = -Inf),linetype = "longdash", lineend = "butt",linejoin="mitre", alpha=.8,color="grey")

ggsave("./doc/fig1_i_kelp_harvest_QuinnS_MEI_MHW_line.jpg",width=5,height=4)


