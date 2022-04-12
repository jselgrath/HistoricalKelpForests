# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to graph estimates of urchin cover using historical and oral history data
##################################################

# setup ########################################
library(tidyverse); library(ggplot2)  
rm(list=ls())
################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

d1_1<-read_csv("./results/urchin_oh_purple_summarized_location.csv")
d1_3<-read_csv("./results/urchin_oh_purple_summarized_location3.csv")

d1a<-read_csv("./results/urchin_oh_all_summarized.csv")

d1b_1<-read_csv("./results/urchin_oh_all_summarized_location.csv")
d1b_3<-read_csv("./results/urchin_oh_all_summarized_location3.csv")


# other sources ##############
d2_1<-read_csv("./results/urchin_historical_purple_abundance_location.csv")%>%
  dplyr::select(year,location,organism,genus,source,abundance_u,abundance_sd,abundance_n)
unique(d2_1$location)

d2_2<-read_csv("./results/urchin_historical_purple_abundance_location23.csv")%>%
  dplyr::select(year,location2,organism,genus,source,abundance_u,abundance_sd,abundance_n)
unique(d2_2$location2)

d2_3<-read_csv("./results/urchin_historical_purple_abundance_location23.csv")%>%
  dplyr::select(year,location3=location2,organism,genus,source,abundance_u,abundance_sd,abundance_n)
unique(d2_3$location3)

d2a<-read_csv("./results/urchin_historical_abundance.csv")%>%
  dplyr::select(year,organism,source,abundance_u,abundance_sd,abundance_n)

d2b_1<-read_csv("./results/urchin_historical_abundance_location.csv")%>%
  dplyr::select(year,location,organism,source,abundance_u,abundance_sd,abundance_n)
d2b_2<-read_csv("./results/urchin_historical_abundance_location23.csv")%>%
  dplyr::select(year,location2,organism,source,abundance_u,abundance_sd,abundance_n)
d2b_3<-read_csv("./results/urchin_historical_abundance_location23.csv")%>%
  dplyr::select(year,location3=location2,organism,source,abundance_u,abundance_sd,abundance_n)

# combine data sources, pruple urchins, location3
d3_1<-rbind(d1_1,d2_1)%>%
  filter(location!="Northern CA")
d3_1$location[d3_1$location=="MontereyPeninsula"]<-"Monterey Peninsula"
unique(d3_1$location)

d3_1$location[d3_1$location=="Pacific Grove by jetty"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="Monterey Peninsula; Asilomar"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="Hopkins"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="Hopkins, 17 Mile Drive"]<-"Monterey Peninsula"

d3_1$location[d3_1$location=="Pinnacles"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="Asilomar"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="HMS; Monterey Peninsula"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="Point Pinos; HMS; Carmel Bay"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="Monterey Peninsula; Asilomar"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="HMS"]<-"Monterey Peninsula"
d3_1$location[d3_1$location=="Otter Point; Point Pinos"]<-"Monterey Peninsula"
unique(d3_1$location)


d3_3<-rbind(d1_3,d2_3)%>%
  filter(location3!="Northern CA")
d3_3$location3[d3_3$location3=="MontereyBay"]<-"Monterey Bay"
unique(d3_3$location3)

# summarize by year #######################
d4<-rbind(d1a,d2a) #no location

# summarize by year & location, all urchins #######################
d5_1<-rbind(d1b_1,d2b_1)
# d5_2<-rbind(d1b_2,d2b_2)
d5_3<-rbind(d1b_3,d2b_3)


# save 
write_csv(d3_1,"./results/urchin_hoh_purple_abundance_location.csv")
write_csv(d3_3,"./results/urchin_hoh_purple_abundance_location3.csv")

write_csv(d4,"./results/urchin_hoh_abundance.csv")

write_csv(d5_1,"./results/urchin_hoh_abundance_location.csv")
write_csv(d5_3,"./results/urchin_hoh_abundance_location3.csv")

