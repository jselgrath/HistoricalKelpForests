# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: oral history otter data
##################################################

# setup ########################################
library(tidyverse);
rm(list=ls())

#################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/")

d1<-read_csv("./OralHistories/results/otter_kelp_oh_subset_long.csv")%>%
  filter(location3!="Other")
names(d1)
unique(d1$location)
unique(d1$location3)

# set wd to kelp project
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp/")

d1$source<-"Oral Histories"

# summarize at location level for all otters########################
d2<-d1%>%
  filter(organism=="sea otters")

# clean location names
unique(d2$location) # not cleaned
d2$location0<-d2$location # save uncleaned locations to another variable name
d2$location<-d2$location2
d2$location[d2$location=="Monterey Bay" | d2$location=="Monterey Outer" | d2$location=="MontereyBay" | d2$location=="MontereyOuter" | d2$location=="MontereyCarmel" | d2$location=="MontereyUnspecified" | d2$location== "Carmel"] <- "Monterey Peninsula"
d2$location[d2$location=="BigSur"]<-"Big Sur"
d2$location[d2$location=="SantaCruz"]<-"Santa Cruz"
          
unique(d2$location) # cleaned

# Summarize by location level
# location3
d2$location3[d2$location3=="MontereyUnspecified"]<-"Monterey Bay"
d3<-d2%>%
  group_by(year,location3,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))
unique(d2$location3) # cleaned
d3
tail(d3)

# location2
# d2$location2[d2$location2=="MontereyUnspecified"]<-"Monterey Bay"
# d4<-d2%>%
#   group_by(year,location2,organism,source)%>%
#   summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))
# d4
# tail(d4)

# location
d5<-d2%>%
  group_by(year,location,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))
unique(d2$location2) # cleaned
d5
tail(d5)



# all otters all sites combined
d6<-d2%>%
  group_by(year,organism,source)%>%
  summarize(abundance_u=round(mean(abundance),1), abundance_sd=sd(abundance),abundance_n=length(year))

d6
tail(d6)

write_csv(d2,"./results/otter_oh_raw.csv")
write_csv(d3,"./results/otter_oh_summarized_location3.csv")
write_csv(d5,"./results/otter_oh_summarized_location.csv")
write_csv(d6,"./results/otter_oh_summarized.csv")
