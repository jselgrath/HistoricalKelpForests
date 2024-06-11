################################################
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: integrate final versions of data for Models
#       set first year to 1921
##################################################

# -----------------------------------------------------
# setup ###########
library(tidyverse); library(RColorBrewer); library(colorspace); library(modelr)  

# -----------------------------------------------------
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

#years --------------------------------------
dy<-read_csv("./data/enso_merged_MEIv2_15_QuinnS_GergisE_MHW.csv")%>%
  select(year)%>% 
  glimpse()

# kelp --------------------
# maps & hoh, not pisco/reef check because that is stipes not canopy
d1<-read_csv("./results/kelp_map_allsources_area_location3.csv")%>%
  arrange(year,location3)%>%
  glimpse()
range(d1$year)
unique(d1$kelp_source)

kelp_year_n<-length(unique(d1$year))%>%glimpse()


# urchins & pycno - short time series, not including here -----------------------------

# otters --------------------------------
# consider present if relative abundance >1.5 (i.e. not just wanderers)
d4<-read_csv("./results/otter_allsources_abundance_location3.csv")%>%
  select(year,location3,otter=otter_abundance_u,otter_source)%>%
  mutate(otter_present=ifelse(otter>1.5,1,0))%>%
  mutate(otter_source=if_else(otter_source=="Literature, Journals, Field Notes","Archival Data",otter_source))%>%
  arrange(year,location3)%>%
  glimpse()

otter_year_n<-length(unique(d4$year))%>%glimpse()
range(d4$otter,na.rm=T)           # 0 to 5

# eliminate NA, summarize when >1 obsv type in a year (recent years only)
unique(d4$otter_source)


d4a<-d4%>%
  filter(!is.na(otter))%>% # no presence only data - most years have PO & other records
  group_by(year,location3)%>%
  summarize(
    otter_u=mean(otter),
    otter_max=max(otter))%>% 
  glimpse()


# SST - not including starts in 1915-------------------------------------


# ENSO ------------------
# merged MEIv2 > 1.5, Quinn: S or greater, Gergis & Fowler: Extreme, MHW
d6<-read_csv("./data/enso_merged_MEIv2_15_QuinnS_GergisE_MHW.csv")%>%
  arrange(year)%>%
  glimpse()



names(d1)
names(d4a)
names(d6)



# combine  var ------------------
# only keep years with kelp maps (left_join)
d7<-d1%>%
  left_join(d4a)%>%
  left_join(d6)%>%
  arrange(year,location3)%>%
  glimpse()
# view(d7)
d7


write_csv(d7,"./results/all_var_location3_all_yr.csv")

