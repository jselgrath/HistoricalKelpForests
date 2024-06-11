# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# GOAL: organize historical data 
# GOAL: calculate means for historical descriptions pycnopodia data
# added a couple of other historical references in June 2023, changed to keep broader CA references because new ones useful - some from people in PG
##################################################

# setup ########################################
library(tidyverse); library(tidyr)
################################################
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# literature and field notes
d1a<-read_csv("./data/pycnopodia_historical_long_20240308.csv")%>% # this estimates years from John's paper in Fig 5 (note: lower than his written estimates)
  glimpse()
d1<-filter(d1a,location!="Other")%>%
  select(year:quantitative,author)%>%
  select(-location_detail,-abundance_quote,-sampling_unit)%>%glimpse() #remove rows with no abundance information or from other locations

# decision rules about abundance: consider using breaks based on ABC reef data
# 0 or unobserved = 0; >0-0.0008 = 1;  0.0008-0.0087 = 2; 0.0087-0.0166/common = 3; 0.0166-0.048/ abundant = 4; >0



# checking and organizing ------------------------------------------
unique(d1$location3)
unique(d1$location)

#Ricketts so assigned to both Monterey BAy and Monterey Outer.
temp<-filter(d1,location3=="Monterey Unspecified")  
temp$location3[temp$location3=="Monterey Unspecified"]<-"Monterey Bay"

d1$location3[d1$location3=="Monterey Unspecified"]<-"Monterey Outer" 
d1$location3[d1$location3=="West Coast"]<-"California" 
d1c<-rbind(d1,temp)

unique(d1c$location)
unique(d1c$location3)
d1c


# organize and remove descriptions that seemed unreliable (e.g. unclear if personal observations or tall tales and arm waving) ##########
# taking out but can add in if needed -mostly did not use that variable


write_csv(d1c,"./results/pycno_historical_allsources_clean.csv")