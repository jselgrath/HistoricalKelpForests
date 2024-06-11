# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to set years otters arrived and relative abundances = historical data
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# OTTER RETURN YEARS BY LOCATION #############

# 1938 = year otters rediscovered in spring in Big Sur

# 1963 = mean value of when otters returned to  peninsula (1959 - pt Joe & 1967 - Hopkins - see A Proposal for Sea Otter Protection and Research and Request for the Return of Management to the State of California Vol 1, 1976 by CA Dept fish & Game. Table 1.)

# 1977 = from Pearse & Hines 1979: Few sea otters were seen [in Santa Cruz] in 1974, 1975, and 1976, and those few were usually seen during the winter and spring. In the spring of 1977, when the northern edge of the expanding range of sea otters reached within 5 km of Point Santa Cruz (personal observation), as many as 11 "advance" sea otters were seen in the study area. Many of the sea otters at Point Santa Cruz in the spring of 1977 were feeding on sea urchins with in or near Station 2, and broken sea urchin tests littered the bottom there during the June, 1977, study period.

# Dates from Table 1 and text of CADFG report mentioned above. Last date in report (1975) otter front is south of Santa Cruz at Sunset Beach

# Ano Nuevo data from Riedman and Estes because it was after CDFG data published
#1955 = pt lobos, could also be 1959 = carmel

d1<-structure(list(location=c("Big Sur", "Monterey Peninsula", "Santa Cruz"),otter_return_loc=c(1914,1963,1977)), class = "data.frame", row.names = c(NA, -3L))
d1
d1$source<-"Literature, Journals, Field Notes"
d1$reference<-"CDFG Report 1976"
d1$organism<-"sea otters"

write_csv(d1,"./results/otter_returnyear_location.csv")



# OTTER RETURN DATES LOCATION2 #################################
d2<-structure(list(location2=c("Big Sur","Carmel","Monterey Outer","Monterey Bay","Santa Cruz Bay",   "Santa Cruz Outer"),otter_return_loc2=c(1914,1956,1959,1966,1977,1984)), class = "data.frame", row.names = c(NA, -6L)) 
d2
d2$source<-"Literature, Journals, Field Notes"
d2$reference<-"CDFG Report 1976"
d2$reference[d2$otter_return_loc2==1984]<-"Redmond & Estes"
d2$organism<-"sea otters"
d2
# write_csv(d2,"./results/otter_returnyear_location2.csv")


# OTTER RETURN DATES LOCATION3 ####################################
d2b<-structure(list(location3=c("Big Sur","Carmel Outer","Carmel","Monterey Outer","Monterey Bay","Santa Cruz Bay", "Santa Cruz Outer"),otter_return_loc2=c(1914,1951,1956,1959,1966,1977,1984)), class = "data.frame", row.names = c(NA, -7L)) 
d2b
d2b$source<-"Literature, Journals, Field Notes"
d2b$reference<-"CDFG Report 1976"
d2b$reference[d2$otter_return_loc2==1984]<-"Redmond & Estes"
d2b$organism<-"sea otters"
d2b

write_csv(d2b,"./results/otter_returnyear_location3.csv")
