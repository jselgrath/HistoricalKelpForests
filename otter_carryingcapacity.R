# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to convert estimated otter carrying capacity to metric
##################################################

# setup ########################################
library(tidyverse)
rm(list=ls())
################################################
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

######################################################
#  #################################

# Estimated carrying capacity 
# CDFG report 1972 - 13 otters/mi2 to 20fm
# Laidre 2001 - mean(5.15, 5.15,5.05) = 5.12 rocky habitat 
#               mean(.95+1.12+1.13) = 1.07 sandy
#               mean(.74+.69+.92) = 0.78 mixed

# conversions #www.conversionsoup.com
# 20 fm = 36.576m
# 1mi2=2.589988km2
# 30/36.576=0.82021  # conversion of 20 fm to 30m
# 40/36.576=1.093613 # changing of 20 fm to 40m
# 30/40 = 0.75

#CDFW
cc_mi2_20fm<-13 # 13 otters/mi2 to 20fm
cc_km2_20fm<-cc_mi2_20fm/2.58999 # covert to km
cc_km2_30m<-cc_km2_20fm*0.82021 # convert 20fm to 30fm
cc_km2_40m<-cc_km2_20fm*1.093613

cc_km2_30m
cc_km2_40m

#LAIDRE
cc_km2_40m_l <- 5.12 # density to 40 fathoms
cc_km2_30m_l= cc_km2_40m_l*0.75 # density to 30 fathoms

#results
cc_km2_30m   # 4.12
cc_km2_30m_l # 3.84

cc_km2_40m
cc_km2_40m_l
