# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

# March 5, 2020
##################################################
# Goal: change enso data from wide to long

##################################################
# Data info - sourced 3/4/2020
# original data from NOAA (NINO3.4), with anomolies removed based on 1981-2010 data
# https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino34/
# Source: Data is from PSD using the HadISST1 dataset.
# References:    Rayner N. A., D. E. Parker, E. B. Horton, C. K. Folland, L. V. Alexander, D. P. Rowell, E. C. Kent, A. Kaplan, Global analyses of sea surface temperature, sea ice, and night marine air temperature since the late nineteenth century, J. Geophys. Res., 108 (D14), 4407, doi:10.1029/2002JD002670, 2003. 
##################################################

# setup ########################################
library(tidyverse); library(RColorBrewer);  library(readr); library(RcppRoll)
rm(list=ls())

#################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/Kelp")

# enso data
d1<-read_csv(file="./data/ENSO_MEI_normalized.csv")
d1


d2<-d1%>%
  dplyr::select(year=YEAR,DECJAN:NOVDEC)%>%
  pivot_longer(-year,names_to="month",values_to="enso_mei")
d2$enso_mei<-round(d2$enso_mei,2) #so all years have same precision

write_csv(d2,"./results/enso_datalong_mei.csv")
