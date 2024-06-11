# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# Goal: to integrate oral history and other historical data

##################################################
library(tidyverse)
library(scales)
library(ggthemes) #tableau colors
library(modelr)
#####################################################
rm(list=ls())
dateToday=Sys.Date()

# oral history data 
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

l1<-list.files("./data/otter_density_location3/", pattern=".csv") #i = includes regions (identity fxn in GIS)
l1

# read historical files
d1a<-read_csv(paste0("./data/otter_density_location3/",l1[[1]]))
d1b<-read_csv(paste0("./data/otter_density_location3/",l1[[2]]))
d1c<-read_csv(paste0("./data/otter_density_location3/",l1[[3]]))
d1d<-read_csv(paste0("./data/otter_density_location3/",l1[[4]]))
d1e<-read_csv(paste0("./data/otter_density_location3/",l1[[5]]))
d1f<-read_csv(paste0("./data/otter_density_location3/",l1[[6]]))
d1g<-read_csv(paste0("./data/otter_density_location3/",l1[[7]]))
d1h<-read_csv(paste0("./data/otter_density_location3/",l1[[8]]))

d1i<-read_csv(paste0("./data/otter_density_location3/",l1[[9]]))
d1j<-read_csv(paste0("./data/otter_density_location3/",l1[[10]]))
d1k<-read_csv(paste0("./data/otter_density_location3/",l1[[11]]))
d1l<-read_csv(paste0("./data/otter_density_location3/",l1[[12]]))
d1m<-read_csv(paste0("./data/otter_density_location3/",l1[[13]]))
d1n<-read_csv(paste0("./data/otter_density_location3/",l1[[14]]))
d1o<-read_csv(paste0("./data/otter_density_location3/",l1[[15]]))
d1pp<-read_csv(paste0("./data/otter_density_location3/",l1[[16]])) # other format
d1qq<-read_csv(paste0("./data/otter_density_location3/",l1[[17]])) # other format
d1rr<-read_csv(paste0("./data/otter_density_location3/",l1[[18]])) # other format
d1ss<-read_csv(paste0("./data/otter_density_location3/",l1[[19]])) # other format

d1r<-read_csv(paste0("./data/otter_density_location3/",l1[[20]]))
d1s<-read_csv(paste0("./data/otter_density_location3/",l1[[21]]))
d1t<-read_csv(paste0("./data/otter_density_location3/",l1[[22]]))
d1u<-read_csv(paste0("./data/otter_density_location3/",l1[[23]]))
d1v<-read_csv(paste0("./data/otter_density_location3/",l1[[24]]))
d1w<-read_csv(paste0("./data/otter_density_location3/",l1[[25]]))
d1x<-read_csv(paste0("./data/otter_density_location3/",l1[[26]]))
d1y<-read_csv(paste0("./data/otter_density_location3/",l1[[27]]))
d1z<-read_csv(paste0("./data/otter_density_location3/",l1[[28]]))
d1aa<-read_csv(paste0("./data/otter_density_location3/",l1[[29]]))

d1bb<-read_csv(paste0("./data/otter_density_location3/",l1[[30]]))
d1cc<-read_csv(paste0("./data/otter_density_location3/",l1[[31]]))
d1dd<-read_csv(paste0("./data/otter_density_location3/",l1[[32]]))
d1ee<-read_csv(paste0("./data/otter_density_location3/",l1[[33]]))
d1ff<-read_csv(paste0("./data/otter_density_location3/",l1[[34]]))

t1<-data.frame(expand_grid(l1=letters)) 

t2<-paste0("d1",t1[[1]])
t2

#combine chart data, drop some numbers - can get later
d2<-rbind(d1a,d1b,d1c,d1d,d1e,d1f,d1g,d1h,d1i,d1j,d1k,d1l,d1m,d1n,d1o,d1r,d1s,d1t,d1u,d1v,d1w,d1x,d1y,d1z,d1aa,d1bb,d1cc,d1dd,d1ee,d1ff,d1pp,d1qq,d1rr,d1ss)%>%
  dplyr::select(year=Year,location3,depth=DEPTH,freq=FREQUENCY,popdens_km_u=MEAN_dens_sm,popdens_km_sd=STD_dens_sm,popdens_km_n=COUNT_dens_sm,trend5yr_u=MEAN_trend5yr,trend5yr_sd=STD_trend5yr,trend5yr_n=COUNT_trend5yr,area_ha=SUM_HECTARES)
d2

tail(d2)

d2$organism<-"sea otters"
d2$source<-"cdfw"


write_csv(d2,"./results/otter_cdfw_dens_location3.csv")



############################################################
# make same as other data ############
#############################################################
range(d2$popdens_km_u) #0.2609642 28.0988837
range(d2$popdens_km_u[d2$depth=="0 to -30m"]) #0.2609642 28.0988837
range(d2$popdens_km_u[d2$depth=="-30 to -60m"]) #0.000000 3.890669

#calculate mean proportion in deeper water
mean((d2$popdens_km_u[d2$depth=="-30 to -60m"]) /((d2$popdens_km_u[d2$depth=="0 to -30m"]) +(d2$popdens_km_u[d2$depth=="-30 to -60m"]))) #0.042

range(d2$trend5yr_u[d2$depth=="0 to -30m"]) #  -0.1892140  0.2728177
range(d2$trend5yr_u[d2$depth=="-30 to -60m"]) # -0.1759078  0.2718126

d3<-d2%>%
  filter(depth=="0 to -30m")%>%
  dplyr::select(year,location3,organism,source,popdens_km_u,popdens_km_sd,popdens_km_n)%>%
  arrange(popdens_km_u)

# d3$otter_km_se<-d3$popdens_km_sd/sqrt(d3$popdens_km_n)


# range
range(d3$popdens_km_u)
# 0.2609642 28.0988837

# EQUAL INTERVAL ####################################################
# options for splitting
seq_range(d3$popdens_km_u,7) # seems too evenly split since recent values are outliers, and above original range
# 0.2609642  4.9006175  9.5402707 14.1799240 18.8195772 23.4592305 28.0988837

t1<-seq_range(0.2609642:4.5,6);t1
#0.2609642 1.0609642 1.8609642 2.6609642 3.4609642 4.2609642
t2<-seq_range(8:28.0988837,4);t2
# 8.00000 14.66667 21.33333 28.00000

d3$otter_abundance_e<-9999
d3$otter_abundance_e[d3$popdens_km_u==0]<-0 #non are 0 fyi
d3$otter_abundance_e[d3$popdens_km_u>=t1[1]]<-1
d3$otter_abundance_e[d3$popdens_km_u>=t1[2]]<-2
d3$otter_abundance_e[d3$popdens_km_u>=t1[3]]<-3
d3$otter_abundance_e[d3$popdens_km_u>=t1[4]]<-4
d3$otter_abundance_e[d3$popdens_km_u>=t1[5]]<-5
d3$otter_abundance_e[d3$popdens_km_u>8]<-6 # split approx evenly between 7 and 28.... [max]
d3$otter_abundance_e[d3$popdens_km_u>14.666]<-7
d3$otter_abundance_e[d3$popdens_km_u>21.333]<-8
tail(d3)


############################################
# STANDARD DEVIATION ######################
range(d3$popdens_km_u) #0.2609642 28.0988837
otter_sd<-sd(d3$popdens_km_u); otter_sd #6.660047
otter_3<-mean(d3$popdens_km_u); otter_3 #7.859334
otter_2<-otter_3-otter_sd
otter_1<-otter_3-(2*otter_sd)
otter_4<-otter_3+otter_sd
otter_5<-otter_3+(2*otter_sd)

otter_rel3<-c(otter_1,otter_2,otter_3,otter_4,otter_5); otter_rel3

# otter_1 is a negative number, so remove from relative ranking. Split between 0 and mean because mean was considered previous max
otter_rel3<-round(c(0,(otter_3/2),otter_3,otter_4,otter_5),2); otter_rel3

d3$otter_abundance_u<-999
d3$otter_abundance_u[d3$popdens_km_u==0]<-0
d3$otter_abundance_u[d3$popdens_km_u>0]<-1
d3$otter_abundance_u[d3$popdens_km_u>=otter_rel3[2]]<-2
d3$otter_abundance_u[d3$popdens_km_u>=otter_rel3[3]]<-3
d3$otter_abundance_u[d3$popdens_km_u>=otter_rel3[4]]<-4
d3$otter_abundance_u[d3$popdens_km_u>=otter_rel3[5]]<-5

d3
range(d3$otter_abundance_u)


d3<-d3%>%
  dplyr::select(year,location3,organism,source,popdens_km_u, popdens_km_sd, popdens_km_n,otter_abundance_u)%>%
  arrange(year,location3)

#save
write_csv(d3,"./results/otter_cdfw_dens_0-30m_location3.csv")




