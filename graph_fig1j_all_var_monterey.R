# -----------------------------------------------------
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
# -----------------------------------------------------
# Goal: graphing variables, data exploration, checking for correlations etc/.
# -----------------------------------------------------

# -----------------------------------------------------
# setup ########
library(tidyverse); library(RColorBrewer); library(colorspace); #library(modelr)

# -----------------------------------------------------
rm(list=ls())
source("./bin/deets.R")
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/all_var_location_2023_new_var.csv")%>%
  mutate(Urchin=urchin_u2,
         Source=kelp_source,
         year=as.numeric(year),
         ERL=yr_after_enso1_5_Mplus,
         Predator=predator3)%>%
  glimpse()


#visualize correlations of explanatory variables. -------------------------
# Other then very similar var, not correlated.
names(d1)
pairs(d1[,6:9]) # urchin data
pairs(d1[,14:17]) # pycno data
pairs(d1[,22:25]) # otter data
pairs(d1[,29:35]) # env  data
pairs(d1[,c(26,6,14,22)]) # temp urchins otters pycno


pairs(d1[,c(6,9,14,22,29)])

cor.test(d1$urchin_u2,d1$pycno_u2) # correlated - p-value = 0.001758
cor.test(d1$urchin_u2,d1$tempc_max) # not correlated - p-value = 0.2586
cor.test(d1$otter_u,d1$pycno_u2) # correlated - p-value = 0.000785
cor.test(d1$otter_u,d1$urchin_u2) # correlated - p-value = 7.059e-06
cor.test(d1$otter_u,d1$tempc_max)  # p-value = 3.071e-05
cor.test(d1$pycno_u2,d1$tempc_max)  # p-value = 0.008645
cor.test(d1$predator,d1$tempc_max) # not correlated
cor.test(d1$predator2,d1$tempc_max) # p val 0.49
cor.test(d1$predator,d1$urchin_u2)   # p<0.001
cor.test(d1$predator2,d1$urchin_u2)  # p<0.001
cor.test(d1$predator2,d1$urchin_u2)
cor.test(d1$ERL,d1$tempc_max)

# graphing correlations
plot(d1$predator,d1$urchin_u2)
plot(d1$predator2,d1$urchin_u2)
plot(d1$otter_u,d1$urchin_u2)
plot(d1$pycno_u2,d1$urchin_u2)

ggplot(d1,aes(x=predator,y=urchin_u2))+geom_point()+
  xlab("Predators\n(Relative Abundance)")+
  ylab("Purple Urchins\n(Relative Abundance-interaction)")


ggplot(d1,aes(x=predator2,y=urchin_u2))+geom_point()+
  xlab("Predators\n(Relative Abundance)")+
  ylab("Purple Urchins\n(Relative Abundance)")

# pred and pred2 have similar interactions with urchins

# box plots
# 2 pred
ggplot(d1,aes(x=factor(predator3),y=urchin_u2))+geom_boxplot()+
  xlab("Predator3")+
  ylab("Urchin\nRelative Abundance")+
  deets9

# 3 pred
ggplot(d1,aes(x=factor(predator4),y=urchin_u2))+geom_boxplot()+
  xlab("Predator(s) in Ecosystem")+
  ylab("Purple Urchins\n(Relative Abundance)")+
  deets9
ggsave("./doc/fig1j_urchins_by_predators.tiff",width=5,height=4)

# reduce urchin levels, selet subset of variables so less is lost with na.omit below.
d2<-d1%>%
  select(
    year,location,kelp,kelp2,kelp3,kelp_source,kelp_source2,urchin_u2,urchin2,pycno_u2,otter_u,enso_b2,enso_b1_5,enso_b1_5_Mplus,enso1_5:Year,tempc_max_lag1,tempc_max, Urchin, Source, Predator)%>%
  glimpse() # kelp4


# remove years with NA var, rescale d4
d3<-na.omit(d2)

# remove years >12 years after ENSO - rare but see year after enso 1_5 and M+, rescale d4
d4<-d3%>%
  filter(yr_after_enso2<=10)%>%
  glimpse()



# -----------------------------------
# graphs #################
# -------------------------------------
source("./bin/deets.R")

# years after ENSO -----------------------
# kelp by year after 1.5 & M+ ensos
ggplot(d3,aes(x=yr_after_enso1_5_Mplus,y=kelp,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5 & M+)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 1.5 & S ensos
ggplot(d3,aes(x=yr_after_enso1_5,y=kelp,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 2 & S ensos
ggplot(d3,aes(x=yr_after_enso2,y=kelp,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (2)")+ylab("Kelp\nRelative Abundance")+deets9

# years after ENSO - kelp 2-----------------------

# kelp by year after 1.5 & M+ ensos
ggplot(d3,aes(x=yr_after_enso1_5_Mplus,y=kelp2,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5 & M+)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 1.5 & S ensos
ggplot(d3,aes(x=yr_after_enso1_5,y=kelp2,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 2 & S ensos
ggplot(d3,aes(x=yr_after_enso2,y=kelp2,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (2)")+ylab("Kelp\nRelative Abundance")+deets9


# years after ENSO - kelp 3-----------------------

# kelp by year after 1.5 & M+ ensos
ggplot(d3,aes(x=yr_after_enso1_5_Mplus,y=kelp3,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5 & M+)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 1.5 & S ensos
ggplot(d3,aes(x=yr_after_enso1_5,y=kelp3,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 2 & S ensos
ggplot(d3,aes(x=yr_after_enso2,y=kelp3,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (2)")+ylab("Kelp\nRelative Abundance")+deets9

# years after ENSO - kelp 4-----------------------

# kelp by year after 1.5 & M+ ensos
ggplot(d3,aes(x=yr_after_enso1_5_Mplus,y=kelp5,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5 & M+)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 1.5 & S ensos
ggplot(d3,aes(x=yr_after_enso1_5,y=kelp5,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5 & S)")+ylab("Kelp\nRelative Abundance")+deets9
ggsave("./doc/kelp_enso_1_5_dist_all_yr.tiff",width=8,height=4)

# kelp by year after 2 & S ensos
ggplot(d3,aes(x=yr_after_enso2,y=kelp5,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (2 & S)")+ylab("Kelp\nRelative Abundance")+deets9


# years after ENSO - kelp 4 - no >10 yr -----------------------

# kelp by year after 1.5 & M+ ensos
ggplot(d4,aes(x=yr_after_enso1_5_Mplus,y=kelp5,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (1.5 & M+)")+ylab("Kelp\nRelative Abundance")+deets9

# kelp by year after 1.5 & S ensos
ggplot(d4,aes(x=yr_after_enso1_5,y=kelp5,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag")+ylab("Kelp\nRelative Abundance")+deets9
ggsave("./doc/kelp_enso_1_5_dist_lessthan10yr_year.tiff",width=8,height=4)

ggplot(d4,aes(x=yr_after_enso1_5,y=kelp5))+geom_jitter(aes(color=Predator))+
  geom_smooth()+
  xlab("ENSO Recovery Lag")+ylab("Kelp\nRelative Abundance")+deets9
ggsave("./doc/kelp_enso_1_5_dist_lessthan10yr_pred.tiff",width=8,height=4)

# kelp by year after 2 & S ensos
ggplot(d4,aes(x=yr_after_enso2,y=kelp5,color=Year))+geom_jitter()+
  geom_smooth()+
  xlab("ENSO Recovery Lag (2 & S)")+ylab("Kelp\nRelative Abundance")+deets9
ggsave("./doc/kelp_enso_dist_2_lessthan10yr.tiff",width=8,height=4)

#box plot
ggplot(d3,aes(x=factor(yr_after_enso1_5),y=kelp5,color=Year))+geom_boxplot()+
  xlab("ENSO Recovery Lag (1.5 & S)")+ylab("Kelp\nRelative Abundance")+deets9

ggplot(d3,aes(x=factor(predator3),y=urchin_u2,color=Year))+geom_boxplot()+
  xlab("Predator3")+ylab("Urchin\nRelative Abundance")+deets9

ggplot(d3,aes(x=year,y=kelp,color=urchin_u2))+geom_point()+deets3
ggplot(d3,aes(x=year,y=kelp,color=pycno_u2))+geom_point()+deets3
ggplot(d3,aes(x=year,y=kelp,color=otter_u))+geom_point()+deets3



# ---------------------------------------
# plot relationships
# -----------------------------------------

ggplot(d3,aes(y=kelp5,x=pycno_u2,color=urchin_u2, shape=kelp_source))+geom_jitter(width=.02,height=0.2,size=2)+
  facet_wrap(vars(enso_b2))+
  deets5

ggplot(d3,aes(y=kelp5,x=otter_u, color=urchin_u2, shape=kelp_source))+geom_jitter(width=.02,height=0.2,size=2)+
  facet_wrap(vars(enso_b2))+
  deets5

# FACETING BY ENSO ----------------
# this is sort of a duplicate of years since ENSO

# faceted by enso,  
ggplot(d4,aes(y=kelp2,x=yr_after_enso1_5,color=Predator, shape=kelp_source,size=urchin_u2))+
  geom_point(alpha=0.8)+
  facet_wrap(vars(enso_b2))+
  deets9



# FACETING BY PREDATOR3 -----------------------------------------
# This one is good

# faceted by # predator3 - 
# enso2
ggplot(d3,aes(y=kelp3,x=yr_after_enso2,color=tempc_max, shape=kelp_source,size=urchin_u2))+
  # ylim(-2.5,2.5)+
  geom_jitter(alpha=0.8)+
  facet_wrap(vars(Predator))+
  deets9

#enso 1.5 & S
ggplot(d4,aes(y=kelp3,x=yr_after_enso1_5, shape=Source,size=Urchin))+
  geom_jitter(alpha=0.8)+
  facet_wrap(vars(Predator))+
  xlab("ENSO Recovery Lag")+ylab("Kelp\nRelative Abundance")+deets9

ggplot(d4,aes(y=kelp3,x=yr_after_enso1_5, shape=Source,color=Urchin))+
  geom_jitter(alpha=0.8)+
  facet_wrap(vars(Predator))+
  xlab("ENSO Recovery Lag")+ylab("Kelp\nRelative Abundance")+deets9

#enso 1.5 & M+
ggplot(d3,aes(y=kelp3,x=yr_after_enso1_5_Mplus,color=tempc_max, shape=kelp_source,size=urchin_u2))+
  # ylim(-2.5,2.5)+
  geom_jitter(alpha=0.8)+
  facet_wrap(vars(predator3))+
  deets9

#  color = year
# enso 2 & S
ggplot(d4,aes(y=kelp5,x=yr_after_enso2,color=Year, shape=Source))+ 
  geom_jitter(alpha=0.8,size=3)+
  ylim(-2.5,2.5)+
  ylab("Kelp Abundance")+
  xlab("ENSO Recovery Lag")+
  facet_wrap(vars(predator3))+
  deets9+
  scale_color_continuous_sequential(palette = "Viridis")
ggsave("./doc/interactions_test_enso2.jpg",width=8,height=4)


# ENSO 1.5
d5<-d4%>%
  mutate(time=factor(ifelse(predator3_orig==1&year<1970,"One\nPredator, Early",
                     ifelse(predator3_orig==2,"Two\nPredators","One\nPredator, Late"))))%>%
  mutate(time=factor(time,order=T,levels=c("One Predator\n(Sunflower Seastar)","Two\nPredators","OnePredator\nOtter")))%>%
  glimpse()
levels(d5$time)
glimpse(tail(d5))

ggplot(d5,aes(y=kelp5,x=yr_after_enso1_5,color=Urchin, shape=Source))+ #,size=Urchin
  geom_jitter(alpha=0.8,size=3)+ #
  ylim(-2.5,2.5)+
  ylab("Kelp Abundance")+
  xlab("ENSO Recovery Lag")+
  facet_wrap(vars(time))+
  deets9+
  scale_color_continuous_sequential(palette = "Viridis")
ggsave("./doc/interactions_test_enso1_5_v2.jpg",width=8,height=3)

# ENso 1.5 & M+
ggplot(d4,aes(y=kelp5,x=yr_after_enso1_5_Mplus,color=Year, shape=Source))+ 
  geom_jitter(alpha=0.8,size=3)+
  ylim(-2.5,2.5)+
  ylab("Kelp Abundance")+
  xlab("ENSO Recovery Lag")+
  facet_wrap(vars(predator3))+
  deets9+
  scale_color_continuous_sequential(palette = "Viridis")
ggsave("./doc/interactions_test_enso1_5_Mplus.jpg",width=8,height=4)


