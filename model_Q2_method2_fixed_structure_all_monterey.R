# -----------------------------------------------------
# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University
# -----------------------------------------------------
# Q2. Method 2: Maximizing variables (1934–2020)
# -----------------------------------------------------
# Goal: model variance strucutre
# 
# -----------------------------------------------------
# urchins, pycno, otters and temp are correlated
# urchins and temp are not correlated
# temp and ERL and correlated so not including both in model
# see graph fig1j all var monterey for correlations
# -----------------------------------------------------
# setup ########
library(tidyverse); library(RColorBrewer); library(colorspace); library(nlme)  
# -----------------------------------------------------
rm(list=ls())
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

d1<-read_csv("./results/all_var_location_2023_new_var.csv")%>%
  mutate(Predator=factor(predator4),
         Urchin=urchin_u2,
         Source=kelp_source,
         ERL=yr_after_enso1_5_Mplus)%>%
  glimpse()

range(d1$year) #1934-2020

# reduce urchin levels, select subset of variables so less is lost with na.omit below.
d2<-d1%>%
  dplyr::select(
    year,location,kelp,kelp2,kelp3,kelp_source,kelp_source2,urchin_u2,pycno_u2,otter_u,enso_b2,enso_b1_5,enso_b1_5_Mplus,enso1_5:Year,tempc_max,ERL)%>%
  
  glimpse()

# remove years with NA var, rescale d4
d3<-na.omit(d2)


# remove years >12 years after ENSO - rare but see year after enso 1_5 and M+, rescale d4
d4<-d3%>%
  filter(ERL<=10)%>%
  mutate(kelp4=as.numeric(scale(kelp)))%>% # calc again for smaller dataset
  glimpse()

# ran model with d3. no sig difference with both and otters, so here combined levels - tried by can explore more interesting things if not reduced. so using d3
d5<-d3%>%
  mutate(Predators=predator4)%>%
  mutate(Predators=if_else(predator4=="Both" | predator4=="Sea otter","Both or Sea otters",predator4))%>%
  glimpse()

cbind(d5$year,d5$predator4,d5$Predators)

source("./bin/deets.R")

# --------------------------------------------------------------
# Modeling #########


# m0 --------------------------------------------------------------
m0<-gls(kelp~ERL+year+predator4*urchin_u2,
        correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),  
        varIdent(form=~1|kelp_source2),
        data=d3, method="ML") 
summary(m0)

m0.a<-update(m0,.~.-year)
m0.b<-update(m0,.~.-ERL)
m0.e<-update(m0,.~.-urchin_u2*predator4+urchin_u2+predator4)

anova(m0,m0.a)
anova(m0,m0.b)
anova(m0,m0.e)

# all significant


# # REFIT FINAL MODEL WITH REML ---------------------------
mFinal<-gls(kelp~ERL+year+predator4*urchin_u2,
            correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0),
            varIdent(form=~1|kelp_source2),
            data=d3, method="REML")
summary(mFinal)
anova(mFinal)

#===================================================
# save final model
#===================================================
save(mFinal,file="./results/table_SI10_model_all_mont_yearlocation_20240318.rda")

#===================================================
# save anova table
a1<-data.frame(anova(mFinal)) #set as df
a1$var<-row.names(a1); #a1$model<-modl # add identifying variables
row.names(a1)<-c(1:length(a1$p.value)); str(a1) # reset row names
a1
a1$F.value=round(a1$F.value,2)
a1$p.value=round(a1$p.value,2) #
a1$p.value<-as.character(a1$p.value)
a1$p.value[a1$p.value<"0.001"]<-"<0.001"
a1<-a1%>%
  dplyr::select(Variable=var,df=numDF,F.value,p.value)
a1
write_csv(a1,"./doc/table_SI10_kelpcover_yearlocationottersenso_anovatable_20240626.csv")

#===================================================
# save coefficents #####
#===================================================
a2<-data.frame(mFinal[4]) 
a2$var<-row.names(a2)
row.names(a2)<-c(1:length(a2$coefficients)) # reset row #s
str(a2)
a2$coefficients<-round(a2$coefficients,3)
a2<-a2%>%
  dplyr::select(Variable=var,coefficients)
a2
write_csv(a2,"./doc/table_SI10_model_all_mont_yearlocation_coefficents_20240626.csv")

