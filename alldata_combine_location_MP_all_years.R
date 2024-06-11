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
  # filter(year>1910)%>%
  glimpse()

# kelp --------------------
# maps & hoh, not pisco/reef check because that is stipes not canopy
d1<-read_csv("./results/kelp_allsources_abundance_location.csv")%>%
  select(year,location,kelp=kelp_abundance,kelp_present=present,kelp_source)%>% 
  filter(location=="Monterey Peninsula")%>% #year>1910,
  arrange(year,location)%>%
  glimpse()

t1<-d1%>%
  filter(!is.na(kelp))%>% #this eliminates presence only data- Do I want to do this?
  summarize(kelp_n=length(kelp))%>%glimpse()
kelp_n<-t1$kelp_n
kelp_year_n<-length(unique(d1$year))
range(d1$kelp,na.rm=T)
range(d1$kelp_present, na.rm=T)

unique(d1$kelp_source)

# urchins -----------------------------
# Published Surveys = old data from texts, contemporary data = pulled from raw spreadsheets
# combining these here. Currently considered differently in counts of n though
d2<-read_csv("./results/urchin_allsources_purple_abundance_location.csv")%>%
  select(year,location,urchin=urchin_abundance_u,urchin_present,urchin_source)%>%
  filter(location=="Monterey Peninsula")%>% #year>1910,
  mutate(urchin_source=if_else(urchin_source=="Published Surveys","Contemporary Data",urchin_source))%>% # think about this choice
  arrange(year,location)%>%
  glimpse()
unique(d2$urchin_source)


t2<-d2%>%
  filter(!is.na(urchin))%>% #this eliminates presence only data- Do I want to do this?
  summarize(urchin_n=length(urchin))%>%glimpse()
urchin_n<-t2$urchin_n
urchin_year_n<-length(unique(d2$year))
range(d2$urchin,na.rm=T)          #1-5
range(d2$urchin_present, na.rm=T) # 1-1

# eliminate NA, summarize when >1 obsv type in a year (recent years only)
unique(d2$urchin_source)

d2a<-d2%>%
  filter(!is.na(urchin))%>% # no presence only data - most years have PO & other records
  group_by(year,location)%>%
  summarize(
    urchin_u=mean(urchin,na.rm=T),
    urchin_max=max(urchin,na.rm=T),
    urchin_contemp_1st=max(urchin[urchin_source=="Contemporary Data"],na.rm=T),
    urchin_OH_1st=max(urchin[urchin_source=="Oral Histories"],na.rm=T),
    urchin_contemp_n=length(urchin[urchin_source=="Contemporary Data"]),
    urchin_OH_n=length(urchin[urchin_source=="Oral Histories"]),
    urchin_AD_n=length(urchin[urchin_source=="Archival Data"]),
    n=n()
  )%>%
  #fill in non-contemp values in older years
  mutate(urchin_contemp_1st=if_else(urchin_contemp_n==0,urchin_u,urchin_contemp_1st))%>% #when no contemp data max and mean are the same
  # fill in non-OH data for years when no OH
  mutate(urchin_OH_1st=if_else(urchin_OH_n==0,urchin_u,urchin_OH_1st))%>% #
  glimpse()

head(d2a)
tail(d2)


# check
ggplot(d2a,(aes(x=year,y=urchin_contemp_1st,color=factor(urchin_contemp_n))))+geom_point()
ggplot(d2a,(aes(x=year,y=urchin_OH_1st,color=factor(urchin_OH_n))))+geom_point()
ggplot(d2a,(aes(x=year,y=urchin_u,color=factor(n))))+geom_point()
ggplot(d2a,(aes(x=year,y=urchin_u,color=factor(urchin_AD_n))))+geom_point()

# pycno -------------------------------
d3<-read_csv("./results/pycno_allsources_abundance_location.csv")%>% #interpolated data
  select(year,location,pycno=abundance_u,pycno_source=source)%>%
  # mutate(pycno_source=if_else(pycno_source=="Published Surveys","Contemporary Data",urchin_source))%>% # think about this
  mutate(pycno_present=ifelse(pycno>0,1,pycno))%>%
  filter(location=="Monterey Peninsula")%>% #year>1910,
  arrange(year,location)%>%
  glimpse()

t3<-d3%>%filter(!is.na(pycno))%>%summarize(pycno_n=length(pycno))%>%glimpse()
pycno_n<-t3$pycno_n
pycno_year_n<-length(unique(d3$year))
range(d3$pycno,na.rm=T)             # 0 - 5
range(d3$pycno_present, na.rm=T)    # 0 -1

# eliminate NA, summarize when >1 obsv type in a year (recent years only)
unique(d3$pycno_source)

d3a<-d3%>%
  filter(!is.na(pycno))%>% # no presence only data - most years have PO & other records
  group_by(year,location)%>%
  summarize(
    pycno_u=mean(pycno),
    pycno_max=max(pycno),
    pycno_contemp_1st=(max(pycno[pycno_source=="Published Surveys" | pycno_source=="Ecological Data"],na.rm=T)),
    pycno_OH_1st=(max(pycno[pycno_source=="Oral Histories"],na.rm=T)),
    pycno_contemp_n=length(pycno[pycno_source=="Published Surveys"])+ length(pycno[pycno_source=="Ecological Data"]),
    pycno_OH_n=length(pycno[pycno_source=="Oral Histories"]),
    pycno_AD_n=length(pycno[pycno_source=="Archival Data"]),)%>%
  
  #fill in non-contemporary ecological values in older years
  mutate(pycno_contemp_1st=if_else(pycno_contemp_n==0,pycno_u,pycno_contemp_1st),
         pycno_OH_1st=if_else(pycno_OH_n==0,pycno_u,pycno_OH_1st))%>% #when no contemp data max and mean are the same
  glimpse()




# otters --------------------------------
d4<-read_csv("./results/otter_allsources_abundance_location.csv")%>%
  select(year,location,otter=otter_abundance_u,otter_source)%>%
  mutate(otter_present=ifelse(otter>1.5,1,otter))%>%
  mutate(otter_source=if_else(otter_source=="Literature, Journals, Field Notes","Archival Data",otter_source))%>%
  filter(location=="Monterey Peninsula")%>% #year>1910,
  arrange(year,location)%>%
  glimpse()

t4<-d4%>%filter(!is.na(otter))%>%summarize(otter_n=length(otter))%>%glimpse()
otter_n<-t4$otter_n
otter_year_n<-length(unique(d4$year))
range(d4$otter,na.rm=T)           # 0 to 5
range(d4$otter_present, na.rm=T)  # 0 to 1



# eliminate NA, summarize when >1 obsv type in a year (recent years only)
unique(d4$otter_source)


d4a<-d4%>%
  filter(!is.na(otter))%>% # no presence only data - most years have PO & other records
  group_by(year,location)%>%
  summarize(
    otter_u=mean(otter),
    otter_max=max(otter))%>%#,
  # otter_contemp_1st=(max(otter[otter_source=="Contemporary Data"])),
  # otter_OH_1st=(max(otter[otter_source=="Oral Histories"])),
  # otter_contemp_n=length(otter[otter_source=="Contemporary Data"]),
  # otter_OH_n=length(otter[otter_source=="Oral Histories"]),
  # otter_AD_n=length(otter[otter_source=="Archival Data"])
  
  #fill in non-contemporary ecological values in older years
  
  # mutate(otter_contemp_1st=if_else(otter_contemp_n==0,otter_u,otter_contemp_1st),
  # otter_OH_1st=if_else(otter_OH_n==0,otter_u,otter_OH_1st))%>% #when no contemp data max and mean are the same 
  glimpse()








# SST -------------------------------------
d5<-read_csv("./results/sst_c_year.csv")%>%
  arrange(year)%>%
  filter(year<2021)%>% #year>1910&
  glimpse()

filter(d5,year>1970)
# ENSO ------------------
# merged MEIv2 > 1.5, Quinn: S or greater, Gergis & Fowler: Extreme, MHW
d6<-read_csv("./data/enso_merged_MEIv2_15_QuinnS_GergisE_MHW.csv")%>%
  # filter(year>1910)%>%
  arrange(year)%>%
  glimpse()



names(d1)
names(d2)
names(d3)
names(d4)
names(d5)
names(d6)



# combine biol var ------------------
# only keep years with kelp (left_join)
d7<-d1%>%
  left_join(d2a)%>%
  left_join(d3a)%>%
  left_join(d4a)%>%
  arrange(year,location)%>%
  glimpse()
# view(d7)
d7

# combine env var -----------------
d8<-d5%>%
  full_join(d6)%>%
  glimpse()
d8

d9<-d7%>%
  full_join(d8)%>%
  arrange(year)%>%
  glimpse()
write_csv(d9,"./results/all_var_location_2024_allyr.csv")

# save samples sizes (no presence only) -------------------------------------
vari<-c("kelp_n","kelp_year_n","urchin_n","urchin_year_n","pycno_n","pycno_year_n","otter_n","otter_year_n")
value<-c(kelp_n,kelp_year_n,urchin_n,urchin_year_n,pycno_n,pycno_year_n,otter_n,otter_year_n)

d10<-data.frame(vari)%>%
  cbind(data.frame(value))%>%
  glimpse()

write_csv(d10,"./doc/sampsz_all_data_location_noPresOnly_allyr.csv")


# ------------------------------------
# interpolating
# -------------------------------------
# URCHIN: lead & lag - up to 5 years - location and mean abundance---------------------------------------
d11<-d9%>%
  arrange(year)%>%
  mutate(urchin1=ifelse(is.na(urchin_u),lead(urchin_u ),urchin_u ),
         urchin2=ifelse(is.na(urchin1),lead(urchin1,n=1),urchin1),
         urchin3=ifelse(is.na(urchin2),lead(urchin2,n=1),urchin2),
         # urchin4=ifelse(is.na(urchin3),lead(urchin3,n=1),urchin3),
         # urchin5=ifelse(is.na(urchin4),lead(urchin4,n=1),urchin4),
         urchin1b=ifelse(is.na(urchin3),lag(urchin3,n=1),urchin3),
         urchin2b=ifelse(is.na(urchin1b),lag(urchin1b,n=1),urchin1b),
         urchin3b=ifelse(is.na(urchin2b),lag(urchin2b,n=1),urchin2b)
         # urchin4b=ifelse(is.na(urchin3b),lag(urchin3b,n=1),urchin3b),
         # urchin5b=ifelse(is.na(urchin4b),lag(urchin4b,n=1),urchin4b)
  )%>%
  mutate(urchin_u2=urchin3b)%>%
  select(year:kelp_source,urchin_u2,urchin_u:urchin_AD_n,pycno_u:sources)%>%
  glimpse()

d11
tail(d11)
names(d11)


# pycno interpolating ----------------------------------
# lead & lag - up to 6 years - location ---------------------------------------
d13<-d11%>%
  arrange(year)%>%
  mutate(pycno1=ifelse(is.na(pycno_u),lead(pycno_u,n=1),pycno_u),
         pycno2=ifelse(is.na(pycno1),lead(pycno1,n=1),pycno1),
         pycno3=ifelse(is.na(pycno2),lead(pycno2,n=1),pycno2),
         pycno4=ifelse(is.na(pycno3),lead(pycno3,n=1),pycno3),
         pycno5=ifelse(is.na(pycno4),lead(pycno4,n=1),pycno4),
         pycno6=ifelse(is.na(pycno5),lead(pycno5,n=1),pycno5),
         
         pycno1b=ifelse(is.na(pycno6),lag(pycno6,n=1),pycno6),
         pycno2b=ifelse(is.na(pycno1b),lag(pycno1b,n=1),pycno1b),
         pycno3b=ifelse(is.na(pycno2b),lag(pycno2b,n=1),pycno2b),
         pycno4b=ifelse(is.na(pycno3b),lag(pycno3b,n=1),pycno3b),
         pycno5b=ifelse(is.na(pycno4b),lag(pycno4b,n=1),pycno4b),
         pycno6b=ifelse(is.na(pycno5b),lag(pycno5b,n=1),pycno5b)
  )%>%
  mutate(pycno_u2=round(pycno6b,2))%>%
  select(year:urchin_AD_n,pycno_u2,pycno_u:sources)%>%
  glimpse()

glimpse(d13%>%filter(is.na(pycno_u2)&year>=1934))

# select for years with kelp data -----------------------------------
d20<-d13%>%
  filter(kelp>=0)%>%
  glimpse()

write_csv(d20,"./results/all_var_location_2024_interpol_allyr.csv")

