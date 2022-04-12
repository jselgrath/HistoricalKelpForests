# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

##################################################
# historical kelp forests
##################################################
setwd("C:/Users/jselg/Dropbox/0Research/R.projects/MontereyBayChange/kelp")

#=============================================
# KELP MAPS #################################

# combine data from GIS that estimates total kelp area (1) in a year given the total area mapped without kelp (0)
# calcluate percent cover
# calulate relative abudance - breaks specific to location groupings
source("./bin/kelp_map_area_combineyr_location3.R")
# input: list.files("./data/kelparea_2020april/", pattern=".csv") 
# output: ./results/kelp_map_allsources_area.csv # this is not that organized below  are best to use
#         ./results/kelp_map_allsources_area_location.csv
#         ./results/kelp_map_allsources_area_location3.csv

# NOTE: # tried removing patches < 0.01 ha. This did not change much so not using

#################################################################
# KELP: ORAL HISTORY AND ARCHIVAL DATA ##########################

# integrate different data sources for kelp
# this has nereo and macro species info where avaliable
# location2 == location3 here, also has location
source("./bin/kelp_historical_orgainze.R")
# input:    ./data/kelphistorical_expanded3.csv
# output:   ./results/kelp_historical_abundance_raw.csv
#           ./results/kelp_historical_abundance_raw_nomapdata.csv                 #removed mcfarland, kept CA AcademyScience report
#           ./results/kelp_historical_sp_abundance_location_reference.csv         # and location23 version
#           ./results/kelp_historical_sp_abundance_location.csv                   # and location23 version
#           ./results/kelp_historical_abundance_location.csv                      # and location23 version
#           ./results/kelp_historical_abundance.csv

# estimate mean and max abundance by year of oral history data
# this has species information
source("./bin/kelp_oh_organize.R") 
# input:  ./OralHistories/results/otter_kelp_oh_subset_long.csv
# output: ./results/kelp_oh_abundance_raw.csv
#         ./results/kelp_oh_sp_abundance_location.csv 
#         ./results/kelp_oh_sp_abundance_location3.csv 
#         ./results/kelp_oh_abundance_location.csv
#         ./results/kelp_oh_abundance_location3.csv
#         ./results/kelp_oh_abundance.csv


# combine historic and oral history kelp estimates by location
source("./bin/kelp_hoh_combine_location.R")
# input:    ./results/kelp_oh_abundance_location.csv               # also this with species data
#           ./results/kelp_historical_abundance_location.csv        # also this with species data
#           ./results/kelp_historical_abundance_location23.csv
#           ./results/kelp_oh_abundance_location3.csv               
# output:   ./results/kelp_hoh_abundance_location.csv")
#           ./results/kelp_hoh_sp_abundance_location.csv            
#           ./results/kelp_hoh_abundance_location3.csv")
#           ./results/kelp_hoh_sp_abundance_location3.csv            


#####################################################################
# INTEGRATING DIFFERENT KELP MAP and ORAL HISTORY DATA SOURCES #############################
# Integrate Kelp Maps and interviews - for all location versions
source("./bin/kelp_allsources_combine_location123.R")
# input:    ./results/kelp_hoh_abundance_location.csv# this is only location3
#           ./results/kelp_map_allsources_area_location3.csv
#           ./results/kelp_map_allsources_area_location.csv
# output:   ./results/kelp_allsources_abundance_location.csv 
#           ./results/kelp_allsources_abundance_location3.csv


# graph kelp sources and abundance by year
source("./bin/kelpallsources_graph.R")
# input:    ./results/kelp_map_allsources_area_location.csv
# output:   ./doc/Fig_S2D_kelp_maparea_location.jpg
#           ./doc/kelp_map_areap_location.jpg
#           ./doc/kelp_map_areap-source_location.jpg


################################
# General PISCO transect information
################################
# list of sites surveyed during all years. (no transect information)
# *****SWATH SURVEYS ONLY*****
source("./bin/PISCO_site_surveyyear.R")
# input:        ./data/PISCO_kelpforest_site_table.1.2.csv # through 2019
#               ./data/MLPA_kelpforest_site_table.4 2021.csv #through 2020
#               ./data/PISCO_RC_location.csv
# output:       ./results/PISCO_site_year.csv #through 2019
#               ./results/PISCO_MLPA_site_year.csv #through 2020

# one line per swath survey of PISCO data
source("./bin/PISCO_transect_site_year.R")
# input:        ./data/MLPA_kelpforest_swath.4.csv 
# output:       ./results/PISCO_MLPA_transects_site_year.csv

# For reference, these files have coordinates of sites
# ./data/PISCO_kelpforest_site_table.1.2.csv
# ./data/abc_site_lat_list.csv  # PISCO and reef check


################################
# KELP SPECIES
#################################
# PISCO KElP SPECIES 

# assign location to PISCO kelp data, calculate stipe density 30m transect, 2m wide
source("./bin/kelp_PISCO_location.R") 
# PISCO NOTES: adult height > 1m (also used as a placeholder in swath data when no MACPYRAD were seen. The data shown are mean number of Macrocystis stipes/transect, not mean number of individuals
# input:    ./data/MLPA_kelpforest_swath.4.csv
#           ./results/PISCO_MLPA_site_year.csv
#           ./results/PISCO_MLPA_transects_site_year.csv
# output:   ./results/kelpspecies_PISCO_location123_bytransect.csv    # summarized by transect
#           ./results/kelpspecies_PISCO_location123.csv               # summarized by site
  
source("./bin/kelpspecies_PISCO_ReefCheck_combine2.R") #has updated PISCO data
# input:    ./results/kelpspecies_PISCO_location123.csv
#           ./data/reefcheckCA_algae_location3.csv
# output:   ./results/kelpspecies_PISCO_RC_density_transect.csv
#           ./results/kelpspecies_PISCO_RC_density_location.csv

################################
# ENSO & TEMP DATA #############

# change enso data from NOAA to long version

# MEI enso data
source("./bin/enso_widetolong_mei.R")
# input:    ./data/ensodatanoaa_MEI_normalized.csv
# output:   ./results/enso_datalong_mei.csv

# calculate lag variables for ENSO MEI - some of tom bells work
source("./bin/enso_rollingvalues_mei.R")
# input:  ./results/enso_datalong_mei.csv
# output: ./results/enso_yr_offset_mei.csv 

# combine various data sources for ENSOs and make categorical
source("./bin/enso_combinevalues_makecategorical_mei.R")
# input:  ./data/ensoyearsquinn.csv
#         ./data/ensoyearsgergisfowler.csv
#         ./results/ensoyr_offset.csv
#         ./results/ensoyr_offset_mei.csv
#         ./data/kelpenso_sources.csv)
# output: ./results/enso_kelp_yearbysource.csv # NOTE: has > 1 line per year when > 1 source
#         ./results/enso_yearbysource.csv      # NOTE: 1 line per year

# summarize SST data by year
source("./bin/SST_year_summarize.R")
# input:  ./data/PacificGrove_TEMP_1919-201906_noheader.csv
# output: ./results/sst_c_year.csv
# note: error messages are ok - check d2 if want to see

##########################################
# KELP MAPS ENSO TEMP ############

# combine map kelp, enso, temp datasets for graphing & analaysis
# NOTE: TEmp is a shorter time series with few pre-kelp datapoints
source("./bin/kelp_map_combine_ensotemp.R")
# input:  ./results/kelp_map_allsources_area_location3.csv
#         ./results/enso_yearbysource_mei.csv
#         ./results/sst_c_year.csv
# output: ./results/kelp_map_allsources_location_enso.csv
#         ./results/kelp_map_allsources_location3_temp.csv
#         ./results/kelp_map_allsources_location_enso.csv
#         ./results/kelp_map_allsources_location3_temp.csv

###################################
# URCHINS #########################

# organize oral history data
# set Monterey Unspecificed to Monterey, because similar trends during later analyses
source("./bin/urchin_oh_organize.R")
# input:      ./OralHistories/results/otter_kelp_oh_subset_long.csv
# output:     ./results/urchin_oh_all_raw.csv                    # includes location
#             ./results/urchin_oh_all_summarized_location3.csv    # mean value
#             ./results/urchin_oh_all_summarized.csv 
#             ./results/urchin_oh_purple_summarized_location3.csv # mean value. purple sea urchins only

#organize historical urchin data - most is purple
# removed andrews for now...
source("./bin/urchin_historical_organize.R")
# input:  ./data/urchins_historical.csv
# output: ./results/urchin_historical_abundance.csv
#         ./results/urchin_historical_abundance_location.csv
#         ./results/urchin_historical_purple_abundance_location.csv

# combine historical and oral history urchin data
# other versions of the data in code. Focus on purple urchins because mot historical surveys discuss them.
source("./bin/urchin_hoh_combine.R")
# input:      ./results/urchin_oh_purple_summarized_location3.csv
#             ./results/urchin_historical_purple_abundance_location.csv
# output:     ./results/urchin_hoh_abundance.csv         
#             ./results/urchin_hoh_purple_abundance_location.csv
#             ./results/urchin_hoh_purple_abundance_location3.csv
#             ./results/urchin_hoh_abundance_location.csv
#             ./results/urchin_hoh_abundance_location3.csv

# graph urchin historical estimates
source("./bin/urchin_historical_graph.R")
# input:      ./data/urchins_historical_clean.csv
#             ./data/urchin_historical_purple_abundance_location.csv
#             ./results/urchin_historical_purple_abundance_location23.csv
# output:     ./doc/urchin_historical_location_raw.jpg
#             ./doc/urchin_historical_source.jpg
#             ./doc/urchin_historical.jpg
#             ./doc/urchin_historical_location3.jpg

# contemporary urchin data #####-----------------------------


# organize PISCO urchin data  
# This code is new. Note its really LOCATION, not lat long
source("./bin/urchin_PISCO_location.R")
# # input:  ./data/MLPA_kelpforest_swath.4.csv
# #         ./results/PISCO_MLPA_site_year.csv              # location data
# #         ./results/PISCO_transects_site_year.csv)        # transects per site
# # output: ./results/urchin_PISCO_location123_raw.csv      # unsummarized
# #       : ./results/urchin_PISCO_location123.csv          # summarized: site mean, sd, n

# calculate abundance per m2 for PISCO urchin   data 
# this is new - not part of analyses. using ABC reef data
# also pre and post SSWD densities
source("./bin/urchin_PISCO_density_location123.R")
# input:  ./results/urchin_PISCO_location123.csv
# output:  
#         ./results/urchin_PISCO_density_transect.csv           #all species
#         ./results/urchin_PISCO_density_location3.csv
#         ./results/urchin_purple_PISCO_density_location3.csv

# organize Reef Check data for purple and red urchins (these are separated)
source("./bin/urchin_ReefCheck_organize.R")
# input:  ./data/reefcheckCA_invert_location3.csv
# output: ./results/urchin_RC_density_transect.csv  # densities at transect level
#         ./results/urchin_RC_density_location.csv
#         ./results/urchin_RC_density_location3.csv
#         ./results/urchin_RC_density_allsites.csv
#         ./results/urchin_RC_density_allsitesyears.csv


# organize abc reef data 
# note this says location3, but since it is point data all location arrangements are included here
# this calculates the grand mean for location levels
# source("./bin/urchin_abcreefs_density_location3.R")
# input:        ./data/urchins_abcreefs_location3.csv # has all location info from GIS. 
# output:       ./results/urchin_abcreef_density_location.csv
#               ./results/urchin_abcreef_density_location3.csv
#               ./results/urchin_abcreef_density_u_sd.csv # mean and sd values only. for calculating relative abundance.

# organize micheli urchin data
source("./bin/urchin_michelidata_organize_location3.R")
# input:         ./data/urchins_data_Micheli_2002.csv
# output:        ./results/urchin_micheli_location3.csv
#                ./results/urchin_micheli_density_transect.csv

# combine contemporary reef data: PISCO, RC and micheli surveys
# estimate relative abundance
# output is purple urchin data only
source("./bin/urchin_contemporary_combine3.R")  # PISCO data  #combine is with ABC reef data
# input:      ./results/urchin_PISCO_location123_bytransect.csv
#             ./results/urchin_micheli_density_transect.csv
#             ./results/urchin_RC_density_transect.csv
# output:     ./results/urchin_contemporary_abundance_location.csv
#             ./results/urchin_contemporary_abundance_location3.csv

#combine urchin data sources - focus on purple urchins because that is the most common 
# this is for all location levels
source("./bin/urchin_allsource_combine_location3.R")
# input:      ./results/urchin_contemporary_abundance_location.csv  # for location 1-3
#             ./results/urchin_hoh_purple_abundance_location.csv    # for location 1-3
# output:     ./results/urchin_allsources_purple_abundance_location.csv
#             ./results/urchin_allsources_purple_abundance_location3.csv

# graphs of urchins from all sources
source("./bin/urchin_allsource_graph.R")
# input:      ./results/urchin_all_abundance_location.csv
# output:     ./doc/urchin_all_datastructure_location_bimodal.jpg
#             ./doc/urchin_all_abundance.jpg
#             ./doc/urchin_all_abundance.jpg
#             ./doc/urchin_all_abundancelocation3.jpg



###########################
# OTTERS #############

# set years for otters showing up in different places
source("./bin/otter_setyears1.R")
# input:    none
# output:   ./results/otter_returnyear_location.csv
#           ./results/otter_returnyear_location3.csv
# note: unknown level is ok. 

# set years for otters showing up in different places
source("./bin/otter_setyears2.R")
# input:    none
# output:   ./results/otter_returnyear_location.csv
#           ./results/otter_returnyear_location3.csv
#           ./data/otter_migrantfront.csv
#           ./results/otter_20thCent_abundance_location3.csv
#           ./data/otter_historical_location3.csv
#           ./results/otter_historical_all_location3.csv
#           ./results/otter_historical_all_location3_allyears.csv
#           ./results/otter_historical_all_location_allyears.csv


# estimate mean and max abundance by year of oral history data
source("./bin/otter_oh_organize.R") 
# input:      ./OralHistories/results/otter_kelp_oh_subset_long.csv
# output:     ./results/otter_oh_raw.csv                      # includes location
#             ./results/otter_oh_summarized_location3.csv     # mean value
#             ./results/otter_oh_summarized_location.csv
#             ./results/otter_oh_summarized.csv

# combine historical and oh data
source("./bin/otter_hoh_combine.R") 
# input:      ./results/otter_oh_summarized_location3.csv
#             ./results/otter_oh_summarized_location.csv
#             ./results/otter_historical_all_location3_allyears.csv
#             ./results/otter_historical_all_location_allyears.csv
# output:     ./results/otter_hoh_abundance_location3.csv
#             ./results/otter_hoh_abundance_location.csv


# CDFW 
# combine otter data from cdfw surveys
source("./bin/otter_cdfw_organize_location.R")
# source("./bin/otter_cdfw_organize_location2.R")
source("./bin/otter_cdfw_organize_location3.R")
# input:      ./data/otter_density_location/...) #location
# input:      ./data/otter_density_location3/...) #location3
# output:     ./results/otter_cdfw_dens_location3.csv  # same for all three location levels
#             ./results/otter_cdfw_dens_0-30m_location3.csv

# graph otter data from cdfw surveys
source("./bin/otter_cdfw_graph_location.R")
# source("./bin/otter_cdfw_graph_location2.R")
source("./bin/otter_cdfw_graph_location3.R")
# input:      ./results/otter_cdfw_dens_0-30m_location3.csv
# output:     ./doc/otter_contemp_abund_location3.jpg   #relative abundance 
#             ./doc/otter_contemp_density_location3.jpg # density measures, not relative

# combine all otter data sources: location
source("./bin/otter_allsources_combine_location.R")
# input:      ./results/otter_hoh_abundance_location.csv
#             ./results/otter_cdfw_dens_0-30m_location.csv
# output:     ./results/otter_allsources_abundance_location.csv


# combine all otter data sources: location3
source("./bin/otter_allsources_combine_location3.R")
# input:      ./results/otter_hoh_abundance_location3.csv
#             ./results/otter_cdfw_dens_0-30m_location3.csv
# output:     ./results/otter_allsources_abundance_location3.csv

#################################
# PYCNOPODIA #####################

# organize oral history data
# set Monterey Unspecificed to Monterey, because similar trends during later analyses
source("./bin/pycno_oh_organize.R")
# input:      ./OralHistories/results/otter_kelp_oh_subset_long.csv
# output:     ./results/pycno_oh_raw.csv                    # includes location
#             ./results/pycno_oh_summarized_location.csv    # mean value
#             ./results/pycno_oh_summarized_location3.csv    

#organize all historical pycno data (observations and surveys)
# removed andrews for now...
source("./bin/pycno_historical_organize.R")
# input:  ./data/pycnopodia_historical_long.csv
# output: ./results/pycno_historical_allsources_clean.csv
#         ./results/pycno_historical_abundance_location1.csv
#         ./results/pycno_historical_abundance_location3.csv 

# calculate mean abundance based on historical observations/descriptions
source("./bin/pycno_historicaldescriptions_abundance.R")
# input:  ./results/pycno_historical_allsources_clean.csv
# output: ./results/pycno_historicaldesc_abundance_location1.csv
#         ./results/pycno_historicaldesc_abundance_location3.csv

# calculate density data for published (historical) ecologcial data (surveys)
source("./bin/pycno_historicalecologicaldata_density.R")
# input:  ./results/pycno_historical_clean.csv
#         ./results/pycno_historical_density_location1.csv
#         ./results/pycno_historical_density_location3.csv   

# combine histoical and oral history values of relative abundance
source("./bin/pycno_hoh_combine.R")
# input:  ./results/pycno_oh_summarized_location.csv    # mean value
#         ./results/pycno_oh_summarized_location3.csv 
#         ./results/pycno_historicaldesc_abundance_location1.csv
#         ./results/pycno_historicaldesc_abundance_location3.csv
# output: ./results/pycno_hoh_abundance_location1.csv
#         ./results/pycno_hoh_abundance_location3.csv

# contemporary pycno data #####
# organize PISCO pycno data
source("./bin/pycno_PISCO_location.R")
# input:  ./data/PISCO_kelpforest_swath.1.2_2020_UCSC.csv
#         ./results/PISCO_RC_location.csv               # location data
#         ./data/MLPA_kelpforest_site_table_UCSC.csv")  # lat/long data
# output: ./results/PISCO_pycno_location123.csv
#         ./results/pycno_PISCO_latlong.csv

# calculate abundance per m2 for PISCO pycno data
source("./bin/pycno_PISCO_density_location123.R")
# input:  ./results/PISCO_pycno_location123.csv
# output: ./results/pycno_PISCO_density_location.csv
#         ./results/pycno_PISCO_density_location3.csv
#         ./results/pycno_PISCO_density_u_sd.csv 
# mean for all sites/years from PISCO data : 0.0126 (all years); 0.0166 (pre-sswd);0.00078 (post SSWD)

# calculate density per m2 for reefcheck transects
source("./bin/pycno_ReefCheck_organize.R")
# input:  ./data/reefcheckCA_invert_location3.csv
# output: ./results/pycno_RC_density_allsitesyears.csv
#         ./results/pycno_RC_density_allsites.csv
#         ./results/pycno_RC_density_location3.csv
#         ./results/pycno_RC_density_location.csv
#         ./results/pycno_RC_density_transect.csv

# combine PISCO and Reef Check data
source("./bin/pycno_contemporary_combine.R")
# input:  ./results/pycno_RC_density_transect.csv      #has all location data in it
#         ./results/pycno_PISCO_density_transect.csv   #has all location data in it
# output: ./results/pycno_contemporary_density_location.csv
#         ./results/pycno_contemporary_density_location3.csv


# combine PISCO and published historical ecolgical density values
source("./bin/pycno_contemporaryhistoricalecological_density.R")
# input: ./results/pycno_contemporary_density_location.csv
#         ./results/pycno_contemporary_density_location3.csv
#         ./results/pycno_historical_density_location1.csv
#         ./results/pycno_historical_density_location3.csv 
# output: ./results/pycno_contemporary_lit_density_location.csv
#         ./results/pycno_contemporary_lit_density_location3.csv
#         ./doc/pycno_contemporary_lit_density.csv

# calculate relative abundance for contemporary PISCO/RC data and historical ecological data
source("./bin/pycno_contemporaryhistoricalecological_abundance.R")
# input:  ./results/pycno_contemporary_lit_density_location.csv
#         ./results/pycno_contemporary_lit_density_location3.csv
#         ./results/pycno_PISCO_abundance_u_sd.csv            # mean for all PISCO values pooled
# output: ./results/pycno_contemporary_abundance_location.csv
#         ./results/pycno_contemporary_abundance_location3.csv

# Combine and model pycno from all sources #############
# this is for all location levels
source("./bin/pycno_allsource_combine_location123.R")
# input:      ./results/pycno_contemporary_abundance_location.csv  # for location 1-3
#             ./results/pycno_hoh_abundance_location.csv    # for location 1-3
# output:     ./results/pycno_allsources_abundance_location.csv
#             ./results/pycno_allsources_abundance_location3.csv

# graphs of pycno from all sources ## FOR PAPER FIG S2 ####################
source("./bin/pycno_allsource_graph.R")
# input:      ./results/pycno_all_abundance_location.csv
# output:     ./doc/pycno_all_abundance_source_location.jpg   # fig S2
#             ./doc/pycno_all_abundance_source.jpg            # this is  useful shows sources track each other
#             ./doc/pycno_all_abundance_location.jpg
#             

##################################
# ALL DATA ###################

# combine all data sources - input and output vary for location 123
source("./bin/alldata_combine_location.R")
source("./bin/alldata_combine_location3.R")
# input:      ./results/urchin_allsources_purple_abundance_location3.csv
#             ./results/otter_allsources_abundance_location3.csv
#             ./results/kelp_allsources.csv #abundance
#             ./results/kelp_map_allsources_area_location3.csv
#             ./results/kelp_map_allsources_location3_enso.csv
#             ./results/kelp_map_allsources_location3_temp.csv
#             ./results/enso_yearbysource_mei.csv
#             ./results/sst_c_year.csv
# output:     ./results/alldata_allyear_location.csv # all data, NAs where missing years
#             ./results/alldata_kelpmap_location.csv # only data for years with kelp maps



# ================================================================================
# MODELS #######################################################################
# ================================================================================

# ================================================================================
# SPECIES SPECIFIC GAM MODELS (TIME SERIES) ######
# ================================================================================

# OTTER GAM MODELS AND GRAPHS BY LOCATION ## FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_otter_quasib.R")
# input:      ./results/otter_allsources_purple_abundance_location.csv # for 1-3
# output:     ./doc/otter_gam_location.jpg
#             ./doc/otter_gam_santacruz2.jpg
#             ./doc/otter_gam_monterey2.jpg
#             ./doc/otter_gam_bigsur2.jpg

# PYCNO GAM MODELS AND GRAPHS BY LOCATION ## FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_pycno_quasib.R")
# input:      ./results/pycno_allsources_purple_abundance_location.csv # for 1-3
# output:     
#             ./doc/pycno_gam_santacruz_qb.jpg
#             ./doc/pycno_gam_monterey_qb.jpg
#             ./doc/pycno_gam_bigsur_qb.jpg

# URCHIN GAM MODELS AND GRAPHS BY LOCATION ## FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_urchin_quasib.R")
# input:      ./results/urchin_allsources_purple_abundance_location.csv # for 1-3
# output:     
#             ./doc/urchin_gam_santacruz_qb.jpg
#             ./doc/urchin_gam_monterey_qb.jpg
#             ./doc/urchin_gam_bigsur_qb.jpg

# KELP GAM MODELS AND GRAPHS BY LOCATION # FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_kelp.R")
# removed earliest map because decided that early map used a different method
# input:      ./results/kelp_allsources_purple_abundance_location.csv # for 1-3
# output:     
#             ./doc/kelp_gam_santacruz_qb.jpg
#             ./doc/kelp_gam_monterey_qb.jpg
#             ./doc/kelp_gam_bigsur_qb.jpg





# ================================================================================
# MODELS WITH ENTIRE TIME SERIES (NO URCHINS) #####################################
# ================================================================================

# account for correlation structure in models with whole time series (no urchins)
# GLS model from Zuur p 130
# version 4 uses model updated when added pycno, but pycno not in model because no data in 1800s
# uses otter_abundance2 value

source("./bin/model_kelpcover_correlation4_otterperiod.R") 
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv
# output:   NO OUTPUT - see notes. Use model 6.

# analyze data: location3, year: models with whole time series (no urchins)
source("./bin/model_kelpcover_modelstructure_FINAL.R")
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv
# output:   ./doc/model_kelpcover_yearlocationsottersenso_coefficents.csv
#           ./doc/model_kelpcover_yearlocationottersenso_anovatable.csv
#           ./results/model_kelpcover_yearlocationottersenso.rda

# ================================================================================
# check if overfitting #####################################
# ================================================================================
# removing correlation to test if overfitting - best model is the same as with correlation
source("./bin/model_kelpcover_modelstructure_location3c_no correlation.R")
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv

# removing correlation and variation structure to test if overfitting - best model is the same as with correlation
source("./bin/model_kelpcover_modelstructure_location3c_no correlation_noVar.R")
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv





# ================================================================================
# graph all #############
source("./bin/graph_timeseries.R")
# input:
# output:

#=============================
# FIGURES FOR PAPER ############
#=============================

# Lines for years of ENSOS - run with other graphing code not alone
source("./bin/graph_ensoyears2.R")

# Figure 2. Change in sources over time
source("fig2_source_abundance.R")
# input:
# output:

# Figure 4c.
source("./bin/fig4C_kelp_otters.R")
# input:     ./results/alldata_kelpmap_location.csv
# output:    ./doc/Fig4C_kelpcover_otters_location.tif


# Figure 4 o & p
source("./bin/kelpspecies_graph_fig4op.R")
# input:
# output: ./doc/kelpsp_abundance_montbay_enso2_fig4op.jpg



# Figure S2A
source("./bin/otter_allsource_graph.R")
# input:./results/otter_allsources_abundance_location.csv
# output: ./doc/Fig_S2A_otter_all_abundance_source_location.jpg

# Figure S2B
source("./bin/pycno_allsource_graph.R")
# input:  ./results/pycno_allsources_abundance_location.csv
# output: ./doc/Fig_S2B_pycno_all_abundance_source_location.jpg

# Figure S2C
source("./bin/urchin_allsource_graph.R")
# input:  ./results/urchin_allsources_purple_abundance_location.csv
# output: ./doc/Fig_S2C_urchin_all_abundance_source_location.jpg # purple urchins only

# Figure S2D
source("./bin/kelp_allsources_graph.R")
# input:  ./results/kelp_map_allsources_area_location.csv
# output: ./doc/Fig_S2D_kelp_all_abundance_source_location.jpg

source()
# input:
# output:

# Figure S5.
source("./bin/kelpspecies_PISCO_RC_graph.R")
# input:  ./results/kelpspecies_PISCO_RC_density_transect.csv
# output: ./doc/Fig_S5_kelpspecies_density.jpg

