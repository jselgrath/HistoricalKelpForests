# Jennifer Selgrath
# Monterey Bay Change, Oral Histories
# Hopkins Marine Station, Stanford University

#----------------------------------------------------
# historical kelp forests
#----------------------------------------------------
setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/MontereyBayChange/Kelp")

# note: location 1 (loc1) = three regions
#       location 3 (loc3)= regions, with sub regions. used in some analyses
#       oh - oral history data
#       hoh - historical and oral history data



# ===================================================
#----------------------------------------------------
# ORGANIZING DATA SETS ##############################
#----------------------------------------------------
# ===================================================




#----------------------------------------------------
# KELP  #################################
#----------------------------------------------------

#----------------------------------------------------
# KELP: MAPS #################################
#----------------------------------------------------

# kelp area from GIS -  updated in 2024 - this removed deep areas from the total possible mapped kelp (as a proxy for rocky habitat)
source("./bin/kelp_maps_area_from_GIS.R")
# input:  ./gis/kelp_maps_2024/kelp_shallow_final_canopy_only.gpkg
#         ./gis/kelp_maps_2024/cdfw_all_yr_loc3.gpkg
#         ./gis/kelp_maps_2024/area_max_year_ha.gpkg
# output: ./results/kelp_map_allsources_area_loc3_2024.csv
#         ./results/kelp_map_allsources_area_loc1_2024.csv

# calcuate relative abundance of kelp from map data
source("./bin/kelp_maps_relative_abundance.R")
# input: ./results/kelp_map_allsources_area_loc3_2024.csv
#        ./results/kelp_map_allsources_area_loc1_2024.csv
# output: ./results/kelp_map_allsources_area_location3.csv
#         ./results/kelp_map_allsources_area_location.csv

# NOTE: # tried removing patches < 0.01 ha. This did not change results meaningfully  so not using



#----------------------------------------------------
# KELP: ORAL HISTORY AND HISTORICAL DATA  #################################
#----------------------------------------------------

# integrate different data sources for kelp
# this has nereo and macro species info where available
# location2 == location3 here, also has location
source("./bin/kelp_historical_clean.R")
# input:    ./data/kelp_historical_expanded_20230521.csv
# output:   ./results/kelphistorical_expanded_clean.csv

source("./bin/kelp_historical_orgainze.R")
# input:   ./data/kelphistorical_expanded_clean.csv
# output:   ./results/kelp_historical_abundance_raw.csv
#           ./results/kelp_historical_sp_abundance_location_reference.csv # and location23 version
#           ./results/kelp_historical_sp_abundance_location.csv           # and location23 version
#           ./results/kelp_historical_abundance_location.csv              # and location23 version
#           ./results/kelp_historical_abundance.csv

# estimate mean and max abundance by year of oral history data -------------
# this has species information
source("./bin/kelp_oh_organize2.R") # v2 weights specific memories of ENSO events over broad (multi-year) abundances
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


#----------------------------------------------------
# KELP: INTEGRATING DIFFERENT KELP MAP and ORAL HISTORY DATA SOURCES #----------------------------------------------------
# Integrate Kelp Maps and interviews - for all location versions
source("./bin/kelp_allsources_combine_location123.R")
# input:    ./results/kelp_hoh_abundance_location.csv# this is only location3
#           ./results/kelp_map_allsources_area_location3.csv
#           ./results/kelp_map_allsources_area_location.csv
# output:   ./results/kelp_allsources_abundance_location.csv 
#           ./results/kelp_allsources_abundance_location3.csv


#----------------------------------------------------
# KELP: Contemporary Survey Data  #################################
#----------------------------------------------------
# General PISCO transect information
# CONTEMP DATA NOT USED FOR KELP ABUNDANCES BECAUSE MEASURES STIPES VS CANOPY COVER
################################

# list of sites surveyed during all years. (no transect information)
# *****SWATH SURVEYS ONLY*****
source("./bin/kelp_PISCO_site_surveyyear.R") #added kelp to organize
# input:        ./data/PISCO_kelpforest_site_table.1.2.csv # through 2019
#               ./data/MLPA_kelpforest_site_table.4 2021.csv #through 2020
#               ./data/PISCO_RC_location.csv
# output:       ./results/PISCO_site_year.csv #through 2019
#               ./results/PISCO_MLPA_site_year.csv #through 2020

# one line per swath survey of PISCO data # added kelp to organize
source("./bin/kelp_PISCO_transect_site_year.R")
# input:        ./data/MLPA_kelpforest_swath.4.csv 
# output:       ./results/PISCO_MLPA_transects_site_year.csv

# For reference, these files have coordinates of sites
# ./data/PISCO_kelpforest_site_table.1.2.csv
# ./data/abc_site_lat_list.csv  # PISCO and reef check


#----------------------------------------------------
# KELP: Species  #################################
#----------------------------------------------------


# PISCO KElP SPECIES -----------------------------

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





#----------------------------------------------------
# ENSO & TEMP DATA  #################################
#----------------------------------------------------

# summarize SST data by year
source("./bin/SST_year_summarize.R")
# input:  ./data/PacificGrove_TEMP_1919-201906_noheader.csv
# output: ./results/sst_c_year.csv
# note: error messages are ok - check d2 if want to see





#----------------------------------------------------
# URCHINS  #################################
#----------------------------------------------------

#----------------------------------------------------
# URCHINS: Oral History and Historical Data  ########
#----------------------------------------------------


# organize oral history data #########################
# set Monterey Unspecificed to Monterey, because similar trends during later analyses
source("./bin/urchin_oh_organize.R")
# input:      ./OralHistories/results/otter_kelp_oh_subset_long.csv
# output:     ./results/urchin_oh_all_raw.csv                    # includes location
#             ./results/urchin_oh_all_summarized_location3.csv    # mean value
#             ./results/urchin_oh_all_summarized.csv 
#             ./results/urchin_oh_purple_summarized_location3.csv # mean value. purple sea urchins only


#organize historical urchin data - most is purple (see supplementary data)
source("./bin/urchin_historical_clean.R")
# input:  ./data/urchins_historical.csv
# output: ./results/urchins_historical_clean.csv


# update abundance value thresholds based on PISCO data
source("./bin/urchin_historical_update_abund_val.R")
# input:  ./results/urchins_historical_clean.csv
# output: ./results/urchins_historical_updated_abund.csv

source("./bin/urchin_historical_organize.R") # errors are from Presence Only data. ok.
# input:  ./results/urchins_historical_updated_abund.csv
# output: ./results/urchin_historical_abundance.csv
#         ./results/urchin_historical_abundance_location.csv
#         ./results/urchin_historical_purple_abundance_location.csv
#         ./results/urchin_historical_presence_loc.csv              # presence only data
#         ./results/urchin_historical_presence_loc3.csv             # presence only data
#         ./doc/urchin_hist_yr_n.csv  # years and count of obsv

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


#----------------------------------------------------
# URCHINS: contemporary urchin data  ########
#----------------------------------------------------



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
source("./bin/urchin_PISCO_density_location13.R")
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

# organize micheli urchin data
source("./bin/urchin_michelidata_organize_location3.R")
# input:         ./data/urchins_data_Micheli_2002.csv
# output:        ./results/urchin_micheli_location3.csv
#                ./results/urchin_micheli_density_transect.csv

# organize published urchin data - excluding Andrews data
source("./bin/urchin_published_data_organize_location13.R")
# input:    ./data/urchins_data_published.csv    
# output:  ./results/urchin_published_location.csv
#          ./results/urchin_published_location3.csv      

# combine contemporary reef data: PISCO, RC and micheli surveys
# estimate relative abundance
# output is purple urchin data only
source("./bin/urchin_contemporary_combine3.R")  # PISCO data  #combine is with ABC reef data
# input:      ./results/urchin_PISCO_location123_bytransect.csv
#             ./results/urchin_micheli_density_transect.csv
#             ./results/urchin_RC_density_transect.csv
#             ./results/urchin_published_location3.csv 
# output:     ./results/urchin_contemporary_abundance_location.csv
#             ./results/urchin_contemporary_abundance_location3.csv


# combined contemporary red data 
# NOTE: not using in further analysis because no old data for this species
source("./bin/urchin_contemporary_combine3_red.R")
# input:      ./results/urchin_PISCO_location123_bytransect.csv
#             ./results/urchin_micheli_density_transect.csv
#             ./results/urchin_RC_density_transect.csv
#             ./results/urchin_published_location3.csv 
# output:     ./results/urchin_red_contemporary_abundance_location.csv
#             ./results/urchin_red_contemporary_abundance_location3.csv


#combine urchin data sources - focus on purple urchins because that is the most common 
# this is for all location levels
source("./bin/urchin_allsource_combine_location.R")
# input:      ./results/urchin_contemporary_abundance_location.csv  # for location 1-3
#             ./results/urchin_hoh_purple_abundance_location.csv    # for location 1-3
# output:     ./results/urchin_allsources_purple_abundance_location.csv
#             ./results/urchin_allsources_purple_abundance_location3.csv

# graphs of urchins from all sources
# source("./bin/urchin_allsource_graph.R")
# input:      ./results/urchin_all_abundance_location.csv
# output:     ./doc/urchin_all_datastructure_location_bimodal.jpg
#             ./doc/urchin_all_abundance.jpg
#             ./doc/urchin_all_abundance.jpg
#             ./doc/urchin_all_abundancelocation3.jpg




#----------------------------------------------------
# OTTERS  #################################
#----------------------------------------------------

#----------------------------------------------------
# OTTERS: Historical Data and Oral History Data ######################
#----------------------------------------------------

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

#----------------------------------------------------
# OTTERS: Contemporary Surveys  #####################
#----------------------------------------------------

# CDFW 
# combine otter data from cdfw surveys
source("./bin/otter_cdfw_organize_location3.R")
# input:      ./data/otter_density_location/...) #location
# input:      ./data/otter_density_location3/...) #location3
# output:     ./results/otter_cdfw_dens_location3.csv  # same for all three location levels
#             ./results/otter_cdfw_dens_0-30m_location3.csv

# combine all otter data sources: location
source("./bin/otter_allsources_combine_location.R")
# input:      ./results/otter_hoh_abundance_location.csv
#             ./data/otter_cdfw_dens_0-30m_location.csv
# output:     ./results/otter_allsources_abundance_location.csv


# combine all otter data sources: location3
source("./bin/otter_allsources_combine_location3.R")
# input:      ./results/otter_hoh_abundance_location3.csv
#             ./results/otter_cdfw_dens_0-30m_location3.csv
# output:     ./results/otter_allsources_abundance_location3.csv


# caluclate carrying capacity
source("./bin/otter_carryingcapacity.R")
# input: no input
# output: no output


#----------------------------------------------------
# PYCNOPODIA  #################################
#----------------------------------------------------


#----------------------------------------------------
# PYCNOPODIA: oral history and historical data  ###########
#----------------------------------------------------

# organize oral history data
# set Monterey Unspecificed to Monterey, because similar trends during later analyses
source("./bin/pycno_oh_organize.R")
# input:      ./OralHistories/results/otter_kelp_oh_subset_long.csv
# output:     ./results/pycno_oh_raw.csv                    # includes location
#             ./results/pycno_oh_summarized_location.csv    # mean value
#             ./results/pycno_oh_summarized_location3.csv    


#clean all historical pycno data (observations and surveys)
source("./bin/pycno_historical_clean.R")
# input:  ./data/pycnopodia_historical_long_20240308.csv
# output: ./results/pycno_historical_allsources_clean.csv


#organize all historical pycno data (observations and surveys)
# calculate mean abundance based on historical observations/descriptions
source("./bin/pycno_historical_organize.R")
# input:  /results/pycno_historical_allsources_clean.csv
# output: ./results/pycno_historical_abundance_location.csv
#         ./results/pycno_historical_abundance_location3.csv 
#         ./results/pycno_historical_density_location.csv
#         ./results/pycno_historical_density_location3.csv  
#         ./results/pycno_historical_location.csv

# combine histoical and oral history values of relative abundance
source("./bin/pycno_hoh_combine.R")
# input:  ./results/pycno_oh_summarized_location.csv    # mean value
#         ./results/pycno_oh_summarized_location3.csv 
#         ./results/pycno_historicaldesc_abundance_location1.csv
#         ./results/pycno_historicaldesc_abundance_location3.csv
# output: ./results/pycno_hoh_abundance_location1.csv
#         ./results/pycno_hoh_abundance_location3.csv


#----------------------------------------------------
# PYCNOPODIA: Contemporary Data  #################################
#----------------------------------------------------
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


# combine PISCO and published historical ecological density values
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






#----------------------------------------------------
# ALL DATA  #################################
# Organizing data sets above for analyses
#----------------------------------------------------


# combine all datasets that are avaliable for the whole time period
source("./bin/alldata_combine_location_all_loc_all_yr.R")
# input:   ./data/enso_merged_MEIv2_15_QuinnS_GergisE_MHW.csv
#          ./results/kelp_map_allsources_area_location3.csv
#          ./results/otter_allsources_abundance_location.csv
#          ./results/sst_c_year.csv
# output:   ./results/all_var_location3_all_yr.csv


#----------------------------------------------------
# ALL DATA: Monterey Peninsula (MP), Post 1934 #######
#----------------------------------------------------

# -------------------------------------
# interpolate pycnopodia and urchin data for some missing years -------
source("./bin/alldata_combine_location_MP_1934onward.R")
# input:   ./data/enso_merged_MEIv2_15_QuinnS_GergisE_MHW.csv
#          ./results/kelp_allsources_abundance_location.csv
#          ./results/urchin_allsources_purple_abundance_location.csv
#          ./results/pycno_allsources_abundance_location.csv
#          ./results/otter_allsources_abundance_location.csv
#          ./results/sst_c_year.csv
# output:  ./results/all_var_location_2023.csv
#          ./doc/sampsz_all_data_location_noPresOnly.csv 
            #sample size : does not include interpolated val for urchins and pycnopodia and otter SS does not include all data sources due to interpolating from Ogen (1941)
#          ./results/all_var_location_2023_interpol.csv # with interpolated data


# combine all data sources - for MONTEREY for ALL YEARS -------------------------------------
# interpolate pycnopodia and urchin data for some missing years
# same as above, but for all years
source("./bin/alldata_combine_location_MP_all_years.R")
# output: ./results/all_var_location_2023_interpol_allyr.csv


# create synthesized variables and combine levels --------------------------------------------
# for new analysis (2023)
source("./bin/all_data_location_calc_var_20240318.R")
# input:   ./results/all_var_location_2023_interpol.csv
# output:  ./results/all_var_location_2023_new_var.csv


# visualize data to figure out groups, where I have enough power, correlations etc
# fig1j - urchins by predators
source("./bin/graph_fig1j_all_var_monterey.R")
# input: ./results/all_var_location_2023_new_var.csv"
# output: ./doc/fig1j_urchins_by_predators.tiff" # in figure 1
#         ./doc/kelp_enso_1_5_dist_lessthan10yr_year.tiff
#         ./doc/kelp_enso_1_5_dist_lessthan10yr_pred.tiff
#         ./doc/kelp_enso_dist_2_lessthan10yr.tiff 
#         ./doc/interactions_test_enso2.jpg 
#          ./doc/interactions_test_enso1_5_v2.jpg
#          ./doc/interactions_test_enso1_5_Mplus.jpg
#          ./doc/interactions_test.jpg

# ===================================================
#----------------------------------------------------
# MODELS  #################################
#----------------------------------------------------
# ===================================================

# =========================================================================
# SPECIES SPECIFIC GAM MODELS (TIME SERIES) and figures ######
# =========================================================================

# OTTER GAM MODELS AND GRAPHS BY LOCATION ---------------------------------
   ## FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_otter_quasib.R")
# input:      ./results/otter_allsources_purple_abundance_location.csv # for 1-3
# output:     ./doc/otter_gam_location.jpg
#             ./doc/otter_gam_santacruz2.jpg
#             ./doc/otter_gam_monterey2.jpg
#             ./doc/otter_gam_bigsur2.jpg

# PYCNO GAM MODELS AND GRAPHS BY LOCATION ---------------------------------
    ## FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_pycno_quasib.R")
# input:      ./results/pycno_allsources_purple_abundance_location.csv # for 1-3
# output:     
#             ./doc/pycno_gam_santacruz_qb.jpg
#             ./doc/pycno_gam_monterey_qb.jpg
#             ./doc/pycno_gam_bigsur_qb.jpg

# URCHIN GAM MODELS AND GRAPHS BY LOCATION ---------------------------------
    ## FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_urchin_quasib.R")
# input:      ./results/urchin_allsources_purple_abundance_location.csv # for 1-3
# output:     
#             ./doc/urchin_gam_santacruz_qb.jpg
#             ./doc/urchin_gam_monterey_qb.jpg
#             ./doc/urchin_gam_bigsur_qb.jpg

# KELP GAM MODELS AND GRAPHS BY LOCATION # FIGURE AND STATS FOR PAPER ##
source("./bin/model_gam_kelp_quasib_maxarea2.R") #model_gam_kelp_maxarea.R
# model_gam_kelp_quasib_maxarea2
# removed earliest map because decided that early map used a different method
# now uses percent of max area mapped IN ANY YEAR
# 
# input:      ./results/kelp_map_allsources_area_location.csv # previously used ENSO joined data, but that was not sig
# output:     
#             ./doc/fig2_kelp_gam_santacruz_qb2.jpg  (no 2 is older model)
#             ./doc/fig2_kelp_gam_monterey_qb2.jpg
#             ./doc/fig2_kelp_gam_bigsur_qb2.jpg


#----------------------------------------------------
# MODELS WITH ENTIRE TIME SERIES, FEWER VARIABLES#######
#----------------------------------------------------

# account for correlation structure in models with whole time series (no urchins)
# GLS model from Zuur p 130
# version 4 uses model updated when added pycno, but pycno not in model because no data in 1800s
# uses otter_abundance2 value

source("./bin/model_kelpcover_correlation4_otterperiod_2023.R") 
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv
# output:   NO OUTPUT - see notes. Use model 6.
# best model: corARMA(c(0.2), form=~1|year, p = 1, q = 0), varIdent(form=~1|location3)

# analyze data: location3, year: models with whole time series (no urchins)
# table S9
source("./bin/model_kelpcover_modelstructure_FINAL_2023June_20240304.R")
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv
# output:   ./doc/table_S9_model_kelpcover_yearlocationsottersenso_coefficents.csv
#           ./doc/table_S9_model_kelpcover_yearlocationottersenso_anovatable.csv
#           ./results/model_kelpcover_yearlocationottersenso.rda

#----------------------------------------------------
# check if overfitting #####################################
#----------------------------------------------------

# removing correlation to test if overfitting - best model is the same as with correlation
# source("./bin/model_kelpcover_modelstructure_location3c_no correlation.R")
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv

# removing correlation and variation structure to test if overfitting - best model is the same as with correlation
# source("./bin/model_kelpcover_modelstructure_location3c_no correlation_noVar.R")
# input:    ./results/alldata_kelpcover_otter-enso_location3.csv


#----------------------------------------------------
# MODELS FOR TRUNCATED TIME PERIOD WITH ALL DATA - RELATIVE ABUND FOR KELP#######
#----------------------------------------------------

# model correlation structures with all data --------------------------------------------
# this code tests the effectiveness of autocorrelation models at reducing autocorrelation
source("./bin/model_all_monterey_correlation_v1_enso1_5.R")
#best: correlation = corARMA(c(0.2), form=~1|year, p = 1, q = 0) - use this in fixed variance structure below
# input:      ./results/all_var_location_2023_new_var.csv
# output:     NONe

# model fixed variance structure --------------------------------------------
source("./bin/model_all_monterey_fixed_structure_v4_gls1_5_20240318.R")
# input:    ./results/all_var_location_2023_new_var.csv
# output:   ./results/table_SI10_model_all_mont_yearlocation_20240318.rda
#           ./doc/table_SI10_kelpcover_yearlocationottersenso_anovatable.csv
#           ./doc/table_SI10_model_all_mont_yearlocation_coefficents_20240318.csv




# ===================================================
#----------------------------------------------------
# FIGURES  ##############################
#----------------------------------------------------
# ===================================================


# Lines for years of ENSOS - run with other graphing code not alone
# source("./bin/graph_ensoyears2.R")


# ----------------------------------------------------
# Figure 1. In Adobe Illustrator
    # Panel a - g - in GIS, Panel j above
    # a. globe with CA
    # b. study region zoomed out
    # c-e. historical kelp map and zoomed in areas
    # f/g. total extent of historical kelp
    # h. model of long time series
    # i. kelp harvest
    # j. urchins vs predators

# Panel h. kelp proportion of max IN ALL YEARS (kelp_area_p) by otters
source("./bin/graph_fig1h_kelp_otters_20240313.R")
# input:      ./results/all_var_location3_all_yr.csv
# output:     ./doc/fig1h_kelpcover_otters_location_20240206.tif

# Panel i. kelp harvest
# source("./bin/graph_fig1i_kelp_harvest.R")
source("./bin/graph_fig1i_kelp_harvest_m_plus.R")
# input:      ./data/kelp_harvest_CDFG_2003.csv
# output:     ./doc/fig1_i_kelp_harvest_QuinnS_MEI_MHW_line.jpg

# panel j - also run above in code
# source("./bin/graph_fig1j_all_var_monterey.R")
# input:    ./results/all_var_location_2023_new_var.csv    
# output:   ./doc/fig1j_urchins_by_predators.tiff    # full list above


# Figure 2. Graphs are output in gam model files above. Combined panels in Photoshop.
# output:     ./doc/fig2_SPECIES_gam_LOCATION.jpg

        
# Figure 3c - 1826-2020
source("./bin/graph_fig3c_ensorecoverylag.R")
# input:      ./bin/all_var_location_2024_interpol_allyr
# output:     ./doc/fig3_c_kelp_enso_1_5_Mp+dist_lessthan12yr.tiff
# Final Figure in Illustrator: 
#   Fig3 - multiple stressors 20240314.ai

# Figure 4. Kelp species
# Figure 4 e & f
# source("./bin/graph_fig4ef_kelpspecies.R")
source("./bin/graph_fig4ef_kelpspecies_M_plus.R")
# input:  ./results/respondentsYear.csv
#         ./results/otter_kelp_oh_subset_long.csv
# output: 
#       ./doc/fig4f_kelpsp_abundance_montbay_enso3.jpg
#       ./doc/fig4e_kelpsp_abundance_montouter_enso3.jpg


# Fig S4. Urchin species abundances
source("./bin/graph_figS4_urchin_purple_red_density.R")
# input:    ./results/urchin_red_contemporary_abundance_location.csv
#           ./results/urchin_contemporary_abundance_location.csv
# output: ./doc/fig_urchin_color_rel_abund.jpg
#         ./doc/figS4_urchin_color_density.jpg # this one in supplementary

# Figure S5.contemporary data species abundances -------------------
source("./bin/graph_figS5_kelpspecies_PISCO_RC.R")
# input:  ./results/kelpspecies_PISCO_RC_density_transect.csv
# output: ./doc/Fig_S5_kelpspecies_density.jpg

# Table S7. combine historical soruces for appendix
source("./bin/Table_SI7.R")
# input: ./data/otter_historical_long_20230702.csv
#       ./data/pycnopodia_historical_long_20240308.csv
#       ./data/urchins_historical_20240308b.csv
#       ./data/kelp_historical_expanded_20230612.csv
# output: ./doc/TableSI7_2024.csv