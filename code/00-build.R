#This script serves as a makefile for the project


##################################################
#Load preliminaries
if(!file.exists("cache/occ_codes.Rdata")){
  get_cps_occ_codes()
}
load("cache/occ_codes.Rdata")

if(!file.exists("cache/met_codes.Rdata")){
  get_cps_metfips_codes()
}
load("cache/met_codes.Rdata")

if(!file.exists("cache/cps_data.Rdata")){
  source("code/01_import_data.R")
}
load("cache/cps_data.Rdata")

if(!file.exists("cache/cps_co.Rdata")){
  source("code/02_process_data.R")
}
load("cache/cps_co.Rdata")

############################################
#Summarize the data
# source("code/03_summarize_data.R")
# 
# #Produce figure 1 map
# source("code/fig_01_map.R")
# 
# #Produce simulation figure
# source("code/fig_02_simulation.R")