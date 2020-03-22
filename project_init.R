#This script initializes the project and should be run at the beginning of each
#session

#########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
init.pacs(c("tidyverse",      #shortcut to many useful packages (eg, dplyr, ggplot)
            "conflicted",     #resolves function conflict across packages
            "lubridate",      #working with dates
            "sf",             #for GIS
            "USAboundaries",  #easily access US maps in sf
            "cowplot",
            "svDialogs",      #dialog boxes on all platforms
            "xlsx",
            "ipumsr",
            "data.table",
            "labelled",
            "gtools",
            "R.utils",
            "scales",
            "srvyr"           #For conducting analysis with survey data

))


#Setting package::function priority with conflicted package
conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
conflict_prefer("ggsave", "ggplot2")

options(scipen = 999)

#########################
#Loading project helper functions (all scripts within folder)
run.script("functions")

#Setup project directory
folder.setup()

#Check if data has been downloaded
if(!file.exists("inputs/cps_00005.dat.gz")){
  stop("Please download the data from: https://drive.google.com/drive/u/1/folders/1O-tKFi0izLq7IuB7irfhSmo_f9CaA5KV")

} else {
  source("code/00-build.R")
}

#Append and large files to the gitignore to prevent accidentally pushing to GitHub
shell("ignore_large.sh")

#dlgMessage("Do you need to pull the repo?")
