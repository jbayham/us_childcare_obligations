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
            "scales"

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
  stop("Please download the data from: https://drive.google.com/file/d/1rKtRz2NlN7U3fnrUBmA6yDYtYJNg1p57/view?usp=sharing")
  
} else {
  source("code/00-build.R")
}



#dlgMessage("Do you need to pull the repo?")
