# The Impact of School Closure for COVID-19 on the US Healthcare Workforce and the Net Mortality Effects

Jude Bayham and Eli P. Fenichel

This repository contains the code to run the analysis for the paper "The Impact of School Closure for COVID-19 on the US Healthcare Workforce and the Net Mortality Effects".  The preprint can be accessed here:
https://www.medrxiv.org/content/10.1101/2020.03.09.20033415v1

********************************************

## Project and Directory Structure

This section describes the directory structure of the project.  The CPS data downloaded from IPUMS-CPS is located in the inputs directory.  The code directory contains all scripts to run the analysis and generate figures in the manuscript.  The functions directory contains scripts to acquire and process the data.  These functions are referenced in the analysis scripts.  The cache directory stores intermediate datasets.  All figures and tables are written to the outputs folder.


****************************************************
## Run the Analysis

Follow these steps to reproduce the analysis:

1. Once you clone the repository, source (run) the script project_init.R.  This will setup the directory structure for the project.

2. The raw data is an extract built and downloaded from the IPUMS-CPS.  The dataset can be downloaded here:
https://drive.google.com/file/d/1rKtRz2NlN7U3fnrUBmA6yDYtYJNg1p57/view?usp=sharing. Place the file in the inputs folder.

3. Rerun the script project_init.R and it will run the scripts to generate tables and figures. All outputs will be saved in the outputs directory.


*************************************************

Note that the dataset is large and is loaded into memory during analysis.

We have also included an elasticity calculator to in the script code/elasticity_calculator.R.  The calculator can be used to find the critical 