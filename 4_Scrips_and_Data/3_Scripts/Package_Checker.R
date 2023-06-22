# script to check if packages are needed
# Author: Juan Bettinelli
# Last edit. 22.05.2023

library(funchir)

setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


funchir::stale_package_check('3_Scripts/Plotting_With_Compleate_CSV_File_Data.R')
