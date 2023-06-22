# Master Thesis Script To execute all Scripts
# This code takes very longe to run. It is better to execute relevants scripts or funktions as needed
# Author: Juan Bettinelli
#Last eddit: 22.05.2023


setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

if (file.exists("4_Data/OutputData/DWDMeteorologicalData.csv")){
  if (askYesNo("Do You want to rerun the 'Combine_All_Metrorological_Data.R' Script? (Takes very longand not neccesery) ")){
    source("3_Scripts/Playing_with_ggplot.R")
  }
}else {
  if (askYesNo("Do You want to run the Metheological Script? (Takes very long and not neccesery) ")){
    source("3_Scripts/Playing_with_ggplot.R")
}
}
  
if (file.exists("4_Data/OutputData/DWDMeteorologicalData_10min.csv") & file.exists("4_Data/OutputData/DWDMeteorologicalData_1h.csv")){
  if (askYesNo("Do You want to rerun the 'Comine_All_Metrological_Data_two_Outputfiles.R' Script? (Takes very long) ")){
    source("3_Scripts/Comine_All_Metrological_Data_two_Outputfiles.R")
  }

}else {
    source("3_Scripts/Comine_All_Metrological_Data_two_Outputfiles.R")
}

  
if (file.exists("4_Data/OutputData/CombineMeteorologicalData.csv") ){
  if (askYesNo("Do You want to rerun the 'Combine_All_Data_To_CSV_File.R' Script? ")){
    source("3_Scripts/Combine_All_Data_To_CSV_File.R")
  }
  
}else {
  source("3_Scripts/Combine_All_Data_To_CSV_File.R")
}

source("3_Scripts/Plotting_With_Compleate_CSV_File_Data.R")

source("3_Scripts/FTIR_CH4_Waterlevel.R")

source("3_Scripts/MethaneWaterlevelRolingAverage.R")

source("3_Scripts/MethanePeaksHistogram.R")

source("3_Scripts/Keeling_Analyse.R")

source("3_Scripts/Keeling_With_Wind.R")

source("3_Scripts/Correlation_Master.R")

source("3_Scripts/CH4_Transportmodel.R")

source("3_Scripts/CH4_Transportmodel.R")

source("3_Scripts/Low_WL_Dist_Transportmodel.R")

# source("3_Scripts/Low_WL_Dist.R") Currently not used

# source("3_Scripts/Low_WL_Time.R") Currently not used



print("The Plots and output CSV Files will bi in the foulder: MasterThesis/4_Scrips_and_Data/4_Data/OutputData")
