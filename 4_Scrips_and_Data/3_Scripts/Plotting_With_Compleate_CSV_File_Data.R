# Script to Plot the Data From the "CombineMeteorologicalData.csv" created by the script "Combine_All_Data_To_CSV_File.R"
# Author Juan Bettinelli
# Last change: 26.1.23

# library(plyr)
# library(dplyr)
# library(plotly)
# library(rio)
# 
# library(plyr)
# library(ggplot2)   
# library(hexbin)
# library(reshape2)
# library(openair)
# library(cowplot)
# library(patchwork)
# library(dplyr)
# library(GGally)
# library(ggvis)
# library(httr)
# library(plotly)
# library(stringr)
# library(tidyr)
# library(pracma)




library(pacman)
library(lubridate)
library(readr)
library(plyr)
library(tidyverse)
library(ggplot2)   
library(hexbin)
library(gridExtra)
library(reshape2)
library(openair)
library(cowplot)
library(patchwork)
library(dplyr)
library(GGally)
library(ggthemes)
library(ggvis)
library(httr)
library(plotly)
library(rio)
library(rmarkdown)
library(shiny)
library(stringr)
library(tidyr)
library(pracma)


#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automatically access the data.
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

source("3_Scripts/Functions.R")
source("3_Scripts/CH4_Transportmodel.R")

StartTime <- as.POSIXct('2021-08-01 22:03:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
# Start Time: 2021-08-01 22:03:00

FinishTime <- as.POSIXct('2022-03-29 00:00:00', 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz ="utc")

# Total Timeseries: 2022-03-29 00:00:00
# Hamburg Campagne Timeseries: 2021-09-06 00:00:00
# Hamburg Campaine #2: 2021-09-17 10:21:00

########### Read the CSV File #############

TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC), 
                                 format = "%Y-%m-%d %H:%M:%S", 
                                 tz = "UTC")

TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)

TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)

TotalData$UTC <- as.POSIXct(TotalData$UTC, 
                                               format = "%d-%m-%Y %H:%M:%S", 
                                               tz = "utc")

TotalData$Direction[TotalData$Direction > 361] <- NA
TotalData$Speed[TotalData$Speed > 99] <- NA



######### Plot Time Between low water level and CH4 Peak ##########

source("3_Scripts/Low_WL_Time.R")
source("3_Scripts/Low_WL_Dist.R")
source("Low_WL_Dist_Transportmodel.R")

######## Finding the Peaks, The Average Meteorological Data during Peak, Saving csv File #########
CH4_Peak_Finder(TotalData, TRUE)


######### Wind Rose Plots (Plot 9) ##########
WindRose_Plots(TotalData)

### Comparison Plots (Plots 1, 2, 3 & 5) #####
Compare_Timeline(TotalData, 4) # Use 0 for fixed Panels, any integer for rest
Compare_Timeline2(TotalData, 10) # Use 0 for fixed Panels, any integer for rest

Compare_Timeline_Basic(TotalData) # Total Timeline

########### 4 Plot CH4 Concentration Timeseries (Plot 4)##############
CH4_TimeLine(TotalData, StartTime, FinishTime, 10, FALSE) #CH4_TimeLine(ImputDataFrame = , StartTime = , FinishTime =, MumberOfPanels = (0=FixedPanelForPaper), TURE = OnePlotMultiplePanels FALSE = MultipePlotsOnePanel)

CH4_TimeLine_No_Peaks(TotalData, StartTime, FinishTime, 10, FALSE) #CH4_TimeLine(ImputDataFrame = , StartTime = , FinishTime =, MumberOfPanels = (0=FixedPanelForPaper), TURE = OnePlotMultiplePanels FALSE = MultipePlotsOnePanel)

CH4_TimeLine_total(TotalData, StartTime , FinishTime )

########### Transportmodel with averaged wind at the CH4 Peaks (Plot 10) ##############
# Wind_Provider = 1(Geomatikum), 2(Mast 50m) 3(Mast 110m), 4(DWD)
# Warning takes quit a wile !!!!!!

Particles_Tracks_Averaged_at_Peak_Wind(TotalData, Released_Particals = 10,  Backwared_Minutes = 60, sd_Speed = 0.5, sd_Direction = 20, Wind_Provider = 1)


########### Transportmodel with measured wind for each time interval (Plot 10) ##############
# Wind_Provider = 1(Geomatikum), 2(Mast 50m) 3(Mast 110m), 4(DWD)
# Warning takes quit a wile !!!!!!

Particles_Tracks_Changing_Wind(TotalData = TotalData, No_of_Releast_Particals = 10, sd_Speed = 0.5, sd_Direction = 20, Wind_Provider = 1)


######## Plot Wind Direction (DWD)/Speed/CH4 (Plot 6.1) #############
Basic_Wind_DWD_CH4(TotalData, StartTime, FinishTime)

######## Plot CH4/Waterlevel/ Winddierction (DWD) (Plot 6.2) #############
Basic_Wind_D_WL_CH4(TotalData, StartTime, FinishTime)

######## Plot Wind Direction (Mast 110m)/Speed/CH4 (Plot 6.3) #############
Basic_Wind_110m_CH4(TotalData, StartTime, FinishTime)

######## Plot Wind Direction (Geomatikum)/CH4 (Plot 6.4) #############
Basic_Wind_Geomatikum_CH4(TotalData, StartTime, FinishTime)

######## Plot CH4/Water level/ Wind direction, Split into 10 Day intervals (Plot 7.2) #############
Basic_CH4_WaterLevel_Wind_Direction(TotalData, StartTime, FinishTime)
  
######## Plot CH4/Water level/ Wind Speed, Split into 10 Day intervals (Plot 7.1) #############
Basic_CH4_WaterLevel_Wind_Speed(TotalData, StartTime, FinishTime)

######## Plot Rain/CH4 (Plot 8.3) #############
# Basic_Rain_CH4(TotalData, StartTime, FinishTime)
Rain_CH4(TotalData, 4)

######## Plot Temp/CH4 (Plot 8.2) #############
# Basic_Temp_CH4(TotalData, StartTime, FinishTime)
Temp_CH4(TotalData, 4)

######## Plot Humid/CH4 (Plot 8.1) #############  
# Basic_Humidity_CH4(TotalData, StartTime, FinishTime)
Humidity_CH4(TotalData, 4)

######## Plot Radiation/CH4 (Plot 8.4) #############
Radiation_CH4(TotalData, 4)

######## Plot Pressure/CH4 (Plot 8.5) #############
Pressure_CH4(TotalData, 4)

######## Plot Dew/CH4 (Plot 8.6) #############
Dew_CH4(TotalData, 4)
