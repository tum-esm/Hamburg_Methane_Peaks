# Script to calculate the the distance traveled between emitter and measurement location
# Author: Juan Bettinelli
# Last edit. 22.05.2023


library(dplyr)
library(plotly)
library(rio)

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



# Get create a data frame with the CH4 values
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

##### Find Loweres 15%
#Select the Data from Dataframe with CH4 Concentration
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

# Sort the dataset in ascending order
sorted_data <- sort(CH4Data$X.CH4.)

# Determine the number of observations corresponding to the lowest 15% of the dataset
n_lowest <- round(length(sorted_data) * 0.15)

# Use the head() function to extract the lowest 15% of the dataset
lowest_15_percent <- max(head(sorted_data, n_lowest))
######


# Find the Peaks in the timeline
CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE)) # Strict peaks: findpeaks(CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) , Peak like in the paper: findpeaks(CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)


# format the Dataframe time
names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]

# Remove the "Peaks" at where no measurements were taken (12h)
CH4_Peaks <- subset(CH4_Peaks, (UTC_Ending - UTC_Beginning) < 12*60 )

# Create a waterlevel data frame
WLData <- TotalData[complete.cases(TotalData[ , "Water_Level"]),c("UTC", "Water_Level")]


# Create a dataframe that will include all Points
CH4_Peaks$WL_Dist <- NA
# Loop throgh all Peaks
for(j in 1:nrow(CH4_Peaks)){
  
  # Find the meak max time
  Peak_Time <- CH4_Peaks[j, "UTC"]
  
  # Get the Waterlevel for the 12h before the Peak
  WL_at_Peak <- WLData[WLData$UTC >= (Peak_Time - (12*60*60)) & WLData$UTC <= Peak_Time,]
  
  # Find the minimum Water level
  MinWL <- WL_at_Peak[which.min(WL_at_Peak$Water_Level),]
  
  # Get all the Wind data between the Max CH4 Peak and the Waterlevel Lowpoint before the Peak plus 0.5 hour (One hour earlier)
  All_Wind <- TotalData[TotalData$UTC >= (MinWL$UTC) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]  
  
  MeanSpeed <- mean(All_Wind$Speed, na.rm=TRUE)
  
  res <- CH4_Peaks[j,"UTC"] - MinWL$UTC
  units(res) <- "secs"
  dist <- (MeanSpeed * res)/1000

  CH4_Peaks[j, "WL_Dist"] <- dist
}
library(ggplot2)
# Basic scatter plot
WL_Peak_Plot <- ggplot(data =CH4_Peaks, aes(x = WL_Dist, y = X.CH4.)) +
  geom_point() +
  geom_smooth() +
  xlab("Distance traveled in the ime between CH4 peak and preavius low water, km")+
  ylab("CH4 Concentration, ppb")+
  ggtitle("CH4 Concentration vs. Distance traveled in the ime between CH4 peak and preavius low water")

ggsave("14_Low_WL_to_Peak_dist.png", WL_Peak_Plot, path = "4_Data/OutputData/Plots/14_Low_WL_to_Peak", width = 10, height = 5)



