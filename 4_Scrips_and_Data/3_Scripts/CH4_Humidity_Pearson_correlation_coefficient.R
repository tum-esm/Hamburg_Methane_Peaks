# Script that produces correlation plots between CH4 concentration and Humidity of the air
# Author:
# Juan Bettinelli,
# Script that preformed a Keeling analyse of the CH4 Measurements at the Geomatikum
# The the Analyse is done depending on the wind direction. 
# The Peaks can be seen separate.
# 22.5.2023

# Declare library's used

library(tidyverse)
library(ggplot2)   
library(hexbin)
library(dplyr)
library(lubridate)
library(plotly)
library(rio)
library(pracma)

# Set Working Directory
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


## Select the dates to be analyed
# Set Starting and Finish Time
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

# Wind_Provider = 2 # Wind_Provider = 1(Geomatikum), 2(Mast 50m) 3(Mast 110m), 4(DWD)


########### Read data from the CSV File #############

# Read the CSV File
TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
# format the Date 'UTC'
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
# Convert the format of 'X.CH4.' to numeric
TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)
# Filter out all the dated that are outside the selected Starting and Finish time of the campaign
TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)
# Remove Empty Cells n data frame
TotalData <- TotalData[!is.na(TotalData$UTC),]

# Calculate 1/Mole Fraction for C13 & H2 for the Keeling analyse and add as new column
TotalData$c13C <- 1/TotalData$X.CH4..13C
TotalData$c2H <- 1/TotalData$X.CH4..2H


for (Wind_Provider in 1:4){
  # Create a dataframe with only the Methan Data
  CH4Data <- TotalData[, c("UTC", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H", "X.CH4.", "c13C", "c2H")]
  CH4Data <- CH4Data[complete.cases(CH4Data[ , "X.CH4."]),]
  
  # Add New empty columns for the wind speed and direction. 
  CH4Data[,"Speed"] <- NA
  CH4Data[,"Direction"] <- NA
  CH4Data[,"humidity"] <- NA
  
  # Select the Wind data provided by Uni. Hamburg or DWD
  if (Wind_Provider == 1){ 
    # Geomatikum
    W_Name <- "Geomatikum"
    W_Speed <- "Speed"
    W_Direction <- "Direction"
  } else if (Wind_Provider == 2){
    # Mast 50m
    W_Name <- "Mast_50m"
    W_Speed <- "Speed50m"
    W_Direction <- "Direction50m"
  } else if (Wind_Provider == 3){
    # Mast 110m
    W_Name <- "Mast_110m"
    W_Speed <- "Speed110m"
    W_Direction <- "Direction110m"
  } else if (Wind_Provider == 4){
    # DWD
    W_Name <- "DWD"
    W_Speed <- "Wind_Speed"
    W_Direction <- "Wind_Direction"
  }
  
  # Get the mean Wind Direction and Speed For 10 min before and after each wind measurement
  for(i in 1:nrow(CH4Data)) {       # for-loop over all rows
    # Find the mean Values during the Peak
    CH4Data[i ,"Speed"] <- mean(TotalData[TotalData$UTC >= (CH4Data[i,"UTC"] - 10*60) & TotalData$UTC <= (CH4Data[i,"UTC"] + 10*60), W_Speed ], na.rm = TRUE)
    CH4Data[i ,"Direction"] <- mean(TotalData[TotalData$UTC >= (CH4Data[i,"UTC"] - 10*60) & TotalData$UTC <= (CH4Data[i,"UTC"] + 10*60), W_Direction ], na.rm = TRUE)
    CH4Data[i , "humidity"] <- mean(TotalData[TotalData$UTC >= (CH4Data[i,"UTC"] - 10*60) & TotalData$UTC <= (CH4Data[i,"UTC"] + 10*60), "humidity" ], na.rm = TRUE)
  }
  
  
  Correlation_Data <- data_frame()
  
  for(j in seq(0, 27, by = 1)){
    CH4Filtered1 <- filter(CH4Data, (Speed > j & Speed < j+1))
    for (i in seq(0, 350, by = 10)){
      CH4Filtered2 <- filter(CH4Filtered1, (Direction > i & Direction < i+10))
      # Calculate Pearson's correlation coefficient
      if (nrow(CH4Filtered2) > 5) {
        r <- cor.test(CH4Filtered2$X.CH4., CH4Filtered2$humidity, use="complete.obs")
        p <- c(r$estimate, i, i+10, j, j+1, r$p.value) #abs()
        Correlation_Data <- rbind(Correlation_Data, p)
        # Print the result
        # print(paste("Pearson's correlation coefficient:", round(r, 2), "Wind direction:", i, "-", i+10))
      }
      
    }
  }
  colnames(Correlation_Data) <- c("Correlation", "Direction_min", "Direction_max", "Speed_min", "Speed_max", "p_value" )
  
  Correlation_Plot <- ggplot()+
    geom_rect(data = Correlation_Data,aes(xmin = Direction_min, ymin = Speed_min, xmax = Direction_max, ymax = Speed_max,
                                          fill = Correlation))+
    scale_fill_gradient2(midpoint=0, low="red", mid="white",
                         high="green", space ="Lab" )+
    labs(x = "Wind direction in °", y = "Wind speed in m/s", title = "Pearson's correlation coefficient between methane and humidity, binned by wind direction and wind speed")
  
  ggsave(paste0("13_CH4_vs_humidity_Correlation_",W_Name, ".png") , Correlation_Plot, path = "4_Data/OutputData/Plots/13_Correlation", width = 10, height = 5)
  
  P_value_Plot <- ggplot()+
    geom_rect(data = Correlation_Data,aes(xmin = Direction_min, ymin = Speed_min, xmax = Direction_max, ymax = Speed_max,
                                          fill = ifelse(p_value>0.05, 'Fail', 'Pass')))+
    labs(x = "Wind direction in °", y = "Wind speed in m/s", title = "5% P-value test for methane and humidity, binned by wind direction and wind speed")+
    scale_fill_discrete(name = "P-Test Results")
  
  ggsave(paste0("13_CH4_vs_humidity_P_Value_",W_Name, ".png") , P_value_Plot, path = "4_Data/OutputData/Plots/13_Correlation", width = 10, height = 5)
  
}


