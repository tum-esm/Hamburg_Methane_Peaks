# Script to with plat of methane concentration and the waterlevel of Elbe with a 30 day roling average
# Author: Juan Bettinelli
# Last edit. 22.05.2023

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
library(zoo)

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




is.na(TotalData$Water_Level) <- TotalData$Water_Level == "-777"
TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]


# Plot CH4, Waterlevel Vs Time
CH4_TimeLine <- ggplot(TotalData_CH4_WL) +
  geom_line(aes(x = UTC,
                y = rollmean(X.CH4., 2160, na.pad=TRUE)),
            col = "red") +
  labs(x = "Fill Time [UTC]",
       y =expression("CH"[4]*" concentration [ppb]"),
       title = "One month rolling Average methane concentration & Elbe water level vs. time") +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%d-%b") +
  # limits = c(as.POSIXct('2021-08-01 00:00:00', 
  #                       format = "%Y-%m-%d %H:%M:%S", 
  #                       tz ="utc"), as.POSIXct('2021-08-18 00:00:00', 
  #                                              format = "%Y-%m-%d %H:%M:%S", 
  #                                              tz ="utc"))) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.title.y = element_text(color = "red",
                                    size=13),
        axis.text.y = element_text(color = "red"),
        axis.title.y.right = element_text(color = "blue",
                                          size=13),
        axis.text.y.right = element_text(color = "blue"),
        strip.text.x = element_blank()) +
  geom_line(aes(x = UTC,
                y = rollmean(Water_Level, 2160, na.pad=TRUE)*3.8),
            col = "blue") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./3.8,
                                         name="Water Level, mm"))
CH4_TimeLine

#Export the plot to PNG file
ggsave("1_CH4_WLRollingAverage.png", CH4_TimeLine, path = "4_Data/OutputData/Plots/1_CH4_vs_Waterlevel", width = 10, height = 5)
