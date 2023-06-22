# Script that combines all water level data into a single file.
# Author: Juan Bettinelli
# Last edit: 1.1.2023


library(lubridate)
library(dplyr)
library(plotly)
library(rio)

setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


mydir1 = "4_Data/6_Water_Data/1"
myfiles1 = list.files(path=mydir1,
                     pattern="*.csv",
                     full.names=TRUE)

WTotal1 <- data.frame(stringsAsFactors=FALSE)


for (f in myfiles1){

  #Read Concentration Data of the CSV Files
  W_csv1<- import(f, ";") #, escape_double = FALSE, trim_ws = TRUE)
  
  W_csv1 <- W_csv1[-1:-6,]
  
  W_csv1$V1 <- as.POSIXlt(W_csv1$V1 ,
                           format="%d.%m.%Y %H:%M",
                           tz = "UTC")

  WTotal1 <- rbind(WTotal1, W_csv1)
}

WTotal1 <- WTotal1[,-7]
WTotal1 <- WTotal1[order(WTotal1$V1),]
WTotal1 <- WTotal1[!duplicated(WTotal1), ]

colnames(WTotal1) <- c("UTC", "TempLuft", "Tw", "O2", "OT", "pH")

#######################################

mydir2 = "4_Data/6_Water_Data/2"
myfiles2 = list.files(path=mydir2,
                     pattern="*.csv",
                     full.names=TRUE)

WTotal2 <- data.frame(stringsAsFactors=FALSE)


for (f in myfiles2){
  
  #Read Concentration Data of the CSV Files
  W_csv2<- import(f, ";") #, escape_double = FALSE, trim_ws = TRUE)
  
  W_csv2 <- W_csv2[-1:-6,]
  
  W_csv2$V1 <- as.POSIXlt(W_csv2$V1 ,
                         format="%d.%m.%Y %H:%M",
                         tz = "UTC")
  
  WTotal2 <- rbind(WTotal2, W_csv2)
}

WTotal2 <- WTotal2[,-7]
WTotal2 <- WTotal2[order(WTotal2$V1),]
WTotal2 <- WTotal2[!duplicated(WTotal2), ]

colnames(WTotal2) <- c("UTC", "LF", "TB", "Cges", "CBlau", "CGrün")


#######################################

mydir3 = "4_Data/6_Water_Data/3"
myfiles3 = list.files(path=mydir3,
                      pattern="*.csv",
                      full.names=TRUE)

WTotal3 <- data.frame(stringsAsFactors=FALSE)


for (f in myfiles3){
  
  #Read Concentration Data of the CSV Files
  W_csv3<- import(f, ";") #, escape_double = FALSE, trim_ws = TRUE)
  
  W_csv3 <- W_csv3[-1:-6,]
  
  W_csv3$V1 <- as.POSIXlt(W_csv3$V1 ,
                          format="%d.%m.%Y %H:%M",
                          tz = "UTC")
  
  WTotal3 <- rbind(WTotal3, W_csv3)
}

WTotal3 <- WTotal3[,-6]
WTotal3 <- WTotal3[order(WTotal3$V1),]
WTotal3 <- WTotal3[!duplicated(WTotal3), ]

colnames(WTotal3) <- c("UTC", "CKie", "CCrypto", "Ut", "AI")


###################

# Create Dataframe
TotalData <- data.frame()

# Mearge one by one, All data is merged by DateTime
TotalData <- merge( WTotal1, WTotal2, 
                    by.x = "UTC",
                    by.y = "UTC",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]

# Mearge one by one, All data is merged by DateTime
TotalData <- merge( TotalData, WTotal3, 
                    by.x = "UTC",
                    by.y = "UTC",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]

write.csv(TotalData,"4_Data/OutputData/CombineWaterData.csv", row.names = FALSE)
