# Script to Combine all Data (Metro, Water, CH4) into one CSV File.
# Some Filtering is done 
# Author: Juan Bettinelli
# Last Change: 19.1.23


library(lubridate)
library(dplyr)
library(plotly)
library(rio)


#Set Working Directory, Set it into the folder "MasterThesis/4_Scrips_and_Data" to automaticaly access the data.
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

# Set The Start and finish time to be exported into the CSV File
StartTime <- as.POSIXct('2021-08-01 00:00:00', 
                        format = "%Y-%m-%d %H:%M:%S", 
                        tz ="utc")
FinishTime <- as.POSIXct('2022-03-29 00:00:00', 
                         format = "%Y-%m-%d %H:%M:%S", 
                         tz ="utc")




######################### Water Level in Hamburg St Pauli (by WSV?????)  ############################
## File Meta Data
#ZRXPVERSION2206.235|*|ZRXPMODEStandard|*|ZRXPCREATORKisters ZRXP-Fileexporter|*|	
#TZMEZ|*|SANR5952050|*|SNAMEHAMBURG ST. PAULI|*|SWATERElbe|*|CNR10|*|CNAMEW|*|	
#CTYPEn-min-equi|*|CMW1440|*|RTIMELVLhigh-resolution|*|CUNITcm|*|RINVAL-777|*|	
#RNR1440|*|REXCHANGE5952050_HAMBURGSTPAULI_W_W_1_O|*|RTYPEmean values|*|	
#RORPROriginal|*|

#import data from csv File an adjust Date time formart
WSV_Waterlevel <- import("4_Data/4_Waterlevel/Water_Level_(20210701-20220505).csv", dec = ",")
colnames(WSV_Waterlevel) <- c("CET", "Water_Level")
WSV_Waterlevel$CET <- as.POSIXct(as.character(WSV_Waterlevel$CET), 
                                 tz = "Etc/GMT-1",  
                                 format = "%Y%m%d%H%M%S")
WSV_Waterlevel$UTC <- with_tz(WSV_Waterlevel$CET, 
                              tzone = "UTC",  
                              format = "%d-%m-%Y %H:%M:%S")

#Filter the usable Days
WSV_Waterlevel <- filter(WSV_Waterlevel, WSV_Waterlevel$UTC > StartTime & WSV_Waterlevel$UTC < FinishTime, .preserve = FALSE)
WSV_Waterlevel$CET      <- NULL
WSV_Waterlevel$Water_Level <- sapply(WSV_Waterlevel$Water_Level, gsub, pattern = ",", replacement= ".")
WSV_Waterlevel$Water_Level <- sapply(WSV_Waterlevel$Water_Level, as.numeric)
WSV_Waterlevel <- WSV_Waterlevel[!is.na(WSV_Waterlevel$UTC),]





#########################  Data Provided by Hamburg Uni ############################
### Geomatikum
#Load the data from the csv file, and Convert the datetime to the correct format
Geomatikum_csv <- import("4_Data/1_Universität_Hamburg_Wind_Data/WindDataGeomatikumTotalTimeline.csv")
Geomatikum_csv$UTC <- as.POSIXct(as.character(Geomatikum_csv$UTC), 
                                 format = "%d-%m-%Y %H:%M:%S", 
                                 tz = "UTC")
#Filter the usable Days
Geomatikum_csv <- filter(Geomatikum_csv, Geomatikum_csv$UTC > StartTime & Geomatikum_csv$UTC < FinishTime, .preserve = FALSE)
Geomatikum_csv <- Geomatikum_csv[!is.na(Geomatikum_csv$UTC),]


### Wether Mast 
#Load the data from the csv file, and Convert the datetime to the correct format
Mast_csv <- import("4_Data/1_Universität_Hamburg_Wind_Data/WindDataMastTotalTimeline.csv")
Mast_csv$UTC <- as.POSIXct(as.character(Mast_csv$UTC), 
                           format = "%d-%m-%Y %H:%M:%S", 
                           tz = "utc")
#Filter the usable Days
Mast_csv <- filter(Mast_csv, Mast_csv$UTC > StartTime & Mast_csv$UTC < FinishTime, .preserve = FALSE)
Mast_csv <- Mast_csv[!is.na(Mast_csv$UTC),]



########################## DWD Data ##########################################

#Load the data from the csv file, and Convert the datetime to the correct format, 10 Minutes and 1 Hour Avereaged Data is avaiable
DWD_Data_10min <- import("4_Data/OutputData/DWDMeteorologicalData_10min.csv")
DWD_Data_1h <- import("4_Data/OutputData/DWDMeteorologicalData_1h.csv")

DWD_Data_10min$UTCDateTime <- as.POSIXct(as.character(DWD_Data_10min$UTCDateTime), 
                                         format = "%Y-%m-%d %H:%M:%S", 
                                         tz = "utc")
DWD_Data_1h$UTCDateTime <- as.POSIXct(as.character(DWD_Data_1h$UTCDateTime), 
                                      format = "%Y-%m-%d %H:%M:%S", 
                                      tz = "utc")
# DWD_Data_10min <- sapply(DWD_Data_10min, gsub, pattern = ",", replacement= ".")
# DWD_Data_1h <- sapply(DWD_Data_1h, gsub, pattern = ",", replacement= ".")

#Filter the usable Days
DWD_Data_10min <- filter(DWD_Data_10min, DWD_Data_10min$UTCDateTime > StartTime & DWD_Data_10min$UTCDateTime < FinishTime, .preserve = FALSE)
DWD_Data_1h <- filter(DWD_Data_1h, DWD_Data_1h$UTCDateTime > StartTime & DWD_Data_1h$UTCDateTime < FinishTime, .preserve = FALSE)

DWD_Data_10min <- DWD_Data_10min[!is.na(DWD_Data_10min$UTCDateTime),]
DWD_Data_1h <- DWD_Data_1h[!is.na(DWD_Data_1h$UTCDateTime),]

########################### Water Level data WSV #############################
### Not used as no compleate data set exist (19.1.23)

# mydir = "Waterlevel1"
# myfiles = list.files(path=mydir, 
#                      pattern="*.csv", 
#                      full.names=TRUE)
# 
# WLTotal <- data.frame(stringsAsFactors=FALSE) 
# 
# 
# for (f in myfiles){
#   
#   #Read Concentration Data of the CSV Files
#   WL_csv<- import(f, ";", escape_double = FALSE, trim_ws = TRUE)
#   
#   WL_csv$UTC <- as.POSIXlt(WL_csv$UTC , 
#                            format="%d.%m.%y %H:%M", 
#                            tz = "UTC")
#   #WL_csv$UTCTimeDate <- as.character(WL_csv$UTC , format="%Y%m%d%H%M")
#   
#   WLTotal <- rbind(WLTotal, WL_csv)
# }




####################Isotope Data###################
### Data provided by Utrecht Uni

#Load the data from the csv file, and Convert the datetime to the correct format
CH4_concentrations <-read.csv2("4_Data/2_Geomatikum_CH4_Concentrations/3_CH4Concentration(1.8.2021-28.3.2022)/Hamburg Methan Measuments 01082021 - 28032022.csv")
# CH4_concentrations <- sapply(CH4_concentrations, gsub, pattern = ",", replacement= ".")

CH4_concentrations$fill.time.utc <- as.POSIXct(as.character(CH4_concentrations$fill.time.utc), 
                                               format = "%d.%m.%y %H:%M", 
                                               tz = "utc")
CH4_concentrations$fill.time.utc <- as.POSIXct(CH4_concentrations$fill.time.utc, 
                                               format = "%d-%m-%Y %H:%M:%S", 
                                               tz = "utc")
CH4_concentrations$fill.time.utc.1 <- as.POSIXct(as.character(CH4_concentrations$fill.time.utc.1), 
                                                 format = "%d.%m.%y %H:%M", 
                                                 tz = "utc")
CH4_concentrations$fill.time.utc.1 <- as.POSIXct(CH4_concentrations$fill.time.utc.1, 
                                                 format = "%d-%m-%Y %H:%M:%S", 
                                                 tz = "utc")
CH4_concentrations$fill.time.utc.2 <- as.POSIXct(as.character(CH4_concentrations$fill.time.utc.2), 
                                                 format = "%d.%m.%y %H:%M", 
                                                 tz = "utc")
CH4_concentrations$fill.time.utc.2 <- as.POSIXct(CH4_concentrations$fill.time.utc.2, 
                                                 format = "%d-%m-%Y %H:%M:%S", 
                                                 tz = "utc")
#Filter Empty Colums
CH4_concentrations$X      <- NULL
CH4_concentrations$X.1      <- NULL

CH4_concentrations$X.CH4..13C <- sapply(CH4_concentrations$X.CH4..13C, gsub, pattern = ",", replacement= ".")
CH4_concentrations$d13C.VPDB <- sapply(CH4_concentrations$d13C.VPDB, gsub, pattern = ",", replacement= ".")
CH4_concentrations$sd..CH4. <- sapply(CH4_concentrations$sd..CH4., gsub, pattern = ",", replacement= ".")
CH4_concentrations$sd.d13C <- sapply(CH4_concentrations$sd.d13C, gsub, pattern = ",", replacement= ".")
CH4_concentrations$X.CH4..2H <- sapply(CH4_concentrations$X.CH4..2H, gsub, pattern = ",", replacement= ".")
CH4_concentrations$d2H.VPDB <- sapply(CH4_concentrations$d2H.VPDB, gsub, pattern = ",", replacement= ".")
CH4_concentrations$sd..CH4..1 <- sapply(CH4_concentrations$sd..CH4..1, gsub, pattern = ",", replacement= ".")
CH4_concentrations$sd.d2H <- sapply(CH4_concentrations$sd.d2H, gsub, pattern = ",", replacement= ".")
CH4_concentrations$X.CH4. <- sapply(CH4_concentrations$X.CH4., gsub, pattern = ",", replacement= ".")

CH4_concentrations$X.CH4..13C <- sapply(CH4_concentrations$X.CH4..13C, as.numeric)
CH4_concentrations$d13C.VPDB <- sapply(CH4_concentrations$d13C.VPDB, as.numeric)
CH4_concentrations$sd..CH4. <- sapply(CH4_concentrations$sd..CH4., as.numeric)
CH4_concentrations$sd.d13C <- sapply(CH4_concentrations$sd.d13C, as.numeric)
CH4_concentrations$X.CH4..2H <- sapply(CH4_concentrations$X.CH4..2H, as.numeric)
CH4_concentrations$d2H.VPDB <- sapply(CH4_concentrations$d2H.VPDB, as.numeric)
CH4_concentrations$sd..CH4..1 <- sapply(CH4_concentrations$sd..CH4..1, as.numeric)
CH4_concentrations$sd.d2H <- sapply(CH4_concentrations$sd.d2H, as.numeric)
CH4_concentrations$X.CH4. <- sapply(CH4_concentrations$X.CH4., as.numeric)

# CH4_concentrations <- CH4_concentrations[!is.na(CH4_concentrations$fill.time.utc),]

################### Merge all Data into one Dataframe #######################

# Create Dataframe
TotalData <- data.frame()

# Mearge one by one, All data is merged by DateTime
TotalData <- merge( Geomatikum_csv, Mast_csv, 
                    by.x = "UTC",
                    by.y = "UTC",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]


TotalData <- merge( TotalData, WSV_Waterlevel[ , c("UTC", "Water_Level")], 
                    by.x = "UTC",
                    by.y = "UTC",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]


TotalData <- merge( TotalData, CH4_concentrations[ , c("fill.time.utc", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C")], 
                    by.x = "UTC",
                    by.y = "fill.time.utc",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]


TotalData <- merge( TotalData, CH4_concentrations[ , c("fill.time.utc.1", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H")], 
                    by.x = "UTC",
                    by.y = "fill.time.utc.1",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]


TotalData <- merge( TotalData, CH4_concentrations[ , c("fill.time.utc.2", "X.CH4.")], 
                    by.x = "UTC",
                    by.y = "fill.time.utc.2",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]


TotalData <- merge( TotalData, DWD_Data_10min, 
                    by.x = "UTC",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
TotalData <- TotalData[!is.na(TotalData$UTC),]



################## Filter the Data by Date ##################

TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)




################## Change the Data type  ##################

TotalData$Water_Level <- as.numeric(TotalData$Water_Level)
TotalData$X.CH4..13C <- as.numeric(TotalData$X.CH4..13C)
TotalData$sd..CH4. <- as.numeric(TotalData$sd..CH4.)
TotalData$sd.d13C <- as.numeric(TotalData$sd.d13C)
TotalData$X.CH4..2H <- as.numeric(TotalData$X.CH4..2H)
TotalData$sd..CH4..1 <- as.numeric(TotalData$sd..CH4..1)
TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)
TotalData$Wind_Direction <- as.numeric(TotalData$Wind_Direction)

# Filtering
TotalData$Direction[TotalData$Direction > 361] <- NA
TotalData$Speed[TotalData$Speed > 99] <- NA
TotalData$Wind_Speed[TotalData$Wind_Speed < 0] <- NA
TotalData$Wind_Direction[TotalData$Wind_Direction < 0] <- NA



################## Output de Data to a CSV File  ##################
### The Output File Can be Found in the folder: "4_Data/OutputData"

write.csv(TotalData,"4_Data/OutputData/CombineMeteorologicalData.csv", row.names = FALSE)



