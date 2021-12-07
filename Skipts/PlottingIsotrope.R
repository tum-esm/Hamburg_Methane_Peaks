library(pacman)
library(lubridate)
library(readr)
library("plyr")
library(tidyverse)
library("ggplot2")   
library("hexbin")

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 


#Set Working Directory
setwd("~/Documents/TUM/GIT/Isotrope Plots")

######################### Geomatikum Data ############################
#Loade the data from the csv file
Geomatikum_csv <- import("WindDataGeomatikumCombine.csv")

#Convert the datetime to the correct format
Geomatikum_csv$V1 <- as.POSIXlt(as.character(Geomatikum_csv$V1), format = "%d-%b-%Y %H:%M:%S", tz = "utc")

######################## Mast Data ###################################
Mast_csv <- import("WindDataMastCombine.csv")

Mast_csv$UTC <- as.POSIXlt(as.character(Mast_csv$UTC), format = "%d-%m-%Y %H:%M:%S", tz = "utc")


########################## DWD Data ##########################################

#Load all the Met data into the script
Wind_txt <- import("DWDMetroData/stundenwerte_FF_01975_akt (1)/produkt_ff_stunde_20200318_20210918_01975.txt")
Niederschlag_txt <- import("DWDMetroData/stundenwerte_RR_01975_akt (1)/produkt_rr_stunde_20200318_20210918_01975.txt")
TempFeuch_txt <- import("DWDMetroData/stundenwerte_TU_01975_akt (1)/produkt_tu_stunde_20200318_20210918_01975.txt")
Erd_txt <- import("DWDMetroData/stundenwerte_EB_01975_akt (1)/produkt_eb_stunde_20200318_20210918_01975.txt")

#Select only the relevant data
Wind_New <- Wind_txt[(Wind_txt$MESS_DATUM > 2021072600),]
Niederschlag_New <- Niederschlag_txt[(Niederschlag_txt$MESS_DATUM > 2021072600),]
TempFeuch_New <- TempFeuch_txt[(TempFeuch_txt$MESS_DATUM > 2021072600),]
Erd_New <- Erd_txt[(Erd_txt$MESS_DATUM > 2021072600),]

#Convert the TimeDate in DataFrame
Wind_New$UTCDateTime <- as.POSIXlt(as.character(Wind_New$MESS_DATUM), format = "%Y%m%d%H")
Niederschlag_New$UTCDateTime <- as.POSIXlt(as.character(Niederschlag_New$MESS_DATUM), format = "%Y%m%d%H")
TempFeuch_New$UTCDateTime <- as.POSIXlt(as.character(TempFeuch_New$MESS_DATUM), format = "%Y%m%d%H")
Erd_New$UTCDateTime <- as.POSIXlt(as.character(Erd_New$MESS_DATUM), format = "%Y%m%d%H")



###########################Water Level data WSV#############################
mydir = "Waterlevel1"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

WLTotal <- data.frame(stringsAsFactors=FALSE) 


for (f in myfiles){
  
  #Read Concentration Data of the CSV Files
  WL_csv<- import(f, ";", escape_double = FALSE, trim_ws = TRUE)
  
  WL_csv$UTC <- as.POSIXlt(WL_csv$UTC , format="%d.%m.%y %H:%M")
  #WL_csv$UTCTimeDate <- as.character(WL_csv$UTC , format="%Y%m%d%H%M")
  
  WLTotal <- rbind(WLTotal, WL_csv)
}


####################Isotrope Data###################

#Read Concentration Data of the CSV Files
CH4_2H <-read.csv2("CH4 2H 20210816.csv",TRUE, ";")
CH4_13C <-read.csv2("CH4 13C 20210816.csv",TRUE, ";")
CH4_concentrations <-read.csv2("CH4 concentrations 20210816.csv",TRUE, ";")


#Convert the date into a readable format
CH4_con_w_d <- CH4_concentrations
CH4_con_w_d$fill.time.utc <- as.POSIXlt(CH4_con_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", tz = "utc")
CH4_2H_w_d <- CH4_2H
CH4_2H_w_d$fill.time.utc <- as.POSIXlt(CH4_2H_w_d$fill.time.utc,
                                       format = "%d.%m.%y %H:%M", tz = "utc")
CH4_13C_w_d <- CH4_13C
CH4_13C_w_d$fill.time.utc <- as.POSIXlt(CH4_13C_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", tz = "utc")


################### Plotting the Data #######################


TotalData <- data.frame()

dataTide <- data.frame( "datetime" = WLTotal$UTC , "level" = WLTotal$Level, "ch4" = NA, "WindSpeed" = NA, "WindDirction" = NA, "RainQ" = NA, "Temp" = NA, "Humid" = NA, "GeoWindDir" = NA, "GeoWindSpeed" = NA, "Mast50mDir" = NA, "Mast50mSpeed" = NA, "Mast110mDir" = NA, "Mast110mSpeed" = NA)
TotalData <- rbind(TotalData,dataTide)
dataCH4 <- data.frame( "datetime" = CH4_con_w_d$fill.time.utc , "level" = NA, "ch4" = CH4_con_w_d$X.CH4., "WindSpeed" = NA, "WindDirction" = NA, "RainQ" = NA, "Temp" = NA, "Humid" = NA, "GeoWindDir" = NA, "GeoWindSpeed" = NA, "Mast50mDir" = NA, "Mast50mSpeed" = NA, "Mast110mDir" = NA, "Mast110mSpeed" = NA)
TotalData <- rbind(TotalData,dataCH4)
dataWind <- data.frame( "datetime" = Wind_New$UTCDateTime , "level" = NA, "ch4" = NA, "WindSpeed" = Wind_New$F, "WindDirction" = Wind_New$D, "RainQ" = NA, "Temp" = NA, "Humid" = NA, "GeoWindDir" = NA, "GeoWindSpeed" = NA, "Mast50mDir" = NA, "Mast50mSpeed" = NA, "Mast110mDir" = NA, "Mast110mSpeed" = NA)
TotalData <- rbind(TotalData,dataWind)
dataRain <- data.frame( "datetime" = Niederschlag_New$UTCDateTime , "level" = NA, "ch4" = NA, "WindSpeed" = NA, "WindDirction" = NA, "RainQ" = Niederschlag_New$R1, "Temp" = NA, "Humid" = NA, "GeoWindDir" = NA, "GeoWindSpeed" = NA, "Mast50mDir" = NA, "Mast50mSpeed" = NA, "Mast110mDir" = NA, "Mast110mSpeed" = NA)
TotalData <- rbind(TotalData,dataRain)
dataTemp <- data.frame( "datetime" = TempFeuch_New$UTCDateTime , "level" = NA, "ch4" = NA, "WindSpeed" = NA, "WindDirction" = NA, "RainQ" = NA, "Temp" = TempFeuch_New$TT_TU, "Humid" = TempFeuch_New$RF_TU, "GeoWindDir" = NA, "GeoWindSpeed" = NA, "Mast50mDir" = NA, "Mast50mSpeed" = NA, "Mast110mDir" = NA, "Mast110mSpeed" = NA )
TotalData <- rbind(TotalData,dataTemp)
dataGeomatikum <- data.frame( "datetime" = Geomatikum_csv$V1 , "level" = NA, "ch4" = NA, "WindSpeed" = NA, "WindDirction" = NA, "RainQ" = NA, "Temp" = NA, "Humid" = NA, "GeoWindDir" = Geomatikum_csv$V2, "GeoWindSpeed" = Geomatikum_csv$V3, "Mast50mDir" = NA, "Mast50mSpeed" = NA, "Mast110mDir" = NA, "Mast110mSpeed" = NA )
TotalData <- rbind(TotalData,dataGeomatikum)
dataMast <- data.frame( "datetime" = Mast_csv$UTC , "level" = NA, "ch4" = NA, "WindSpeed" = NA, "WindDirction" = NA, "RainQ" = NA, "Temp" = NA, "Humid" = NA, "GeoWindDir" = NA, "GeoWindSpeed" = NA, "Mast50mDir" = Mast_csv$Direction50m, "Mast50mSpeed" = Mast_csv$Speed50m, "Mast110mDir" = Mast_csv$Direction110m, "Mast110mSpeed" = Mast_csv$Speed110m )
TotalData <- rbind(TotalData,dataMast)

write.csv(TotalData,"CombineMeteorologicalData.csv", row.names = FALSE)

############### create Compleate Data frame ###############
TotalDataComp <- data.frame()

dataTideComp <- data.frame( "datetime" = WLTotal$UTC , "level" = WLTotal$Level, "ch4" = NA, "WindSpeed" = NA, "WindDirction" = NA, "RainQ" = NA, "Temp" = NA, "Humid" = NA, "GeoWindDir" = NA, "GeoWindSpeed" = NA, "Mast50mDir" = NA, "Mast50mSpeed" = NA, "Mast110mDir" = NA, "Mast110mSpeed" = NA)
TotalData <- rbind(TotalData,dataTide)



# ######## Plot CH4/Waterlevel#############
# par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$level,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Elbe Waterlevel, mm",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      main = "WaterLevel(WSV)/CH4 Concentation Vs. Time",
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)

######## Plot Wind Direction (DWD)/Speed/CH4#############

par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# first plot
plot(TotalData$datetime, TotalData$WindDirction,
     main = "Wind Direction & Speed (DWD)/CH4 Concentation Vs. Time",
     type = "p",
     pch='.',
     cex = 5,
     xlab = "Date/Time UTC",
     ylab = "Wind Direction, °",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$datetime, TotalData$ch4,
     type = "l",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$datetime, TotalData$WindSpeed,
     type = "p",
     pch='.',
     cex = 5,
     xlab = "Date/Time UTC",
     ylab = "Wind Speed, m/s",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$datetime, TotalData$ch4,
     type = "l",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

par(mfrow=c(1,1))

# ######## Plot Wind Direction (Mast 110m)/Speed/CH4#############
# 
# par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$Mast110mDir,
#      main = "Wind Direction & Speed (MAST 110m)/CH4 Concentation Vs. Time",
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Direction, °",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$Mast110mSpeed,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Speed, m/s",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mfrow=c(1,1))
# 

# ######## Plot Wind Direction (Geomatikum)/CH4#############
# par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$GeoWindDir,
#      main = "Wind Direction (Geomatikum)/CH4 Concentation Vs. Time",
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Direction, °",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# 
# par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$GeoWindSpeed,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Wind Speed, m/s",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      type = "l",
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# mtext("CH4 Concentration",
#       col="red",
#       side=4,
#       line=3)
# 
# par(mfrow=c(1,1))


######## Plot Rain/CH4#############
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$datetime, TotalData$RainQ,
     main = "Rain quantity (DWD)/CH4 Concentation Vs. Time",
     type = "l",
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Rain Quantity, mm",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$datetime, TotalData$ch4,
     type = "l",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

######## Plot Temp/CH4#############
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$datetime, TotalData$Temp,
     main = "Temperature (DWD)/CH4 Concentation Vs. Time",
     type = "l",
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Temperature, °C",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$datetime, TotalData$ch4,
     type = "l",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)

######## Plot Humid/CH4#############
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# first plot
plot(TotalData$datetime, TotalData$Humid,
     main = "Relative humidity (DWD)/CH4 Concentation Vs. Time",
     type = "l",
     cex = 2,
     xlab = "Date/Time UTC",
     ylab = "Relative humidity, g/m3",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

par(new = TRUE)
plot(TotalData$datetime, TotalData$ch4,
     type = "l",
     col="red",
     axes = FALSE,
     bty = "n",
     xlab = "",
     ylab = "",
     xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))

axis(side=4,
     col.axis="red",
     col="red")
mtext("CH4 Concentration",
      col="red",
      side=4,
      line=3)


# ######## Plot CH4/Waterlevel/ Winddierction#############
# par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
# # first plot
# plot(TotalData$datetime, TotalData$level,
#      type = "p",
#      pch='.',
#      cex = 2,
#      xlab = "Date/Time UTC",
#      ylab = "Elbe Waterlevel, mm",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$ch4,
#      main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
#      type = "l",
#      lwd = 1.5,
#      col="red",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# 
# axis(side=4,
#      col.axis="red",
#      col="red")
# 
# 
# par(new = TRUE)
# plot(TotalData$datetime, TotalData$WindDirction,
#      type = "p",
#      pch='.',
#      cex = 2,
#      col="Blue",
#      axes = FALSE,
#      bty = "n",
#      xlab = "",
#      ylab = "",
#      xlim = c(as.POSIXct('2021-08-01 22:03:00', format = "%Y-%m-%d %H:%M:%S"),as.POSIXct('2021-09-17 10:21:00', format = "%Y-%m-%d %H:%M:%S")))
# axis(side=4,
#      col.axis="blue",
#      col="blue") 
# mtext("CH4 Concentration & Wind Direction, °",
#       col="red",
#       side=4,
#       line=3)




######## Plot CH4/Water level/ Wind direction, Split into 10 Day intervals#############
IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")

for(j in IntervalDate){
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  # first plot
  plot(TotalData$datetime, TotalData$level,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Elbe Waterlevel, mm",
       xlim = c(i,j))
  
  par(new = TRUE)
  plot(TotalData$datetime, TotalData$ch4,
       main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
       type = "l",
       lwd = 1.5,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(i,j))
  
  axis(side=4,
       col.axis="red",
       col="red")
  
  
  par(new = TRUE)
  plot(TotalData$datetime, TotalData$WindDirction,
       type = "p",
       pch='.',
       cex = 4,
       col="Blue",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(i,j))
  axis(side=4,
       col.axis="blue",
       col="blue") 
  mtext("CH4 Concentration & Wind Direction, °",
        col="red",
        side=4,
        line=3)
  
  i <- j
}

######## Plot CH4/Water level/ Wind Speed, Split into 10 Day intervals#############
IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
i <- as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")

for(j in IntervalDate){
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  # first plot
  plot(TotalData$datetime, TotalData$level,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Elbe Waterlevel, mm",
       xlim = c(i,j))
  
  par(new = TRUE)
  plot(TotalData$datetime, TotalData$ch4,
       main = "WaterLevel(WSV)/CH4 Concentation/Wind Speed (DWD) Vs. Time",
       type = "l",
       lwd = 1.5,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(i,j))
  
  axis(side=4,
       col.axis="red",
       col="red")
  
  
  par(new = TRUE)
  plot(TotalData$datetime, TotalData$WindSpeed,
       type = "p",
       pch='.',
       cex = 4,
       col="Blue",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(i,j))
  axis(side=4,
       col.axis="blue",
       col="blue") 
  mtext("CH4 Concentration & Wind Speed",
        col="red",
        side=4,
        line=3)
  
  i <- j
}

