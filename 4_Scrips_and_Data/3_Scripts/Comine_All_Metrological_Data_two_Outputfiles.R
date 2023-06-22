# Script to combine all meterological Data.
#Creates two CSV files one for 1h measumens one for 10min measuments
# Author Juan Bettinelli

#Caution very slow

library(rio)

setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


#Load all the Meteorological DWD data into the script
#Wind
Wind_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/wind_stundenwerte_FF_01975_19500101_20211231_hist/produkt_ff_stunde_19500101_20211231_01975.txt")
Wind_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/wind_stundenwerte_FF_01975_akt/produkt_ff_stunde_20201105_20220508_01975.txt")
Wind_txt_Historical_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Historical/wind_produkt_zehn_min_ff_20200101_20211231_01975.txt")
Wind_txt_Recent_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Recent/wind_produkt_zehn_min_ff_20201105_20220508_01975.txt")

#cloud_type
cloud_type_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/cloud_type_stundenwerte_CS_01975_19490101_20211231_hist/produkt_cs_stunde_19490101_20211231_01975.txt")
cloud_type_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/cloud_type_stundenwerte_CS_01975_akt/produkt_cs_stunde_20201105_20220508_01975.txt")

#cloudiness 
cloudiness_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/cloudiness_stundenwerte_N_01975_19490101_20211231_hist/produkt_n_stunde_19490101_20211231_01975.txt")
cloudiness_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/cloudiness_stundenwerte_N_01975_akt/produkt_n_stunde_20201105_20220508_01975.txt")

#dew_point
dew_point_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/dew_point_stundenwerte_TD_01975_19490101_20211231_hist/produkt_td_stunde_19490101_20211231_01975.txt")
dew_point_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/dew_point_stundenwerte_TD_01975_akt/produkt_td_stunde_20201105_20220508_01975.txt")

#extreme_wind
extreme_wind_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/extreme_wind_stundenwerte_FX_01975_19900701_20211231_hist/produkt_fx_stunde_19900701_20211231_01975.txt")
extreme_wind_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/extreme_wind stundenwerte_FX_01975_akt/produkt_fx_stunde_20201105_20220508_01975.txt")
extreme_wind_txt_Historical_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Historical/extreme_wind_produkt_zehn_min_fx_20200101_20211231_01975.txt")
extreme_wind_txt_Recent_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Recent/extreme_wind_produkt_zehn_min_fx_20201105_20220508_01975.txt")

#moisture
moisture_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/moisture_stundenwerte_TF_01975_19490101_20211231_hist/produkt_tf_stunde_19490101_20211231_01975.txt")
moisture_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/moisture_stundenwerte_TF_01975_akt/produkt_tf_stunde_20201105_20220508_01975.txt")

#precipitation
precipitation_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/precipitation_stundenwerte_RR_01975_19950905_20211231_hist/produkt_rr_stunde_19950905_20211231_01975.txt")
precipitation_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/precipitation_stundenwerte_RR_01975_akt/produkt_rr_stunde_20201105_20220508_01975.txt")
precipitation_txt_Historical_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Historical/precipitation_produkt_zehn_min_rr_20200101_20211231_01975.txt")
precipitation_txt_Recent_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Recent/precipitation_produkt_zehn_min_rr_20201105_20220508_01975.txt")

#pressure
pressure_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/pressure_stundenwerte_P0_01975_19490101_20211231_hist/produkt_p0_stunde_19490101_20211231_01975.txt")
pressure_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/pressure_stundenwerte_P0_01975_akt/produkt_p0_stunde_20201105_20220508_01975.txt")

#soil_temperature
soil_temperature_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/soil_temperature_stundenwerte_EB_01975_19610101_20211231_hist/produkt_eb_stunde_19610101_20211231_01975.txt")
soil_temperature_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/soil_temperature_stundenwerte_EB_01975_akt/produkt_eb_stunde_20201105_20220508_01975.txt")

#solar
solar_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/solar_stundenwerte_ST_01975_row/produkt_st_stunde_20050101_20221231_01975.txt")
# solar_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/solar_stundenwerte_ST_01975_row/produkt_st_stunde_20050101_20221231_01975.txt")
solar_txt_Historical_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Historical/solar_produkt_zehn_min_sd_20200101_20211231_01975.txt")
solar_txt_Recent_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Recent/solar_produkt_zehn_min_sd_20201105_20220508_01975.txt")

#sun
sun_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/sun_stundenwerte_SD_01975_19490101_20211231_hist/produkt_sd_stunde_19490101_20211231_01975.txt")
sun_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/sun_stundenwerte_SD_01975_akt/produkt_sd_stunde_20201105_20220508_01975.txt")

#Temperature
Temperature_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/Temperature_stundenwerte_TU_01975_19490101_20211231_hist/produkt_tu_stunde_19490101_20211231_01975.txt")
Temperature_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/Temperature_stundenwerte_TU_01975_akt/produkt_tu_stunde_20201105_20220508_01975.txt")
Temperature_txt_Historical_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Historical/air_temperature_produkt_zehn_min_tu_20200101_20211231_01975.txt")
Temperature_txt_Recent_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Recent/air_temperature_produkt_zehn_min_tu_20201105_20220508_01975.txt")

#extreme_temperature
extreme_temperature_txt_Historical_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Historical/extreme_temperature_produkt_zehn_min_tx_20200101_20211231_01975.txt")
extreme_temperature_txt_Recent_10min <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/10_minutes/Recent/extreme_temperature_produkt_zehn_min_tx_20201105_20220508_01975.txt")

#visibility 
visibility_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/visibility_stundenwerte_VV_01975_19490101_20211231_hist/produkt_vv_stunde_19490101_20211231_01975.txt")
visibility_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/visibility_stundenwerte_VV_01975_akt/produkt_vv_stunde_20201105_20220508_01975.txt")

#weather_phenomena
weather_phenomena_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/weather_phenomena_stundenwerte_WW_01975_19950905_20211231_hist/produkt_ww_stunde_19950905_20211231_01975.txt")
weather_phenomena_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/weather_phenomena_stundenwerte_WW_01975_akt/produkt_ww_stunde_20201105_20220508_01975.txt")

#wind_synop
wind_synop_txt_Historical_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Historical/wind_synop_stundenwerte_F_01975_19490101_20211231_hist/produkt_f_stunde_19490101_20211231_01975.txt")
wind_synop_txt_Recent_Hour <- import("4_Data/3_DWD_Meteorological_Data/DWD_Wetherdata/Hourly/Recent/wind_synop_stundenwerte_F_01975_akt/produkt_f_stunde_20201105_20220508_01975.txt")


StartDate <- 2021072600
EndDate <- 2022032900
StartDate_10min <- 202107260000
EndDate_10min <- 202203290000

#Select only the relevant data
Wind_1h <- Wind_txt_Historical_Hour[(Wind_txt_Historical_Hour$MESS_DATUM > StartDate & Wind_txt_Historical_Hour$MESS_DATUM < EndDate),]
Wind_1h <- rbind(Wind_1h, Wind_txt_Recent_Hour[(Wind_txt_Recent_Hour$MESS_DATUM > StartDate & Wind_txt_Recent_Hour$MESS_DATUM < EndDate),]) 

cloud_type_1h <- cloud_type_txt_Historical_Hour[(cloud_type_txt_Historical_Hour$MESS_DATUM > StartDate & cloud_type_txt_Historical_Hour$MESS_DATUM < EndDate),]
cloud_type_1h <- rbind(cloud_type_1h, cloud_type_txt_Recent_Hour[(cloud_type_txt_Recent_Hour$MESS_DATUM > StartDate & cloud_type_txt_Recent_Hour$MESS_DATUM < EndDate),])

cloudiness_1h <- cloudiness_txt_Historical_Hour[(cloudiness_txt_Historical_Hour$MESS_DATUM > StartDate & cloudiness_txt_Historical_Hour$MESS_DATUM < EndDate),]
cloudiness_1h <- rbind(cloudiness_1h, cloudiness_txt_Recent_Hour[(cloudiness_txt_Recent_Hour$MESS_DATUM > StartDate & cloudiness_txt_Recent_Hour$MESS_DATUM < EndDate),])

dew_point_1h <- dew_point_txt_Historical_Hour[(dew_point_txt_Historical_Hour$MESS_DATUM > StartDate & dew_point_txt_Historical_Hour$MESS_DATUM < EndDate),]
dew_point_1h <- rbind(dew_point_1h, dew_point_txt_Recent_Hour[(dew_point_txt_Recent_Hour$MESS_DATUM > StartDate & dew_point_txt_Recent_Hour$MESS_DATUM < EndDate),])

extreme_wind_1h <- extreme_wind_txt_Historical_Hour[(extreme_wind_txt_Historical_Hour$MESS_DATUM > StartDate & extreme_wind_txt_Historical_Hour$MESS_DATUM < EndDate),]
extreme_wind_1h <- rbind(extreme_wind_1h, extreme_wind_txt_Recent_Hour[(extreme_wind_txt_Recent_Hour$MESS_DATUM > StartDate & extreme_wind_txt_Recent_Hour$MESS_DATUM < EndDate),])

moisture_1h <- moisture_txt_Historical_Hour[(extreme_wind_txt_Recent_Hour$MESS_DATUM > StartDate & extreme_wind_txt_Recent_Hour$MESS_DATUM < EndDate),]
moisture_1h <- rbind(moisture_1h, moisture_txt_Recent_Hour[(moisture_txt_Recent_Hour$MESS_DATUM > StartDate & moisture_txt_Recent_Hour$MESS_DATUM < EndDate),])

precipitation_1h <- precipitation_txt_Historical_Hour[(precipitation_txt_Historical_Hour$MESS_DATUM > StartDate & precipitation_txt_Historical_Hour$MESS_DATUM < EndDate),]
precipitation_1h <- rbind(precipitation_1h, precipitation_txt_Recent_Hour[(precipitation_txt_Recent_Hour$MESS_DATUM > StartDate & precipitation_txt_Recent_Hour$MESS_DATUM < EndDate),])

pressure_1h <- pressure_txt_Historical_Hour[(pressure_txt_Historical_Hour$MESS_DATUM > StartDate & pressure_txt_Historical_Hour$MESS_DATUM < EndDate),]
pressure_1h <- rbind(pressure_1h, pressure_txt_Recent_Hour[(pressure_txt_Recent_Hour$MESS_DATUM > StartDate & pressure_txt_Recent_Hour$MESS_DATUM < EndDate),])

soil_temperature_1h <- soil_temperature_txt_Historical_Hour[(soil_temperature_txt_Historical_Hour$MESS_DATUM > StartDate & soil_temperature_txt_Historical_Hour$MESS_DATUM < EndDate),]
soil_temperature_1h <- rbind(soil_temperature_1h, soil_temperature_txt_Recent_Hour[(soil_temperature_txt_Recent_Hour$MESS_DATUM > StartDate & soil_temperature_txt_Recent_Hour$MESS_DATUM < EndDate),])

solar_1h <- solar_txt_Historical_Hour[((solar_txt_Historical_Hour$MESS_DATUM > StartDate & solar_txt_Historical_Hour$MESS_DATUM < EndDate)),]
# solar_1h <- rbind(solar_1h, solar_txt_Recent_Hour[(solar_txt_Recent_Hour$MESS_DATUM < EndDate),])

sun_1h <- sun_txt_Historical_Hour[(sun_txt_Historical_Hour$MESS_DATUM > StartDate & sun_txt_Historical_Hour$MESS_DATUM < EndDate),]
sun_1h <- rbind(sun_1h, sun_txt_Recent_Hour[(sun_txt_Recent_Hour$MESS_DATUM > StartDate & sun_txt_Recent_Hour$MESS_DATUM < EndDate),])

Temperature_1h <- Temperature_txt_Historical_Hour[(Temperature_txt_Historical_Hour$MESS_DATUM > StartDate & Temperature_txt_Historical_Hour$MESS_DATUM < EndDate),]
Temperature_1h <- rbind(Temperature_1h, Temperature_txt_Recent_Hour[(Temperature_txt_Recent_Hour$MESS_DATUM > StartDate & Temperature_txt_Recent_Hour$MESS_DATUM < EndDate),])

visibility_1h <- visibility_txt_Historical_Hour[(visibility_txt_Historical_Hour$MESS_DATUM > StartDate & visibility_txt_Historical_Hour$MESS_DATUM < EndDate),]
visibility_1h <- rbind(visibility_1h, visibility_txt_Recent_Hour[(visibility_txt_Recent_Hour$MESS_DATUM > StartDate & visibility_txt_Recent_Hour$MESS_DATUM < EndDate),])

weather_phenomena_1h <- weather_phenomena_txt_Historical_Hour[(weather_phenomena_txt_Historical_Hour$MESS_DATUM > StartDate & weather_phenomena_txt_Historical_Hour$MESS_DATUM < EndDate),]
weather_phenomena_1h <- rbind(weather_phenomena_1h, weather_phenomena_txt_Recent_Hour[(weather_phenomena_txt_Recent_Hour$MESS_DATUM > StartDate & weather_phenomena_txt_Recent_Hour$MESS_DATUM < EndDate),])

wind_synop_1h <- wind_synop_txt_Historical_Hour[(wind_synop_txt_Historical_Hour$MESS_DATUM > StartDate & wind_synop_txt_Historical_Hour$MESS_DATUM < EndDate),]
wind_synop_1h <- rbind(wind_synop_1h, wind_synop_txt_Recent_Hour[(wind_synop_txt_Recent_Hour$MESS_DATUM > StartDate & wind_synop_txt_Recent_Hour$MESS_DATUM < EndDate),])

Wind_10min <- Wind_txt_Historical_10min[(Wind_txt_Historical_10min$MESS_DATUM > StartDate_10min & Wind_txt_Historical_10min$MESS_DATUM < EndDate_10min),]
Wind_10min <- rbind(Wind_10min, Wind_txt_Recent_10min[(Wind_txt_Recent_10min$MESS_DATUM > StartDate_10min & Wind_txt_Recent_10min$MESS_DATUM < EndDate_10min),])

extreme_wind_10min <- extreme_wind_txt_Historical_10min[(extreme_wind_txt_Historical_10min$MESS_DATUM > StartDate_10min & extreme_wind_txt_Historical_10min$MESS_DATUM < EndDate_10min),]
extreme_wind_10min <- rbind(extreme_wind_10min, extreme_wind_txt_Recent_10min[(extreme_wind_txt_Recent_10min$MESS_DATUM > StartDate_10min & extreme_wind_txt_Recent_10min$MESS_DATUM < EndDate_10min),])

precipitation_10min <- precipitation_txt_Historical_10min[(precipitation_txt_Historical_10min$MESS_DATUM > StartDate_10min & precipitation_txt_Historical_10min$MESS_DATUM < EndDate_10min),]
precipitation_10min <- rbind(precipitation_10min, precipitation_txt_Recent_10min[(precipitation_txt_Recent_10min$MESS_DATUM > StartDate_10min & precipitation_txt_Recent_10min$MESS_DATUM < EndDate_10min),])

solar_10min <- solar_txt_Historical_10min[(solar_txt_Historical_10min$MESS_DATUM > StartDate_10min & solar_txt_Historical_10min$MESS_DATUM < EndDate_10min),]
solar_10min <- rbind(solar_10min, solar_txt_Recent_10min[(solar_txt_Recent_10min$MESS_DATUM > StartDate_10min & solar_txt_Recent_10min$MESS_DATUM < EndDate_10min),])

Temperature_10min <- Temperature_txt_Historical_10min[(Temperature_txt_Historical_10min$MESS_DATUM > StartDate_10min & Temperature_txt_Historical_10min$MESS_DATUM < EndDate_10min),]
Temperature_10min <- rbind(Temperature_10min, Temperature_txt_Recent_10min[(Temperature_txt_Recent_10min$MESS_DATUM > StartDate_10min & Temperature_txt_Recent_10min$MESS_DATUM < EndDate_10min),])

extreme_temperatur_10min <- extreme_temperature_txt_Historical_10min[(extreme_temperature_txt_Historical_10min$MESS_DATUM > StartDate_10min & extreme_temperature_txt_Historical_10min$MESS_DATUM < EndDate_10min),]
extreme_temperatur_10min <- rbind(extreme_temperatur_10min, extreme_temperature_txt_Recent_10min[(extreme_temperature_txt_Recent_10min$MESS_DATUM > StartDate_10min & extreme_temperature_txt_Recent_10min$MESS_DATUM < EndDate_10min),])

#Convert the TimeDate in DataFrame
Wind_1h$UTCDateTime <- as.POSIXlt(as.character(Wind_1h$MESS_DATUM), format = "%Y%m%d%H")
cloud_type_1h$UTCDateTime <- as.POSIXlt(as.character(cloud_type_1h$MESS_DATUM), format = "%Y%m%d%H")
cloudiness_1h$UTCDateTime <- as.POSIXlt(as.character(cloudiness_1h$MESS_DATUM), format = "%Y%m%d%H")
dew_point_1h$UTCDateTime <- as.POSIXlt(as.character(dew_point_1h$MESS_DATUM), format = "%Y%m%d%H")
extreme_wind_1h$UTCDateTime <- as.POSIXlt(as.character(extreme_wind_1h$MESS_DATUM), format = "%Y%m%d%H")
moisture_1h$UTCDateTime <- as.POSIXlt(as.character(moisture_1h$MESS_DATUM), format = "%Y%m%d%H")
precipitation_1h$UTCDateTime <- as.POSIXlt(as.character(precipitation_1h$MESS_DATUM), format = "%Y%m%d%H")
pressure_1h$UTCDateTime <- as.POSIXlt(as.character(pressure_1h$MESS_DATUM), format = "%Y%m%d%H")
soil_temperature_1h$UTCDateTime <- as.POSIXlt(as.character(soil_temperature_1h$MESS_DATUM), format = "%Y%m%d%H")
solar_1h$UTCDateTime <- as.POSIXlt(as.character(solar_1h$MESS_DATUM), format = "%Y%m%d%H")
sun_1h$UTCDateTime <- as.POSIXlt(as.character(sun_1h$MESS_DATUM), format = "%Y%m%d%H")
Temperature_1h$UTCDateTime <- as.POSIXlt(as.character(Temperature_1h$MESS_DATUM), format = "%Y%m%d%H")
visibility_1h$UTCDateTime <- as.POSIXlt(as.character(visibility_1h$MESS_DATUM), format = "%Y%m%d%H")
weather_phenomena_1h$UTCDateTime <- as.POSIXlt(as.character(weather_phenomena_1h$MESS_DATUM), format = "%Y%m%d%H")
wind_synop_1h$UTCDateTime <- as.POSIXlt(as.character(wind_synop_1h$MESS_DATUM), format = "%Y%m%d%H")

Wind_10min$UTCDateTime <- as.POSIXct(as.character(Wind_10min$MESS_DATUM), format = "%Y%m%d%H%M")
extreme_wind_10min$UTCDateTime <- as.POSIXct(as.character(extreme_wind_10min$MESS_DATUM), format = "%Y%m%d%H%M")
precipitation_10min$UTCDateTime <- as.POSIXct(as.character(precipitation_10min$MESS_DATUM), format = "%Y%m%d%H%M")
solar_10min$UTCDateTime <- as.POSIXct(as.character(solar_10min$MESS_DATUM), format = "%Y%m%d%H%M")
Temperature_10min$UTCDateTime <- as.POSIXct(as.character(Temperature_10min$MESS_DATUM), format = "%Y%m%d%H%M")
extreme_temperatur_10min$UTCDateTime <- as.POSIXct(as.character(extreme_temperatur_10min$MESS_DATUM), format = "%Y%m%d%H%M")



##########Create Complete Dataframe#########


TotalData <- data.frame()


TotalData <- merge( Wind_1h[ , c("F", "D", "UTCDateTime")], cloud_type_1h[ , c("V_N", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

colnames(TotalData)[colnames(TotalData) %in% c("F", "D", "V_N")] <- c("Wind_Speed", "Wind_Direction", "Cloud_Cover")
TotalData <- TotalData[!duplicated(TotalData), ]

# TotalData <- merge( TotalData, cloudiness_1h,
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)

TotalData <- merge( TotalData, dew_point_1h[ , c("TT", "TD", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("TT", "TD")] <- c("dew_point_temperatuer", "airtemperature")
TotalData <- TotalData[!duplicated(TotalData), ]


# TotalData <- merge( TotalData, extreme_wind_1h, 
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# 


TotalData <- merge( TotalData, moisture_1h[ , c("ABSF_STD", "VP_STD", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("ABSF_STD", "VP_STD")] <- c("humidity_absolute", "pressure_vapor")
TotalData <- TotalData[!duplicated(TotalData), ]


TotalData <- merge( TotalData, pressure_1h[ , c("P", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("P")] <- c("pressure_air_sea_level")
TotalData <- TotalData[!duplicated(TotalData), ]


TotalData <- merge( TotalData, soil_temperature_1h[ , c("V_TE005", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("V_TE005")] <- c("temperature_soil_mean_005cm")
TotalData <- TotalData[!duplicated(TotalData), ]


TotalData <- merge( TotalData, solar_1h[ , c("FG_LBERG", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("FG_LBERG")] <- c("radiaion_global")
TotalData <- TotalData[!duplicated(TotalData), ]


TotalData <- merge( TotalData, sun_1h[ , c("SD_SO", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("SD_SO")] <- c("sunshine_duration")
TotalData <- TotalData[!duplicated(TotalData), ]

# TotalData <- merge( TotalData, Temperature_1h[ , c("ABSF_STD", "VP_STD", "UTCDateTime")],
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# colnames(TotalData)[colnames(TotalData) %in% c("ABSF_STD", "VP_STD")] <- c("1h_humidity_absolute", "1h_pressure_vapor")

# 
# TotalData <- merge( TotalData, visibility_1h[ , c("ABSF_STD", "VP_STD", "UTCDateTime")], 
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# colnames(TotalData)[colnames(TotalData) %in% c("ABSF_STD", "VP_STD")] <- c("1h_humidity_absolute", "1h_pressure_vapor")

# 
# TotalData <- merge( TotalData, weather_phenomena_1h[ , c("ABSF_STD", "VP_STD", "UTCDateTime")],
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# colnames(TotalData)[colnames(TotalData) %in% c("ABSF_STD", "VP_STD")] <- c("humidity_absolute", "pressure_vapor")

# 
# TotalData <- merge( TotalData, wind_synop_1h[ , c("ABSF_STD", "VP_STD", "UTCDateTime")], 
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# colnames(TotalData)[colnames(TotalData) %in% c("ABSF_STD", "VP_STD")] <- c("humidity_absolute", "pressure_vapor")

TotalData <- merge( TotalData, precipitation_1h[ , c("R1", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("SR1")] <- c("sunshine_duration")
TotalData <- TotalData[!duplicated(TotalData), ]


TotalData <- merge( TotalData, precipitation_1h[ , c("R1", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData)[colnames(TotalData) %in% c("R1")] <- c("precipitation_height")
TotalData <- TotalData[!duplicated(TotalData), ]

# ###### 10 Minutes ####

TotalData_10min <- data.frame()


TotalData_10min <- merge( precipitation_10min[ , c("RWS_10", "UTCDateTime")], Wind_10min[ , c("FF_10", "DD_10", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData_10min)[colnames(TotalData_10min) %in% c("RWS_10", "FF_10", "DD_10")] <- c( "precipitation_height", "Wind_Speed", "Wind_Direction")
TotalData_10min <- TotalData_10min[!duplicated(TotalData_10min), ]
# 
# TotalData_10min <- merge( TotalData_10min, extreme_wind_10min[ , c("ABSF_STD", "VP_STD", "UTCDateTime")], 
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# colnames(TotalData_10min)[colnames(TotalData_10min) %in% c("ABSF_STD", "VP_STD")] <- c("humidity_absolute", "pressure_vapor")

# 
# TotalData_10min <- merge( TotalData_10min, precipitation_10min[ , c("ABSF_STD", "VP_STD", "UTCDateTime")], 
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# colnames(TotalData_10min)[colnames(TotalData_10min) %in% c("ABSF_STD", "VP_STD")] <- c("humidity_absolute", "pressure_vapor")


TotalData_10min <- merge( TotalData_10min, solar_10min[ , c("GS_10", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData_10min)[colnames(TotalData_10min) %in% c("GS_10")] <- c("radiation_global")
TotalData_10min <- TotalData_10min[!duplicated(TotalData_10min), ]


TotalData_10min <- merge( TotalData_10min, Temperature_10min[ , c("PP_10", "TT_10", "RF_10", "TD_10", "UTCDateTime")],
                    by.x = "UTCDateTime",
                    by.y = "UTCDateTime",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)
colnames(TotalData_10min)[colnames(TotalData_10min) %in% c("PP_10", "TT_10", "RF_10", "TD_10")] <- c("pressure_air_site", "temperature_air_mean_200", "humidity", "temperature_dew_point_mean_200")
TotalData_10min <- TotalData_10min[!duplicated(TotalData_10min), ]

# 
# TotalData_10min <- merge( TotalData_10min, extreme_temperatur_10min[ , c("ABSF_STD", "VP_STD", "UTCDateTime")], 
#                     by.x = "UTCDateTime",
#                     by.y = "UTCDateTime",
#                     all.x = TRUE,
#                     all.y = TRUE,
#                     sort = TRUE)
# colnames(TotalData_10min)[colnames(TotalData_10min) %in% c("ABSF_STD", "VP_STD")] <- c("humidity_absolute", "pressure_vapor")

TotalData <- TotalData[!is.na(TotalData$UTCDateTime),]
TotalData_10min <- TotalData_10min[!is.na(TotalData_10min$UTCDateTime),]

write.csv(TotalData,"4_Data/OutputData/DWDMeteorologicalData_1h.csv", row.names = FALSE)

write.csv(TotalData_10min,"4_Data/OutputData/DWDMeteorologicalData_10min.csv", row.names = FALSE)
