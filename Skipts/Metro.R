library(lubridate)
library(pacman)
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 

#Load all the Met data into the skript
Wind_txt <- import("Metro Data/stundenwerte_FF_01975_akt/produkt_ff_stunde_20200224_20210826_01975.txt")
Niederschlag_txt <- import("Metro Data/stundenwerte_RR_01975_akt/produkt_rr_stunde_20200224_20210826_01975.txt")
TempFeuch_txt <- import("Metro Data/stundenwerte_TU_01975_akt/produkt_tu_stunde_20200224_20210826_01975.txt")
Erd_txt <- import("Metro Data/stundenwerte_EB_01975_akt/produkt_eb_stunde_20200224_20210826_01975.txt")

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


#Plot the Data
plot(Wind_New$UTCDateTime,
     Wind_New$D,
     type = "b",
     pch = 20,
     col = "Black",
     xlab = "Date",
     ylab = "Directions"
     )