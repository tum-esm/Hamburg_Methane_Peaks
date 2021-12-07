library(lubridate)
library(readr)
library("plyr")
library(tidyverse)


setwd("/Users/juanbettinelli/Desktop/TUM/2021_Hamburg Campaign/Isotope Measuments")
mydir = "Waterlevel1"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

WLTotal <- data.frame(
  DateTime = NA, 
  WaterLevel = NA,
  stringsAsFactors = FALSE
)


for (f in myfiles){
  
  #Read Concentration Data of the CSV Files
  WL.Raw<- read_delim(f, ";", escape_double = FALSE, trim_ws = TRUE)
  
  
  #W_Time <- W_Level_Raw[,1]
  i <- nrow(WL.Raw)
  (W_Date <- as.POSIXlt(colnames(WL.Raw[1]),  format="%d.%m.%y"))
  (W_Date2 <- as.character(colnames(WL.Raw[1]),  format="%d%m%y"))
  
  WL.data <- data.frame(
    DateTime = NA, 
    WaterLevel = NA,
    stringsAsFactors = FALSE
  )
  
  for (j in 1:i){
    D <-  as.POSIXlt(paste0(W_Date2, WL.Raw[[j,1]]), format="%d.%m.%y %H:%M")
    L <-  WL.Raw[[j,2]]
    
    WL.newdata <- data.frame(
      DateTime = D, 
      WaterLevel = L,
      stringsAsFactors = FALSE
    )
    WL.data <- rbind(WL.data,WL.newdata)
    
  }
  
  
  WLTotal <- rbind(WLTotal,WL.data)
  
}

# WLTotal %>% filter(WaterLevel < 450)
# 
# plot(WLTotal)

# 
# WL.Low <- data.frame(
#   DateTime = NA, 
#   WaterLevel = NA,
#   stringsAsFactors = FALSE
# )
#   
# l <- nrow(WLTotal)
# for(k in 1:l)
# {
#   
#   if (is.na(WLTotal[[k,3]]))
#   {
#   }
#   else if (WLTotal[[k,3]] > 450)
#     {
#     }
#   else
#   {
#     WL.Low <- rbind(WL.Low,WLTotal[[k,]])
#   }
# }




