library(pacman)
library(lubridate)
library(readr)
library("plyr")
library(tidyverse)
library("ggplot2")   
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr) 

setwd("/Users/juanbettinelli/Desktop/TUM/2021_Hamburg Campaign/Isotope Measuments")
mydir = "Waterlevel1"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)

WLTotal <- data.frame(stringsAsFactors=FALSE) 


for (f in myfiles){

  #Read Concentration Data of the CSV Files
  WL_csv<- import(f, ";", escape_double = FALSE, trim_ws = TRUE)
  
WL_csv$UTC <- as.POSIXlt(WL_csv$UTC , format="%d.%m.%y %H:%M")
WL_csv$UTCTimeDate <- as.character(WL_csv$UTC , format="%Y%m%d%H%M")

WLTotal <- rbind(WLTotal, WL_csv)
}

plot(WLTotal$UTC,WLTotal$Level)

