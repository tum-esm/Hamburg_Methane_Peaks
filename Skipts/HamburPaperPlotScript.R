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
setwd("~/Documents/Git/HamburgIsotope/Skipts")


####################Isotrope Data###################

#Read Concentration Data of the CSV Files
CH4_2H <-read.csv2("CH4 2H 20210816.csv",TRUE, ";")
CH4_13C <-read.csv2("CH4 13C 20210816.csv",TRUE, ";")
CH4_concentrations <-read.csv2("CH4 concentrations 20210816.csv",TRUE, ";")


#Convert the date into a readable format
CH4_con_w_d <- CH4_concentrations
CH4_con_w_d$fill.time.utc <- as.POSIXct(CH4_con_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", 
                                        tz = "utc")
CH4_2H_w_d <- CH4_2H
CH4_2H_w_d$fill.time.utc <- as.POSIXct(CH4_2H_w_d$fill.time.utc,
                                       format = "%d.%m.%y %H:%M", 
                                       tz = "utc")
CH4_13C_w_d <- CH4_13C
CH4_13C_w_d$fill.time.utc <- as.POSIXct(CH4_13C_w_d$fill.time.utc,
                                        format = "%d.%m.%y %H:%M", 
                                        tz = "utc")


################### Plotting the Data #######################


TotalData <- data.frame()

TotalData <- merge( CH4_con_w_d, CH4_2H_w_d, 
                    by.x = "fill.time.utc",
                    by.y = "fill.time.utc",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

TotalData <- merge( TotalData, CH4_13C_w_d, 
                    by.x = "fill.time.utc",
                    by.y = "fill.time.utc",
                    all.x = TRUE,
                    all.y = TRUE,
                    sort = TRUE)

################## Calculate 1/Mole Fraction ##################

TotalData$c13C <- 1/TotalData$X.CH4..13C

TotalData$c2H <- 1/TotalData$X.CH4..2H

################## Save the Data ##################

write.csv(TotalData,"CombineCH4Data.csv", row.names = FALSE)

################## Plot CH4Concentration ##################

p <- ggplot(TotalData, aes(x = fill.time.utc, y = X.CH4.)) +
        geom_line() + 
        labs(x = "Fill Time [UTC]", y ="CH4 mole fraction [ppb]", title = "CH4 mole fraction vs. Time") +
        scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%m-%Y") + #, limit=c(as.POSIXct("2021-08-01 22:00:00"),as.POSIXct("2021-09-06 00:00:00"))) +
        theme(axis.text.x=element_text(angle=60, hjust=1))

# q <- ggplot(TotalData, aes(x = c13C, y = d13C.VPDB)) +
#         geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
#         expand_limits(x = 0) +
#         geom_smooth(method = "lm", se=FALSE, col='black', size=0.5, fullrange = TRUE) +
#         labs(x = "Mole Fraction", y = "Isotopic Signatures, ‰", title = " Keeling Plot, 12C") +
#         theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# k <- ggplot(TotalData, aes(x = c2H, y = d2H.VPDB)) +
#       expand_limits(x = 0) +
#       geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
#       geom_smooth(method = "lm", se=FALSE, col='black', size=0.5, fullrange = TRUE) +
#       labs(x = "Mole Fraction", y = "Isotopic Signatures, ‰", title = " Keeling Plot, 2H") +
#       theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# grid.arrange(q,k, ncol = 2)
p
