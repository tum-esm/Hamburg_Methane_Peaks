
# Author:
# Juan Bettinelli,
# Script that preformes a Keeling analyse of the CH4 Measuments at the Geomatikum
# The the Analyse is done depending on the wind direction. 
# The Peaks kan be seen seperatel.
# 22.5.2023

# Declare librarys used

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

Wind_Provider = 1 # Wind_Provider = 1(Geomatikum), 2(Mast 50m) 3(Mast 110m), 4(DWD)



# Values taken from literature as seen below:
# "Characterisation of methane sources in Lutjewad, The Netherlands, using quasi-continuous isotopic composition measurements"

# ## Creating elipses of emission sorces to use in the plots
# # Extraction and distribution of fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
# FF <- Ellipse$new(center = c(-40.0, -198.5), rmajor = 17.75, rminor = 12, alpha = 0)
# # ellipse as a path
# FFpath <- FF$path()
# # the path is not closed; close it
# FFpath <- rbind(FFpath, FFpath[1,])

# # Agriculture 12C 68.0 [70.6; 46.0] 2H 319 [361; 295]
# AG <- Ellipse$new(center = c(-68.0, -319), rmajor = 33 , rminor = 12.3, alpha = 90)
# # ellipse as a path
# AGpath <- AG$path()
# # the path is not closed; close it
# AGpath <- rbind(AGpath, AGpath[1,])
# 
# # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
# WA <- Ellipse$new(center = c(-55, -293), rmajor =  28.4, rminor = 9.5 , alpha = 0)
# # ellipse as a path
# WApath <- WA$path()
# # the path is not closed; close it
# WApath <- rbind(WApath, WApath[1,])
# 
# # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
# OA <- Ellipse$new(center = c(-35, -175), rmajor =  28.4, rminor = 9.5 , alpha = 0)
# # ellipse as a path
# OApath <- OA$path()
# # the path is not closed; close it
# OApath <- rbind(OApath, OApath[1,])
# 
# # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
# WL <- Ellipse$new(center = c(-69, -330), rmajor =  56 , rminor = 18.7 , alpha = 90)
# # ellipse as a path
# WLpath <- WL$path()
# # the path is not closed; close it
# WLpath <- rbind(WLpath, WLpath[1,])

## Create Ploygon to be used in the Plots
# Thermogenic
TH <- data.frame(x = c(-75, -40, -15, -40, -60), y = c(-350, -100, -150, -300, -350))
# Abiotic
A <- data.frame(x = c(-50, -50, -25, -10, -10), y = c(-450, -300, -50, -50, -450))



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

# Create a dataframe with only the Methane Data
CH4Data <- TotalData[, c("UTC", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H", "X.CH4.", "c13C", "c2H")]
CH4Data <- CH4Data[complete.cases(CH4Data[ , "X.CH4."]),]

# Add New empty columns for the wind speed and direction. 
CH4Data[,"Speed"] <- NA
CH4Data[,"Direction"] <- NA

# Select the Wind data provided by Uni. Hamburg or DWD
if (Wind_Provider == 1){ 
  # Geomatikum
    W_Speed <- "Speed"
    W_Direction <- "Direction"
} else if (Wind_Provider == 2){
    # Mast 50m
    W_Speed <- "Speed50m"
    W_Direction <- "Direction50m"
} else if (Wind_Provider == 3){
    # Mast 110m
    W_Speed <- "Speed110m"
    W_Direction <- "Direction110m"
} else if (Wind_Provider == 4){
    # DWD
    W_Speed <- "Wind_Speed"
    W_Direction <- "Wind_Direction"
  }

# Get the mean Wind Direction and Speed For 10 min before and after each wind measurement
for(i in 1:nrow(CH4Data)) {       # for-loop over all rows
  # Find the mean Values during the Peak
  CH4Data[i ,"Speed"] <- mean(TotalData[TotalData$UTC >= (CH4Data[i,"UTC"] - 10*60) & TotalData$UTC <= (CH4Data[i,"UTC"] + 10*60), W_Speed ], na.rm = TRUE)
  CH4Data[i ,"Direction"] <- mean(TotalData[TotalData$UTC >= (CH4Data[i,"UTC"] - 10*60) & TotalData$UTC <= (CH4Data[i,"UTC"] + 10*60), W_Direction ], na.rm = TRUE)
}

# Round the direction to nearest 10°
CH4Data$Direction <- round(CH4Data$Direction, digits = -1)
# replace 360° values with 0° to remain consistent
CH4Data$Direction[CH4Data$Direction == 360] <- 0 

# Create new dataframe for the Keenling analyse results
Keeling_output <- data_frame()

# Loop over all 10° values
for (i in seq(0, 350, by = 10)){
  # Keeling Analyse for total data of the champagne Time series
  # For C13
  v_c13C_Line <- lm(d13C.VPDB ~ c13C, CH4Data[CH4Data$Direction == i,])
  v_c13C_coef <- coef(summary(v_c13C_Line))[, "Estimate"]
  v_c13C_se <- coef(summary(v_c13C_Line))[, "Std. Error"] 
  # For H2
  v_c2H_Line <- lm(d2H.VPDB ~ c2H, CH4Data[CH4Data$Direction == i,] )
  v_c2H_coef <- coef(summary(v_c2H_Line))[, "Estimate"]
  v_c2H_se <- coef(summary(v_c2H_Line))[, "Std. Error"] 
  
  # Save results in output dataframe
  Keeling_output[nrow(Keeling_output) + 1, 1] <- as.integer(i)
  Keeling_output[nrow(Keeling_output) , 2] <- v_c13C_coef[[1]]
  Keeling_output[nrow(Keeling_output) , 3] <- v_c13C_se[[1]]
  Keeling_output[nrow(Keeling_output) , 4] <- v_c2H_coef[[1]]
  Keeling_output[nrow(Keeling_output) , 5] <- v_c2H_se[[1]]
}
# Change the collounm names
colnames(Keeling_output) <- c("Direction", "c13C_coef", "c13C_se", "c2H_coef", "c2H_se")
# remove empty values
Keeling_output <- Keeling_output[complete.cases(Keeling_output[ , ]),]

# export the reslts to a CSV file
write.csv(Keeling_output, "4_Data/OutputData/Keeling_By_Wind_Total.csv", row.names=FALSE)


# Plotting the results in a combine isotope plot to identify emmittor type.
# regions are highlightet for the emmition type

p0 <- ggplot() +
  geom_point(data=Keeling_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction), width=.02, alpha=0.5) + #, position=pd
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "purple")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="yellow")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="#6B8E23")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures")+
  scale_color_gradient2(midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )+
  theme(legend.position = "none")


# fill only
p1 <- ggplot() +
  geom_point(data=Keeling_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction), width=.02, alpha=0.5) + #, position=pd
  scale_color_gradient2(name = "Wind Direction, °", midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )+
  theme(legend.direction = "vertical", legend.box = "vertical")


# color only
p2 <- ggplot() +
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175, color = "Fossil fuels & nonindustrial combustion"), alpha=0.1) + #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295, color = "Agriculture"),alpha=0.1) + # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293, color = "Waste"),alpha=0.1)+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81, color = "Other anthropogenic sources"),alpha=0.1)+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246, color = "Natural wetlands"), alpha=0.1) +# Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  scale_color_manual('Type',
                     breaks=c("Fossil fuels & nonindustrial combustion", "Agriculture", "Waste", "Other anthropogenic sources","Natural wetlands"),
                     values = c("Fossil fuels & nonindustrial combustion" = "red", "Agriculture" = "green", "Waste" = "purple", "Other anthropogenic sources" = "blue", "Natural wetlands" = "black"),  
                     guide = guide_legend(override.aes = list(alpha = 1)))
# theme(legend.direction = "vertical", legend.box = "vertical")

p3 <- ggplot() +
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250, fill="Microbial CO2 reduction"),alpha=0.1)+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125, fill="Microbial Fermentation"),alpha=0.5)+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y, fill="Thermogenic"), alpha=0.1)+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y, fill="Abiotic"), alpha=0.4)+ # A: Abiotic
  scale_fill_manual('Methode',
                    breaks=c("Microbial CO2 reduction", "Microbial Fermentation", "Thermogenic", "Abiotic"),
                    values = c("Microbial CO2 reduction" = 'yellow', "Microbial Fermentation" = "pink", "Thermogenic" = "#6B8E23", "Abiotic" = "lightblue"),  
                    guide = guide_legend(override.aes = list(alpha = 1)))

leg1 <- get_legend(p1)
leg2 <- get_legend(p2)
leg3 <- get_legend(p3)

# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()


# combine all legends
leg123 <- plot_grid(leg1, leg2, leg3,
                    align = "hv",
                    # axis = "tb",
                    nrow = 3
)

Total_Plot <- plot_grid(p0,
                     leg123,
                     nrow = 1,
                     align = "h",
                     axis = "t",
                     rel_widths = c(1, 0.3)
)

# Save and export the plot
ggsave("12_Keeling_Total_Wind.png", Total_Plot, path = "4_Data/OutputData/Plots/12_Keeling_with_Wind", width = 12, height = 5.2)


########### Legent not working yet!!!!!!!!!!!!!!!!!
# Total_Plot <- ggplot() +
#   geom_point(data=Keeling_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
#   geom_errorbar(data=Keeling_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction), width=.1) + #, position=pd
#   # geom_path(aes(x = x, y = y), as.data.frame(FFpath), color = "red")+
#   geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
#   # geom_path(aes(x = x, y = y), as.data.frame(AGpath), color = "green")+
#   geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
#   # geom_path(aes(x = x, y = y), as.data.frame(WApath), color = "yellow")+
#   geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "yellow")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
#   # geom_path(aes(x = x, y = y), as.data.frame(OApath), color = "blue")+
#   geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
#   # geom_path(aes(x = x, y = y), as.data.frame(WLpath), color = "black")+
#   geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
#   geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="purple")+ # MC: Microbial CO2 reduction
#   geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
#   geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="orange")+ #TH: Thermogenic
#   geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
#   labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures")+
#   scale_color_gradient2(midpoint=180, low="blue", mid="red",
#                       high="blue", space ="Lab" )
  # scale_color_gradientn(colours = rainbow(5))

  


############# Only the Peaks ###########




##### Find Loweres 15%
#Select the Data from Data frame with CH4 Concentration
CH4Data2 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

# Sort the dataset in ascending order
sorted_data <- sort(CH4Data2$X.CH4.)

# Determine the number of observations corresponding to the lowest 15% of the dataset
n_lowest <- round(length(sorted_data) * 0.15)

# Use the head() function to extract the lowest 15% of the dataset
lowest_15_percent <- max(head(sorted_data, n_lowest))
######

# Find the Peaks in the Remaining timeline
CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)

# Format the Peak Data frame 'CH4_Peaks'
# Rename the Columns
names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
# Replace the Index with Timestemps 'UTC'
CH4_Peaks$UTC_Beginning <- CH4Data2[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data2[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data2[CH4_Peaks$UTC,"UTC"]

# Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
# Get all Columns Names from 'TotalData
Heads <- colnames(TotalData)
# Remove empty Columns
Heads <- Heads[-1]
Heads <- Heads[-16]

# Create Data frame with only peaks
Total_Peaks <- data.frame()
# for-loop over rows
for(i in 1:nrow(CH4_Peaks)) {
  Single_Peak <- TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], ]
  Total_Peaks <- rbind(Total_Peaks,Single_Peak)
}

# remove rows with no CH4 data 
Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "X.CH4."]),]

# select only the CH4 data
Total_Peaks <- Total_Peaks[, c("UTC", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H", "X.CH4.", "c13C", "c2H")]
Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "X.CH4."]),]

# Add cllounms for wind speed and direction
Total_Peaks[,"Speed"] <- NA
Total_Peaks[,"Direction"] <- NA

# Find the mean wind direction an speed at each the methan measuent
for(i in 1:nrow(Total_Peaks)) {       # for-loop over rows
  # Find the mean Values during the Peak
  Total_Peaks[i ,"Speed"] <- mean(TotalData[TotalData$UTC >= (Total_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (Total_Peaks[i,"UTC"] + 10*60), W_Speed ], na.rm = TRUE)
  Total_Peaks[i ,"Direction"] <- mean(TotalData[TotalData$UTC >= (Total_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (Total_Peaks[i,"UTC"] + 10*60), W_Direction ], na.rm = TRUE)
}

# round to nearest 10°
Total_Peaks$Direction <- round(Total_Peaks$Direction, digits = -1)
# replace 360° with 0° values
Total_Peaks$Direction[Total_Peaks$Direction == 360] <- 0 

# Create Dataframe to ouput Keeling analyse data
Keeling_Peaks_output <- data_frame()

# Loop for Keeling analyse at each 10 °
for (i in seq(0, 350, by = 10)){
  if (length(which(Total_Peaks$Direction == i)) >= 10){
    # Keeling Analyse for total data of the champagne Time series
    # For C13
    p_c13C_Line <- lm(d13C.VPDB ~ c13C, Total_Peaks[Total_Peaks$Direction == i,])
    p_c13C_coef <- coef(summary(p_c13C_Line))[, "Estimate"]
    p_c13C_se <- coef(summary(p_c13C_Line))[, "Std. Error"] 
    # For H2
    p_c2H_Line <- lm(d2H.VPDB ~ c2H, Total_Peaks[Total_Peaks$Direction == i,] )
    p_c2H_coef <- coef(summary(p_c2H_Line))[, "Estimate"]
    p_c2H_se <- coef(summary(p_c2H_Line))[, "Std. Error"] 
    
    # Save data in output dataframe
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) + 1, 1] <- as.integer(i)
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 2] <- p_c13C_coef[[1]]
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 3] <- p_c13C_se[[1]]
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 4] <- p_c2H_coef[[1]]
    Keeling_Peaks_output[nrow(Keeling_Peaks_output) , 5] <- p_c2H_se[[1]]
  }
}
# Change the collounm names
colnames(Keeling_Peaks_output) <- c("Direction", "c13C_coef", "c13C_se", "c2H_coef", "c2H_se")
# remove empty rows
Keeling_Peaks_output <- Keeling_Peaks_output[complete.cases(Keeling_Peaks_output[ , ]),]

# save and export the keeling analyse results to a CSV file
write.csv(Keeling_Peaks_output, "4_Data/OutputData/Keeling_By_Wind_Peaks.csv", row.names=FALSE)


# Plotting the results in a combine isotope plot to identify emmittor type.
# regions are highlightet for the emmition type


p0 <- ggplot() +
  geom_point(data=Keeling_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction),  width=.02, alpha=0.5) + #, position=pd
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "purple")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="yellow")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="#6B8E23")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures, only Peaks")+
  scale_color_gradient2(midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )+
  theme(legend.position = "none")


# fill only
p1 <- ggplot() +
  geom_point(data=Keeling_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction),  width=.02, alpha=0.5) + #, position=pd
  scale_color_gradient2(name = "Wind Direction, °", midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )+
  theme(legend.direction = "vertical", legend.box = "vertical")


# color only
p2 <- ggplot() +
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175, color = "Fossil fuels & nonindustrial combustion"), alpha=0.1) + #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295, color = "Agriculture"),alpha=0.1) + # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293, color = "Waste"),alpha=0.1)+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81, color = "Other anthropogenic sources"),alpha=0.1)+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246, color = "Natural wetlands"), alpha=0.1) +# Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  scale_color_manual('Type',
                     breaks=c("Fossil fuels & nonindustrial combustion", "Agriculture", "Waste", "Other anthropogenic sources","Natural wetlands"),
                     values = c("Fossil fuels & nonindustrial combustion" = "red", "Agriculture" = "green", "Waste" = "purple", "Other anthropogenic sources" = "blue", "Natural wetlands" = "black"),  
                     guide = guide_legend(override.aes = list(alpha = 1)))
# theme(legend.direction = "vertical", legend.box = "vertical")

p3 <- ggplot() +
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250, fill="Microbial CO2 reduction"),alpha=0.1)+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125, fill="Microbial Fermentation"),alpha=0.5)+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y, fill="Thermogenic"), alpha=0.1)+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y, fill="Abiotic"), alpha=0.4)+ # A: Abiotic
  scale_fill_manual('Methode',
                    breaks=c("Microbial CO2 reduction", "Microbial Fermentation", "Thermogenic", "Abiotic"),
                    values = c("Microbial CO2 reduction" = 'yellow', "Microbial Fermentation" = "pink", "Thermogenic" = "#6B8E23", "Abiotic" = "lightblue"),  
                    guide = guide_legend(override.aes = list(alpha = 1)))

leg1 <- get_legend(p1)
leg2 <- get_legend(p2)
leg3 <- get_legend(p3)

# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()


# combine all legends
leg123 <- plot_grid(leg1, leg2, leg3,
                    align = "hv",
                    # axis = "tb",
                    nrow = 3
)

Peaks_Plot <- plot_grid(p0,
                        leg123,
                        nrow = 1,
                        align = "h",
                        axis = "t",
                        rel_widths = c(1, 0.3)
)


########### Legent not working yet!!!!!!!!!!!!!!!!!
# Peaks_Plot <- ggplot() +
#   geom_point(data=Keeling_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
#   geom_errorbar(data=Keeling_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se, colour=Direction), width=.1) + #, position=pd
#   # geom_path(aes(x = x, y = y), as.data.frame(FFpath), color = "red")+
#   geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
#   # geom_path(aes(x = x, y = y), as.data.frame(AGpath), color = "green")+
#   geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
#   # geom_path(aes(x = x, y = y), as.data.frame(WApath), color = "yellow")+
#   geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "yellow")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
#   # geom_path(aes(x = x, y = y), as.data.frame(OApath), color = "blue")+
#   geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
#   # geom_path(aes(x = x, y = y), as.data.frame(WLpath), color = "black")+
#   geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
#   geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="purple")+ # MC: Microbial CO2 reduction
#   geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
#   geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="orange")+ #TH: Thermogenic
#   geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
#   labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures, only Peaks")+
#   scale_color_gradient2(midpoint=180, low="blue", mid="red",
#                         high="blue", space ="Lab" )
#   # scale_color_gradientn(colours = rainbow(5))

# Save the plot
ggsave("12_Keeling_Peaks_Wind.png", Peaks_Plot, path = "4_Data/OutputData/Plots/12_Keeling_with_Wind", width = 12, height = 5.2)




############### Exuding the Peaks #############
# Create a datafram the dose not have the Peaks
No_Peaks <- subset(TotalData, UTC = Total_Peaks$UTC)
# remove rows with no CH4 Measuments
No_Peaks <- No_Peaks[complete.cases(No_Peaks[ , "X.CH4."]),]

# selct only the Meathan Values
No_Peaks <- No_Peaks[, c("UTC", "X.CH4..13C", "d13C.VPDB", "sd..CH4.", "sd.d13C", "X.CH4..2H", "d2H.VPDB", "sd..CH4..1", "sd.d2H", "X.CH4.", "c13C", "c2H")]
No_Peaks <- No_Peaks[complete.cases(No_Peaks[ , "X.CH4."]),]

# Add new Rows for the wind data
No_Peaks[,"Speed"] <- NA
No_Peaks[,"Direction"] <- NA

# Loop to find the mean wind speed and direction during measument (10 min)
for(i in 1:nrow(No_Peaks)) {       # for-loop over rows
  # Find the mean Values during the Peak
  No_Peaks[i ,"Speed"] <- mean(TotalData[TotalData$UTC >= (No_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (No_Peaks[i,"UTC"] + 10*60), W_Speed ], na.rm = TRUE)
  No_Peaks[i ,"Direction"] <- mean(TotalData[TotalData$UTC >= (No_Peaks[i,"UTC"] - 10*60) & TotalData$UTC <= (No_Peaks[i,"UTC"] + 10*60), W_Direction ], na.rm = TRUE)
}

# round to the nearest 10° wind direction
No_Peaks$Direction <- round(No_Peaks$Direction, digits = -1)
# replace 360° values with 0° to remain consistent
No_Peaks$Direction[No_Peaks$Direction == 360] <- 0 

# Create new dataframe to save the Keeling analyse results
Keeling_No_Peaks_output <- data_frame()

# Keeling Anlayse for every wind direction in 10 ° steps
for (i in seq(0, 350, by = 10)){
  if (length(which(No_Peaks$Direction == i)) >= 10){
    # Keeling Analyse for total data of the champagne Time series
    # For C13
    n_c13C_Line <- lm(d13C.VPDB ~ c13C, No_Peaks[No_Peaks$Direction == i,])
    n_c13C_coef <- coef(summary(n_c13C_Line))[, "Estimate"]
    n_c13C_se <- coef(summary(n_c13C_Line))[, "Std. Error"]
    # For H2
    n_c2H_Line <- lm(d2H.VPDB ~ c2H, No_Peaks[No_Peaks$Direction == i,] )
    n_c2H_coef <- coef(summary(n_c2H_Line))[, "Estimate"]
    n_c2H_se <- coef(summary(n_c2H_Line))[, "Std. Error"]
    
    # Save the results to output dataframe
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) + 1, 1] <- as.integer(i)
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 2] <- n_c13C_coef[[1]]
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 3] <- n_c13C_se[[1]]
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 4] <- n_c2H_coef[[1]]
    Keeling_No_Peaks_output[nrow(Keeling_No_Peaks_output) , 5] <- n_c2H_se[[1]]
  }
}

# Change the dataframe collounm names
colnames(Keeling_No_Peaks_output) <- c("Direction", "c13C_coef", "c13C_se", "c2H_coef", "c2H_se")
# remove empty rows
Keeling_No_Peaks_output <- Keeling_No_Peaks_output[complete.cases(Keeling_No_Peaks_output[ , ]),]

# Save the Keeling analyse results to CSV File
write.csv(Keeling_No_Peaks_output, "4_Data/OutputData/Keeling_By_Wind_No_Peaks.csv", row.names=FALSE)

# Plotting the results in a combine isotope plot to identify emmittor type.
# regions are highlightet for the emmition type

p0 <- ggplot() +
  geom_point(data=Keeling_No_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_No_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se , colour=Direction), width=.02, alpha=0.5) + #, position=pd
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "purple")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="yellow")+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="#6B8E23")+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
  labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures, exluding Peaks")+
  scale_color_gradient2(midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )+
  theme(legend.position = "none")


# fill only
p1 <- ggplot() +
  geom_point(data=Keeling_No_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
  geom_errorbar(data=Keeling_No_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se , colour=Direction), width=.02, alpha=0.5) + #, position=pd 
  scale_color_gradient2(name = "Wind Direction, °", midpoint=180, low="blue", mid="red",
                        high="blue", space ="Lab" )+
  theme(legend.direction = "vertical", legend.box = "vertical")


# color only
p2 <- ggplot() +
  geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175, color = "Fossil fuels & nonindustrial combustion"), alpha=0.1) + #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
  geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295, color = "Agriculture"),alpha=0.1) + # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
  geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293, color = "Waste"),alpha=0.1)+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
  geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81, color = "Other anthropogenic sources"),alpha=0.1)+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
  geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246, color = "Natural wetlands"), alpha=0.1) +# Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
  scale_color_manual('Type',
                     breaks=c("Fossil fuels & nonindustrial combustion", "Agriculture", "Waste", "Other anthropogenic sources","Natural wetlands"),
                     values = c("Fossil fuels & nonindustrial combustion" = "red", "Agriculture" = "green", "Waste" = "purple", "Other anthropogenic sources" = "blue", "Natural wetlands" = "black"),  
                     guide = guide_legend(override.aes = list(alpha = 1)))
# theme(legend.direction = "vertical", legend.box = "vertical")

p3 <- ggplot() +
  geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250, fill="Microbial CO2 reduction"),alpha=0.1)+ # MC: Microbial CO2 reduction
  geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125, fill="Microbial Fermentation"),alpha=0.5)+ # MF: Microbial Fermentation
  geom_polygon(data = TH, aes(x, y, fill="Thermogenic"), alpha=0.1)+ #TH: Thermogenic
  geom_polygon(data = A, aes(x, y, fill="Abiotic"), alpha=0.4)+ # A: Abiotic
  scale_fill_manual('Methode',
                    breaks=c("Microbial CO2 reduction", "Microbial Fermentation", "Thermogenic", "Abiotic"),
                    values = c("Microbial CO2 reduction" = 'yellow', "Microbial Fermentation" = "pink", "Thermogenic" = "#6B8E23", "Abiotic" = "lightblue"),  
                    guide = guide_legend(override.aes = list(alpha = 1)))

leg1 <- get_legend(p1)
leg2 <- get_legend(p2)
leg3 <- get_legend(p3)

# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()


# combine all legends
leg123 <- plot_grid(leg1, leg2, leg3,
                    align = "hv",
                    # axis = "tb",
                    nrow = 3
)

No_Peaks_Plot <- plot_grid(p0,
                        leg123,
                        nrow = 1,
                        align = "h",
                        axis = "t",
                        rel_widths = c(1, 0.3)
)


# ########### Legent not working yet!!!!!!!!!!!!!!!!!
# No_Peaks_Plot <- ggplot() +
#   geom_point(data=Keeling_No_Peaks_output, aes(x = c13C_coef, y = c2H_coef, color = Direction))+
#   geom_errorbar(data=Keeling_No_Peaks_output, aes(x = c13C_coef, xmin=c13C_coef-c13C_se, xmax=c13C_coef+c13C_se, y = c2H_coef, ymin=c2H_coef-c2H_se, ymax=c2H_coef+c2H_se , colour=Direction), width=.02, alpha=0.5) + #, position=pd
#   # geom_path(aes(x = x, y = y), as.data.frame(FFpath), color = "red")+
#   geom_rect(aes(xmin=-66.4,xmax=-30.9,ymin=-199,ymax=-175), alpha=0.1, color = "red")+ #fossil fuels & nonindustrial combustion 12C 40.0 [66.4; 30.9], 2H 175 [199; 175]
#   # geom_path(aes(x = x, y = y), as.data.frame(AGpath), color = "green")+
#   geom_rect(aes(xmin=-70.6,xmax=-46.0,ymin=-361,ymax=-295),alpha=0.1, color = "green")+ # Agriculture 12C 68.0 [70.6; 46.0], 2H 319 [361; 295]
#   # geom_path(aes(x = x, y = y), as.data.frame(WApath), color = "yellow")+
#   geom_rect(aes(xmin=-73.9,xmax=-45.5,ymin=-312,ymax=-293),alpha=0.1, color = "yellow")+ # Waste 12C 55 [73.9; 45.5], 2H 293 [312; 293]
#   # geom_path(aes(x = x, y = y), as.data.frame(OApath), color = "blue")+
#   geom_rect(aes(xmin=-60,xmax=-9,ymin=-175,ymax=-81),alpha=0.1, color = "blue")+ # Other anthropogenic sources 12C 35.0 [60; 9], 2H 175 [175; 81]
#   # geom_path(aes(x = x, y = y), as.data.frame(WLpath), color = "black")+
#   geom_rect(aes(xmin=-88.9,xmax=-51.5,ymin=-358,ymax=-246),alpha=0.1, color = "black")+ # Natural wetlands 12C 69 [88.9; 51.5], 2H 330 [358; 246]
#   geom_rect(aes(xmin=-90,xmax=-50,ymin=-450,ymax=-250),alpha=0.1, fill="purple")+ # MC: Microbial CO2 reduction
#   geom_rect(aes(xmin=-60,xmax=-90,ymin=-350,ymax=-125),alpha=0.5, fill="pink")+ # MF: Microbial Fermentation
#   geom_polygon(data = TH, aes(x, y), alpha=0.1,fill="orange")+ #TH: Thermogenic
#   geom_polygon(data = A, aes(x, y), alpha=0.4, fill="lightblue")+ # A: Abiotic
#   labs(x = expression(delta^13*'C in ‰'), y = expression(delta*'D in ‰'), title = "Dual isotope plots of the isotopic source signatures, exluding Peaks")+
#   scale_color_gradient2(midpoint=180, low="blue", mid="red",
#                         high="blue", space ="Lab" )
#   # scale_color_gradientn(colours = rainbow(5))
  
# Save the Plot 
ggsave("12_Keeling_No_Peaks_Wind.png", No_Peaks_Plot, path = "4_Data/OutputData/Plots/12_Keeling_with_Wind", width = 12, height = 5.2)

