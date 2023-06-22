
# Author:
# Juan Bettinelli,
# Script to be used in the Paper "Quantification of methane emissions in Hamburg using a network of FTIR spectrometers and an inverse modeling approach"
# Data from the Hamburg campaign 2021-2022.
# This Script is used for a Keeling analysed of the data collected in Hamburg Geomatikum in 2021.
# The script produces a timeline of the total Methan concentration
#Last edit: 22.05.2023

# Declare librarys used

library(ggplot2)   
library(hexbin)
library(gridExtra)
library(dplyr)
library(plotly)
library(rio)
library(gridExtra)
library(grid)


# Set Working Directory
setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")


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

########### Read the CSV File #############

# Read the CSV File
TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
# format the Date 'UTC'
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC), 
                            format = "%Y-%m-%d %H:%M:%S", 
                            tz = "UTC")
# Convert the format of 'X.CH4.' to numeric
TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)
# Filter out all the dated that are outside the selected Strating and Finish time of the campaign
TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)
# Remove Empty Cells n data frame
TotalData <- TotalData[!is.na(TotalData$UTC),]

# Calculate 1/Mole Fraction for C13 & H2
TotalData$c13C <- 1/TotalData$X.CH4..13C
TotalData$c2H <- 1/TotalData$X.CH4..2H

############# Find the Peaks and the  Create a dataframe with only the Peaks ###########

#Select the Data from Data frame with CH4 Concentration
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

##### Find Loweres 15%
#Select the Data from Dataframe with CH4 Concentration
CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]

# Sort the dataset in ascending order
sorted_data <- sort(CH4Data$X.CH4.)

# Determine the number of observations corresponding to the lowest 15% of the dataset
n_lowest <- round(length(sorted_data) * 0.15)

# Use the head() function to extract the lowest 15% of the dataset
lowest_15_percent <- max(head(sorted_data, n_lowest))
######

# Find the Peaks in the Remaining timeline
CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)

# Format the Peak Data frame 'CH4_Peaks'
# Rename the Columns
names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
# Replace the Index with Timestemps 'UTC'
CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]

# # Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
# # Get all Columns Names from 'TotalData
# Heads <- colnames(TotalData)
# # Remove empty Columns
# Heads <- Heads[-1]
# Heads <- Heads[-16]

# Create Data frame with only peaks
Total_Peaks <- data.frame()
# for-loop over rows
for(i in 1:nrow(CH4_Peaks)) {
  Single_Peak <- TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], ]
  Total_Peaks <- rbind(Total_Peaks,Single_Peak)
}

Total_Peaks <- Total_Peaks[complete.cases(Total_Peaks[ , "X.CH4."]),]


No_Peaks <- subset(TotalData, UTC = Total_Peaks$UTC) ###### check if it works!!!!
No_Peaks <- No_Peaks[complete.cases(No_Peaks[ , "X.CH4."]),]


################ Keeling analyse ##############

# Keeling Analyse for total data of the champagne Time series
# For C13
c13C_Line <- lm(d13C.VPDB ~ c13C, TotalData )
c13C_coef <- coef(summary(c13C_Line))[, "Estimate"]
c13C_se <- coef(summary(c13C_Line))[, "Std. Error"] 
# For H2
c2H_Line <- lm(d2H.VPDB ~ c2H, TotalData )
c2H_coef <- coef(summary(c2H_Line))[, "Estimate"]
c2H_se <- coef(summary(c2H_Line))[, "Std. Error"] 

# Keeling analyse for Peaks
# Peaks selected with Peak Finder
# For C13
p_c13C_Line <- lm(d13C.VPDB ~ c13C, Total_Peaks )
p_c13C_coef <- coef(summary(p_c13C_Line))[, "Estimate"]
p_c13C_se <- coef(summary(p_c13C_Line))[, "Std. Error"]
# For H2
p_c2H_Line <- lm(d2H.VPDB ~ c2H, Total_Peaks )
p_c2H_coef <- coef(summary(p_c2H_Line))[, "Estimate"]
p_c2H_se <- coef(summary(p_c2H_Line))[, "Std. Error"] 

# Keeling analyse excluding the peaks 
# Peaks selected with Peak Finder
# For C13
r_c13C_Line <- lm(d13C.VPDB ~ c13C, No_Peaks )
r_c13C_coef <- coef(summary(r_c13C_Line))[, "Estimate"]
r_c13C_se <- coef(summary(r_c13C_Line))[, "Std. Error"] 
# For H2
r_c2H_Line <- lm(d2H.VPDB ~ c2H, No_Peaks )
r_c2H_coef <- coef(summary(r_c2H_Line))[, "Estimate"]
r_c2H_se <- coef(summary(r_c2H_Line))[, "Std. Error"] 

############## Keeling Plots ############

# Complete Timeline including peaks and base measurements
q <- ggplot(TotalData, aes(x = c13C, y = d13C.VPDB)) +
        geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
        expand_limits(x = 0) +
        geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
        labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(c13C_coef[[1]], digits = 1),"‰ ± ", round(c13C_se[[1]], digits = 1),"‰ s.e)")) +
        theme(axis.text.x=element_text(angle=60, hjust=1),
                plot.title = element_text(size=10))
      
k <- ggplot(TotalData, aes(x = c2H, y = d2H.VPDB)) +
      expand_limits(x = 0) +
      geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
      geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
      labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("2H, δ(2)H \n (mean = ", round(c2H_coef[[1]], digits = 1),"‰ ±", round(c2H_se[[1]], digits = 1),"‰ s.e)")) +
      theme(axis.text.x=element_text(angle=60, hjust=1),
              plot.title = element_text(size=10))
KP_Total <- grid.arrange(q,k, ncol = 2,  top = textGrob("Keeling plot of compleate measument campaign",gp=gpar(fontsize=15,font=3)))

ggsave("11_Keeling_Plot_Total.png", KP_Total, path = "4_Data/OutputData/Plots/11_Keeling_Analyse", width = 10, height = 5)


# Plot Keeling Plot Only Peaks
KP_13C_Peaks <- ggplot(Total_Peaks, aes(x = c13C, y = d13C.VPDB)) +
  geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(p_c13C_coef[[1]], digits = 1),"‰ ± ", round(p_c13C_se[[1]], digits = 1),"‰ s.e)")) + 
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_2H_Peaks <- ggplot(Total_Peaks, aes(x = c2H, y = d2H.VPDB)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("D, δD \n (mean = ", round( p_c2H_coef[[1]], digits = 1),"‰ ± ", round(p_c2H_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_Peaks <- grid.arrange(KP_13C_Peaks,KP_2H_Peaks, ncol = 2,  top = textGrob("Keeling plot of only the Peaks",gp=gpar(fontsize=15,font=3)))

ggsave("11_Keeling_Plot_Peaks.png", KP_Peaks, path = "4_Data/OutputData/Plots/11_Keeling_Analyse", width = 10, height = 5)

# Plot Keeling Plot No Peaks
KP_13C_NoPeaks <- ggplot(No_Peaks, aes(x = c13C, y = d13C.VPDB)) +
  geom_point(aes(x = c13C, y = d13C.VPDB), shape = 3, size = 1, col='red') +
  expand_limits(x = 0) +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta^13*'C in ‰'), title = paste0("13C, δ(13)C \n (mean = ", round(r_c13C_coef[[1]], digits = 1),"‰ ± ", round( r_c13C_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_2H_NoPeaks <- ggplot(No_Peaks, aes(x = c2H, y = d2H.VPDB)) +
  expand_limits(x = 0) +
  geom_point(aes(x = c2H, y = d2H.VPDB), shape = 3, size = 1, col='blue') +
  geom_smooth(method = "lm", se=TRUE, col='black', size=0.5, fullrange = TRUE) +
  labs(x = expression('(c'[CH[4]]*')'^-1*' in ppb'^-1), y = expression(delta*'D in ‰'), title = paste0("D, δD \n (mean = ", round(r_c2H_coef[[1]], digits = 1),"‰ ± ", round(r_c2H_se[[1]], digits = 1),"‰ s.e)")) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        plot.title = element_text(size=10))

KP_Ex_Peaks <- grid.arrange(KP_13C_NoPeaks,KP_2H_NoPeaks, ncol = 2,  top = textGrob("Keeling plot of compleate measument campaign excluding methane peaks",gp=gpar(fontsize=15,font=3)))

ggsave("11_Keeling_Plot_Ex_Peaks.png", KP_Ex_Peaks, path = "4_Data/OutputData/Plots/11_Keeling_Analyse", width = 10, height = 5)

##### Show Keeling analyse Data in output Console ######
message("\n \nTotal timeseries: \n 12C, δ(13)C (mean = ", c13C_coef[[1]],"‰ ± ", c13C_se[[1]],"‰ s.e; n = 1)","\n D, δD  (mean =", c2H_coef[[1]],"‰ ±", c2H_se[[1]],"‰ s.e; n = 1)")
message("Just the peaks: \n 12C, δ(13)C (mean = ", p_c13C_coef[[1]],"‰ ± ", p_c13C_se[[1]],"‰ s.e; n = 1)", " \n D, δD  (mean =", p_c2H_coef[[1]],"‰ ±", p_c2H_se[[1]],"‰ s.e; n = 1)")
message("Excluding the peaks: \n 12C, δ(13)C (mean =", r_c13C_coef[[1]],"‰ ±", r_c13C_se[[1]],"‰ s.e; n = 1)","\n D, δD (mean =", r_c2H_coef[[1]],"‰ ±", r_c2H_se[[1]],"‰ s.e; n = 1)")





# ######### not needed ##########
# # Plot CH4 Concentration Timeline
# # Total CH4 Concentration Timeline (Without a Gap)
# Timeline_Total_CH4 <- ggplot(TotalData[!is.na(TotalData$X.CH4..13C), ], aes(x = UTC, y = X.CH4.)) +
#         geom_line() + 
#         labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Compleate Timeline) Stationary in-Situ Measurement") +
#         scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#         theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 13C Measurement
# Timeline_13C_CH4 <- ggplot(TotalData[!is.na(TotalData$X.CH4..13C), ], aes(x = UTC, y = X.CH4..13C)) +
#       geom_line() + 
#       labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 13C Timeline) Stationary in-Situ Measurement") +
#       scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#       theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 2H Measurement
# Timeline_2H_CH4 <- ggplot(TotalData[!is.na(TotalData$X.CH4..2H), ], aes(x = UTC, y = X.CH4..2H)) +
#       geom_line() + 
#       labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 2H Timeline) Stationary in-Situ Measurement") +
#       scale_x_datetime(date_breaks = "2 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#       theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Combine The Three timelines in one Plot
# grid.arrange(Timeline_Total_CH4,Timeline_13C_CH4, Timeline_2H_CH4, nrow = 3)
# 
# 
# # Total CH4 Concentration Timeline (With a Gap)
# #introduce a NA value to produce a gap
# TotalData_13C_CH4 <- TotalData[!is.na(TotalData$X.CH4..13C), ]
# test <- TotalData_13C_CH4[0, ]
# test[1, 1] <- as.POSIXct("2021-08-26 00:00:00")
# TotalData_13C_CH4 <- rbind(TotalData_13C_CH4, test)
# TotalData_13C_CH4 <- TotalData_13C_CH4[order(TotalData_13C_CH4$UTC),]
# 
# TotalData_2HC_CH4 <- TotalData[!is.na(TotalData$X.CH4..2H), ]
# test <- TotalData_2HC_CH4[0, ]
# test[1, 1] <- as.POSIXct("2021-08-26 00:00:00")
# TotalData_2HC_CH4 <- rbind(TotalData_2HC_CH4, test)
# TotalData_2HC_CH4 <- TotalData_2HC_CH4[order(TotalData_2HC_CH4$UTC),]
# 
# 
# Timeline_Total_CH4 <- ggplot(TotalData, aes(x = UTC, y = X.CH4.)) +
#   geom_line() + 
#   labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Compleate Timeline) Stationary in-Situ Measurement") +
#   scale_x_datetime(date_breaks = "10 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#   theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 13C Measurement
# Timeline_13C_CH4 <- ggplot(TotalData_13C_CH4, aes(x = UTC, y = X.CH4..13C)) +
#   geom_line() + 
#   labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 13C Timeline) Stationary in-Situ Measurement") +
#   scale_x_datetime(date_breaks = "10 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#   theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Only CH4 concentration at 2H Measurement
# Timeline_2H_CH4 <- ggplot(TotalData_2HC_CH4, aes(x = UTC, y = X.CH4..2H)) +
#   geom_line() + 
#   labs(x = "Fill Time [UTC]", y = expression(CH[4] * " [ppb]"), title = "(Only 2H Timeline) Stationary in-Situ Measurement") +
#   scale_x_datetime(date_breaks = "10 day", date_labels = "%d-%b", limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))) +
#   theme(axis.text.x=element_text(angle=60, hjust=1))
# 
# # Combine The Three timelines in one Plot
# grid.arrange(Timeline_Total_CH4,Timeline_13C_CH4, Timeline_2H_CH4, nrow = 3)
# 
# 

