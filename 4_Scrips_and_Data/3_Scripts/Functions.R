# Scripts with functions used in Plotting_With_Compleate_CSV_File_Data.R
# Author: Juan Bettinelli
# Last edit: 22.5.2023

# library(plyr)
# library(ggplot2)   
# library(hexbin)
# library(reshape2)
# library(openair)
# library(cowplot)
# library(patchwork)
# library(dplyr)
# library(GGally)
# library(ggvis)
# library(httr)
# library(plotly)
# library(stringr)
# library(tidyr)
# library(pracma)


#------------------------------------------------------------------------------------------------------------
# Function to split the Timeline into separate Plots/Panels

panel_function <- function(TotalData, n){
  # library("dplyr")

  # 0 is used in Campain paper, here Equal Sized Plots are produced regadles of the compleatness of the data
  if (n == 0){
    TotalData$panel[TotalData$UTC <= "2021-08-10 23:59:00"] <- 0
    TotalData$panel[TotalData$UTC >= "2021-08-11 00:00:00" & TotalData$UTC <= "2021-08-18 23:59:00"] <- 1
    TotalData$panel[TotalData$UTC >= "2021-08-19 00:00:00" & TotalData$UTC <= "2021-08-28 23:59:00"] <- 2
    TotalData$panel[TotalData$UTC >= "2021-08-29 00:00:00"] <- 3
    return(TotalData)
  } else{ # Automaticaly splits the timeline into panels/Plotts, n is the number of Panels
    TotalData <- TotalData %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData))*n))
    return(TotalData)
  }
}

#------------------------------------------------------------------------------------------------------------
# function that checks Fixed panel sizes are uesd and changes n if that is the case 
panel_No_function <- function(n){
  if (n == 0){
    m <- 4
    return(m)
  }
  else{
    m <- n
    return(m)
  }
}

#------------------------------------------------------------------------------------------------------------
# Function to Find CH4 Peaks in Timeline
CH4_Peak_Finder <- function(TotalData = TotalData, Export_CSV = TRUE){
  
  #Select the Data from Dataframe with CH4 Concentration
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
  
  
  # Find the Peaks in the timeline
  CH4_Peaks <- as.data.frame(findpeaks(CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE)) # Strict peaks: CH4Data$X.CH4.,minpeakheight = 2400, minpeakdistance = 15, threshold = 5, sortstr=TRUE) ,medium peaks: CH4Data$X.CH4.,minpeakheight = 2100, minpeakdistance = 25, threshold = 5, sortstr=TRUE , Peak like in the paper: (CH4Data$X.CH4.,minpeakheight = lowest_15_percent, minpeakdistance = 5, threshold = 5, sortstr=TRUE)
  
  # Format the Peak Dataframe
  names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
  CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
  CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
  CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]
  
  # Find the average during the Peak, (Average all values that lay between the Peak beginning and Peak End)
  # get all Coloum Names
  Heads <- colnames(TotalData)
  Heads <- Heads[-1]
  Heads <- Heads[-16]
  for (j in Heads){
    # Create new Coloums with same Names
    CH4_Peaks[,j] <- NA
    for(i in 1:nrow(CH4_Peaks)) {       # for-loop over rows
      # Find the mean Values during the Peak
      CH4_Peaks[i, j] <- mean(TotalData[TotalData$UTC >= CH4_Peaks[i,"UTC_Beginning"] & TotalData$UTC <= CH4_Peaks[i,"UTC_Ending"], j], na.rm = TRUE)
    }
  }
  
  # Remove the "Peaks" at where no measurements were taken (12h)
  CH4_Peaks <- subset(CH4_Peaks, (UTC_Ending - UTC_Beginning) < 12*60 )

  # Checks if the Data Should be returend to the Script ode exported into a CSV File
  if (Export_CSV){
    write.csv(CH4_Peaks, "4_Data/OutputData/CH4_Peaks.csv", row.names=TRUE)
  }
  else {
    return(CH4_Peaks)
  }
}

#------------------------------------------------------------------------------------------------------------
  

# Function to Generate Wind Rode Plots
WindRose_Plots <- function(TotalData = TotalData){
  library("dplyr")
  # Get the Peaks from the data
  CH4_Peaks <- CH4_Peak_Finder(TotalData, FALSE)
  
  # Create and save a windrose Plot with the Total wind data from the Geomatikum
  png("4_Data/OutputData/Plots/9_WindRose/Geomatikum/WindRose_Total.png")
  windRose(TotalData, ws = "Speed", wd = "Direction", angle = 10)
  dev.off()
  
  # Create and save a Windrose Plot with the Averaged Geomatikum wind data at the methane Peaks
  png("4_Data/OutputData/Plots/9_WindRose/Geomatikum/WindRose_Peaks.png")
  windRose(CH4_Peaks, ws = "Speed", wd = "Direction", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot with the Total data, Wind Data from the Geomatikum
  png("4_Data/OutputData/Plots/9_WindRose/Geomatikum/PollutionRose_Total.png")
  pollutionRose(TotalData, ws = "Speed", wd = "Direction", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot Only from the Peaks, Wind Data from the Geomatikum
  png("4_Data/OutputData/Plots/9_WindRose/Geomatikum/PollutionRose_Peaks.png")
  pollutionRose(CH4_Peaks, ws = "Speed", wd = "Direction", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  
  # Create and save a comparison Windrose with the total Wind data vs. the Peak wind data (At the Geomatikum)
  All_Wind <- TotalData[complete.cases(TotalData[ , c("Speed", "Direction")]), c("UTC", "Speed", "Direction")]  
  All_Wind <- rename(All_Wind,c("ws_All"="Speed","wd_All"="Direction"))
  Peaks_Wind <- CH4_Peaks[complete.cases(CH4_Peaks[ , c("Speed", "Direction")]), c("UTC", "Speed", "Direction")]
  Peaks_Wind <- rename(Peaks_Wind,c("ws_Peaks"="Speed","wd_Peaks"="Direction"))
  Wind_Compare <- merge( All_Wind, Peaks_Wind,
                         by.x = "UTC",
                         by.y = "UTC",
                         all.x = TRUE,
                         all.y = TRUE,
                         sort = TRUE)
  
  png("4_Data/OutputData/Plots/9_WindRose/Geomatikum/Comparison_Total_Vs_Peaks.png")
  pollutionRose(Wind_Compare, ws = "ws_All", wd = "wd_All", ws2 = "ws_Peaks", wd2 = "wd_Peaks", angle = 10)
  dev.off()

  ###############

  # Create and save a windrose Plot with the Total wind data from the Mast_50m
  png("4_Data/OutputData/Plots/9_WindRose/Mast_50m/WindRose_Total.png")
  windRose(TotalData, ws = "Speed50m", wd = "Direction50m", angle = 10)
  dev.off()
  
  # Create and save a Windrose Plot with the Averaged Mast_50m wind data at the methane Peaks
  png("4_Data/OutputData/Plots/9_WindRose/Mast_50m/WindRose_Peaks.png")
  windRose(CH4_Peaks, ws = "Speed50m", wd = "Direction50m", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot with the Total data, Wind Data from the Mast_50m
  png("4_Data/OutputData/Plots/9_WindRose/Mast_50m/PollutionRose_Total.png")
  pollutionRose(TotalData, ws = "Speed50m", wd = "Direction50m", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot Only from the Peaks, Wind Data from the Mast_50m
  png("4_Data/OutputData/Plots/9_WindRose/Mast_50m/PollutionRose_Peaks.png")
  pollutionRose(CH4_Peaks, ws = "Speed50m", wd = "Direction50m", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  
  # Create and save a comparison Windrose with the total Wind data vs. the Peak wind data (At the Mast_50m)
  All_Wind <- TotalData[complete.cases(TotalData[ , c("Speed50m", "Direction50m")]), c("UTC", "Speed50m", "Direction50m")]  
  All_Wind <- rename(All_Wind,c("ws_All"="Speed50m","wd_All"="Direction50m"))
  Peaks_Wind <- CH4_Peaks[complete.cases(CH4_Peaks[ , c("Speed50m", "Direction50m")]), c("UTC", "Speed50m", "Direction50m")]
  Peaks_Wind <- rename(Peaks_Wind,c("ws_Peaks"="Speed50m","wd_Peaks"="Direction50m"))
  Wind_Compare <- merge( All_Wind, Peaks_Wind,
                         by.x = "UTC",
                         by.y = "UTC",
                         all.x = TRUE,
                         all.y = TRUE,
                         sort = TRUE)
  
  png("4_Data/OutputData/Plots/9_WindRose/Mast_50m/Comparison_Total_Vs_Peaks.png")
  pollutionRose(Wind_Compare, ws = "ws_All", wd = "wd_All", ws2 = "ws_Peaks", wd2 = "wd_Peaks", angle = 10)
  dev.off()
  
  ###########
  
  # Create and save a windrose Plot with the Total wind data from the Mast_100m
  png("4_Data/OutputData/Plots/9_WindRose/Mast_110m/WindRose_Total.png")
  windRose(TotalData, ws = "Speed110m", wd = "Direction110m", angle = 10)
  dev.off()
  
  # Create and save a Windrose Plot with the Averaged Mast_100m wind data at the methane Peaks
  png("4_Data/OutputData/Plots/9_WindRose/Mast_110m/WindRose_Peaks.png")
  windRose(CH4_Peaks, ws = "Speed110m", wd = "Direction110m", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot with the Total data, Wind Data from the Mast_100m
  png("4_Data/OutputData/Plots/9_WindRose/Mast_110m/PollutionRose_Total.png")
  pollutionRose(TotalData, ws = "Speed110m", wd = "Direction110m", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot Only from the Peaks, Wind Data from the Mast_100m
  png("4_Data/OutputData/Plots/9_WindRose/Mast_110m/PollutionRose_Peaks.png")
  pollutionRose(CH4_Peaks, ws = "Speed110m", wd = "Direction110m", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  
  # Create and save a comparison Windrose with the total Wind data vs. the Peak wind data (At the Mast_100m)
  All_Wind <- TotalData[complete.cases(TotalData[ , c("Speed110m", "Direction110m")]), c("UTC", "Speed110m", "Direction110m")]  
  All_Wind <- rename(All_Wind,c("ws_All"="Speed110m","wd_All"="Direction110m"))
  Peaks_Wind <- CH4_Peaks[complete.cases(CH4_Peaks[ , c("Speed110m", "Direction110m")]), c("UTC", "Speed110m", "Direction110m")]
  Peaks_Wind <- rename(Peaks_Wind,c("ws_Peaks"="Speed110m","wd_Peaks"="Direction110m"))
  Wind_Compare <- merge( All_Wind, Peaks_Wind,
                         by.x = "UTC",
                         by.y = "UTC",
                         all.x = TRUE,
                         all.y = TRUE,
                         sort = TRUE)
  
   png("4_Data/OutputData/Plots/9_WindRose/Mast_110m/Comparison_Total_Vs_Peaks.png")
  pollutionRose(Wind_Compare, ws = "ws_All", wd = "wd_All", ws2 = "ws_Peaks", wd2 = "wd_Peaks", angle = 10)
  dev.off()
  
  ###########
  
  # Create and save a windrose Plot with the Total wind data from the Mast_100m
  png("4_Data/OutputData/Plots/9_WindRose/DWD/WindRose_Total.png")
  windRose(TotalData, ws = "Wind_Speed", wd = "Wind_Direction", angle = 10)
  dev.off()
  
  # Create and save a Windrose Plot with the Averaged Mast_100m wind data at the methane Peaks
  png("4_Data/OutputData/Plots/9_WindRose/DWD/WindRose_Peaks.png")
  windRose(CH4_Peaks, ws = "Wind_Speed", wd = "Wind_Direction", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot with the Total data, Wind Data from the Mast_100m
  png("4_Data/OutputData/Plots/9_WindRose/DWD/PollutionRose_Total.png")
  pollutionRose(TotalData, ws = "Wind_Speed", wd = "Wind_Direction", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  # Create and save a Pollutionrose Plot Only from the Peaks, Wind Data from the Mast_100m
  png("4_Data/OutputData/Plots/9_WindRose/DWD/PollutionRose_Peaks.png")
  pollutionRose(CH4_Peaks, ws = "Wind_Speed", wd = "Wind_Direction", pollutant = "X.CH4.",statistic = "prop.mean", angle = 10)
  dev.off()
  
  
  # Create and save a comparison Windrose with the total Wind data vs. the Peak wind data (At the Mast_100m)
  All_Wind <- TotalData[complete.cases(TotalData[ , c("Wind_Speed", "Wind_Direction")]), c("UTC", "Wind_Speed", "Wind_Direction")]  
  All_Wind <- rename(All_Wind,c("ws_All"="Wind_Speed","wd_All"="Wind_Direction"))
  Peaks_Wind <- CH4_Peaks[complete.cases(CH4_Peaks[ , c("Wind_Speed", "Wind_Direction")]), c("UTC", "Wind_Speed", "Wind_Direction")]
  Peaks_Wind <- rename(Peaks_Wind,c("ws_Peaks"="Wind_Speed","wd_Peaks"="Wind_Direction"))
  Wind_Compare <- merge( All_Wind, Peaks_Wind,
                         by.x = "UTC",
                         by.y = "UTC",
                         all.x = TRUE,
                         all.y = TRUE,
                         sort = TRUE)
  
  png("4_Data/OutputData/Plots/9_WindRose/DWD/Comparison_Total_Vs_Peaks.png")
  pollutionRose(Wind_Compare, ws = "ws_All", wd = "wd_All", ws2 = "ws_Peaks", wd2 = "wd_Peaks", angle = 10)
  dev.off()
  

}

#------------------------------------------------------------------------------------------------------------
  

# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Compare_Timeline <- function(TotalData = TotalData, n = 4) {
  # replace Error points with NA
  is.na(TotalData$Wind_Speed) <- TotalData$Wind_Speed == "-999"
  is.na(TotalData$Water_Level) <- TotalData$Water_Level == "-777"
  
  # Filter Data frame, selcts only datapoints where "UTC", "X.CH4." values exist in the dataframe
  TotalData_CH4_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  
  #Split Timeline into Panels
  TotalData_CH4_WL <- panel_function(TotalData_CH4_WL, n)
  m <- panel_No_function(n)
  
  # if (n == 0){
  #   # fixed panel
  #   TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC <= "2021-08-10 23:59:00"] <- 0
  #   TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC >= "2021-08-11 00:00:00" & TotalData_CH4_WL$UTC <= "2021-08-18 23:59:00"] <- 1
  #   TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC >= "2021-08-19 00:00:00" & TotalData_CH4_WL$UTC <= "2021-08-28 23:59:00"] <- 2
  #   TotalData_CH4_WL$panel[TotalData_CH4_WL$UTC >= "2021-08-29 00:00:00"] <- 3
  #   m <- 4
  # }
  # # for automatic panel
  # else{
  #   # for automatic panel
  #   TotalData_CH4_WL <- TotalData_CH4_WL %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData_CH4_WL))*n))
  #   m <- n
  # }
  
  # Plot CH4, Waterlevel Vs Time
  CH4_TimeLine <- ggplot(TotalData_CH4_WL) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y =expression("CH"[4]*" concentration [ppb]"),
         title = "Methane concentration & Elbe water level vs. time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
                     # limits = c(as.POSIXct('2021-08-01 00:00:00', 
                     #                       format = "%Y-%m-%d %H:%M:%S", 
                     #                       tz ="utc"), as.POSIXct('2021-08-18 00:00:00', 
                     #                                              format = "%Y-%m-%d %H:%M:%S", 
                     #                                              tz ="utc"))) +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = Water_Level*5),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./5,
                                           name="Water Level, mm"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  CH4_TimeLine
  
  #Export the plot to PNG file
  ggsave("1_CH4_WL.png", CH4_TimeLine, path = "4_Data/OutputData/Plots/1_CH4_vs_Waterlevel", width = 10, height = 5)
  

  # Filter Data frame for Wind
  TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Wind_Direction", "Wind_Speed")]),]

  #Split Timeline into Panels
  TotalData_Wind <- panel_function(TotalData_Wind, n)
  m <- panel_No_function(n)

  # if (n == 0){
  #   #for fixed panel
  #   TotalData_Wind$panel[TotalData_Wind$UTC <= "2021-08-10 23:59:00"] <- 0
  #   TotalData_Wind$panel[TotalData_Wind$UTC >= "2021-08-11 00:00:00" & TotalData_Wind$UTC <= "2021-08-18 23:59:00"] <- 1
  #   TotalData_Wind$panel[TotalData_Wind$UTC >= "2021-08-19 00:00:00" & TotalData_Wind$UTC <= "2021-08-28 23:59:00"] <- 2
  #   TotalData_Wind$panel[TotalData_Wind$UTC >= "2021-08-29 00:00:00"] <- 3
  #   n <- 4
  # }
  # else{
  # #for automatic panel
  #   TotalData_Wind <- TotalData_Wind %>% mutate(panel = as.integer(((row_number()-1)/nrow(TotalData_Wind))*n))
  #   m <- n
  # }

  # Plot Wind, Speed, Direction vs Time
  Wind_TimeLine <- ggplot(TotalData_Wind) +
    geom_line(aes(x = UTC,
                  y = Wind_Direction),
              col = "black") +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Speed & Wind Direction vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "purple",
                                            size=13),
          axis.text.y.right = element_text(color = "purple"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = Wind_Speed*70),
              col = "purple") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./70,
                                           name="Wind Speed, m/s"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  Wind_TimeLine

  #Export the plot ti PNG file
  ggsave("2_CH4_vs_Wind_D_S.png", Wind_TimeLine, path = "4_Data/OutputData/Plots/2_CH4_vs_Wind", width = 10, height = 5)



  # Only wind Direction
  Wind_Direction_TimeLine <- ggplot(TotalData_Wind) +
    geom_line(aes(x = UTC,
                  y = Wind_Direction),
              col = "black") +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Direction vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "purple",
                                            size=13),
          axis.text.y.right = element_text(color = "purple"),
          strip.text.x = element_blank()) +
    # geom_line(aes(x = UTC,
    #               y = Wind_Speed*70),
    #           col = "green") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./70,
                                           name="Wind Speed, m/s"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  Wind_Direction_TimeLine

  # Export Plot to PNG file
  ggsave("2.1_CH4_vs_Wind_D.png", Wind_Direction_TimeLine, path = "4_Data/OutputData/Plots/2_CH4_vs_Wind", width = 10, height = 5)



  # Plot Wind Speed
  Wind_Speed_TimeLine <- ggplot(TotalData_Wind) +
    # geom_line(aes(x = UTC,
    #               y = Wind_Direction),
    #           col = "black") +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Speed vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          axis.title.y.right = element_text(color = "purple",
                                            size=13),
          axis.text.y.right = element_text(color = "purple"),
          strip.text.x = element_blank()) +
    geom_line(aes(x = UTC,
                  y = Wind_Speed*70),
              col = "purple") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./70,
                                           name="Wind Speed, m/s"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  Wind_Speed_TimeLine

  # Export Plot to PNG file
  ggsave("2.2_CH4_vs_Wind_S.png", Wind_Speed_TimeLine, path = "4_Data/OutputData/Plots/2_CH4_vs_Wind", width = 10, height = 5)


  # Plot another Wind, Speed, Direction vs Time
  options(ggplot2.continuous.colour="viridis")
  Wind_TimeLine <- ggplot(TotalData_Wind, aes(x = UTC,
                                              y = Wind_Direction,
                                              colour = Wind_Speed)) +
    geom_line() +
    labs(x = "Fill Time [UTC]",
         y ="Wind Direction, °",
         title = "Wind Speed & Wind Direction vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "black",
                                      size=13),
          axis.text.y = element_text(color = "black"),
          strip.text.x = element_blank()) +
    facet_wrap(~panel, scales = 'free', nrow = n) +
    guides(color = guide_legend(title = "Wind Speed, m/s"))
  Wind_TimeLine

  #Export the plot ti PNG file
  ggsave("3_Wind_D_S.png", Wind_TimeLine, path = "4_Data/OutputData/Plots", width = 10, height = 5)
}


#------------------------------------------------------------------------------------------------------------



# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Compare_Timeline2 <- function(TotalData = TotalData, n =10 ) {
  # replace Error points with NA
  is.na(TotalData$Wind_Speed) <- TotalData$Wind_Speed == "-999"
  is.na(TotalData$Water_Level) <- TotalData$Water_Level == "-777"

  # Filter Data frame for Wind
  TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Wind_Direction", "Wind_Speed")]),]
  
  # #Split Timeline into Panels
  # TotalData_Wind <- panel_function(TotalData_Wind, n)
  # m <- panel_No_function(n)
  # 
  
  #Split Timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  

  
  # Split the TotalData Dataframe into seperate Datatframes, ther are used indifidualy to plot them in same Graph
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),c("UTC", "X.CH4.","panel")]
  TotalData_WL <- TotalData[complete.cases(TotalData[ , c("UTC", "Water_Level")]),c("UTC", "Water_Level","panel")]
  TotalData_Wind <- TotalData[complete.cases(TotalData[ , c("UTC", "Direction", "Speed")]),c("UTC", "Direction", "Speed","panel")]
  
  # With in the Loop the timeline is split into multiple Plots
  for(i in 0:(m-1)){
    # First plot only created to tse the Axsi
    p1 <- ggplot(TotalData_CH4[TotalData_CH4$panel == i,], aes(x = UTC,
                                                               y = X.CH4.)) +
      ylim(1600, 4300) +
      labs(y ="CH4 Concentration")+
      geom_line() +
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 0))
    p1
    
    # Second Plot only created to use the axis
    p2 <- ggplot(TotalData_WL[TotalData_WL$panel == i,], aes(x = UTC,
                                                             y = Water_Level)) +
      geom_line() +
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 0))
    p2
    
    # Plot inclues all Timelines
    p3 <- ggplot(data = TotalData_Wind[TotalData_Wind$panel == i,], aes(x = UTC, y = Direction)) +
      geom_line(aes(color = "Wind Dircection")) +
      ylim(0, 360) +
      labs(x = "UTC",
           y ="Wind Direction, °",
           title = "Wind Direction, Waterlevel, CH4 Concentration vs. Time") +
      geom_line(data = TotalData_WL[TotalData_WL$panel == i,], aes(x = UTC, y = (Water_Level/1.5-200), color = "Water Level")) +
      scale_x_datetime(date_breaks = "1 day",
                       date_labels = "%d-%b")+
      geom_line(TotalData_CH4[TotalData_CH4$panel == i,], mapping = aes(x = UTC, y = (X.CH4./7-250) , color = "CH4.")) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~(.*1.5+200),
                                             name="Waterlevel, mm"))+
      theme(axis.line = element_line(),
            plot.margin = margin(0, 0, 0, 20),
            axis.text.x=element_text(angle=60, hjust=1),
            axis.title.y = element_text(color = "black",
                                        size=13),
            axis.text.y = element_text(color = "black"),
            strip.text.x = element_blank(),
            legend.position = "bottom",
            legend.title=element_blank())
    p3
    
    # the Axis from the first to Plots is now includet in the third plot
    p4 <- wrap_elements(get_plot_component(p1, "ylab-l")) +
      wrap_elements(get_y_axis(p1)) +
      # wrap_elements(get_plot_component(p2, "ylab-l")) +
      # wrap_elements(get_y_axis(p2)) +
      p3 +
      plot_layout(widths = c(1, 1, 40))
    p4
    
    #Save the plot
    ggsave(paste0("5_CH4_WaterLevel_WindDirection_",i,".png"), p4, path = "4_Data/OutputData/Plots/5_CH4_vs_Waterlevel_vs_Wind_Direction", width = 10, height = 5)
  }
}



#------------------------------------------------------------------------------------------------------------

# Function to Create Basic Plot for CH4 vs Waterlevel
Compare_Timeline_Basic <- function(TotalData = TotalData) {
  
  # Select complete case data from totalData Dataframe
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , "X.CH4."]),]
  TotalData_CH4 <- TotalData_CH4[,c("UTC", "X.CH4.", "Water_Level")]
  WL_CH4_Data <- melt(TotalData_CH4, id.var="UTC")
  
  png(file="4_Data/OutputData/Plots/4_Basic_Plot_CH4_Wl.png",
      width=600, height=350)
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  plot(TotalData_CH4$UTC, TotalData_CH4$Water_Level,
       type = "l",
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Elbe Waterlevel, mm",
       xlim = c(StartTime, FinishTime))
  
  par(new = TRUE)
  plot(TotalData_CH4$UTC, TotalData_CH4$X.CH4.,
       main = "WaterLevel(WSV)/CH4 Concentation Vs. Time",
       type = "l",
       cex = 2,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(c(StartTime, FinishTime)))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
  
}

#------------------------------------------------------------------------------------------------------------

# Function to Plot a CH4 Timeline with A Peak detection
CH4_TimeLine <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime, n = 10, Panel_Plot = FALSE){
  
  # calling funktions to splite timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  #Select the Data from dataframe with CH4 Concentration
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "panel")]
  
  # Find the Methan Peaks
  CH4_Peaks <- CH4_Peak_Finder(TotalData, FALSE)
  CH4_Peaks$panel <- CH4Data[match(CH4_Peaks$UTC, CH4Data$UTC),"panel"]
  CH4_min <- min(CH4Data$X.CH4.)
  CH4_max <- max(CH4Data$X.CH4.)
  
  
  # Select Plot in Panels or in separate files
  if (Panel_Plot == FALSE) {
    
    # Loop throw individual panels
    for (i in (0:(m-1))){
      # Create the Timeline plot
      CH4_TimeLine <- ggplot(CH4Data[CH4Data$panel == i, ], aes(x = UTC, y = X.CH4.)) +
        geom_line() +
        labs(x = "Fill Time [UTC]",
             y = expression("CH"[4]*" concentration [ppb]"),
             title = "Methane concentration vs. Time") +
        scale_x_datetime(date_breaks = "2 day",
                         date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
        theme(axis.text.x=element_text(angle=60,
                                       hjust=1),
              strip.text.x = element_blank(),
              legend.position="none")+
        geom_rect(data=CH4_Peaks[CH4_Peaks$panel == i, ], inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
                                                                                 ymax=CH4_max), color="transparent", fill="orange", alpha=0.3)+ 
        geom_point(data=CH4_Peaks[CH4_Peaks$panel == i, ], aes(x = UTC, y = X.CH4., col = "red"))
      
      # Save the Plot
      ggsave(paste0("4_CH4_Timeline",i,".png"),
             CH4_TimeLine,
             path = "4_Data/OutputData/Plots/4_CH4_Timeline",
             width = 10,
             height = 5)
    }
  }
  
  # Select a plot with seperate panels
  else if (Panel_Plot == TRUE){
    
    # Create the Plot
    CH4_TimeLine <- ggplot(CH4Data, aes(x = UTC, y = X.CH4.)) +
      geom_line() +
      labs(x = "Fill Time [UTC]",
           y = expression("CH"[4]*" concentration [ppb]"),
           title = "Methane concentration vs. Time") +
      scale_x_datetime(date_breaks = "2 day",
                       date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
      theme(axis.text.x=element_text(angle=60,
                                     hjust=1),
            strip.text.x = element_blank(),
            legend.position="none")+
      geom_rect(data=CH4_Peaks, inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
                                                       ymax=CH4_max), color="transparent", fill="orange", alpha=0.3)+ #, group=group
      geom_point(data=CH4_Peaks, aes(x = UTC, y = X.CH4., col = "red"))
    facet_wrap(~panel,
               scales = 'free',
               nrow = m)
    
    # Save the plot
    ggsave(paste0("4_CH4_Timeline_Panels.png"),
           CH4_TimeLine,
           path = "4_Data/OutputData/Plots/4_CH4_Timeline",
           width = 10,
           height = 5)
  }
}

#------------------------------------------------------------------------------------------------------------


# Function to Plot a CH4 Timeline with A Peak detection
CH4_TimeLine_total <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime) { #, n = 10, Panel_Plot = FALSE)
  
  # # calling funktions to splite timeline into Panels
  # TotalData <- panel_function(TotalData, n)
  # m <- panel_No_function(n)
  
  #Select the Data from dataframe with CH4 Concentration
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.")]
  C13Data <- TotalData[complete.cases(TotalData[ , "d13C.VPDB"]),c("UTC", "d13C.VPDB")]
  DData <- TotalData[complete.cases(TotalData[ , "d2H.VPDB"]),c("UTC", "d2H.VPDB")]
  
# Create the Timeline plot
CH4_TimeLine <- ggplot(CH4Data[, ], aes(x = UTC, y = X.CH4.)) +
  geom_line() +
  labs(
    y = expression("CH"[4]*" con. [ppb]")) +
  scale_x_datetime(date_breaks = "15 day",
                   date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_blank(),
        legend.position="none")

CH4_13C_TimeLine <- ggplot(C13Data[, ], aes(x = UTC, y = d13C.VPDB)) +
  geom_line() +
  labs(
    y = expression("δ"^13*"C VPDB [‰]")) +
  scale_x_datetime(date_breaks = "15 day",
                   date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_blank(),
        legend.position="none")

CH4_2H_TimeLine <- ggplot(DData[, ], aes(x = UTC, y = d2H.VPDB)) +
  geom_line() +
  labs(x = "Fill Time [UTC]",
       y = expression("δD VSMOW [‰]")) +
  scale_x_datetime(date_breaks = "15 day",
                   date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
  theme(axis.text.x=element_text(angle=60,
                                 hjust=1),
        strip.text.x = element_blank(),
        legend.position="none")


Total_Timeline <- CH4_TimeLine + CH4_13C_TimeLine + CH4_2H_TimeLine +
  plot_layout(ncol = 1, guides = "collect")+
  plot_annotation(
    title = expression("CF-IRMS time series for methane concentration, δ"^13*"C and δD"))

Total_Timeline

# Save the Plot
ggsave(paste0("4_CH4_Total_Timeline.png"),
       Total_Timeline,
       path = "4_Data/OutputData/Plots/4_CH4_Timeline",
       width = 10,
       height = 5)
}


#------------------------------------------------------------------------------------------------------------

# Function to Plot a CH4 Timeline without Peaks
CH4_TimeLine_No_Peaks <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime, n = 10, Panel_Plot = FALSE){
  
  # calling funktions to splite timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  #Select the Data from dataframe with CH4 Concentration
  CH4Data <- TotalData[complete.cases(TotalData[ , "X.CH4."]),c("UTC", "X.CH4.", "panel")]
  
  # Find the Methan Peaks
  # CH4_Peaks <- CH4_Peak_Finder(TotalData, FALSE)
  # CH4_Peaks$panel <- CH4Data[match(CH4_Peaks$UTC, CH4Data$UTC),"panel"]
  # CH4_min <- min(CH4Data$X.CH4.)
  # CH4_max <- max(CH4Data$X.CH4.)
  
  
  # Select Plot in Panels or in separate files
  if (Panel_Plot == FALSE) {
    
    # Loop throw individual panels
    for (i in (0:(m-1))){
      # Create the Timeline plot
      CH4_TimeLine <- ggplot(CH4Data[CH4Data$panel == i, ], aes(x = UTC, y = X.CH4.)) +
        geom_line() +
        labs(x = "Fill Time [UTC]",
             y = expression("CH"[4]*" concentration [ppb]"),
             title = "Methane concentration vs. Time") +
        scale_x_datetime(date_breaks = "2 day",
                         date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
        theme(axis.text.x=element_text(angle=60,
                                       hjust=1),
              strip.text.x = element_blank(),
              legend.position="none")
        # geom_rect(data=CH4_Peaks[CH4_Peaks$panel == i, ], inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
        #                                                                          ymax=CH4_max), color="transparent", fill="orange", alpha=0.3)+ 
        # geom_point(data=CH4_Peaks[CH4_Peaks$panel == i, ], aes(x = UTC, y = X.CH4., col = "red"))
      
      # Save the Plot
      ggsave(paste0("4_CH4_Timeline_No_Peaks",i,".png"),
             CH4_TimeLine,
             path = "4_Data/OutputData/Plots/4_CH4_Timeline",
             width = 10,
             height = 5)
    }
  }
  
  # Select a plot with seperate panels
  else if (Panel_Plot == TRUE){
    
    # Create the Plot
    CH4_TimeLine <- ggplot(CH4Data, aes(x = UTC, y = X.CH4.)) +
      geom_line() +
      labs(x = "Fill Time [UTC]",
           y = expression("CH"[4]*" concentration [ppb]"),
           title = "Methane concentration vs. Time") +
      scale_x_datetime(date_breaks = "2 day",
                       date_labels = "%d-%b") + # , limit=c(as.POSIXct(StartTime),as.POSIXct(FinishTime))
      theme(axis.text.x=element_text(angle=60,
                                     hjust=1),
            strip.text.x = element_blank(),
            legend.position="none")
      # geom_rect(data=CH4_Peaks, inherit.aes=FALSE, aes(xmin=UTC_Beginning, xmax=UTC_Ending, ymin=CH4_min,
      #                                                  ymax=CH4_max), color="transparent", fill="orange", alpha=0.3)+ #, group=group
      # geom_point(data=CH4_Peaks, aes(x = UTC, y = X.CH4., col = "red"))
    facet_wrap(~panel,
               scales = 'free',
               nrow = m)
    
    # Save the plot
    ggsave(paste0("4_CH4_Timeline_Panels_No_Peaks.png"),
           CH4_TimeLine,
           path = "4_Data/OutputData/Plots/4_CH4_Timeline",
           width = 10,
           height = 5)
  }
}

#------------------------------------------------------------------------------------------------------------

# Function to create a Basic plot of Wind Direction & Speed (DWD)/CH4 Concentation Vs. Time
Basic_Wind_DWD_CH4 <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime){
  png(file="4_Data/OutputData/Plots/6_Basic_CH4_vs_Wind/6.1_Basic_Plot_CH4_Wind_DWD.png",
      width=1200,
      height=700)
  par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Wind_Direction,
       main = "Wind Direction & Speed (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Direction, °",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Wind_Speed,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Speed, m/s",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  par(mfrow=c(1,1))
  dev.off() 
}

#------------------------------------------------------------------------------------------------------------

# Function to plot Wind Direction, Waterleven and CH4 Concentration
Basic_Wind_D_WL_CH4 <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime){ 
  png(file="4_Data/OutputData/Plots/6_Basic_CH4_vs_Wind/6.2_Basic_Plot_CH4_Wind_DWD_Waterlevel.png",
      width=1200,
      height=700)
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Water_Level,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Elbe Waterlevel, mm",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
       type = "p",
       pch='.',
       lwd = 1.5,
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")

  par(new = TRUE)
  plot(TotalData$UTC, TotalData$Wind_Direction,
       type = "p",
       pch='.',
       cex = 2,
       col="Blue",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  axis(side=4,
       col.axis="blue",
       col="blue")
  mtext("CH4 Concentration & Wind Direction, °",
        col="red",
        side=4,
        line=3)
  dev.off() 
}

#------------------------------------------------------------------------------------------------------------

#Function to basic plot Wind Direction & Speed (MAST 110m)/CH4 Concentation Vs. Time
Basic_Wind_110m_CH4 <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime){

  png(file="4_Data/OutputData/Plots/6_Basic_CH4_vs_Wind/6.3_Basic_Plot_CH4_Wind_110m.png",
      width=1200,
      height=700)
  
  par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  # first plot
    plot(TotalData$UTC, TotalData$Direction110m,
         main = "Wind Direction & Speed (MAST 110m)/CH4 Concentation Vs. Time",
         type = "p",
         pch='.',
         cex = 2,
         xlab = "Date/Time UTC",
         ylab = "Wind Direction, °",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")),
         ylim = c(0, 360))
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$X.CH4.,
         type = "p",
         pch='.',
         col="red",
         axes = FALSE,
         bty = "n",
         xlab = "",
         ylab = "",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
    
    axis(side=4,
         col.axis="red",
         col="red")
    mtext("CH4 Concentration",
          col="red",
          side=4,
          line=3)
    
    par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
    # first plot
    plot(TotalData$UTC, TotalData$Speed110m,
         type = "p",
         pch='.',
         cex = 2,
         xlab = "Date/Time UTC",
         ylab = "Wind Speed, m/s",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")),
         ylim = c(0, 24))
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$X.CH4.,
         type = "p",
         pch='.',
         col="red",
         axes = FALSE,
         bty = "n",
         xlab = "",
         ylab = "",
         xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
    
    axis(side=4,
         col.axis="red",
         col="red")
    mtext("CH4 Concentration",
          col="red",
          side=4,
          line=3)
    par(mfrow=c(1,1))
    par(mfrow=c(1,1))
    dev.off() 
}

#------------------------------------------------------------------------------------------------------------

#Function to basic plot Wind Direction & Speed (Geomatikum)/CH4 Concentation Vs. Time
Basic_Wind_Geomatikum_CH4 <- function(TotalData = TotalData, StartTime = StartTime, FinishTime = FinishTime){
  
  png(file="4_Data/OutputData/Plots/6_Basic_CH4_vs_Wind/6.4_Basic_Plot_CH4_Wind_Geomatikum.png",
      width=1200,
      height=700)

  par(mar = c(1.5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Direction,
       main = "Wind Direction & Speed (Geomatikum)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Direction, °",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  par(mar = c(4, 4, 0, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$Speed,
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Wind Speed, m/s",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  
  par(mfrow=c(1,1))
  dev.off() 
}

#------------------------------------------------------------------------------------------------------------

#Function to basic plot Wind Speed, Waterlevel /CH4 Concentation Vs. Time
# Split into Time intervals as decleard in this function
Basic_CH4_WaterLevel_Wind_Speed <- function(TotalData, StartTime = StartTime, FinishTime = FinishTime){
  
  # Declare the Time intervals
    # IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
    IntervalDate <- seq(StartTime, FinishTime, by="10 days")
    
    i <- StartTime  # as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
    
    # Declare how the This in the block should look like and when the are set
    ticks <- seq(from=min(TotalData$UTC), by='1 days', length=50)
    
    # Loop to generate the separte panels
    k <- 1
    for(j in IntervalDate){
      png(file=paste0("4_Data/OutputData/Plots/7_Basic_CH4_vs_Wind_(10Days)/7.1_Basic_Plot_CH4_WL_Speed_",k,".png"),
          width=1200,
          height=700)
      par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(1,1))  # Leave space for z axis
      # first plot
      plot(TotalData$UTC, TotalData$Water_Level,
           type = "p",
           pch='.',
           cex = 2,
           # lwd = 1,
           bty = "n",
           axes = FALSE,
           # xlab = "Date/Time UTC",
           # ylab = "Elbe Waterlevel, mm",
           xlim = c(i,j))
      axis(side=2,
           col.axis="Black",
           col="Black",
           ylab = "Elbe Waterlevel, mm")
      
      par(new = TRUE)
      plot(TotalData$UTC, TotalData$X.CH4.,
           main = "WaterLevel(WSV)/CH4 Concentation/Wind Speed (DWD) Vs. Time",
           type = "p",
           pch='.',
           cex = 2,
           # lwd = 1.5,
           col="red",
           axes = FALSE,
           bty = "n",
           xlab = "",
           ylab = "",
           xlim = c(i,j))
      
      axis(side=4,
           col.axis="red",
           col="red")
      axis.POSIXct(side=1,
                   at = ticks,
                   #labels=format(TotalData$UTC,"%Y-%m-%d"),
                   las=2)
      
      
      par(new = TRUE)
      plot(TotalData$UTC, TotalData$Wind_Speed,
           type = "p",
           pch='.',
           cex = 2,
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
      dev.off() 
      i <- j
      k <- k+1
    }
}

#------------------------------------------------------------------------------------------------------------

#Function to basic plot Wind Direction & Waterlevel /CH4 Concentation Vs. Time
# Split into Time intervals as decleard in this function
Basic_CH4_WaterLevel_Wind_Direction <- function(TotalData, StartTime = StartTime, FinishTime = FinishTime){
  
  # Declare the Time intervals
  # IntervalDate <- c(as.POSIXct('2021-08-10 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-20 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-08-30 00:00:00', format = "%Y-%m-%d %H:%M:%S"), as.POSIXct('2021-09-17 23:59:00', format = "%Y-%m-%d %H:%M:%S"))
  IntervalDate <- seq(StartTime, FinishTime, by="10 days")
  
  i <- StartTime  # as.POSIXct('2021-08-01 00:00:00', format = "%Y-%m-%d %H:%M:%S")
  
  ticks <- seq(from=min(TotalData$UTC), by='1 days', length=50)
  k <- 1
  par(mar = c(5, 4, 4, 4) + 0.3, mfrow=c(2,1))  # Leave space for z axis
  for(j in IntervalDate){
    png(file=paste0("4_Data/OutputData/Plots/7_Basic_CH4_vs_Wind_(10Days)/7.2_Basic_Plot_CH4_WL_Direction_",k,".png"),
        width=1200,
        height=700)
    # first plot
    plot(TotalData$UTC, TotalData$Water_Level,
         type = "p",
         pch='.',
         cex = 1,
         # lwd = 1,
         bty = "n",
         axes = FALSE,
         # xlab = "Date/Time UTC",
         # ylab = "Elbe Waterlevel, mm",
         xlim = c(i,j))
    axis(side=2,
         col.axis="Black",
         col="Black",
         las = 1,
         ylab = "Elbe Waterlevel, mm")
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$X.CH4.,
         # main = "WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time",
         type = "p",
         pch='.',
         cex = 3,
         col="red",
         axes = FALSE,
         xlab = "",
         ylab = "",
         xlim = c(i,j))
    
    axis(side=4,
         col.axis="red",
         col="red",
         las = 1)
    
    axis.POSIXct(side=1,
                 at = ticks,
                 #labels=format(TotalData$UTC,"%Y-%m-%d"),
                 las=2)
    
    par(new = TRUE)
    plot(TotalData$UTC, TotalData$Wind_Direction,
         type = "p",
         pch='.',
         cex = 3,
         col="Blue",
         axes = FALSE,
         xlab = "",
         ylab = "",
         xlim = c(i,j))
    axis(side=4,
         col.axis="blue",
         col="blue",
         las = 1)
    mtext("CH4 Concentration & Wind Direction, °",
          col="red",
          side=4,
          line=3)
    mtext("WaterLevel(WSV)/CH4 Concentation/Wind Direction (DWD) Vs. Time", side = 3, line = - 2, outer = TRUE)
    
    dev.off() 
    i <- j
    k <- k+1
  }
}

#------------------------------------------------------------------------------------------------------------

#Function to basic plot rain vs CH4 Concentation Vs. Time
Basic_Rain_CH4 <- function(TotalData, StartTime, FinishTime){
  
  png(file="4_Data/OutputData/Plots/8_Basic_Plot_CH4_Rain.png",
      width=1200,
      height=700)
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$precipitation_height,
       main = "Rain quantity (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Rain Quantity, mm",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
}

#------------------------------------------------------------------------------------------------------------

#Function to basic plot Temperatue vs CH4 Concentation Vs. Time
Basic_Temp_CH4 <- function(TotalData, StartTime, FinishTime){
  
  png(file="4_Data/OutputData/Plots/8_Basic_Plot_CH4_Temp.png",
      width=1200,
      height=700)
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$temperature_air_mean_200,
       main = "Temperature (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Temperature, °C",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
}

#------------------------------------------------------------------------------------------------------------

#Function to basic plot Humidity vs CH4 Concentation Vs. Time
Basic_Humidity_CH4 <- function(TotalData, StartTime, FinishTime){
  
  png(file="4_Data/OutputData/Plots/8_Basic_Plot_CH4_Humidity.png",
      width=1200,
      height=700)
  par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
  # first plot
  plot(TotalData$UTC, TotalData$humidity,
       main = "Relative humidity (DWD)/CH4 Concentation Vs. Time",
       type = "p",
       pch='.',
       cex = 2,
       xlab = "Date/Time UTC",
       ylab = "Relative humidity, g/m3",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  par(new = TRUE)
  plot(TotalData$UTC, TotalData$X.CH4.,
       type = "p",
       pch='.',
       col="red",
       axes = FALSE,
       bty = "n",
       xlab = "",
       ylab = "",
       xlim = c(as.POSIXct(StartTime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(FinishTime, format = "%Y-%m-%d %H:%M:%S")))
  
  axis(side=4,
       col.axis="red",
       col="red")
  mtext("CH4 Concentration",
        col="red",
        side=4,
        line=3)
  dev.off() 
}


#------------------------------------------------------------------------------------------------------------


# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Humidity_CH4 <- function(TotalData = TotalData, n = 4) {
  # replace Error points with NA
  is.na(TotalData$humidity) <- TotalData$humidity == "-999"
  # is.na(TotalData$Water_Level) <- TotalData$Water_Level == "-777"

  # Filter Data frame, selcts only datapoints where "UTC", "X.CH4." values exist in the dataframe
  # TotalData_CH4_H <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  #Split Timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  TotalData_H <- TotalData[complete.cases(TotalData[ , c("UTC", "humidity")]),]
  
  
  
  # Plot CH4, Humidity Vs Time
  H_TimeLine <- ggplot(TotalData_CH4) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y ="CH4 Concentration [ppb]",
         title = "CH4 Concentration & Humidity vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(data = TotalData_H, aes(x = UTC,
                  y = humidity*40),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./40,
                                           name="Humidity, %"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  H_TimeLine
  
  #Export the plot to PNG file
  ggsave("8.1_CH4_H.png", H_TimeLine, path = "4_Data/OutputData/Plots/8_DWD_Data", width = 10, height = 5)
  
}

#------------------------------------------------------------------------------------------------------------

# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Temp_CH4 <- function(TotalData = TotalData, n = 4) {
  # replace Error points with NA
  is.na(TotalData$temperature_air_mean_200) <- TotalData$temperature_air_mean_200 == "-999"
  # is.na(TotalData$Water_Level) <- TotalData$Water_Level == "-777"
  
  # Filter Data frame, selcts only datapoints where "UTC", "X.CH4." values exist in the dataframe
  # TotalData_CH4_H <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  #Split Timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  TotalData_T <- TotalData[complete.cases(TotalData[ , c("UTC", "temperature_air_mean_200")]),]
  
  
  
  # Plot CH4, Humidity Vs Time
  H_TimeLine <- ggplot(TotalData_CH4) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y ="CH4 Concentration [ppb]",
         title = "CH4 Concentration & Temperature vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(data = TotalData_T, aes(x = UTC,
                                      y = temperature_air_mean_200*120),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./120,
                                           name="Temperature, °C"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  H_TimeLine
  
  #Export the plot to PNG file
  ggsave("8.2_CH4_T.png", H_TimeLine, path = "4_Data/OutputData/Plots/8_DWD_Data", width = 10, height = 5)
  
}

#------------------------------------------------------------------------------------------------------------

# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Rain_CH4 <- function(TotalData = TotalData, n = 4) {
  # replace Error points with NA
  is.na(TotalData$precipitation_height) <- TotalData$precipitation_height == "-999"
  # is.na(TotalData$Water_Level) <- TotalData$Water_Level == "-777"
  
  # Filter Data frame, selcts only datapoints where "UTC", "X.CH4." values exist in the dataframe
  # TotalData_CH4_H <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  #Split Timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  TotalData_R <- TotalData[complete.cases(TotalData[ , c("UTC", "precipitation_height")]),]
  
  
  
  # Plot CH4, Humidity Vs Time
  H_TimeLine <- ggplot(TotalData_CH4) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y ="CH4 Concentration [ppb]",
         title = "CH4 Concentration & Precipitation height vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(data = TotalData_R, aes(x = UTC,
                                      y = precipitation_height*800),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./800,
                                           name="Precipitation Height, mm"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  H_TimeLine
  
  #Export the plot to PNG file
  ggsave("8.3_CH4_R.png", H_TimeLine, path = "4_Data/OutputData/Plots/8_DWD_Data", width = 10, height = 5)
}

#------------------------------------------------------------------------------------------------------------

# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Radiation_CH4 <- function(TotalData = TotalData, n = 4) {
  # replace Error points with NA
  is.na(TotalData$radiation_global) <- TotalData$radiation_global == "-999"

  
  # Filter Data frame, selcts only datapoints where "UTC", "X.CH4." values exist in the dataframe
  # TotalData_CH4_H <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  #Split Timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  TotalData_Rad <- TotalData[complete.cases(TotalData[ , c("UTC", "radiation_global")]),]
  
  
  
  # Plot CH4, Humidity Vs Time
  H_TimeLine <- ggplot(TotalData_CH4) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y ="CH4 Concentration [ppb]",
         title = "CH4 Concentration & Radiation vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(data = TotalData_Rad, aes(x = UTC,
                                      y = radiation_global*100),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./100,
                                           name="Radiation"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  H_TimeLine
  
  #Export the plot to PNG file
  ggsave("8.4_CH4_Rad.png", H_TimeLine, path = "4_Data/OutputData/Plots/8_DWD_Data", width = 10, height = 5)
}


#------------------------------------------------------------------------------------------------------------

# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Pressure_CH4 <- function(TotalData = TotalData, n = 4) {
  # replace Error points with NA
  is.na(TotalData$pressure_air_site) <- TotalData$pressure_air_site == "-999"
  
  
  # Filter Data frame, selcts only datapoints where "UTC", "X.CH4." values exist in the dataframe
  # TotalData_CH4_H <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  #Split Timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  TotalData_Pres <- TotalData[complete.cases(TotalData[ , c("UTC", "pressure_air_site")]),]
  
  
  
  # Plot CH4, Humidity Vs Time
  H_TimeLine <- ggplot(TotalData_CH4) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y ="CH4 Concentration [ppb]",
         title = "CH4 Concentration & Pressure vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(data = TotalData_Pres, aes(x = UTC,
                                        y = pressure_air_site*2.5),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./2.5,
                                           name="Pressure, HPa"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  H_TimeLine
  
  #Export the plot to PNG file
  ggsave("8.5_CH4_Pres.png", H_TimeLine, path = "4_Data/OutputData/Plots/8_DWD_Data", width = 10, height = 5)
}

#------------------------------------------------------------------------------------------------------------

# This Function Creates a Plot of the Total CH4 Timeline,
# It can crate a multi panel plot or mulipe plots
# The CH4 Peaks are found and highlighted in the Plots
Dew_CH4 <- function(TotalData = TotalData, n = 4) {
  # replace Error points with NA
  is.na(TotalData$temperature_dew_point_mean_200) <- TotalData$temperature_dew_point_mean_200 == "-999"
  
  
  # Filter Data frame, selcts only datapoints where "UTC", "X.CH4." values exist in the dataframe
  # TotalData_CH4_H <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  #Split Timeline into Panels
  TotalData <- panel_function(TotalData, n)
  m <- panel_No_function(n)
  
  TotalData_CH4 <- TotalData[complete.cases(TotalData[ , c("UTC", "X.CH4.")]),]
  TotalData_Pres <- TotalData[complete.cases(TotalData[ , c("UTC", "temperature_dew_point_mean_200")]),]
  
  
  
  # Plot CH4, Humidity Vs Time
  H_TimeLine <- ggplot(TotalData_CH4) +
    geom_line(aes(x = UTC,
                  y = X.CH4.),
              col = "red") +
    labs(x = "Fill Time [UTC]",
         y ="CH4 Concentration [ppb]",
         title = "CH4 Concentration & Dew point Temp. vs. Time") +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%d-%b") +
    theme(axis.text.x=element_text(angle=60, hjust=1),
          axis.title.y = element_text(color = "red",
                                      size=13),
          axis.text.y = element_text(color = "red"),
          axis.title.y.right = element_text(color = "blue",
                                            size=13),
          axis.text.y.right = element_text(color = "blue"),
          strip.text.x = element_blank()) +
    geom_line(data = TotalData_Pres, aes(x = UTC,
                                         y = temperature_dew_point_mean_200*200),
              col = "blue") +
    scale_y_continuous(sec.axis = sec_axis(trans = ~./200,
                                           name="Dew point Temp., °C"))+
    facet_wrap(~panel, scales = 'free', nrow = m)
  H_TimeLine
  
  #Export the plot to PNG file
  ggsave("8.6_CH4_Dew.png", H_TimeLine, path = "4_Data/OutputData/Plots/8_DWD_Data", width = 10, height = 5)
}
