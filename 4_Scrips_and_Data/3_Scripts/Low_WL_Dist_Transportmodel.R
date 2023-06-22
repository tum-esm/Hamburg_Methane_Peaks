# Script to calculate the the distanze between emitter and measurment location using the  transport model
# Author: Juan Bettinelli
# Last edit. 22.05.2023

library(geosphere)
library(pracma)
library(mapview)
library(MASS)
library(raster)

setwd("/Users/juanbettinelli/Documents/Uni/MasterThesis/4_Scrips_and_Data")

source("3_Scripts/Functions.R")

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

TotalData <- import("4_Data/OutputData/CombineMeteorologicalData.csv")
TotalData$UTC <- as.POSIXct(as.character(TotalData$UTC),
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "UTC")

TotalData$X.CH4. <- as.numeric(TotalData$X.CH4.)

TotalData <- filter(TotalData, TotalData$UTC > StartTime & TotalData$UTC < FinishTime, .preserve = FALSE)

TotalData$UTC <- as.POSIXct(TotalData$UTC,
                            format = "%d-%m-%Y %H:%M:%S",
                            tz = "utc")

TotalData$Direction[TotalData$Direction > 361] <- NA
TotalData$Speed[TotalData$Speed > 99] <- NA


######## Finding the Peaks, The Average Meteorological Data during Peak, Saving csv File #########
# CH4_Peak_Finder(TotalData, TRUE)


No_of_Releast_Particals = 1
sd_Speed = 0
sd_Direction = 0
Wind_Provider = 1

# Particles_Tracks_Changing_Wind <- function(TotalData, No_of_Releast_Particals = 10, sd_Speed = 0.5, sd_Direction = 30, Wind_Provider = 1){
  
  # Select the Wind data provided by Uni. Hamburg or DWD
  if (Wind_Provider == 1){
    # Geomatikum
    W_Speed <- "Speed"
    W_Direction <- "Direction"
  }
  # else if (Wind_Provider == 2){
  #   # Mast 50m
  #   W_Speed <- "Speed50m"
  #   W_Direction <- "Direction50m"
  # }
  # else if (Wind_Provider == 3){
  #   # Mast 110m
  #   W_Speed <- "Speed110m"
  #   W_Direction <- "Direction110m"
  # }
  # else if (Wind_Provider == 4){
  #   # Mast 110m
  #   W_Speed <- "Wind_Speed"
  #   W_Direction <- "Wind_Direction"
  # }
  # 
  # Get create a data frame with the CH4 values
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
  
  
  # format the Dataframe time
  names(CH4_Peaks) <- c("X.CH4.", "UTC", "UTC_Beginning", "UTC_Ending")
  CH4_Peaks$UTC_Beginning <- CH4Data[CH4_Peaks$UTC_Beginning,"UTC"]
  CH4_Peaks$UTC_Ending <- CH4Data[CH4_Peaks$UTC_Ending,"UTC"]
  CH4_Peaks$UTC <- CH4Data[CH4_Peaks$UTC,"UTC"]
  
  CH4_Peaks$Dist <- NA
  
  # Remove the "Peaks" at where no measurements were taken (12h)
  CH4_Peaks <- subset(CH4_Peaks, (UTC_Ending - UTC_Beginning) < 12*60 )
  
  # Create a waterlevel data frame
  WLData <- TotalData[complete.cases(TotalData[ , "Water_Level"]),c("UTC", "Water_Level")]
  
  # Geomatikum Location
  Geomatikum <- cbind(9.973287, 53.568073) # (Lon, Lat)
  
  # Create a dataframe that will include all Points
  Total_Points <- data.frame()
  # Loop throgh all Peaks
  for(j in 1:nrow(CH4_Peaks)){
    
    # loop for Number of released Particals
    for(l in 1:No_of_Releast_Particals){
      # Create the Startpoint (at Geomatikum) data frame
      Single_Point <- data.frame(UTC = CH4_Peaks[j, "UTC"], lon = Geomatikum[1], lat = Geomatikum[2], Peak_No = j, it_No = l)
      
      # Find the meak max time
      Peak_Time <- CH4_Peaks[j, "UTC"]
      
      # Get the Waterlevel for the 12h before the Peak
      WL_at_Peak <- WLData[WLData$UTC >= (Peak_Time - (12*60*60)) & WLData$UTC <= Peak_Time,]
      
      # Find the minimum Water level
      MinWL <- WL_at_Peak[which.min(WL_at_Peak$Water_Level),]
      
      # Get all the Wind data between the Max CH4 Peak and the Waterlevel Lowpoint before the Peak plus 0.5 hour (One hour earlier)
      All_Wind <- TotalData[TotalData$UTC >= (MinWL$UTC) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]  
      
      # Check if there is less then 4 wind data enterys (peak at min for example), if that's the case take 1 hour before the peak
      if (nrow(All_Wind) < 2) {
        All_Wind <- TotalData[TotalData$UTC >= (CH4_Peaks[j,"UTC"]-60*60) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]
      }

      # Selects only enteris with Wind an Speed Direction, takes only wind data and time
      All_Wind <- All_Wind[complete.cases(All_Wind[ , c(W_Speed, W_Direction)]), c("UTC", W_Speed, W_Direction)]

      # checks if data frame is empty
      if (nrow(All_Wind) == 0) {
        All_Wind <- TotalData[TotalData$UTC >= (CH4_Peaks[j,"UTC"]-1*60*60) & TotalData$UTC <= CH4_Peaks[j,"UTC"], ]}

      
      if (nrow(All_Wind) > 2) {
      # Loop to find the the wind direction and speed for every avaleble entry in the dataframe
      for(i in 2:nrow(All_Wind)){ 
        # Find the time difference to previus point
        Time_dif <- as.numeric(difftime(strptime(All_Wind[i, "UTC"], "%Y-%m-%d %H:%M:%S", tz = "UTC"), strptime(All_Wind[i-1, "UTC"], "%Y-%m-%d %H:%M:%S", tz = "UTC")))
        # find destination point of particle 
        Single_Point[i,2:3]  <- destPoint(Single_Point[(i-1),2:3],(All_Wind[i,W_Direction]+ rnorm(1, mean = 0, sd = sd_Direction)),(All_Wind[i,W_Speed]*60*Time_dif + rnorm(1, mean = 0, sd = sd_Speed))) ################## Check Wind direktion
        # Enter the Peak number in data frame
        Single_Point[i,4] <- j
        # Enter the time in data frame
        Single_Point[i,1] <- All_Wind[i, "UTC"]
        # enter iteration Number in data frame
        Single_Point[i,5] <- l
      }
      # Attach the data frame to Final Data frame
      Total_Points <- rbind(Total_Points,Single_Point)
      
      Single_Point_no_NA <- Single_Point[complete.cases(Single_Point[ , c("lon", "lat")]),]
      test <-  tail(Single_Point_no_NA, n=1)

      dist <- distm(c(test$lon, test$lat), Geomatikum, fun = distHaversine)
      dist <- dist/1000
      }
    }
    CH4_Peaks[j, "Dist"] <- dist
  }
  
  # Filter empty rows out of Data frame
  Total_Points <- Total_Points[complete.cases(Total_Points[ , c("lon", "lat")]),]
  
  
  library(ggplot2)
  # Basic scatter plot
  WL_Peak_Plot <- ggplot(data =CH4_Peaks, aes(x = Dist, y = X.CH4.)) +
    geom_point() +
    geom_smooth() +
    xlab(expression("Distance between CH"[4]*" Emitter and measurement location [km]"))+
    ylab(expression("Peak CH"[4]*" concentration [ppb]"))+
    ggtitle(expression("CH"[4]*" concentration vs distance between emitter and measurement location"))

  ggsave("14_Low_WL_to_Peak_dist_new.png", WL_Peak_Plot, path = "4_Data/OutputData/Plots/14_Low_WL_to_Peak", width = 10, height = 5)
  
  
  WL_Peak_Plot
  
# Dist_test <- CH4_Peaks$Dist[CH4_Peaks$Dist >= 1 & CH4_Peaks$Dist <=12]
# sd(Dist_test)

  # # Plot the Particals as Individual points on interactive map
  # mv_points <- mapview(Total_Points, xcol = "lon", ycol = "lat", zcol = "Peak_No",cex = 0.5, alpha = 0.5, crs = 4326, map.types = "Stamen.Toner")
  # # Save the Map as PNG
  # mapshot(mv_points, file = "4_Data/OutputData/Plots/10_Maps/10_Emission_Points_with_Changing_Measured_Wind.png")
  
  
  # # concert the data frame into a sf 'simple form' location object
  # Total_Peaks_sf <- st_as_sf(Total_Points, coords = c("lon", "lat"),  crs = 4326)
  # # Plot the Particals from sf object as Individual points on interactive map
  # mapview(Total_Peaks_sf,  zcol = "Peak_No",cex = 0.5, alpha = 0.5, map.types = "Stamen.Toner") 
  
  
  # # Calculate density distribution of points
  # density_est <- kde2d(Total_Points$lon, Total_Points$lat, n = 300)
  # 
  # # Convert density into raster
  # density_raster <- raster(density_est)
  # 
  # # Select the color palette of the raster plot
  # colorPalette <- colorRampPalette(c("lightskyblue", "red", "white"))(255) # lightskyblue
  # # Replaces very small values with NA, to make the rasta transperant in this section
  # values(density_raster)[values(density_raster) < 0.1] = NA
  # # Plot the raster on a map
  # 
  # mv_Raster <- mapview(density_raster, 
  #                      na.color = "transparent", 
  #                      alpha = 0.7,  
  #                      col.regions = colorPalette, 
  #                      trans = "log10",
  #                      legend = TRUE) 
  # # Save the Map
  # mapshot(mv_Raster, file = "4_Data/OutputData/Plots/10_Maps/10_Emission_Distribution_with_Changing_Measured_Wind.png")
# }
