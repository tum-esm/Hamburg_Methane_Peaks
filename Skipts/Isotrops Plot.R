

#Read Concentration Data of the CSV Files
CH4_2H <-read.csv2("CH4 2H 20210816.csv",TRUE, ",")
CH4_13C <-read.csv2("CH4 13C 20210816.csv",TRUE, ",")
CH4_concentrations <-read.csv2("CH4 concentrations 20210816.csv",TRUE, ",")
#W_Level_Raw <- read_delim("down (1).csv", ";", escape_double = FALSE, trim_ws = TRUE)

# #Convert the date into a readamble formart
    CH4_con_w_d <- CH4_concentrations
    CH4_con_w_d$fill.time.utc <- as.POSIXlt(CH4_con_w_d$fill.time.utc,
                                         format = "%d/%m/%Y %H:%M", tz = "utc")
    CH4_2H_w_d <- CH4_2H
    CH4_2H_w_d$fill.time.utc <- as.POSIXlt(CH4_2H_w_d$fill.time.utc,
                                           format = "%d/%m/%Y %H:%M", tz = "utc")
    CH4_13C_w_d <- CH4_13C
    CH4_13C_w_d$fill.time.utc <- as.POSIXlt(CH4_13C_w_d$fill.time.utc,
                                           format = "%d/%m/%Y %H:%M", tz = "utc")

# #Convert Time in Level file
#     W_Date <- as.POSIXlt(colnames(W_Level_Date[1]),  format="%d.%m.%y")
#     # W_Time <- strptime(W_Level_Raw[,1], "%H:%M:%S")
#     W_Level_Date <- W_Level_Raw
#     W_Level_Date$Mez <- as.POSIXlt(paste(W_Date, W_Level_Date$`26.07.2021`), format="%Y-%m-%d %H:%M")
# 

#Plot the data
plot(CH4_con_w_d, type="b", pch = 20, cex = 0.5)
plot(CH4_2H_w_d$fill.time.utc, CH4_2H_w_d$X.CH4..2H, type="b", pch = 20, cex = 0.5)
plot(CH4_13C_w_d)
# plot(W_Level_Date$Mez, W_Level_Date$`Hamburg Port Authority`, type="b", pch = 20, cex = 0.5)

