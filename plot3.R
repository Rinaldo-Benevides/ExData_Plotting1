# Exploratory Data Analysis
#
# Course Projects
#
# Plot 3
#
# Operating Instructions
# 1. Copy the files plot3.R and household_power_consumption.txt to your work directory
# 2. Load program: source("plot3.R")
# 3. Execute function: show()
#
# Package dependency: data.table

show <- function() {
    
    print(paste("Necessary memory (MB): ", necessaryMemoryInMegabytes()))
    
    data <- readWorkData()
        
    # Configuration 
    par(mfrow = c(1,1), mar = c(5, 5, 3, 2))    
    
    # Plot 3: february, Day 1 and 2
    with(data, {
        plot (Sub1 ~ DateTime, type="l", xlab="", ylab="Energy sub metering")
        lines(Sub2 ~ DateTime, col='Red')
        lines(Sub3 ~ DateTime, col='Blue')
    })
    
    legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
           , col=c("black", "red", "blue"), lty=1, lwd=2, xjust = 0, cex=0.75
    )
        
    # Saving to PNG file
    dev.copy(png, file="plot3.png", height=480, width=480)
    dev.off()
}


readWorkData <- function() {
    
    #library("data.table")
    require("data.table")
        
    data <- fread("household_power_consumption.txt"
          , header = TRUE, sep = ";", na.strings = "?", stringsAsFactors = FALSE
          , showProgress = TRUE, data.table = TRUE, verbose = FALSE
          , select = c("Date", "Time", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
          , colClasses = c("character","character","character","character","character"
                          ,"character","character","character","character")
    )
    
    # Measures: february days 1 and 2
    data <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007", ]
    
    
    # Creating columns: Date Time and Convert Sub metering 1, 2 and 3 to number
    data[, ':=' (
        Sub1 = as.numeric(data$Sub_metering_1),
        Sub2 = as.numeric(data$Sub_metering_2),
        Sub3 = as.numeric(data$Sub_metering_3),
        DateTime = as.POSIXct(paste(as.Date(data$Date, format="%d/%m/%Y"), data$Time))
    )]
    
    data
}


necessaryMemoryInMegabytes <- function() {
    
    rows <- 2075259
    columns <- 9
    bytes <- 8
    megabytes <- bytes / 2^20

    # rows * columns * 8 bytes / 2^20
    rows * columns * megabytes    
}