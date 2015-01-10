# Exploratory Data Analysis
#
# Course Projects
#
# Plot 4
#
# Operating Instructions
# 1. Copy the files plot4.R and household_power_consumption.txt to work directory
# 2. Load program: source("plot4.R")
# 3. Execute function: show()
#
# Package dependency: data.table

show <- function() {
    
    print(paste("Necessary memory (MB): ", necessaryMemoryInMegabytes()))
    
    data <- readWorkData()    
    
    # Configuration 
    par(mfrow = c(2,2), mar = c(5, 5, 3, 2), mex = 0.75)

    # Plot 4: 
    with(data, {
        plot(Global_active_power ~ DateTime
             , type="l", xlab="",         ylab="Global Active Power")
        plot(Voltage ~ DateTime
             , type="l", xlab="datetime", ylab="Voltage")
        
        plot(Sub_metering_1 ~ DateTime
             , type="l", xlab=""        , ylab="Energy sub metering")
        lines(Sub_metering_2 ~ DateTime, col='Red')
        lines(Sub_metering_3 ~ DateTime, col='Blue')
        
        legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
               , col=c("black", "red", "blue"), bty="n", lty=1, lwd=2, xjust = 0, cex=0.75
        )
        
        plot(Global_reactive_power ~ DateTime, type="l", xlab="datetime")
    })
    
    
    # Saving to PNG file
    dev.copy(png, file="plot4.png", height=480, width=480)
    dev.off()
}

readWorkData <- function() {
    
    #library("data.table")
    require("data.table")
    
    data <- fread("household_power_consumption.txt"
          , header = TRUE, sep = ";", na.strings = "?", stringsAsFactors = FALSE
          , showProgress = TRUE, data.table = TRUE, verbose = FALSE
          , colClasses = c("character","character","character","character","character"
                          ,"character","character","character","character")
    )
    
    
    # Measures: february days 1 and 2
    data <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007", ]
    
    
    # Convert to original types
    dataTable <- data.table(
        DateTime = as.POSIXct(paste(as.Date(data$Date, format="%d/%m/%Y"), data$Time)),
        Global_active_power = as.numeric(data$Global_active_power),
        Global_reactive_power = as.numeric(data$Global_reactive_power),
        Voltage = as.numeric(data$Voltage),
        Sub_metering_1 = as.numeric(data$Sub_metering_1),
        Sub_metering_2 = as.numeric(data$Sub_metering_2),
        Sub_metering_3 = as.numeric(data$Sub_metering_3)
    )
    
    
    dataTable
}


necessaryMemoryInMegabytes <- function() {
    
    rows <- 2075259
    columns <- 9
    bytes <- 8
    megabytes <- bytes / 2^20

    # rows * columns * 8 bytes / 2^20
    rows * columns * megabytes    
}