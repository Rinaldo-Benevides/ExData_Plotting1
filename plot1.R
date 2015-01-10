# Exploratory Data Analysis
#
# Course Projects
#
# Plot 1 - Global Active Power (Frequency X Kilowatts)
#
# Operating Instructions
# 1. Copy the files plot1.R and household_power_consumption.txt to your work directory
# 2. Load program: source("plot1.R")
# 3. Execute function: show()
#
# Package dependency: data.table

show <- function() {
    
    print(paste("Necessary memory (MB): ", necessaryMemoryInMegabytes()))
    
    data <- readWorkData()
      
    # Configuration 
    par(mfrow = c(1,1), mar = c(5, 5, 3, 2))
    
    # Plot 1: february, Day 1 and 2
    hist(data$Global_active_power
         , main="Global Active Power" 
         , xlab="Global Active Power (kilowatts)"
         , ylab="Frequency", col="Red")
    
    
    # Saving to PNG file
    dev.copy(png, file="plot1.png", height=480, width=480)
    dev.off()
}

readWorkData <- function() {
    
    #library("data.table")
    require("data.table")
    
    data <- fread("household_power_consumption.txt"
          , header = TRUE, sep = ";", na.strings = "?", stringsAsFactors = FALSE
          , showProgress = TRUE, data.table = TRUE, verbose = FALSE
          , select = c("Date", "Time", "Global_active_power")
          , colClasses = c("character","character","character","character","character"
                          ,"character","character","character","character")
    )
    
    # Measures: february days 1 and 2
    data <- data[data$Date == "1/2/2007" | data$Date == "2/2/2007", ]
    
    
    # Convert Global Active Power to number
    data$Global_active_power <- as.numeric(data$Global_active_power)
    
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