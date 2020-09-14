# This function will read house hold power consumption data.
# Filtered the data for february 1 & 2
# TODO: need to refactor the function so that only necessary data can read

readData <- function(){
    data <- read.table("household_power_consumption.txt", 
                       header = TRUE, sep = ";", dec = ".")
    filtered_data <- subset(data, 
                            data$Date == "1/2/2007" | data$Date == "2/2/2007")
}

drawPlot <- function(){
    data <- readData()
    activePower <- as.numeric(data$Global_active_power)
    meter1 <- as.numeric(data$Sub_metering_1)
    meter2 <- as.numeric(data$Sub_metering_2)
    meter3 <- as.numeric(data$Sub_metering_3)
    voltage_data <- as.numeric(data$Voltage)
    reactivePower <- as.numeric(data$Global_reactive_power)
    
    dt <- createDateTimeVector(data)
    
    png(file = "plot4.png")
    
    par(mfcol = c(2,2), mar=c(4, 4, 2, 1))
    
    globalActivePower(dt, activePower)
    subMetering(dt, meter1, meter2, meter3)
    voltage(dt, voltage_data)
    globalReactivePower(dt, reactivePower)
    
    dev.off()
}

globalActivePower <- function(dt, activePower){
    plot(dt, activePower, type = "l", xaxt='n',
         ylab = "Global Active Power (kilowatts)",
         xlab = "")
    
    at_vec <- rangeVector(dt)
    axis(1,  at=at_vec,
         labels=c('Thu','Fri','Sat'))
}

subMetering <- function(dt, meter1, meter2, meter3){
    plot(dt, meter1, type = "l", xaxt='n', col='black',
         ylab = "Energy sub metering",
         xlab = "")
    
    points(dt, meter2, type = 'l', col='red')
    points(dt, meter3, type = 'l', col='blue')
    
    at_vec <- rangeVector(dt)
    axis(1,  at=at_vec,
         labels=c('Thu','Fri','Sat'))
    
    legend("topright", col=c('black', 'red', 'blue'), lty = 1, 
           legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
}

voltage <- function(dt, voltage_data){
    plot(dt, voltage_data, type = "l", xaxt='n',
         ylab = "Voltage",
         xlab = "datetime")
    
    at_vec <- rangeVector(dt)
    axis(1,  at=at_vec,
         labels=c('Thu','Fri','Sat'))
}

globalReactivePower <- function(dt, reactivePower){
    plot(dt, reactivePower, type = "l", xaxt='n',
         ylab = "Global_reactive_power",
         xlab = "datetime")
    
    at_vec <- rangeVector(dt)
    axis(1,  at=at_vec,
         labels=c('Thu','Fri','Sat'))
}

createDateTimeVector <- function(data){
    date <- as.Date(data$Date, format = "%d/%m/%Y")
    time <- as.character(data$Time)
    date_time <- as.numeric(as.POSIXct(paste(date, time), 
                                       format = "%Y-%m-%d %H:%M:%S"))
}

rangeVector <- function(data){
    data <- c(min(data), median(data), max(data))
}