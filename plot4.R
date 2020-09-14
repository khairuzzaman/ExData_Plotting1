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
    
    png(file = "plot4.png")
    
    par(mfcol = c(2,2), mar=c(4, 4, 2, 1))
    
    globalActivePower(activePower)
    subMetering(meter1, meter2, meter3)
    voltage(voltage_data)
    globalReactivePower(reactivePower)
    
    dev.off()
}

globalActivePower <- function(activePower){
    plot(activePower, type = "l", xaxt='n',
         ylab = "Global Active Power (kilowatts)",
         xlab = "")
    
    axis(1,  at=c(0, 1500, 2900),
         labels=c('Thu','Fri','Sat'))
}

subMetering <- function(meter1, meter2, meter3){
    plot(meter1, type = "l", xaxt='n', col='black',
         ylab = "Energy sub metering",
         xlab = "")
    
    points(meter2, type = 'l', col='red')
    points(meter3, type = 'l', col='blue')
    
    axis(1,  at=c(0, 1500, 2900),
         labels=c('Thu','Fri','Sat'))
    
    legend("topright", col=c('black', 'red', 'blue'), lty = 1, 
           legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
}

voltage <- function(voltage_data){
    plot(voltage_data, type = "l", xaxt='n',
         ylab = "Voltage",
         xlab = "datetime")
    
    axis(1,  at=c(0, 1500, 2900),
         labels=c('Thu','Fri','Sat'))
}

globalReactivePower <- function(reactivePower){
    plot(reactivePower, type = "l", xaxt='n',
         ylab = "Global_reactive_power",
         xlab = "datetime")
    
    axis(1,  at=c(0, 1500, 2900),
         labels=c('Thu','Fri','Sat'))
}