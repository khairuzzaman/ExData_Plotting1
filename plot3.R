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
    meter1 <- as.numeric(data$Sub_metering_1)
    meter2 <- as.numeric(data$Sub_metering_2)
    meter3 <- as.numeric(data$Sub_metering_3)
    
    png(file = "plot3.png")
    plot(meter1, type = "l", xaxt='n', col='black',
         ylab = "Energy sub metering",
         xlab = "")
    
    points(meter2, type = 'l', col='red')
    points(meter3, type = 'l', col='blue')
    
    axis(1,  at=c(0, 1500, 2900),
         labels=c('Thu','Fri','Sat'))
    
    legend("topright", col=c('black', 'red', 'blue'), lty = 1, 
           legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
    
    
    dev.off()
}