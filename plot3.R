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
    dt <- createDateTimeVector(data)
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
    
    
    dev.off()
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