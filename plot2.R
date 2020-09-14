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
    num_data <- as.numeric(data$Global_active_power)
    png(file = "plot2.png")
    
    dt <- createDateTimeVector(data)
    plot(dt, num_data, type = "l", xaxt='n',
         ylab = "Global Active Power (kilowatts)",
         xlab = "")
    
    at_vec <- rangeVector(dt)
    axis(1,  at=at_vec,
         labels=c('Thu','Fri','Sat'))
    
    
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