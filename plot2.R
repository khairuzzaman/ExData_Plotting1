# This function will read house hold power consumption data.
# Filtered the data for february 1 & 2
# TODO: need to refactor the function so that only necessary data can read

readData <- function(){
    data <- read.table("household_power_consumption.txt", 
                       header = TRUE, sep = ";", dec = ".")
    filtered_data <- subset(data, 
                            data$Date == "1/2/2007" | data$Date == "2/2/2007")
}

plot2 <- function(){
    data <- readData()
    num_data <- as.numeric(data$Global_active_power)
    day <- as.numeric(strftime(as.Date(data$Date, "%d/%m/%Y"), "%u"))
    day_name <- format(as.Date(data$Date), "%a")
    png(file = "plot2.png")
    plot(num_data, type = "l", xaxt='n',
         ylab = "Global Active Power (kilowatts)",
         xlab = "")
    
    #axis(1, at = day, 
     #    labels=day_name)
    
    axis(1,  at=c(0, 1500, 2900),
         labels=c('Thu','Fri','Sat'))
    
    
    dev.off()
}