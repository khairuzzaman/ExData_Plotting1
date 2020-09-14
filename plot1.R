# This function will read house hold power consumption data.
# Filtered the data for february 1 & 2
# TODO: need to refactor the function so that only necessary data can read

readData <- function(){
    data <- read.table("household_power_consumption.txt", 
                       header = TRUE, sep = ";", dec = ".")
    filtered_data <- subset(data, 
                       data$Date == "1/2/2007" | data$Date == "2/2/2007")
}

plot1 <- function(){
    data <- readData()
    png(file = "plot1.png")
    hist(as.numeric(data$Global_active_power), col = "red",
         main = "Global Active Power",
         xlab = "Global Active Power (kilowatts)",
         ylab = "Frequency")
    
    
    dev.off()
}