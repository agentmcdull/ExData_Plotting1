library(lubridate)

loaddata <- function() {
  url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
  if (file.exists('household_power_consumption.zip') == FALSE) {
    download.file(url, destfile='household_power_consumption.zip', mode = 'wb')
  }
  unzip('household_power_consumption.zip')
  pdata <- read.csv('household_power_consumption.txt', sep = ';', na.strings='?', as.is = c(1:9))
  pdata$DateType <- as.Date(pdata$Date, format='%d/%m/%Y')
  pdata <- pdata[as.Date('2007-02-01') <= pdata$DateType & pdata$DateType <= as.Date('2007-02-02'),]
  pdata$datetime <- paste(pdata$Date, pdata$Time)
  pdata$datetime <- strptime(pdata$datetime, format='%d/%m/%Y %H:%M:%S')
  pdata
}

saveplot1 <- function(pdata) {
  #pdata <- loaddata()
  png(filename = 'plot1.png', width = 480, height = 480)
  hist(pdata$Global_active_power, col='red', xlab = 'Global Active Power (kilowatts)', main='Global Activve Power')
  dev.off()
}

drawplot2 <- function(pdata) {
  plot(pdata$datetime, pdata$Global_active_power, type='n', xlab='', ylab='Global Active Power (kilowatts)')
  lines(pdata$datetime, pdata$Global_active_power)
}

saveplot2 <- function(pdata) {
  png(filename = 'plot2.png', width = 480, height = 480)
  drawplot2(pdata)
  dev.off()
}

drawplot3 <- function(pdata) {
  plot(pdata$datetime, pdata$Sub_metering_1, type='n', xlab='', ylab='Energy sub metering')
  lines(pdata$datetime, pdata$Sub_metering_1, col='black')
  lines(pdata$datetime, pdata$Sub_metering_2, col='red')
  lines(pdata$datetime, pdata$Sub_metering_3, col='purple')
  legend('topright', legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), lty=c(1,1,1), lwd=c(1,1,1), col=c('black','red','purple'))
}

saveplot3 <- function(pdata) {
  png(filename = 'plot3.png', width = 480, height = 480)
  drawplot3(pdata)  
  dev.off()
}

drawplot4 <- function(pdata) {
  par(mfrow=c(2,2))
  drawplot2(pdata)
  plot(pdata$datetime, pdata$Voltage, xlab='datetime', ylab='Voltage', type='n')
  lines(pdata$datetime, pdata$Voltage)
  drawplot3(pdata)
  plot(pdata$datetime, pdata$Global_reactive_power, xlab='datetime', ylab='Global_reactive_power', type='n')
  lines(pdata$datetime, pdata$Global_reactive_power)
}

saveplot4 <- function(pdata) {
  png(filename = 'plot4.png', width = 480, height = 480)
  drawplot4(pdata)  
  dev.off()
}

pdata <- loaddata()
saveplot1(pdata)