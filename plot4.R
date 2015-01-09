#
# Exploratory Data Analysis - Course Project 1
#
# Plot #4
#
# 4 plots panel
#
# 
# This assignment uses data from the UC Irvine Machine Learning Repository, a popular repository for machine learning datasets. In particular, we will be using the “Individual household electric power consumption Data Set” which I have made available on the course web site:
# 
#     Dataset: Electric power consumption [20Mb]
# 
#     Description: Measurements of electric power consumption in one household with a one-minute sampling rate over a period of almost 4 years. Different electrical quantities and some sub-metering values are available.
# 
# The following descriptions of the 9 variables in the dataset are taken from the UCI web site:
# 
#     Date: Date in format dd/mm/yyyy
#     Time: time in format hh:mm:ss
#     Global_active_power: household global minute-averaged active power (in kilowatt)
#     Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#     Voltage: minute-averaged voltage (in volt)
#     Global_intensity: household global minute-averaged current intensity (in ampere)
#     Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#     Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#     Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.
# 
# Loading the data
# 
# When loading the dataset into R, please consider the following:
# 
#     The dataset has 2,075,259 rows and 9 columns. First calculate a rough estimate of how much memory the dataset will require in memory before reading into R. Make sure your computer has enough memory (most modern computers should be fine).
# 
#     We will only be using data from the dates 2007-02-01 and 2007-02-02. One alternative is to read the data from just those dates rather than reading in the entire dataset and subsetting to those dates.
# 
#     You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime() and as.Date() functions.
# 
#     Note that in this dataset missing values are coded as ?.
# 
# Making Plots
# 
# Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. Your task is to reconstruct the following plots below, all of which were constructed using the base plotting system.
# 
# First you will need to fork and clone the following GitHub repository: https://github.com/rdpeng/ExData_Plotting1
# 
# For each plot you should
# 
#     Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
# 
#     Name each of the plot files as plot1.png, plot2.png, etc.
# 
#     Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. Your code file should include code for reading the data so that the plot can be fully reproduced. You should also include the code that creates the PNG file.
# 
#     Add the PNG file and R code file to your git repository
# 
# When you are finished with the assignment, push your git repository to GitHub so that the GitHub version of your repository is up to date. There should be four PNG files and four R code files.
#

# Step 0 - used libraries
library(dplyr)
library(lubridate)
library(grDevices)
library(graphics)

# Step 1 - downloading the data

download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "household_power_consumption.zip")

# Step 2 - unzipping and reading the data

unzip("household_power_consumption.zip")
data <- read.csv("household_power_consumption.txt", sep=";", stringsAsFactors=FALSE)

# Step 3 - build the dataframe to be used in charts: "We will only be using data from the dates 2007-02-01 and 2007-02-02"

filtered_data <- data %>% 
			filter(dmy(Date) %in% c(ymd("2007-02-01"), ymd("2007-02-02"))) %>% 
			mutate(Global_active_power = as.double(Global_active_power), DateTime=dmy(Date) + hms(Time))

plotfn <- function(dataset) {			
	# Step 4 - building the plots
	par(mfrow=c(2,2)) # 2x2 grid

	#top-left
	plot(dataset$DateTime, dataset$Global_active_power, type="l", ylab="Global Active Power", xlab="")
	#top-right
	plot(dataset$DateTime, dataset$Voltage, type="l", ylab="Voltage", xlab="datetime")
	#bottom-left
	plot(dataset$DateTime, dataset$Sub_metering_1, type="n", ylab="Energy sub metering", xlab="")
	points(dataset$DateTime, dataset$Sub_metering_1, type="l", col="black")
	points(dataset$DateTime, dataset$Sub_metering_2, type="l", col="red")
	points(dataset$DateTime, dataset$Sub_metering_3, type="l", col="blue")
	legend("topright",  col = c("black", "blue", "red"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1, 1, 1), bty="n")
	#bottom-right
	plot(dataset$DateTime, dataset$Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime")
}

plotfn(filtered_data)

# create chart in png. using an alternative way this time
png(file="plot4.png", 480, 480)
plotfn(filtered_data)
dev.off()
