## Data Science Group
## Faculty of Computing and Informatics
## Copyright (c) 2015, 2016

## This is an outdated sample as http://data.gov.my/ has revamped their
## site and hence the original access is different.  As such, the two
## relevant files are also uploaded to the github for sample purposes.

## Data from http://data.gov.my/
## Rain, Wind, Humidity and Radition
## Years January 2008 - April 2015
## http://data.gov.my/folders/MOSTI/TaburanHujanKelajuanAnginKelembapanRadiasiGlobal.xls
##
## Air Pollutant Index (API)
## August 2013 - February 2015
## http://data.gov.my/folders/MOSTI/AirPollutentIndex.xlsx

##############################################################################
## Read Data: read.xlsx() (Note, Module 4 has reading RDS which we don't do)
##############################################################################

library(xlsx)

# Read the data for Sandakan only
RWMRData <- read.xlsx("rainWind.xls", sheetIndex = 36)
#RWMRData <- read.xlsx("rainWind.xls", sheetName = "Sandakan")
dim(RWMRData)
View(RWMRData)

APIData <- read.csv("API.csv", sep = ":")
dim(APIData)
View(APIData)

##############################################################################
## Clean Data: Subsetting and using aggregate()
##############################################################################

## First thing we need to decide is to clean both the datasets
## For RWMRData, we are interested only in 
## Rows 12 - 2688 (11 - 2688 if want some sort of header)
## Columns 2 - 6
RWMRData <- RWMRData[12:2688, 2:6]

## Name the variables
names(RWMRData) <- c("Year", "Month", "Day", "Rain_Fall", "Wind")

## Convert the Rain_Fall into numeric (real number)
RWMRData$Rain_Fall <- as.character(RWMRData$Rain_Fall)
RWMRData$Rain_Fall <- as.numeric(RWMRData$Rain_Fall)
## Clean the data for any negative values or others (e.g. -33.33 or NA)
RWMRData$Rain_Fall[(RWMRData$Rain_Fall < 0 | is.na(RWMRData$Rain_Fall))] <- 0

## Convert the Wind into numeric (real number)
## This part is not necessary as there are no data to clean actually.
RWMRData$Wind <- as.character(RWMRData$Wind)
RWMRData$Wind <- as.numeric(RWMRData$Wind)
## Clean the data for any negative values or others
RWMRData$Wind[(RWMRData$Wind < 0 | is.na(RWMRData$Wind))] <- 0

## For APIData, we are interested only in 
## All Rows
## Columns 3 - 6
APIData <- APIData[,3:6]

## Name the variables
names(APIData) <- c("Date", "Hour", "API", "Contaminants")

## Not needed here but the Contaminants are indicated as follows
## *   =  Habuk Halus (PM10)               c =  Ozon (O3)
## a   =  Sulfur Dioksida (SO2)            d =  Karbon Monoksida (CO)
## b   =  Nitrogen Dioksida (NO2)          & =  Lebih daripada satu pencemar

## The Rain and Wind data is "by day" while the API is "by hour"
## Use aggregate() to compute it based on "Date"

APIData <- aggregate(APIData$API, list(APIData$Date), mean)
names(APIData) <- c("Date", "API")

##############################################################################
## More Subsetting Data: Using paste() and manipulating dates
##############################################################################

## Since the date range are different, we take the common dates that are
## covered by both data, which is from August 2013 and January 2015.

## The date on RWMRData is separated, let's combine them
RWMRData$Date <- as.POSIXct(paste(RWMRData$Year, RWMRData$Month, RWMRData$Day, sep = "-"))

## The date on APIData looks like in POSIXct format but in a data frame
## it is by default read in as factor.  So, we convert the class()
APIData$Date <- as.POSIXct(APIData$Date)

## I am not sure why the >= and < seem to work like this in R (need to investigate)
RWMRData <- RWMRData[as.Date(RWMRData$Date) >= as.Date("2013-07-31") & as.Date(RWMRData$Date) < as.Date("2015-01-31"),]

APIData <- APIData[as.Date(APIData$Date) >= as.Date("2013-07-31") & as.Date(APIData$Date) < as.Date("2015-01-31"),]

## RWMRDate and APIData should now have 549 observations each

##############################################################################
## Plot 1: Rainfall histogram plot
##############################################################################

png(file="plot1.png", width=480, height=480)

hist(RWMRData$Rain_Fall, col="blue", main="Rain Fall Data for Sandakan", xlab = "Rain Fall in mm", ylab = "")

## The histogram presents the frequency of the rain fall in mm

## Turn off the selected R Graphics Driver
dev.off()

##############################################################################
## Plot 2: Rainfall line plot
##############################################################################

png(file="plot2.png", width=480, height=480)

plot(RWMRData$Date, RWMRData$Wind, type = "l", main = "Wind Data for Sandakan", xlab = "Year", ylab = "Wind Strength in m/s")

## Don't worry, the data measurement actually is wrongly entered from September 2014
## onwards.

## Turn off the selected R Graphics Driver
dev.off()

##############################################################################
## Merging of Data: Using merge()
##############################################################################

allData <- merge(RWMRData, APIData, by = "Date", all = TRUE)
allData <- allData[,c(1,5:7)]

##############################################################################
## Plot 3: Legends
##############################################################################

png(file="plot3.png", width=480, height=480)

## plot() one first and to add on, we use the lines()
## NOTE THAT THE PLOT IS NOT REALLY INFORMATIVE AS THE MEASUREMENTS (on Y-Axis) ARE
## NOT OF THE SAME METRICS

plot(allData$Date, allData$Rain_Fall, type="l", ylab="Rain in (mm), Wind in (m/s), API", xlab="")
lines(allData$Date, allData$Wind, type="l", col="green")
lines(allData$Date, allData$API, type="l", col="brown")
legend("topright", c("Rain Fall (mm)", "Wind (m/s)", "API"), lty=1, col=c("black", "green", "brown"))

## Turn off the selected R Graphics Driver
dev.off()

##############################################################################
## Plot 4: Panel
##############################################################################

png(file="plot4.png", width=960, height=960)

## Make the 2 x 2 (r x c) graph layout
par(mfrow = c(2, 2)) 

## Top Left Graph
hist(RWMRData$Rain_Fall, col="blue", main="Rain Fall Data for Sandakan", xlab = "Rain Fall in mm", ylab = "")

## Top Right Graph
plot(RWMRData$Date, RWMRData$Rain_Fall, type = "l", main = "Rain Fall Data for Sandakan", xlab = "Year", ylab = "Rain Fall in mm")

## Bottom Left Graph
plot(RWMRData$Date, RWMRData$Wind, type = "l", main = "Wind Data for Sandakan", xlab = "Year", ylab = "Wind Strength in m/s")

## Bottom Right Graph
plot(allData$Date, allData$Rain_Fall, type="l", ylab="Rain in (mm), Wind in (m/s), API", xlab="")
lines(allData$Date, allData$Wind, type="l", col="red")
lines(allData$Date, allData$API, type="l", col="blue")
legend("topright", c("Rain Fall (mm)", "Wind (m/s)", "API"), lty=1, col=c("black", "red", "blue"))

## Turn off the selected R Graphics Driver
dev.off()

