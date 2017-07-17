# This code answer the first plot
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total 
#PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

#Loading libraries
library(lubridate)
library(dplyr)

#Preparing the data
if(!file.exists("dataset.zip"))
{
        fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"        
        download.file(fileurl,"dataset.zip")
        unzip(dataset.zip)
}
#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")
NEI$type <- as.factor(NEI$type)
NEI$Pollutant <- as.factor(NEI$Pollutant)

# function to get the pm25 values
totalpm25 <- function(x,y, f = NULL)
{
        pm25 <- filter(x,year==y & fips == f )
        total <- (sum(pm25$Emissions))
        }

#Creates numeric vector for each year
total0 <- totalpm25(NEI,1999,"24510")
total1 <- totalpm25(NEI,2002,"24510")
total2 <- totalpm25(NEI,2005,"24510")
total3 <- totalpm25(NEI,2008,"24510")

# Setting the range
rng <- range(total0,total1,total2,total3)

#Plotting
png("Plot2.png")
plot(1999,total0,pch=20, ylim=c(rng[1]-2000,rng[2]+2000), xlim=c(1998,2008),ylab="Total Emissions (in tons)", xlab="Year", main="Total PM 2.5 Emissions Baltimore")
points(2002,total1,pch=20)
points(2005,total2,pch=20)
points(2008,total3,pch=20)
segments(1999,total0,2002,total1)
segments(2002,total1,2005,total2)
segments(2005,total2,2008,total3)
dev.off()