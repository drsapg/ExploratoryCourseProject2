# This code answer the third plot
# Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

#Loading libraries
library(lubridate)
library(dplyr)
library(ggplot2)

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
NEI$year <- as.factor(NEI$year)


pm25baltimore <- filter(NEI,fips=="24510")

#Plotting
baltimoreplot <- ggplot(pm25baltimore, aes(x=year,y=log10(Emissions), group=year)) + geom_boxplot()
baltimoreplot + facet_grid(.~type)  + ggtitle("Baltimore Emissions")
ggsave("Plot3.png",width=20,height=10)
