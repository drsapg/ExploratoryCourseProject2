# This code answer the fourt plot
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008? 

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

# Select the SCC codes of coal related products in the SCC table
coal <- filter(SCC,grepl(".[Co]al$",EI.Sector)) %>% select(SCC)
coal <- coal[[1]]

#grouping by year
coalpm25 <- filter(NEI,SCC %in% coal) %>% group_by(year) %>% summarize(emission = sum(Emissions))

#Plotting
png("Plot4.png")
plot(as.numeric(coalpm25$year),coalpm25$emission, type="l", xaxt = "n", xlab="Year", ylab="PM 2.5 Emissions (tons)", main="Coal Combustion PM 2.5 production")
axis(1,1:4,labels=c(1999,2002,2005,2008))
dev.off()