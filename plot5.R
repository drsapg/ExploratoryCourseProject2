# This code answer the fifth plot
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City 

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
motor <- filter(SCC,grepl("^[Mm]obile",EI.Sector)) %>% select(SCC)
motor <- motor[[1]]

#grouping by year
motorpm25 <- filter(NEI,SCC %in% motor, fips=="24510") %>% group_by(year) %>% summarize(emission = sum(Emissions))

#Plotting
png("Plot5.png")
plot(as.numeric(motorpm25$year),motorpm25$emission, type="l", xaxt = "n", xlab="Year", ylab="PM 2.5 Emissions (tons)", main="Mobile Combustion PM 2.5 production - Baltimore")
axis(1,1:4,labels=c(1999,2002,2005,2008))
dev.off()