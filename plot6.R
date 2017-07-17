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
#Baltimore
motorpm25b <- filter(NEI,SCC %in% motor, fips=="24510") %>% group_by(year) %>% summarize(emission = sum(Emissions), city="Baltimore")
#Los Angeles
motorpm25a <- filter(NEI,SCC %in% motor, fips=="06037") %>% group_by(year) %>% summarize(emission = sum(Emissions), city="Los Angeles")

motorpm25 <- rbind(motorpm25b,motorpm25a)

rng <- range(motorpm25b$emission,motorpm25a$emission)

# Using all the data
alldatabaltimore <- filter(NEI,SCC %in% motor, fips=="24510") %>% group_by(year)
alldatabaltimore <- mutate(alldatabaltimore, city="Baltimore")
alldatala <- filter(NEI,SCC %in% motor, fips=="06037") %>% group_by(year)
alldatala <- mutate(alldatala, city="Los Angeles")
alldata <- rbind(alldatabaltimore,alldatala)
rng1 <- range(log(alldatabaltimore$Emissions),log(alldatala$Emissions))

#Plotting
png("Plot6.png",width = 960)
par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(as.numeric(motorpm25b$year),motorpm25b$emission, ylim=rng,type="l", col=3,xaxt = "n", xlab="Year", ylab="PM 2.5 Emissions (tons)", main="Total Mobile Combustion PM 2.5 production")
points(as.numeric(motorpm25a$year),motorpm25a$emission, pch=20, col=2)
segments(1,motorpm25a$emission[1],2,motorpm25a$emission[2], col=2)
segments(2,motorpm25a$emission[2],3,motorpm25a$emission[3], col=2)
segments(3,motorpm25a$emission[3],4,motorpm25a$emission[4], col=2)
legend("right","center",legend = c("Baltimore","Los Angeles"),col = c(3,2), lwd=2)
axis(1,1:4,labels=c(1999,2002,2005,2008))
boxplot(log(alldatabaltimore$Emissions)~alldatabaltimore$year, at=1:4*2 + 1,notch=TRUE, xlim=c(1,12),xaxt = "n",col=c(3), xlab="Year", ylab="Log pm 2.5 Emissions", main="Mobile Combustion PM 2.5 production")
boxplot(log(alldatala$Emissions)~alldatala$year,  at=1:4*2 + 2, xaxt = "n", notch=TRUE,add = TRUE, col=2)
legend("bottomleft",legend = c("Baltimore","Los Angeles"),col = c(3,2), pch=20)
axis(1,1:4*2+1.5,labels=c(1999,2002,2005,2008))
dev.off()