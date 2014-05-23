# read data

Code <- readRDS("~/Data for EDA PA2/Source_Classification_Code.rds")
eData <- readRDS("~/Data for EDA PA2/summarySCC_PM25.rds")

# 1 Have total emissions reduced?

library(reshape2)
dataMelt <- melt(eData, id = "year", measure.vars = "Emissions")
datacast <- dcast(dataMelt, year ~ variable, sum)

plot(datacast$year, datacast$Emissions, type = "l", col = "green", lwd = 2,
     main = "Total Emissions across US 1999-2008",
     xlab = "Year", ylab = "Total Emissions")
points(datacast$year, datacast$Emissions, pch=20, cex=2)
text(datacast$year, datacast$Emissions, pos = c(4, 1, 3, 2),
     labels = signif(datacast$Emissions, 3), offset = 1)

dev.copy(png, file = "EDA_PA2_1.png", height = 480, width = 480)
dev.off()

# 2 Have total emissions reduced in Baltimore/Marryland fips = 24510

maryland_data <- subset(eData, eData$fips == "24510")
maryland_Melt <- melt(maryland_data, id = c("year", "type"),
                      measure.vars = "Emissions")
maryland_cast <- dcast(maryland_Melt, year ~ variable, sum)

plot(maryland_cast$year, maryland_cast$Emissions, 
     type = "l", col = "green", lwd = 2,
     main = "Total Emissions in Maryland 1999-2008",
     xlab= "year", ylab = "Total Emissions")
points(maryland_cast$year, maryland_cast$Emissions, pch=20, cex=2)
text(maryland_cast$year, maryland_cast$Emissions, 
     pos = c(4, 1, 4, 2),
     labels = signif(maryland_cast$Emissions, 3), offset = 1)

dev.copy(png, file = "EDA_PA2_2.png", height = 480, width = 480)
dev.off()

# 3 comparing among 4 types of sources

library(ggplot2)
baltimore_cast <- dcast(maryland_Melt, type + year ~ variable, sum)
qplot(year, Emissions, data = baltimore_cast, facets = .~type, 
      main = "Total Emissions by Sources: Baltimore",
      ylab = "Total Emissions",
      geom = c("point", "smooth"), method = "lm")

dev.copy(png, file = "EDA_PA2_3.png", height = 480, width = 480)
dev.off()

## 4 comparing emissions from coal combustion related sources

# finding the sector (EI.Sector that has activity of Combustion and Coal related
# by using grep function to find any value that contains "Comb" and "Coal"

comb_coal <- grep("Comb .* Coal", Code$EI.Sector, value = TRUE)

# subset the data that contains only Comb Coal value

comb_coalData <- subset(Code, Code$EI.Sector %in% comb_coal)

# now we subset the data from the original dataset that contains the SCC that's
# matched the SCC in the comb_coalData

comb_coalEmission <- subset(eData, eData$SCC %in% comb_coalData$SCC)

head(comb_coalEmission)

# reshape the data and make a plot
comb_coalMelt <- melt(comb_coalEmission, id = c("type", "year"), 
                      measure.vars = "Emissions")
comb_coalSum <- dcast(comb_coalMelt, year ~ variable, sum)

plot(comb_coalSum$year, comb_coalSum$Emissions, 
     type = "l", lwd = 2, col = "green",
     main = "Emissions from Coal Combustion 1999-2008",
     xlab = "Year", ylab = "Total Emissions")

points(comb_coalSum$year, comb_coalSum$Emissions, pch=20, cex=2)
text(comb_coalSum$year, comb_coalSum$Emissions, pos = c(4, 3, 4, 2),
     labels = signif(comb_coalSum$Emissions, 3), offset = 1)

dev.copy(png, file = "EDA_PA2_4.png", height = 480, width = 480)
dev.off()

## 5 Emission from motor vehicle sources

motor <- grep("[Mm]otor", Code$Short.Name, value = TRUE)

motorSCC <- subset(Code, Code$Short.Name %in% motor)

motorData <- subset(eData, eData$SCC %in% motorSCC$SCC & eData$fips == "24510")

head(motorData)

# reshape and plot

motorMelt <- melt(motorData, id = c("type", "year"), 
                      measure.vars = "Emissions")
motorSum <- dcast(motorMelt, year ~ variable, sum)

plot(motorSum$year, motorSum$Emissions, 
     type = "l", lwd = 2, col = "green",
     main = "Total Emissions from Motor Vehicles 1999-2008",
     xlab = "Year", ylab = "Total Emissions")

points(motorSum$year, motorSum$Emissions, pch=20, cex=2)
text(motorSum$year, motorSum$Emissions, pos = c(4, 1, 4, 2),
     labels = signif(motorSum$Emissions, 3), offset = 1)

dev.copy(png, file = "EDA_PA2_5.png", height = 480, width = 480)
dev.off()
# 6

county <- c("24510", "06037")
motorData1 <- subset(eData, eData$SCC %in% motorSCC$SCC & eData$fips %in% county)

Melt <- melt(motorData1, id = c("fips", "year"), 
                  measure.vars = "Emissions")
Sum <- dcast(Melt, fips + year ~ variable, sum)

Sum$fips <- factor(Sum$fips)
levels(Sum$fips) <- c("Los Angeles", "Baltimore")
names(Sum) <- c("county", "year", "Emissions")

qplot(year, Emissions, data = Sum, color = county, 
      main = "Motor Vehicle Emissions: Los Angeles vs. Baltimore",
      ylab = "Total Emissions",
      geom = c("point", "smooth"), method = "lm")

dev.copy(png, file = "EDA_PA2_6.png", height = 480, width = 480)
dev.off()
