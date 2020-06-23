#### Plot 1
# read in data
NEI <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds")
# subset data
data_sub <- tapply(NEI$Emissions, NEI$year, sum)
xnames <- names(tapply(NEI$Emissions, NEI$year, sum))
# plot data
png("plot1.png", width = 480, height = 480)
plot(data_sub, type = "o", col = "darkblue", pch = 19, xaxt = "n", yaxt = "n",
     xlab = "Year", ylab = "PM2.5 Emitted (Millions of tons)",
     main = "Total PM2.5 Emissions/Year")
axis(1, at = 1 : length(xnames), labels = xnames)
axis(2, at = c(4e+06, 5e+06, 6e+06, 7e+06), labels = c("4", "5", "6", "7"))
dev.off()

#### Plot 2
# read in data
NEI <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds")
# subset data
baltimore <- subset(NEI, NEI$fips == "24510", select = 1:6)
balt_year <- tapply(baltimore$Emissions, baltimore$year, sum)
xnames <- names(tapply(baltimore$Emissions, baltimore$year, sum))
# plot data
png("plot2.png", width = 480, height = 480)
plot(balt_year, type = "o", col = "darkblue", pch = 19, xaxt = "n",
     xlab = "Year", ylab = "PM2.5 Emitted (in tons)",
     main = "Baltimore City PM2.5 Emissions")
axis(1, at = 1 : length(xnames), labels = xnames)
dev.off()

#### Plot 3
library(ggplot2)
# read in data
NEI <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds")
# format and subset data
baltimore <- subset(NEI, NEI$fips == "24510", select = 1:6)
baltimore$type <- as.factor(baltimore$type)
agg <- with(baltimore, aggregate(Emissions ~ type + year, FUN = sum))
# plot data
png("plot3.png", width = 550, height = 380)
bd <- ggplot(data = agg, mapping = aes(year, Emissions, colour = type)) +
    theme_bw() +
    geom_line(lwd = 1) +
    geom_point(size = 1.5) +
    labs(title = "Emission Sources in Baltimore from 1999 & 2008",
         y = "Emissions (in tons)", x = "Year",
         caption = "Source: EPA National Emissions Inventory") +
    theme(plot.caption = element_text(face = "italic",  hjust = 1.3),
          legend.title = element_blank())
bd
dev.off()

#### Plot 4
library(dplyr)
library(ggplot2)
# read in data
NEI <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds")
# format and subset data
NEI_join <- left_join(NEI, SCC, by = "SCC")
rm(NEI)
coal_emission <- NEI_join[grep("coal", NEI_join$Short.Name, ignore.case = TRUE,
                               fixed = FALSE), ]
coal_emission$type <- as.factor(coal_emission$type)
agg <- with(coal_emission, aggregate(Emissions ~ SCC.Level.Two + year,
                                     FUN = sum))
# plot data
png("plot4.png", width = 650, height = 400)
bd <- ggplot(data = agg, aes(x = year, y = log10(Emissions))) +
    geom_point(size = .6) +
    geom_line() +
    theme_bw() +
    facet_wrap(. ~ SCC.Level.Two, ncol = 4) +
    labs(title = "Coal Combustion-related Emissions in the U.S., 1998-2008",
         y = "log10[Emissions (in tons)]", x = "Year")
bd
dev.off()

#### Plot 5
library(dplyr)
library(ggplot2)
# read in data
NEI <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds")
# format and subset data
baltimore <- subset(NEI, NEI$fips == "24510", select = 1:6)
baltimore$type <- as.factor(baltimore$type)
db3 <- left_join(baltimore, SCC, by = "SCC")
db4 <- db3[grep("Vehicles", db3$Short.Name, ignore.case = TRUE, fixed = FALSE), ]
agg2 <- with(db4, aggregate(Emissions ~ SCC.Level.Three + year, FUN = sum))
# plot data
png("plot5.png", width = 550, height = 380)
bd2 <- ggplot(data = agg2, mapping = aes(year, Emissions, colour = SCC.Level.Three)) +
    theme_bw() +
    geom_line(lwd = 1) +
    geom_point(size = 1.5) +
    labs(title = "Motor Vehicle Emissions in Baltimore, 1988-2008",
         y = "Emissions (in tons)", x = "Year",
         caption = "Source: EPA National Emissions Inventory") +
    theme(plot.caption = element_text(face = "italic",  hjust = 1),
          legend.title = element_blank(),
          legend.position = c(0.7,0.85))
bd2
dev.off()

#### Plot 6
library(dplyr)
library(ggplot2)
# read in data
NEI <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds")
# format and subset data
NEI_join <- left_join(NEI, SCC, by = "SCC")
rm(NEI)
twoCounty <- subset(NEI_join, NEI_join$fips == c("24510","06037"), select = 1:20)
twoCounty$type <- as.factor(twoCounty$type)
twoCounty$fips <- as.factor(twoCounty$fips)
df <- twoCounty[grep("Vehicles", twoCounty$Short.Name, ignore.case = TRUE,
                     fixed = FALSE), ]
# plot data
png("plot6.png", width = 480, height = 600)
gp <- ggplot(data = df, mapping = aes(x = factor(year), y = log10(Emissions),
                                       fill = fips)) +
    geom_boxplot(position = position_dodge2()) +
    facet_grid(SCC.Level.Three ~ .) +
    theme_bw() +
    labs(title = "Motor Vehicle Emissions in Baltimore & Los Angeles",
         x = "Year", y = "log10[Emissions(in tons)]",
         caption = "* No Baltimore data for Recreational Equipment") +
    theme(legend.title = element_blank(),
          plot.caption = element_text(face = "italic", hjust = 1.5)) +
    scale_fill_discrete(breaks = c("06037", "24510"),
                        labels = c("Los Angeles", "Baltimore"))
gp
dev.off()
