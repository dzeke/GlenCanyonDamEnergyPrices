####################
#     GCDEnergyPrice.R
#     Glen Canyon Dam Energy Prices
#
#     This repository has the purpose to answer the question: How do energy prices at Glen Canyon Dam change by hour and month?

#      We use data from Bair, L., and Yackulic, C. (2024). "Predicted hydropower impacts of different management scenarios for Lake Powell releases." U.S. Geological Survey data release. https://doi.org/10.5066/P135BOD8.

#     The Data Wrangling strategy is to:
#       1. Read in the generation data for the No action alternative in the folder generation/generation_hourly_noaction.csv [MW-hour]
#           This  FIRST file represents hourly generation at Glen Canyon Dam in megawatt hours by hour, month and hydrologic trace in the LTEM sEIS. The purpose of these data tables are to allow for a comparison of the difference in generation between LTEMP eEIS alternatives. Columns are hours in a month. Rows are month and 30 hydrologic traces. For example, the first 30 rows are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of October 2023. Rows 31-60 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2023. Rows 1471-1500 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2027. The 11 data tables are separate 1500 by 744 matrices. The months with days less than 31 days contain "0" entries for those hours and days.</enttypd>
#           Columns hour_1 to hour_744 in the data table represents megawatt hours by hour, month and hydrologic trace in the LTEM sEIS. Months with days less than 31 days contain "0" entries for those hours and days.</attrdef>
#
##      2. Read in the economic data for the No action alternative in the folder econ/econ_hourly_noaction.csv
##           The SECOND file represents the economic value of energy generated at Glen Canyon Dam in nominal dollars by hour, month and hydrologic trace in the LTEM sEIS. The purpose of these data tables are to allow for a comparison of the difference in economic value between LTEMP sEIS alternatives. Columns are hours in a month. Rows are month and 30 hydrologic traces. For example, the first 30 rows are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of October 2023. Rows 31-60 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2023. Rows 1471-1500 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2027. The 11 data tables are separate 1500 by 744 matrices. The months with days less than 31 days contain "0" entries for those hours and days.</enttypd>
#            Columns hour_1 to hour_744 in the data table represent economic value by hour, month and hydrologic trace in the LTEM sEIS.</attrdef>
  
#       3. Add row labels to differentiate each row.
#
#       4. Convert the Generation and Econ data frames to Narrow format.
#             So the generation data frame has Columns of [Year][Month][Trace][HourAsText][Generation]
#             So  the Econ data frame has Columns of [Year][Month][Trace][HourAsText][Value]
#
#       5. Join the two tables on Year, Month, Trace, and HourAsText so we have a new data frame with columns [Year][Month][Trace][HourAsText][Generation][Value]
#
#       6. Convert the HourAsText from 1 to 744 to numerical hour, Calculate the day of month, hour of day, and on-peak/off-peak for each row.
#
#       7. Divide the Value column by Generation column to get a Price in $/MW-hour. Set rows with Zero generation to NA
#
#       8. Plot the pricing data in different formats.
#
#     Plots:

# Figure 1. Box and Whiskers of variation in prices for each month. This includes all scenarios and all weeks/days/hours/years.
# Figure 2. Time series of prices for all years all months, years overlaid on each other.
# Figure 3. Time series of prices for all traces for the first two weeks of August, 2023.
# Figure 4. Time series of generation for all traces for the first two weeks of August 2023.
# Figure 5. Time series of economic value for all traces for the first two weeks of August 2023, There is difference here, although all traces have the same shape, just different magnitudes.
# Figure 6. Time series for 1st week of August 2024 separating on-peak prices from off-peak prices.
# Figure 7. Time-series showing on-peak prices highlighted in blue and off-peak prices highlighted in red for a single week in August 2024. Note, these on-, off-peak definitions are for our study and look to be different that what is used in the current data set.
# Figure 8. Time-series of prices showing the first week of June, July, August, and September for Year 2024, Trace 1.
# Figure 9. Time series of first week of July 2024 showing on-peak and off-peak prices
# Figure 10. Period average on-peak and off-peak prices for first week of June, July, August and September. #     David E. Rosenberg
#     
# David Rosenberg
# November 17, 2025
# david.rosenberg@usu.edu

#################


rm(list = ls())  #Clear history

#Load packages in one go
  #List of packages
  load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2", "knitr", "tidyr")
# Then we select only the packages that aren't currently installed.
  install.lib <- load.lib[!load.lib %in% installed.packages()]
# And finally we install the missing packages, including their dependency.
  for(lib in install.lib) install.packages(lib,dependencies=TRUE)
  # After the installation process completes, we load all packages.
  sapply(load.lib,require,character=TRUE)



### Steps #1 and #2. Read in the Generation and Economic Data
sGenData <- 'generation/generation_hourly_noaction.csv'
sEconData <- 'econ/econ_hourly_noaction.csv'

dfGenerationData <- read.csv(file = sGenData, header = TRUE)
dfEconData <- read.csv(file = sEconData, header = TRUE)

nLength <- nrow(dfEconData)

### Step 3. Add row labels of years and months and traces. Reminder: Rows are Month and 30 hydrologic traces. 
#   For example, the first 30 rows are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of October 2023. Rows 31-60 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2023
#   Rows 1471-1500 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2027.

cYears <- c(rep(2023,30*3), rep(2024,30*12),rep(2025,30*12), rep(2026,30*12),rep(2027,30*11))
cOneYear <- c(rep(1,30),rep(2,30),rep(3,30), rep(4,30), rep(5,30), rep(6,30), rep(7,30), rep(8,30), rep(9, 30), rep(10,30), rep(11,30), rep(12,30))
cMonths <- c(rep(10,30), rep(11,30), rep(12,30), cOneYear, cOneYear, cOneYear, cOneYear[1:(11*30)])
cTraces <- rep(seq(1,30,1), 3 + 12*3 + 11)

dfGenerationData$Year <- cYears
dfGenerationData$Month <- cMonths
dfGenerationData$Trace <- cTraces

dfEconData$Year <- cYears
dfEconData$Month <- cMonths
dfEconData$Trace <- cTraces

### Step 4. Convert the data frames to narrow format
#             So the generation data frame has Columns of [Year][Month][Trace][Hour][Generation]
#             So  the Econ data frame has Columns of [Year][Month][Trace][Hour][DollarValue]
#

#Create a list of all hours from 1 to 744
cHours <- c("hour_1")
for (i in seq(2,744,1)) {
  cHours <- c(cHours, paste0("hour_",i))
}
  
dfGenerationDataLong <- melt(dfGenerationData, id.vars = c("Year","Month", "Trace"), measure.vars = cHours, variable.name = "HourText", value.name = "Generation" )
dfEconDataLong <- melt(dfEconData, id.vars = c("Year","Month", "Trace"), measure.vars = cHours, variable.name = "HourText", value.name = "DollarValue" )

### Step 5. Join the two tables on Year, Month, Trace, and Hour so we have a new data frame with columns [Year][Month][Trace][Hour][Generation][Value]
dfAllData <- inner_join(dfGenerationDataLong, dfEconDataLong, by = c("Year" = "Year", "Month" = "Month", "Trace" = "Trace", "HourText" = "HourText"))

### Step 6. Convert the HourAsText from 1 to 744 to numerical hour, day of month, hour of day, and on-peak/off-peak for each row.
#Convert Hour in month as text to hour in month as number (1 to 744)
dfAllData$HourInMonth <- as.numeric(gsub("[^0-9.]", "", dfAllData$HourText))

#Sort the dataframe by Year, Month, Trace, Hour
dfAllData <- dfAllData %>% arrange(Year, Month, Trace, HourInMonth)
#Convert the hour in month to hour of the day
dfAllData$Hour <-  mod(dfAllData$HourInMonth-1,24)
#Calculate the day of the month
dfAllData$Day <- round((dfAllData$HourInMonth)/24 + 0.4999)

### Step 7. Divide the Value column by Generation column to get a Price in $/MW-hour
dfAllData$Price <- ifelse(dfAllData$Generation == 0, NA, dfAllData$DollarValue/dfAllData$Generation)

#Remove all rows with an NA
dfAllData <- na.omit(dfAllData)

#Calculate a date-time value
dfAllData$DateTime <- ISOdate(dfAllData$Year, dfAllData$Month, dfAllData$Day, dfAllData$Hour, 0, 0)
#dfAllData$DateTime <- ymd_h(dfAllData$Year, dfAllData$Month, dfAllData$Day, dfAllData$Hour)

#Calculate a date-time with a single year to allow overlapping months of different years
dfAllData$DateTimeSingleYear <- ISOdate(2024, dfAllData$Month, dfAllData$Day, dfAllData$Hour, 0, 0)

#Calculate the day of the week
dfAllData$DayOfWeek <- wday(dfAllData$DateTime)
dfAllData$DayOfWeekWord <- wday(dfAllData$DateTime, label = TRUE)
dfAllData$MonthWord <- month(dfAllData$DateTime, label = TRUE, abbr=FALSE)

#Calculate Off-peak (hours 4 to 11) and On-peak all other hours
nStartHourOffPeak <- 6
nEndHourOffPeak <- nStartHourOffPeak + 8
dfAllData$Period <- ifelse((dfAllData$Hour >= nStartHourOffPeak) & (dfAllData$Hour <= nEndHourOffPeak), "Off-peak", "On-peak")

#Calculate Weekday (Days 2 to 6) verses weekend (Days 1 and 7)
dfAllData$DayType <- ifelse((dfAllData$DayOfWeek >= 2) & (dfAllData$DayOfWeek <= 6), "Weekday", "Weekend")

cColorsToPlot <- brewer.pal(9, "Blues")
#cColorsToPlot <- colorRampPalette((brewer.pal(9, "Blues"))(10 - 3 + 1))

#Font sizes for different components of the plot
nAxisTickSize <- 16
nLegendSize <- 16
nMainSize <- 20




#### Figure 1 - Box and whiskers by month 
# Insight - March, April, May, June, and October all have low and reletively stable prices.

ggplot(dfAllData %>% filter(Month >= 3, Month <= 10), aes(x = as.factor(Month), y = Price)) +

  geom_boxplot() +
    
  
  #scale_color_manual(values = cColorsToPlot) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 12, breaks = seq(1,12,1), labels = month.abb[seq(1,12,1)]) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="Month", y = "Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_text("Month"), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))


### Figure 2 - compare time series across years
ggplot(dfAllData %>% filter(Month >= 6, Month <= 10), aes(x = DateTimeSingleYear, y = Price, color = as.factor(Year))) +
  
  geom_line() +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 12, breaks = seq(1,12,1), labels = month.abb[seq(1,12,1)]) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="", y = "Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))

print(wday("2024-08-01"), label = FALSE)

### Figure 3 - daily time series August
# Insight - Year only bumps up price a small amount. Weekly pattern repeats
ggplot(dfAllData %>% filter(Month == 8, Day <= 14), aes(x = DateTimeSingleYear, y = Price, color = as.factor(Year))) +
  
  geom_line() +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot[3,5,7, 8, 9]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 7*24, breaks = seq(1,7*24,24), labels = unique(dfAllData$DayOfWeekWord)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="", y = "Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))

### Figure 4 - Look at changing prices across traces
# Insight - No variability
ggplot(dfAllData %>% filter(Month == 8, Day <= 14, Year == 2024), aes(x = DateTimeSingleYear, y = Price, color = as.factor(Trace))) +
  
  geom_line() +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot[3,5,7, 8, 9]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 7*24, breaks = seq(1,7*24,24), labels = unique(dfAllData$DayOfWeekWord)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
theme_bw() +
  
  labs(x="", y = "Price\n($/MW-hr)", color = "Trace") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_text(size = nLegendSize), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))

### Figure 4 - Look at changing generation across traces
# Insight - THere is varaibility. Same shape across traces, variability in magnitude
ggplot(dfAllData %>% filter(Month == 8, Day <= 14, Year == 2024), aes(x = DateTimeSingleYear, y = Generation, color = as.factor(Trace))) +
  
  geom_line() +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot[3,5,7, 8, 9]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 7*24, breaks = seq(1,7*24,24), labels = unique(dfAllData$DayOfWeekWord)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="", y = "Generation\n(MW-hr)", color = "Trace") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_text(size = nLegendSize), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))

### Figure 5 - Look at changing economics across traces
# Insight - THere is varaibility. Same shape across traces, variability in magnitude
ggplot(dfAllData %>% filter(Month == 8, Day <= 14, Year == 2024), aes(x = DateTimeSingleYear, y = DollarValue, color = as.factor(Trace))) +
  
  geom_line() +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot[3,5,7, 8, 9]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 7*24, breaks = seq(1,7*24,24), labels = unique(dfAllData$DayOfWeekWord)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
theme_bw() +
  
  labs(x="", y = "Economic Value\n($)", color = "Trace") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_text(size = nLegendSize), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))


### Figure 6 - Look at changing prices on- and off-peak for a single week of August 2024
# Insight - No variability
ggplot(dfAllData %>% filter(Month == 8, Day <= 7, Year == 2024, Trace == 1), aes(x = DateTimeSingleYear, y = Price, color = as.factor(Period))) +
  
  geom_line() +
  geom_point(size = 3, aes(shape = as.factor(Period))) +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot[3,5,7, 8, 9]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 7*24, breaks = seq(1,7*24,24), labels = unique(dfAllData$DayOfWeekWord)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="", y = "Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))

### Figure 7 - Compare weekly prices by month for the first week of the month
ggplot(dfAllData %>% filter(Day <= 7, Year == 2024, Trace == 1), aes(x = DateTimeSingleYear, y = Price, color = as.factor(Month))) +
  
  geom_line() +
  geom_point() +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot[3,5,7, 8, 9]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(1, 7*24, breaks = seq(1,7*24,24), labels = unique(dfAllData$DayOfWeekWord)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
theme_bw() +
  
  labs(x="", y = "Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))



### Figure 8 - Overlap monthly prices on same week
ggplot(dfAllData %>% filter(Day <= 7, Year == 2024, Trace == 1, Month >= 6, Month <= 9), aes(x = HourInMonth, y = Price, color = as.factor(MonthWord), shape = as.factor(MonthWord))) +
  
  geom_line() +
  geom_point() +
  
  #Show on-peak, off peak
  #geom_point(aes(y = 20, marker = Period), size = 2) +
  
  #facet_wrap(~ Month) +
  
  scale_color_manual(values = cColorsToPlot[c(3,5,7,9)]) +
  #scale_shape_manual(values = )
  #scale_linetype_manual(values = c("solid","longdash")) +
  
   scale_x_continuous(breaks = seq(1,7*24,24)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
theme_bw() +
  
  labs(x="Hour of first week", y = "Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))



### Figure 9 - Show On-peak, off peak for first week of a single month
ggplot(dfAllData %>% filter(Day <= 7, Year == 2024, Trace == 1, Month == 7), aes(x = HourInMonth, y = Price)) +
  
  geom_line() +
  geom_point(aes(color = as.factor(Period), shape = as.factor(Period)), size = 5) +
  
  #Show on-peak, off peak
  #geom_point(aes(y = 20, marker = Period), size = 2) +
  
  #facet_wrap(~ Month) +
  
  #scale_color_manual(values = cColorsToPlot[3,5,7, 8, 9]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  scale_x_continuous(breaks = seq(1,7*24,24)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="Hour of first week", y = "Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))


### Calculate an average price for each day and period
dfAllDataAvgPrice <- dfAllData %>% dplyr::group_by(Day, Year, Trace, Month, MonthWord, Period) %>% dplyr::summarise(AvgPrice = mean(Price))

### Filter so looking at first week of each month
dfAllDataAvgPriceFilter <- dfAllDataAvgPrice %>% filter(Year == 2024, Trace == 1, Day <= 7, Month >= 3, Month <= 10)

## Add a field so we can plot 2 periods per day
dfAllDataAvgPrice$PeriodAsPartOfDay <- dfAllDataAvgPrice$Day + ifelse(dfAllDataAvgPrice$Period == "On-peak", nEndHourOffPeak/24, nStartHourOffPeak/24)

nStartMonth <- 6
nEndMonth <- 9

### Figure 10 - Average On-peak and off peak price for first week of selected months
ggplot(dfAllDataAvgPrice %>% filter(Day <= 7, Year == 2024, Trace == 1, Month >= nStartMonth, Month <= nEndMonth), aes(x = PeriodAsPartOfDay, y = AvgPrice, color = as.factor(MonthWord))) +
  
  geom_step(direction = "hv", size = 2) +
  #geom_point(aes(color = as.factor(Period), shape = as.factor(Period)), size = 2) +
  
  #Show on-peak, off peak
  #geom_point(aes(y = 20, marker = Period), size = 2) +
  
  #facet_wrap(~ Month) +
  
  scale_color_manual(values = cColorsToPlot[seq(3,9,2)]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  scale_x_continuous(breaks = seq(1,7,1)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="Day of the Week", y = "Average Period Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))



## Figure 11 - Average On-peak and off peak price for first week selected months
ggplot(dfAllDataAvgPrice %>% filter(Day <= 7, Year == 2024, Trace == 1, Month %in% c(3,4,5,10)), aes(x = PeriodAsPartOfDay, y = AvgPrice, color = as.factor(MonthWord))) +
  
  geom_step(direction = "hv", size = 2) +
  #geom_point(aes(color = as.factor(Period), shape = as.factor(Period)), size = 2) +
  
  #Show on-peak, off peak
  #geom_point(aes(y = 20, marker = Period), size = 2) +
  
  #facet_wrap(~ Month) +
  
  scale_color_manual(values = cColorsToPlot[seq(3,9,2)]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  scale_x_continuous(breaks = seq(1,7,1)) +
  
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="Day of the Week", y = "Average Period Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))



#### Figure 12 Average weekly on- and off-peak prices
### Calculate an average price for all weekday and weekend on- and off-peak prices

#Calculate Weekday (Days 2 to 6) verses weekend (Days 1 and 7)
# Pull out first week
dfAllDataAvgPrice1Week <- dfAllDataAvgPrice %>% filter(Day <= 7, Year == 2024, Trace == 1)

dfAllDataAvgPrice1Week$DayType <- ifelse((dfAllDataAvgPrice1Week$Day >= 2) & (dfAllDataAvgPrice1Week$Day <= 6), "Weekday", "Weekend")

dfAllDataAvgWeekPrice <- dfAllDataAvgPrice1Week %>% dplyr::group_by(Year, Trace, Month, MonthWord, Period, DayType) %>% dplyr::summarise(AvgPrice = mean(AvgPrice))

dfAllDataAvgWeekPriceFilter <- dfAllDataAvgWeekPrice %>% filter(Year == 2024, Trace == 1, Month %in% c(6,7,8,9))

## Add a field so we can plot 2 periods per day
dfAllDataAvgWeekPrice$PeriodAsPartOfDay <- ifelse(dfAllDataAvgWeekPrice$Period == "Off-peak", nStartHourOffPeak/24, nEndHourOffPeak/24)

## Add a field 
dfAllDataAvgWeekPrice$PeriodAsPartOfWeek <- ifelse(dfAllDataAvgWeekPrice$DayType == "Weekday",1,2) + dfAllDataAvgWeekPrice$PeriodAsPartOfDay

## Figure 12 - Average On-peak and off peak price for first week selected months
ggplot(dfAllDataAvgWeekPrice %>% filter(Year == 2024, Trace == 1, Month %in% c(6,7,8,9)), aes(x = PeriodAsPartOfWeek, y = AvgPrice, color = as.factor(MonthWord))) +
 
  geom_step(direction = "hv", size = 2) +
  #geom_point(aes(shape = as.factor(MonthWord)), size = 5) +
  
  #Show on-peak, off peak
  #geom_point(aes(y = 20, marker = Period), size = 2) +
  
  #facet_wrap(~ Month) +
  
  scale_color_manual(values = cColorsToPlot[seq(3,9,2)]) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  #scale_x_continuous(limits = c(1.25,3.25), breaks = c(1.25,1.58,2.25,2.58), labels = c("Weekday\nOff-peak", "Weekday\nOn-peak", "Weekend\nOff-peak", "Weekend\nOn-peak")) +
  scale_x_continuous(limits = c(1.25,2.9), breaks = c(mean(c(1.25,1.58)), mean(c(1.58,2.25)), mean(c(2.25,2.58)), mean(c(2.58,2.95))), labels = c("Weekday\nOff-peak", "Weekday\nOn-peak", "Weekend\nOff-peak", "Weekend\nOn-peak")) +
  
    
  #Make one combined legend
  #guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="Day of the Week", y = "Average Period Price\n($/MW-hr)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=nMainSize), legend.title = element_blank(), legend.text=element_text(size=nLegendSize), axis.text.x = element_text(size=nAxisTickSize))


## Push the Weekend / Weekday on- and off-peak prices to a table
dfWeekShortNarrow <- dfAllDataAvgWeekPrice %>% filter(Year == 2024, Trace == 1, Month %in% seq(3,10)) %>% select(Year, Month, MonthWord, Period, DayType, AvgPrice)
## Reshape to Wide so we are looking at On-peak and Off-peak prices

## Remove "-" In Period field
dfWeekShortNarrow$Period <- ifelse(dfWeekShortNarrow$Period == "On-peak", "OnPeak", "OffPeak")
# Shorten decimal point to 1
dfWeekShortNarrow$AvgPrice <- round(dfWeekShortNarrow$AvgPrice, digits = 1)

# Pivot to Wide
dfWeekShortWide <- dfWeekShortNarrow %>% pivot_wider(names_from = c(DayType, Period), values_from = AvgPrice)
dfWeekShortWide <- dfWeekShortWide[, c("Month", "MonthWord", "Weekday_OffPeak", "Weekday_OnPeak", "Weekend_OffPeak", "Weekend_OnPeak")]
#Export the Wide format to CSV
write.csv(dfWeekShortWide, "dfWeekShortAvgPriceWide.csv")
kable(dfWeekShortWide)
