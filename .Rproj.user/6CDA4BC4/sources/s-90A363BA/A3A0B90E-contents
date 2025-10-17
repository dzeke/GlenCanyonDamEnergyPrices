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
#     David E. Rosenberg
#     October 15, 2025
#     david.rosenberg@usu.edu

#
#####


rm(list = ls())  #Clear history

#Load packages in one go
  #List of packages
  load.lib <- c("tidyverse", "readxl", "RColorBrewer", "dplyr", "expss", "reshape2", "pracma", "lubridate", "directlabels", "plyr", "stringr", "ggplot2")
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
#Calculate Off-peak (hours 0 to 7) and Off-peak (hours 8 to 23)
dfAllData$Period <- ifelse(dfAllData$Hour <= 7, "Off-peak", "On-peak")

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

cColorsToPlot <- brewer.pal(9, "Blues")
#cColorsToPlot <- colorRampPalette((brewer.pal(9, "Blues"))(10 - 3 + 1))

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
  theme(text = element_text(size=20), legend.title = element_text("Month"), legend.text=element_text(size=14), axis.text.x = element_text(size=16))


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
  theme(text = element_text(size=20), legend.title = element_text("Month"), legend.text=element_text(size=14), axis.text.x = element_text(size=16))

print(wday("2024-08-01"), label = FALSE)

### Figure 3 - daily time series August
# Insight - Year only bumps up price a small amount. Weekly pattern repeats
# Insight - March, April, May, and June all have prices between $20 and $100/MW-hour.
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
  theme(text = element_text(size=20), legend.title = element_text("Month"), legend.text=element_text(size=14), axis.text.x = element_text(size=16))

### Figure 4 - daily time series August
# Insight - Year only bumps up price a small amount. Weekly pattern repeats
# Insight - March, April, May, and June all have prices between $20 and $100/MW-hour.
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
  theme(text = element_text(size=20), legend.title = element_text("Month"), legend.text=element_text(size=14), axis.text.x = element_text(size=16))

