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
#             So the generation data frame has Columns of [Year][Month][Trace][Hour][Generation]
#             So  the Econ data frame has Columns of [Year][Month][Trace][Hour][Value]
#
#       5. Join the two tables on Month, Trace, and Hour so we have a new data frame with columns [Year][Month][Trace][Hour][Generation][Value]
#
#       6. Divide the Value column by Generation column to get a Price in $/MW-hour
#
#       7. Plot the pricing data in different formats.
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
#             So  the Econ data frame has Columns of [Year][Month][Trace][Hour][Value]
#



# Rename the columns/variables to the annual flow amount

nAnnualVolumes <- c(7, 7.48, 8.23, seq(9.0, 17.5, 0.5), 18, 20, 30, 50, 75,100)
colnames(dfMonthlyRelease) <- as.character(nAnnualVolumes)
#Remove the 100 column
#dfMonthlyRelease <- subset(dfMonthlyRelease, select = -c("100"))

#Add column for numeric month
dfMonthlyRelease$Month <- c(10, 11, 12, seq(1,9,1))
#Add column for text month
dfMonthlyRelease$MonthTxt <- month.abb[dfMonthlyRelease$Month]

#Reshape wide format (annual releases as columns) to long
dfMonthlyReleaseLong <- reshape(data = dfMonthlyRelease, 
                                idvar = c("Month","MonthTxt"), 
                                varying = as.character(nAnnualVolumes), 
                                v.names = "Monthly Release", 
                                timevar = "Annual Release Volume",
                                times = as.character(nAnnualVolumes),
                                #new.row.names = 1:(length(nAnnualVolumes)*12)
                                direction="long")

#Convert Annual Release Volume to numeric
dfMonthlyReleaseLong$`Annual Release Volume` <- as.numeric(dfMonthlyReleaseLong$`Annual Release Volume`)
dfMonthlyReleaseLong$`Annual Release VolumeMAF` <- as.factor(as.numeric(dfMonthlyReleaseLong$`Annual Release Volume`))

#Plot by month


cColorsToPlot <- colorRampPalette((brewer.pal(9, "Blues")))(length(nAnnualVolumes))

#### Figure 1 - Lines as 

ggplot() +
  #Data after 1989
  geom_line(data = dfMonthlyReleaseLong %>% filter(`Annual Release Volume` < 20), aes(x=Month , y=`Monthly Release`/1e6, color=`Annual Release VolumeMAF`), size=1.5) +
  theme_bw() +
  
  scale_color_manual(values = cColorsToPlot) +
  #scale_linetype_manual(values = c("solid","longdash")) +
  
  scale_x_continuous(1, 12, breaks = seq(1,12,1), labels = month.abb[seq(1,12,1)]) +
  
  #Make one combined legend
  guides(color = guide_legend(""), linetype = guide_legend("")) +
  
  theme_bw() +
  
  labs(x="Month", y="Release Volume\n(MAF per mo nth)", color="Annual Release Volume") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), legend.title = element_text("Annual Release\nMAF"), legend.text=element_text(size=14), axis.text.x = element_text(size=12))

  


