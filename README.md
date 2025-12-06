# How do energy prices at Glen Canyon Dam change by hour and month?

This repository has the purpose to answer the question: How do energy prices at Glen Canyon Dam change by hour and month?

We use data from Bair, L., and Yackulic, C. (2024). "Predicted hydropower impacts of different management scenarios for Lake Powell releases." U.S. Geological Survey data release. https://doi.org/10.5066/P135BOD8.

## More about the dataset

This dataset includes release, energy generation (MW-hour), and economic value of releases (nominal $ per hour) by hour of the day for Months from October 2023 to November 2027.
Thus the data include peak hour of the day and peak hour of the week release, energy generation, and economic value.

The data were used to estimate the economic effects
of releases through Glen Canyon Dam to disrupt spawning of small mouth bass as part of a Supplemental Environmental Impact Statement 
(SEIS) in 2023. The SEIS covers the period November 2023 to November 2027.

The data are organized into separate folders titled "econ" "flow" and "generation." Each folder has 9 csv files representing current operations (_noaction)
and 8 other flow scenarios. Each csv file is organized into 1501 rows and 744 columns as follows:

1. Rows are Month and 30 hydrologic traces. For example, the first 30 rows are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of October 2023. Rows 31-60 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2023
   Rows 1471-1500 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2027.
2. Columns are the 744 hours in a month (hour_1, hour_2, ... hour_744).

Data for week 1 of the month (hours 1 to 168) are representative and thus replicated over weeks 2 to 4 of the month.

Our analysis focuses on the No Action Alternative (econ_hourly_noaction.csv, flow_hourly_noaction.csv, and generation_hourly_noaction.csv) as we mostly interested
in energy prices which are the same across all the alternatives. Additionally, we are interested in the econ and economic value data as we
use these two values to compute the energy price ($ per MW-hour).

## Data wrangling strategy

1. Read in the generation data for the No action alternative in the folder generation/generation_hourly_noaction.csv [MW-hour].
This  FIRST file represents hourly generation at Glen Canyon Dam in megawatt hours by hour, month and hydrologic trace in the LTEM sEIS. The purpose of these data tables are to allow for a comparison of the difference in generation between LTEMP eEIS alternatives. Columns are hours in a month. Rows are month and 30 hydrologic traces. For example, the first 30 rows are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of October 2023. Rows 31-60 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2023. Rows 1471-1500 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2027. The 11 data tables are separate 1500 by 744 matrices. The months with days less than 31 days contain "0" entries for those hours and days.</enttypd>
           Columns hour_1 to hour_744 in the data table represents megawatt hours by hour, month and hydrologic trace in the LTEM sEIS. Months with days less than 31 days contain "0" entries for those hours and days.</attrdef>

1. Read in the economic data for the No action alternative in the folder econ/econ_hourly_noaction.csv
           The SECOND file represents the economic value of energy generated at Glen Canyon Dam in nominal dollars by hour, month and hydrologic trace in the LTEM sEIS. The purpose of these data tables are to allow for a comparison of the difference in economic value between LTEMP sEIS alternatives. Columns are hours in a month. Rows are month and 30 hydrologic traces. For example, the first 30 rows are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of October 2023. Rows 31-60 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2023. Rows 1471-1500 are the 30 hydrologic traces in the LTEMP sEIS (1991-2020) for the month of November 2027. The 11 data tables are separate 1500 by 744 matrices. The months with days less than 31 days contain "0" entries for those hours and days.</enttypd>
            Columns hour_1 to hour_744 in the data table represent economic value by hour, month and hydrologic trace in the LTEM sEIS.</attrdef>
  
1. Add row labels to differentiate each row (Month and Year and scenario) since the original data have no row label.

1. Convert the Generation and Econ data frames to Narrow format. So the generation data frame has Columns of [Year][Month][Trace][HourAsText][Generation]
So  the Econ data frame has Columns of [Year][Month][Trace][HourAsText][Value]

1. Join the two tables on Year, Month, Trace, and HourAsText so we have a new data frame with columns [Year][Month][Trace][HourAsText][Generation][Value]

1. Convert the HourAsText from 1 to 744 to numerical hour, Calculate the day of month, hour of day, and on-peak/off-peak for each row.

1. Divide the Value column by Generation column to get a Price in $/MW-hour. Set rows with Zero generation to NA

1. Plot the pricing data in different formats.

## Plots
1. Box and Whiskers of variation in prices for each month. This includes all scenarios and all weeks/days/hours/years.
1. Time series of prices for all years all months, years overlaid on each other.
1. Time series of prices for all traces for the first two weeks of August, all years overlaid on each other.
1. Time series of generation for all traces for the first two weeks of August 2024.
1. Time series of economic value for all traces for the first two weeks of August 2024, There is difference here, although all traces have the same shape, just different magnitudes.
1. Time-series showing on-peak prices highlighted in blue and off-peak prices highlighted in red for a single week in August 2024. Note, these on-, off-peak definitions are for our study and look to be different that what is used in the current data set.
1. Time-series showing the first week of prices each month.
1. Time-series of prices showing the first week of June, July, August, and September for Year 2024, Trace 1.
1. Time series of first week of July 2024 showing on-peak and off-peak prices
1. Period average on-peak and off-peak prices for first week of June, July, August and September. 



## Findings
1. Energy prices, including peak hour of the week prices are less than $100/MW-hour for months March, April, May, and June. For July through October, the predominate prices are also less than $100/MW-hour with a few
outliers greater than $100/MW-hr up to $325/MW-hr (August) (Figure 1).
1. There is a small increase in energy prices year-to-year. i.e., ~ $10/MW-hr increase from 2024 to 2027 (Figure 2).
1. There is no variation in energy prices across the 30 traces (as expected, Figure 3). Peak prices are also on the 3rd and 4th day of the week. Week 2 prices are the same as week 1 prices (also as expected since the 1st week is representative and prices are copied over to weeks 2 to 4).
1. Energy generation for traces do vary in magnitude but have the same overall shape. Again, week 2 generation is the same as week 1 generation (Figure 4).
1. Economic value for traces also differ in magnitude but have the same overall shape. Again, week 2 generation is the same as week 1 generation (Figure 5).
1. Figure 6 - come back to
1. Results largely confirm Figure 1 findings with March, April, May, and June all prices below $100/MW-hour. And a few peak-hour-of-the week prices greater than $150/MW-hour for remaining month of July to October.
1. Weekly prices for Months June, July, August and September have the same weekly pattern, just different magnitudes with August having the highest prices (Figure 8).
1. On-peak prices exceed $100/MW-hour for only 3 or 4 hours each day (Figure 9)
1. On-peak average prices only exceed $100/MW-hour during 3 days per week for the first week of August and 1 day per week for the first week in September (Figure 10). All other peak peridos are less than $100/MW-hour and in many months less than $75/MW-hour.

## View Results
Open the file **[GCDEnergyPrice.pdf](GCDEnergyPrice.pdf)**

## Requirements to Run
* R version 4.1.1. Download from https://cran.r-project.org/.
* R Studio 1.1.456. Download from https://www.rstudio.com/.

## Directions to Reproduce Results
1. Download and install R and RStudio (see requirements)
1. Within this subfolder, open the **PowelleMonthlyRelease.Rproject** file. R Studio should open.
1. Select the **PowelleMonthlyRelease.Rmd** tab (R markdown file) within R Studio.
1. Just below the tab, click the **Knit** button.
1. The code will run and generate the file **PowellMonthlyRelease.pdf**. Open the pdf file to view results.

## Explanation of Contents
1. **GCDEnergyPrice.pdf** - Output file created when knit **PowellMonthlyRelease.Rmd** within R Studio.
1. **GCDEnergyPrice.Rmd** - R markdown file with code to knit (run) to generate primary output file **PowellMonthlyRelease.pdf**.
1. **GCDEnergyPrice.r** - R file with same code as **GCDEnergyPrice.Rmd** but pushes results to console. Use for testing code.
1. **PowellMonthlyRelease.Rproject** - R project file. Use to open the project in R Studio.
1. **Powell-MonthlyReleaseSchedule.txt** - Comma seperated values (CSV) file with data downloaded from CRSS slot Powell.MonthlyReleaseTable. Rows are month of the year. Columns are annual release target. All values million acre-feet.
1. **Energy_Rates_2014.xlsx** - Excel file with Contract and market prices by month used in this study, a summary of all months, and comparison to period average prices from the data by Bair and Yackulic (2024). 
1. **econ** -- Folder containing data on the economic value (Nominal $$) for all scenarios, months, years, and hour of the month.
1. **flow** -- Folder containing data on the Glen Canyon dam releases for all scenarios, months, years, and hour of the monnth.
1. **generation** -- Folder containing data on the energy generation (MW-hour) of all scenarios, months, years, and hour of the monnth.

## Requested Citation
David E. Rosenberg (2025), “How do energy prices at Glen Canyon Dam change by hour and month?” Utah State University. Logan, Utah.
https://github.com/dzeke/GlenCanyonDamEnergyPrices.

