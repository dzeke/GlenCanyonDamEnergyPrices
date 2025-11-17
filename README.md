# Glen Canyon Dam Energy Prices

This repository has the purpose to answer the question: How do energy prices at Glen Canyon Dam change by hour and month?

We use data from Bair, L., and Yackulic, C. (2024). "Predicted hydropower impacts of different management scenarios for Lake Powell releases." U.S. Geological Survey data release. https://doi.org/10.5066/P135BOD8.

This dataset includes release, energy generation (MW-hour), and economic value of releases (nominal $ per hour). The data were used to estimate the economic effects
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

The general data wrangling strategy is:

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
#
#     Plots:
#       1. Box and Whiskers of variation in prices for each month. This includes all scenarios and all weeks/days/hours.
#       2. Time series of prices for all years all months, years overlaid on each other.
#       3. Time series of prices for all years for the first two weeks of August
#       4. Time series of prices for all traces for the first two weeks of August, 2023. There is no difference.
#       5. Time series of generation for all traces for the first two weeks of August 2023, THere is difference here, although all traces have the same shape, just different magnitudes.
#       6. Time series of economic value for all traces for the first two weeks of August 2023, THere is difference here, although all traces have the same shape, just different magnitudes.
#       7. Time-series showing on-peak prices highlighted in blue and off-peak prices highlighted in red for a single week in August 2024. Note, these on-, off-peak definitions are for our study and look to be different that what is used in the current data set.
#       8. Time series of each month for a single year
#       9. Time series of first week of 4 months 
#       10.Time seriues of first week of 1 month showing off-peak and on-peak periods


## Findings
1. The high release volume months are January, February, March, June, July, and August.

## Requested Citation
David E. Rosenberg (2025), “How do energy prices at Glen Canyon Dam change by hour and month.” Utah State University. Logan, Utah.
https://github.com/dzeke/GlenCanyonDamEnergyPrices.

## View Results
Open the file **[PowellMonthlyRelease.pdf](PowellMonthlyRelease.pdf)**

## Requirements to Run
* R version 4.1.1. Download from https://cran.r-project.org/.
* R Studio 1.1.456. Download from https://www.rstudio.com/.

## Directions to Generate Results
1. Download and install R and RStudio (see requirements)
1. Within this subfolder, open the **PowelleMonthlyRelease.Rproject** file. R Studio should open.
1. Select the **PowelleMonthlyRelease.Rmd** tab (R markdown file) within R Studio.
1. Just below the tab, click the **Knit** button.
1. The code will run and generate the file **PowellMonthlyRelease.pdf**. Open the pdf file to view results.

## Explanation of Contents
1. **PowellMonthlyRelease.pdf** - Output file created when knit **PowellMonthlyRelease.Rmd** within R Studio.
1. **PowellMonthlyRelease.Rmd** - R markdown file with code to knit (run) to generate primary output file **PowellMonthlyRelease.pdf**.
1. **PowellMonthlyRelease.r** - R file with same code as **PowellMonthlyRelease.Rmd** but pushes results to console. Use for testing code.
1. **PowellMonthlyRelease.Rproject** - R project file. Use to open the project in R Studio.
1. **Powell-MonthlyReleaseSchedule.txt** - Comma seperated values (CSV) file with data downloaded from CRSS slot Powell.MonthlyReleaseTable. Rows are month of the year. Columns are annual release target. All values million acre-feet.

## Requested Citation
David E. Rosenberg (2020). "Powell Monthly Releases". Utah State University. Logan, Utah. https://github.com/dzeke/ColoradoRiverCollaborate/tree/main/PowellMonthlyRelease.
