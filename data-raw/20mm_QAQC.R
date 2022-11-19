# This script contains ALL requested QAQC steps for the 20 mm survey. The product of this
# script will be a data table with all of the outliers for the ES in charge
# to check over. Fixing these potential outliers is up to the ES in charge and is a manual process.

# libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(leaflet)
library(geosphere)
library(httr)
library(rvest)
library(readr)

# Reading in the base tables ----------------------------------------------
# This script should be ran after creating the databases script. I will assume
# that the rds file is also created during that process and will only import
# that file here. It contains all of the individual base .csv files in one

data <- readRDS(file.path("data-raw", "20mm", "TTmmTables.rds"))

# Empty list to house the outliers in
outliers <- list()

# When this analysis is up to date, will likely only want to see data from
# the current season. Currently, the season for 20 mm spans from Mar to ~July
# Also adding a second requirement that the latest data entry is later than Mar of current year

yearOfInterest <- (year(today()) - 1) + (month(today()) > 3 & max(data$Survey$SampleDate) > as.Date(paste0(year(today()), "-03-01")))

# Make sure that the file names/table names are the same as what will be called here. They NEED to be:
# "Catch", "Lengths", "MeterCorrections", "Station_Lookup", "TowInfo", and "WaterInfo"
# Order of names listed in expectedNames variable does not matter
expectedNames <- list.files(file.path("data-raw", "20mm"), pattern = "*.csv") %>% 
  gsub(".csv", "", .)

if (!all(names(data) %in% expectedNames)) {
  stop("Table name(s) ", shQuote(names(data)[which(!names(data) %in% expectedNames)]),
       " do/does not have the defaults.")
}

# Load the helper functions
source("R/gpsHelpers.R")

# Now moving on to checking outliers 
# The calculations below are those that were requested by Adam on 11-9-2021

# The "NAFlag" column will flag values with NAs;
# However, these NAs will be ignored when calculating mean and sd, as is default behavior in Access
# NAFlag will be present across all "queries"
# Outlier column will also be present across all "queries"

# Plotting the GPS coordinates --------------------------------------------

# This section is in response to the "Edit – GPS Coordinates" query. Adam stated that this query is used to allow
# the ES in charge to quickly visualize the locations of the tows for the season.
# Overall motivation of this section: plot out the locations to visually help determine outliers; also, use
# geodistance calculations to cluster potential outliers to potentially flag outliers

GPSDF <- data$Station %>% 
  # Both Station and Survey have a "comments" column; will fix this to keep unique table it came from
  # Changing name to be consistent with the SLS version of this script, for LatLon
  rename(Comments.Station = Comments, 
         LatD = LatDeg, LatM = LatMin, LatS = LatSec,
         LonD = LonDeg, LonM = LonMin, LonS = LonSec) %>% 
  full_join(data$Survey %>% 
              rename(Comments.Survey = Comments), by = "SurveyID") %>% 
  # Converting the GPS coordinates from H/M/S to degrees
  mutate(across(c(LatD, LatM, LatS, LonD, LonM, LonS), ~as.numeric(.x)),
         Lat = LatD + LatM/60 + LatS/3600,
         Long = -(LonD + LonM/60 + LonS/3600), # Creating season year to help with the plotting function below
         Year = year(SampleDate),
         # group here differentiates between the tow coordinates (where the tows actually take place) vs the
         # theoretical
         group = "Tow") %>% 
  # Now binding to lon/lat of the 20 mm stations; these will serve as the "average"
  # coordinates that these stations potentially should be at and will serve as a visual
  # comparison to where the tow coordinates are
  bind_rows(data$StationCords %>% 
              mutate(LatD = sapply(strsplit(.$Lat, "\\s"), "[", 1),
                     LatM = sapply(strsplit(.$Lat, "\\s"), "[", 2),
                     LatS = sapply(strsplit(.$Lat, "\\s"), "[", 3),
                     LonD = sapply(strsplit(.$Long, "\\s"), "[", 1),
                     LonM = sapply(strsplit(.$Long, "\\s"), "[", 2),
                     LonS = sapply(strsplit(.$Long, "\\s"), "[", 3),
                     across(c(LatD, LatM, LatS, LonD, LonM, LonS), as.numeric)) %>% 
              transmute(Station,
                        Lat = LatD + LatM/60 + LatS/3600,
                        Long = -(LonD + LonM/60 + LonS/3600),
                        group = "TheoreticalCoords"))

# First, we can plot the points
# The plotGPS() function is housed in the gpsHelper script that is read in above
# There are several arguments to this function:
# year, survey, station
# You can also filter beforehand and not specify any filter arguments
plotGPS(GPSDF, year = yearOfInterest)

# Edit – GPS Coordinates
# You can use the map to help identify outliers. There is a survey toggle. You can also click
# on each point to display a pop up menu with additional information on the selected station

# If instead you want to automatically find outliers, you can use the gpsOutlier function
# This function will automatically flag points that are > d miles away from the theoretical
# lat/lon for that station as an outlier; by default, d is set to 0.5 miles

outliers$GPS <- gpsOutlier(GPSDF, d = 0.5, year = yearOfInterest, returnDF = T)

# Cable outliers ----------------------------------------------------------

# For 20mm, the joining keys are autonumbered ID columns. To get date, the Survey dataset has to be joined.
# Since this step is repeated before every query, will simply define a table with these joins:
# Survey, Station, and Tow dataframes together
surveyStation <- full_join(data$Survey,
                           data$Station,
                           by = "SurveyID")

surveyStationTow <- full_join(surveyStation, data$Tow, 
                              by = c("StationID"))

# Each of query is a replication of the access version. As such, what is outputted here should be the same
# as the Access version; this following function will compare that
# Need to be connected to access for this


# Begin QAQC queries ------------------------------------------------------

# Equivalent to "Edit - Cable Out (outliers) ------------------------------

# What the script does: If CableOut is NOT standard during a tow with bottom depth > 5, return as outlier
# Removing this entire query as it is redundant compared to the Cable out VS depth query (approved by Adam)

# Equivalent to "Edit - Cable out vs Depth" -------------------------------

# What the script does: For each CableOut, there is a range of accompanying BottomDepth. If outside that combo,
# return as an outlier

outliers$CableDepth <- surveyStationTow %>% 
  transmute(Year = year(SampleDate),
            SampleDate = as.Date(SampleDate),
            Survey, Station, TowNum, BottomDepth, CableOut,
            Comments.Station = Comments.y) %>% 
  arrange(SampleDate) %>% 
  # Ranges overlap due to bythemetry not always consistent that may need to be accounted for; 
  # boat operators should try and follow bypthemetry
  mutate(Outlier = ifelse(BottomDepth > 31 & !CableOut %in% 225 |
                            BottomDepth > 26 & BottomDepth < 32 & !CableOut %in% 200 |
                            BottomDepth > 21 & BottomDepth < 27 & !CableOut %in% 175 |
                            BottomDepth > 17 & BottomDepth < 22 & !CableOut %in% 150 |
                            BottomDepth > 13 & BottomDepth < 18 & !CableOut %in% 125 |
                            BottomDepth > 10 & BottomDepth < 14 & !CableOut %in% 100 |
                            BottomDepth < 11 & !CableOut %in% 75, 
                          T, F),
         # If Tow, BottomDepth, or CableOut columns are missing row values, flag them
         NAFlag = ifelse(is.na(TowNum) | is.na(BottomDepth) | is.na(CableOut),
                         T, F)) %>% 
  filter(Outlier == T | NAFlag == T,
         # Only care about outliers this season
         Year %in% yearOfInterest) %>% 
  arrange(Outlier, SampleDate, Station)

# Meter readings ----------------------------------------------------------

# Equivalent to "Edit - Net meter reading out of range"
# What the script does: Check values of duration and flow meter readings; those beyond these thresholds
# are outliers; these values thresholds = field tested

outliers$MeterReading <- surveyStationTow %>% 
  full_join(data$Gear, by = "TowID") %>% 
  select(SampleDate, ends_with("ID"), Survey, Station, TowNum, Duration, 
         MeterCheck, GearCode, contains("Comments")) %>% 
  mutate(Year = year(SampleDate),
         Outlier = ifelse((Duration %in% 5 & (MeterCheck < 5000 | MeterCheck > 15000)) & GearCode %in% 2 |
                            (Duration %in% 10 & (MeterCheck < 10000 | MeterCheck > 30000) & GearCode %in% 2),
                          T, F),
         NAFlag = ifelse(is.na(Duration) | is.na(MeterCheck), T, F)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(Outlier, SampleDate, Station)

# Equivalent to "Edit - CB meter out of range" ----------------------------

# What the script does: Check values of duration and flow meter readings; those beyond these thresholds
# are outliers; these values thresholds = field tested

outliers$CB_MeterReading <- surveyStationTow %>% 
  full_join(data$Gear, by = "TowID") %>% 
  select(SampleDate, ends_with("ID"), Survey, Station, TowNum, Duration, 
         MeterCheck, GearCode, contains("Comments")) %>% 
  mutate(Year = year(SampleDate),
         Outlier = ifelse((Duration %in% 5 & (MeterCheck < 2500 | MeterCheck > 15000)) & GearCode %in% 1 |
                            (Duration %in% 10 & (MeterCheck < 5000 | MeterCheck > 25000) & GearCode %in% 1),
                          T, F),
         NAFlag = ifelse(is.na(Duration) | is.na(MeterCheck), T, F)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(Outlier, SampleDate, Station)

# Equivalent to "Edit - Net Meter Serial" ---------------------------------

# plot out flow meter checks, by diff flow meter serials; to determine when a certain flow meter failed during a season

outliers$NetMeterSerial <- surveyStationTow %>% 
  full_join(data$Gear, by = "TowID") %>% 
  transmute(Year = year(SampleDate), 
            Survey, 
            MeterSerial,
            GearCode) %>% 
  filter(GearCode %in% 2,
         Year %in% yearOfInterest) %>% 
  group_by(Year, Survey, MeterSerial, GearCode) %>% 
  count(name = "CountOfMeterSerial")

# Equivalent to "Edit - Tow duration (outliers)" --------------------------

# What the script does: find data points where duration is NOT equal to 2.5, 5, or 10 min; these are
# the standard tow lengths for this survey

outliers$TowDuration <- surveyStationTow %>% 
  mutate(Year = year(SampleDate),
         Outlier = ifelse(!Duration %in% c(2.5, 5, 10),
                          T, F),
         NAFlag = ifelse(is.na(Duration),
                         T, F)) %>% 
  select(SampleDate, Survey, Year, ends_with("ID"), Station,
         TowNum, Duration, contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest)
# Checked 02-01-22 by TN with original query. across all years, OK

# Physical parameters -----------------------------------------------------

# What the script does: IMPORTANT: this is applicable to ALL the physical predictors below, so
# BottomDepth, BottomDepthMonth, Temp, TempMonth, TopEC, TopECMonth, BottomEC, BottomECMonth,
# Secchi, SecchiMonth, Turbidity, and TurbidityMonth
# Basically, the script find entries that are beyond the 2 stdevs of the mean, so the 2 and 98 percentiles;
# Flags values that are beyond ~95% of the all other entries. This is per station, across ALL months for
# the non "Month" variants and is PER month for the "Month" variants. Motivation of including the
# month variants is due to the seasonality of these predictors and the need to be slightly more sensitive
# to the general conditions of these predictors across time, e.g., water temperature in Dec will on average
# be lower than those in March.

# For these scripts, the first part is always to find the mean and stdev of the
# environmental variable of interest. Since there are 3 tows per lane, there are duplications
# of these values that is not consistently 3 and can throw off calculations if
# this isn't accounted for. So, writing a small function here to calculate mean/std
# of the variable you want without joining to the tow function to avoid duplication problems
findMeanAndStdev <- function(data, variable, stdev = 2, byMonth = F) {
  
  # This is equivalent to step 1 in the edit queries, finding mean and std of data
  variable <- enquo(variable)
  
  if (byMonth) {
    data %>%
      group_by(Station, 
               Month = month(SampleDate)) %>%
      summarise(Mean = mean(!!variable, na.rm = T),
                SdTwoDown = Mean - stdev * sd(!!variable, na.rm = T),
                SdTwoUp = Mean + stdev * sd(!!variable, na.rm = T),
                .groups = "drop")
  } else {
    data %>%
      group_by(Station) %>%
      summarise(Mean = mean(!!variable, na.rm = T),
                SdTwoDown = Mean - stdev * sd(!!variable, na.rm = T),
                SdTwoUp = Mean + stdev * sd(!!variable, na.rm = T),
                .groups = "drop")
  }
  # countStation = n(), # Removing this portion of the code; it is a part of query 1 but not used elsewhere
}

# Equivalent to "Edit - Bottom Depth (outliers)"

outliers$BottomDepth <- surveyStationTow %>% 
  full_join(findMeanAndStdev(surveyStationTow, 
                             variable = BottomDepth),
            by = "Station") %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((BottomDepth < SdTwoDown | BottomDepth > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomDepth), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Survey, Station, BottomDepth, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)
# BottomDepthMonth

outliers$BottomDepthMonth <- surveyStationTow %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(surveyStationTow, 
                             variable = BottomDepth,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((BottomDepth < SdTwoDown | BottomDepth > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomDepth), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Month, Survey, Station, BottomDepth, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit - Temp (outliers)
outliers$Temp <- surveyStationTow %>% 
  full_join(findMeanAndStdev(full_join(data$Survey,
                                       data$Station,
                                       by = "SurveyID"), 
                             variable = Temp),
            by = "Station") %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Temp < SdTwoDown | Temp > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Temp), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Survey, Station, Temp, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# TempMonth
outliers$TempMonth <- surveyStationTow %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = Temp,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Temp < SdTwoDown | Temp > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Temp), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Month, Survey, Station, Temp, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit - Top EC (outliers)
outliers$TopEC <- surveyStationTow %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = TopEC),
            by = "Station") %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < SdTwoDown | TopEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(TopEC), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Station, TopEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# TopECMonth
outliers$TopECMonth <- surveyStationTow %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = TopEC,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < SdTwoDown | TopEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(TopEC), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Month, Survey, Station, TopEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Bottom EC III
outliers$BottomEC <- surveyStationTow %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = BottomEC),
            by = "Station") %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < SdTwoDown | BottomEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomEC), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Survey, Station, BottomEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# BottomECMonth
outliers$BottomECMonth <- surveyStationTow %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = BottomEC,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < SdTwoDown | BottomEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomEC), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Month, Survey, Station, BottomEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Secchi III
outliers$Secchi <- surveyStationTow %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = Secchi),
            by = "Station") %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < SdTwoDown | Secchi > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Secchi), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Survey, Station, Secchi, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# SecchiMonth
outliers$SecchiMonth <- surveyStationTow %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = Secchi,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < SdTwoDown | Secchi > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Secchi), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Month, Survey, Station, Secchi, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# There is no equivalent Turbidity script in Access for 20 mm
outliers$Turbidity <- surveyStationTow %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = Turbidity),
            by = "Station") %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < SdTwoDown | Turbidity > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Turbidity), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Survey, Station, Turbidity, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# checks$Turbidity <- checkQueries(outliers$Turbidity, "Edit - Turbidity (outliers)")

# TurbidityMonth
outliers$TurbidityMonth <- surveyStationTow %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(surveyStation, 
                             variable = Turbidity,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(TowTime = as.POSIXct(paste(SampleDate, format(TowTime, "%H%M")), 
                              format = "%Y-%m-%d %H%M", tz = "America/Los_Angeles")) %>% 
  arrange(TowTime) %>% 
  group_by(SampleDate, Survey, Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < SdTwoDown | Turbidity > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Turbidity), T, F),
         # Only want TowTime for the first tow since env conditions are only recorded once
         TowTime = first(TowTime)) %>% 
  ungroup() %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, TowTime, SurveyID, StationID, Year, Month, Survey, Station, Turbidity, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)


# Adding CDEC station info to the Temp, ec, and turbidity outlier  --------
source("R/cdecHelpers.R")

# Prep to populate CDEC data to a table of outliers
# Read in a table of CDEC stations assigned to each station
cdec <- read_csv("data-raw/CDEC20mm.csv",
                 col_types = cols(
                   `20-mm Station` = col_double(),
                   `CDEC station 1` = col_character(),
                   `CDEC station 2` = col_character(),
                   `CDEC station 3` = col_character(),
                   Comment = col_character()
                 )) %>% 
  rename(Station = `20-mm Station`,
         first = `CDEC station 1`,
         second = `CDEC station 2`,
         third = `CDEC station 3`,
         CDECcomment = Comment)

# Function to pull in metadata of all stations in the CDEC data table
metadata <- cdec %>% 
  pivot_longer(c(first, second, third),
               names_to = "priority", values_to = "gage") %>% 
  filter(!is.na(gage)) %>% 
  distinct(gage) %>% 
  pull(gage) %>% 
  # This function is sourced from cdecHelpers.R
  pullMetadataCDEC()

# Update these outlier data frames from original with these new ones. A more effective way to do this is to
# actually pipe the populate CDEC function right into the QAQC scripts. However, doing this here in case
# ES in charge do not want to run this additional step and only want the QAQC output.
outliers[which(names(outliers) %in% c("Temp", "TempMonth", "TopEC", "TopECMonth", 
                                      "BottomEC", "BottomECMonth", "Turbidity", "TurbidityMonth"))] <- 
  mapply(popCDEC, 
         df = list(outliers$Temp, outliers$TempMonth, outliers$TopEC, outliers$TopECMonth, 
                   outliers$BottomEC, outliers$BottomECMonth, outliers$Turbidity, outliers$TurbidityMonth),
         variable = c("temp", "temp", "ec", "ec", "ec", "ec", "turbidity", "turbidity"),
         waterColumn = c("top", "top", "top", "top", "lower", "lower", "top", "top"),
         MoreArgs = list(cdec = cdec, metadata = metadata)) %>% 
  setNames(c("Temp", "TempMonth", "TopEC", "TopECMonth", "BottomEC", "BottomECMonth", "Turbidity", "TurbidityMonth"))

# Saving all outlier DFs to an excel sheet for analysis -------------------

# This is a temp name
# Adding the 3 columns that Adam requested for the ES in charge to fill in to document result of analysis
saveSheet <- lapply(outliers, function(x) mutate(x,
                                                 IsOutlier = NA, ChangedTo = NA, CommentsOutlier = NA,
                                                 # Strange XML character in some rows of the comments column in the
                                                 # Water Info table; removing this so that the excel file saves correctly
                                                 across(where(is.character), ~str_replace_all(.x, "\uFFFD", ""))))

# Saving the file; this will save to the current directory, which by default is in the parent folder; will change this later
writexl::write_xlsx(saveSheet, file.path("data-raw", "Outliers", "20mm", paste0("TMM_outliers_", gsub(" |:", "_", now()), ".xlsx")))
