# This script contains ALL requested QAQC steps for the SLS survey. The product of this
# script will be a data table with all of the outliers for the ES in charge
# to check over. Fixing these potential outliers is up to the ES in charge and is a manual process.

# libraries ---------------------------------------------------------------

library(dplyr, warn.conflicts = F)
library(readr)
library(tidyr)
library(lubridate, warn.conflicts = F)
library(stringr)
library(leaflet)
library(geosphere)
library(httr)
library(rvest, warn.conflicts = F)

# Reading in the base tables ----------------------------------------------
# This script should be ran after creating the databases script. I will assume
# that the rds file is also created during that process and will only import
# that file here. It contains all of the individual base .csv files in one

data <- readRDS(file.path("data-raw", "SLS", "SLSTables.rds"))

# Empty list to house the outliers in
# This is a stylistic preference. I believe that it is more efficient to house the data in this way
# It keeps the environment cleaner and allows you to run apply functions through each element in the list
outliers <- list()

# When this analysis is up to date, you will likely only want to see data from
# the current season. Currently, the season for SLS spans from Dec-Mar of each WY
# Also adding a second requirement that the latest data entry is later than Dec 1 of current year
# To find yearOfInterest, take today and add a 1 to it IF the current season is underway, 
# so if today is AFTER Dec 1 AND if there is data entered that is AFTER Dec 1 as well.

yearOfInterest <- year(today()) + (month(today()) > 11 & max(data$WaterInfo$Date) > as.Date(paste0(year(today()), "-12-01")))

# Make sure that the file names/table names are the same as what will be called here. They NEED to be:
# "Catch", "Lengths", "MeterCorrections", "Station_Lookup", "TowInfo", "FishCodes", and "WaterInfo"
# Order of names listed in expectedNames variable does not matter
expectedNames <- c("Catch", "Lengths", "MeterCorrections", "TowInfo", "WaterInfo", "Station_Lookup", "FishCodes")

# This is a simple check to stop the script (if the user opt to run the entire script in its entirety)
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

GPSDF <- data$WaterInfo %>% 
  # Converting the GPS coordinates from H/M/S to degrees
  mutate(# Across here is a function that applies a function ACROSS the specified columns (StartLat, StartLong here)
         # To these columns, remove "." and "-"
         across(c(StartLat, StartLong), ~str_remove(.x, "\\.") %>% str_remove_all("\\-")),
         # str_sub to pull digits from the StartLat column based on their character index
         StartLatD = str_sub(StartLat, start = 1, end = 2),
         StartLatM = str_sub(StartLat, start = 3, end = 4),
         # Very specific pattern in in sub and involves knowledge of "regex"
         # Essentially, in StartLat, pull data from position 5 on, find the first 2 digits
         # replace that with the first two digits you just matched (\\1), a period (.), and the rest
         # of the match (\\2)
         StartLatS = sub("(.{2})(.*)", "\\1.\\2", str_sub(StartLat, start = 5)),
         StartLonD = str_sub(StartLong, start = 1, end = 3),
         StartLonM = str_sub(StartLong, start = 4, end = 5),
         StartLonS = sub("(.{2})(.*)", "\\1.\\2", str_sub(StartLong, start = 6)),
         # Converting StartLat/StartLong to numeric
         across(c(StartLatD, StartLatM, StartLatS, StartLonD, StartLonM, StartLonS), ~as.numeric(.x)),
         # Now finally converting to StartLat/StartLong in 
         StartLat = StartLatD + StartLatM/60 + StartLatS/3600,
         StartLong = -(StartLonD + StartLonM/60 + StartLonS/3600),
         # Creating season year to help with the plotting function below
         Year = year(Date) + (month(Date) > 11),
         # This group column is to differentiate between the coordinates recorded on the actual tows
         # vs the theoretical below
         group = "Tow") %>% 
  rename(Comments.Station = Comments) %>% 
  # Now binding to lon/StartLat of the 20 mm stations; these will serve as the "average"
  # coordinates that these stations potentially should be at and will serve as a visual
  # comparison to where the tow coordinates are
  bind_rows(data$Station_Lookup %>% 
              mutate(# Current structure of the StartLat/StartLong is each component separated by spaces: pull that out
                     LatD = sapply(strsplit(.$Lat, "\\s"), "[", 1),
                     LatM = sapply(strsplit(.$Lat, "\\s"), "[", 2),
                     LatS = sapply(strsplit(.$Lat, "\\s"), "[", 3),
                     LonD = sapply(strsplit(.$Long, "\\s"), "[", 1),
                     LonM = sapply(strsplit(.$Long, "\\s"), "[", 2),
                     LonS = sapply(strsplit(.$Long, "\\s"), "[", 3),
                     across(c(LatD, LatM, LatS, LonD, LonM, LonS), as.numeric)) %>% 
              transmute(Station,
                        # Not truly the theoretical start and ending coordinates but more so the
                        # center coordinates.
                        StartLat = LatD + LatM/60 + LatS/3600,
                        StartLong = -(LonD + LonM/60 + LonS/3600),
                        group = "TheoreticalCoords"))

# This step will be in two parts, using two main functions: plotGPS and findOutlierGPS below:
# The plotGPS() function is housed in the gpsHelper script that is read in above
# There are several arguments to this function:
# year, survey, station
# You can also filter beforehand and not specify any filter arguments
plotGPS(GPSDF, year = yearOfInterest)

gpsOutlier <- function(df, d = 0.5, station = NULL, year = NULL, survey = NULL,
                       returnDF = F) {
  
  if (!is.null(survey)) df <- filter(df, Survey %in% survey | group %in% "TheoreticalCoords")
  
  if (!is.null(year)) df <- filter(df, Year %in% year | group %in% "TheoreticalCoords")
  
  if (!is.null(station)) df <- filter(df, Station %in% station)
  
  theoretical <- df %>% 
    filter(group %in% "TheoreticalCoords")
  
  outlierDF <- lapply(unique(df$Station), function(x) {
    tows <- df %>% 
      filter(group %in% "Tow", Station == x)
    
    theoretical <- df %>% 
      filter(group %in% "TheoreticalCoords", Station == x)
    
    if (nrow(theoretical) > 0 & nrow(tows) > 0) {
    
      df <- tows %>%
        mutate(longTheoretical = theoretical$StartLong,
               latTheoretical = theoretical$StartLat,
               distance = distVincentyEllipsoid(cbind(StartLong, StartLat),
                                                cbind(longTheoretical, latTheoretical))/1609.34,
               outlier = ifelse(distance > d, T, F),
               NAflag = ifelse((is.na(StartLat) | is.na(StartLong)), T, F)) %>% 
        filter(outlier == T | NAflag == T)
    } else df <- NULL
    
    df
  }) %>% 
    bind_rows()
  
  fin <- outlierDF %>% 
    bind_rows(df %>% 
                filter(group %in% "TheoreticalCoords",
                       Station %in% unique(outlierDF$Station)))
  
  if (isTRUE(returnDF)) {
    fin %>% 
      transmute(Date, Year, Survey, Station, 
                Comments.Station,
                group, StartLat, StartLong, longTheoretical, latTheoretical,
                distance, outlier, NAflag)
  } else {
    message("Plotting all outlying coordinates.")
    plotGPS(df = fin)
  }
}

# # Leave returnDF as F to plot just the outlying points
# gpsOutlier(GPSDF, d = 0.5, year = yearOfInterest)
outliers$GPS <- gpsOutlier(GPSDF, d = 0.5, year = yearOfInterest, returnDF = T)

# Cable outliers ----------------------------------------------------------

# Equivalent to "Edit - Cable Out (outliers)

# What the script does: If CableOut is NOT standard during a tow with bottom depth > 5, return as outlier
# Removing this entire query as it is redundant compared to the Cable out VS depth query (approved by Adam)

# Equivalent to "Edit - Cable out VS depth"

# What the script does: For each CableOut, there is a range of accompanying BottomDepth. If outside that combo,
# return as an outlier

outliers$CableDepth <- full_join(data$WaterInfo, data$TowInfo, 
                                 by = c("Date", "Station")) %>% 
  transmute(Date = as.Date(Date),
            Survey, Station, Tow, BottomDepth, CableOut,
            Year = year(Date),
            SeasonYear = year(Date) + (month(Date) > 11)) %>% 
  arrange(Date) %>% 
  # Ranges overlap due to bythemetry not always consistent that may need to be accounted for; 
  # boat operators should try and follow bypthemetry
  mutate(Outlier = ifelse(BottomDepth > 39 & !CableOut %in% c(140, 155) |
                            BottomDepth > 34 & BottomDepth < 40 & !CableOut %in% c(140, 125, 155) |
                            BottomDepth > 29 & BottomDepth < 35 & !CableOut %in% c(140, 125, 110) |
                            BottomDepth > 24 & BottomDepth < 30 & !CableOut %in% c(125, 110, 90) |
                            BottomDepth > 19 & BottomDepth < 25 & !CableOut %in% c(110, 90, 75) |
                            BottomDepth > 14 & BottomDepth < 20 & !CableOut %in% c(90, 75, 60) |
                            BottomDepth > 9 & BottomDepth < 15 & !CableOut %in% c(75, 60, 45) |
                            BottomDepth > 4 & BottomDepth < 10 & !CableOut %in% c(60, 45), 
                          T, F),
         # If Tow, BottomDepth, or CableOut columns are missing row values, flag them
         NAFlag = ifelse(is.na(Tow) | is.na(BottomDepth) | is.na(CableOut),
                         T, F)) %>% 
  filter(Outlier == T | NAFlag == T,
         # Only care about outliers this season
         SeasonYear %in% yearOfInterest) %>% 
  arrange(Outlier, Date, Station)
# Join to the water info table is needed to get "survey"
# Checked 11-24-21 by TN that this matches the DB query, by summing each column of resulting DF
# 12-6-21; changing this to Outlier column format == still same output as 11-24-21 BEFORE adding in values with
# NAFlag; Adding in the NAFlag column will mean the final table will have more flagged values than on 11-24-21 (thus on Access DB)

# Meter readings ----------------------------------------------------------

# Equivalent to "Edit - Meter Reading Out of Range"
# What the script does: Check values of duration and flow meter readings; those beyond these thresholds
# are outliers; these values thresholds = field tested

outliers$MeterReading <- full_join(data$WaterInfo, data$TowInfo,
                                   by = c("Date", "Station")) %>% 
  # Did remove CBMeterCheck, which is in the Access query. This CB column is a relic of the 20 mm survey that was ported over
  select(Date, Survey, Station, Tow, Duration, 
         NetMeterCheck, contains("Comments")) %>% 
  mutate(Date = as.Date(Date),
         SeasonYear = year(Date) + (month(Date) > 11),
         Outlier = ifelse((Duration %in% 2.5 & (NetMeterCheck < 2500 | NetMeterCheck > 12000)) |
                            (Duration %in% 5 & (NetMeterCheck < 5000 | NetMeterCheck > 15000)) |
                            (Duration %in% 10 & (NetMeterCheck < 10000 | NetMeterCheck > 30000)),
                          T, F),
         NAFlag = ifelse(is.na(Duration) | is.na(NetMeterCheck), T, F)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(Outlier, Date, Station)
# Checked 11-24-21 by TN that this matches the DB query, small enough to check that all is equal
# Checked 12-06-21 by TN that addition of Outlier and NAFlag does not break code

# Equivalent to "Edit - Net Meter Serial"
# plot out flow meter checks, by diff flow meter serials; to determine when a certain flow meter failed during a season

outliers$NetMeterSerial <- full_join(data$WaterInfo, data$TowInfo,
                                     by = c("Date", "Station")) %>% 
  transmute(Year = year(Date), 
            Survey, 
            NetMeterSerial) %>% 
  filter(Year %in% yearOfInterest) %>% 
  # Opting to keep the groupings here. This specific query relies on the Year being present
  group_by(Year, Survey, NetMeterSerial) %>% 
  count(name = "CountOfNetMeterSerial") 

# I believe this script is used for other analyses by Native Fishes. Will keep for now
# Checked 11-24-21 by TN that this matches the DB query, small enough to check that all is equal
# # From conversations with Adam, these meter values are used to potentially plot out when a meter
# # goes bad in the season. The following code attempts to plot that
# full_join(data$WaterInfo, data$TowInfo,
#           by = c("Date", "Station")) %>% 
#   transmute(Date, 
#             Year = year(Date), 
#             Survey, 
#             NetMeterSerial,
#             NetMeterCheck) %>% 
#   filter(Year %in% 2021) %>% 
#   ggplot(aes(Date, NetMeterCheck)) +
#   geom_point() +
#   facet_wrap(~NetMeterSerial, nrow = 3)
# # Don't really see how this would be used; likely interpreted Adam wrong.

# Equivalent to "Edit - Tow duration (outliers)"

# What the script does: find data points where duration is NOT equal to 2.5, 5, or 10 min; these are
# the standard tow lengths for this survey

outliers$TowDuration <- full_join(data$WaterInfo, data$TowInfo,
                                  by = c("Date", "Station")) %>% 
  mutate(Date = as.Date(Date),
         SeasonYear = year(Date) + (month(Date) > 11),
         Outlier = ifelse(!Duration %in% c(2.5, 5, 10),
                          T, F),
         NAFlag = ifelse(is.na(Duration),
                         T, F)) %>% 
  select(Date, SeasonYear, Station, Tow, Duration, contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest)
# water info is used here to grab the comment section from the water info table
# Checked 11-24-21 by TN that this matches the DB query, both queries returned 0 rows
# These 0s are true across ALL years. 

# Physical parameters -----------------------------------------------------

# Equivalent to Edit – Bottom Depth III

# What the script does: IMPORTANT: this is applicable to ALL the physical predictors below, so
# BottomDepth, BottomDepthMonth, Temp, TempMonth, TopEC, TopECMonth, BottomEC, BottomECMonth,
# Secchi, SecchiMonth, Turbidity, and TurbidityMonth
# Basically, the script find entries that are beyond the 2 stdevs of the mean, so the 2 and 98 percentiles;
# Flags values that are beyond ~95% of the all other entries. This is per station, across ALL months for
# the non "Month" variants and is PER month for the "Month" variants. Motivation of including the
# month variants is due to the seasonality of these predictors and the need to be slightly more sensitive
# to the general conditions of these predictors across time, e.g., water temperature in Dec will on average
# be lower than those in March.

outliers$BottomDepth <- data$TowInfo %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanBottomDepth = mean(BottomDepth, na.rm = T),
         sd2down = meanBottomDepth - 2 * sd(BottomDepth, na.rm = T),
         sd2up = meanBottomDepth + 2 * sd(BottomDepth, na.rm = T),
         # countStation = n(), # Removing this portion of the code; it is a part of query 1 but not used elsewhere
         # This is part 2 of the query
         Outlier = ifelse((BottomDepth < sd2down | BottomDepth > sd2up), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomDepth), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(Date, SeasonYear, Station, BottomDepth, meanBottomDepth, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)
# Checked 11-24-21 by TN that this matches the DB query, summation of the numeric columns equal

# BottomDepthMonth

outliers$BottomDepthMonth <- data$TowInfo %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanBottomDepth = mean(BottomDepth, na.rm = T),
         sd2down = meanBottomDepth - 2 * sd(BottomDepth, na.rm = T),
         sd2up = meanBottomDepth + 2 * sd(BottomDepth, na.rm = T),
         # countStation = n(), # Removing this portion of the code; it is a part of query 1 but not used elsewhere
         # This is part 2 of the query
         Outlier = ifelse((BottomDepth < sd2down | BottomDepth > sd2up), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomDepth), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for the NAFlag column
  select(Date, SeasonYear, SeasonYear, Month, Station, BottomDepth, meanBottomDepth, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Temp III
outliers$Temp <- data$WaterInfo %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTemp = mean(TopTemp, na.rm = T),
         sd2down = meanTemp - 2 * sd(TopTemp, na.rm = T),
         sd2up = meanTemp + 2 * sd(TopTemp, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopTemp < sd2down | TopTemp > sd2up), T, F),
         NAFlag = ifelse(is.na(TopTemp), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, TopTemp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# TempMonth
outliers$TempMonth <- data$WaterInfo %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTemp = mean(TopTemp, na.rm = T),
         sd2down = meanTemp - 2 * sd(TopTemp, na.rm = T),
         sd2up = meanTemp + 2 * sd(TopTemp, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopTemp < sd2down | TopTemp > sd2up), T, F),
         NAFlag = ifelse(is.na(TopTemp), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, TopTemp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Top EC III
outliers$TopEC <- data$WaterInfo %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTopEC = mean(TopEC, na.rm = T),
         sd2down = meanTopEC - 2 * sd(TopEC, na.rm = T),
         sd2up = meanTopEC + 2 * sd(TopEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < sd2down | TopEC > sd2up), T, F),
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, TopEC, meanTopEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# TopECMonth
outliers$TopECMonth <- data$WaterInfo %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTopEC = mean(TopEC, na.rm = T),
         sd2down = meanTopEC - 2 * sd(TopEC, na.rm = T),
         sd2up = meanTopEC + 2 * sd(TopEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < sd2down | TopEC > sd2up), T, F),
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, TopEC, meanTopEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Bottom EC III
outliers$BottomEC <- data$WaterInfo %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanBottomEC = mean(BottomEC, na.rm = T),
         sd2down = meanBottomEC - 2 * sd(BottomEC, na.rm = T),
         sd2up = meanBottomEC + 2 * sd(BottomEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < sd2down | BottomEC > sd2up), T, F),
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, BottomEC, meanBottomEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# BottomECMonth
outliers$BottomECMonth <- data$WaterInfo %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanBottomEC = mean(BottomEC, na.rm = T),
         sd2down = meanBottomEC - 2 * sd(BottomEC, na.rm = T),
         sd2up = meanBottomEC + 2 * sd(BottomEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < sd2down | BottomEC > sd2up), T, F),
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, BottomEC, meanBottomEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Secchi III
outliers$Secchi <- data$WaterInfo %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanSecchi = mean(Secchi, na.rm = T),
         sd2down = meanSecchi - 2 * sd(Secchi, na.rm = T),
         sd2up = meanSecchi + 2 * sd(Secchi, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < sd2down | Secchi > sd2up), T, F),
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, Secchi, meanSecchi, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# SecchiMonth
outliers$SecchiMonth <- data$WaterInfo %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanSecchi = mean(Secchi, na.rm = T),
         sd2down = meanSecchi - 2 * sd(Secchi, na.rm = T),
         sd2up = meanSecchi + 2 * sd(Secchi, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < sd2down | Secchi > sd2up), T, F),
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, Secchi, meanSecchi, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# This isn't found in the most current version of the database, but was in one of the iterations
# Wil leave this here regardless.
# Edit – Turbidity III
outliers$Turbidity <- data$WaterInfo %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTurbidity = mean(FNU, na.rm = T),
         sd2down = meanTurbidity - 2 * sd(FNU, na.rm = T),
         sd2up = meanTurbidity + 2 * sd(FNU, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((FNU < sd2down | FNU > sd2up), T, F),
         NAFlag = ifelse(is.na(FNU), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, FNU, meanTurbidity, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# TurbidityMonth
outliers$TurbidityMonth <- data$WaterInfo %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTurbidity = mean(FNU, na.rm = T),
         sd2down = meanTurbidity - 2 * sd(FNU, na.rm = T),
         sd2up = meanTurbidity + 2 * sd(FNU, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((FNU < sd2down | FNU > sd2up), T, F),
         NAFlag = ifelse(is.na(FNU), T, F)) %>% 
  ungroup() %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, FNU, meanTurbidity, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

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
         MoreArgs = list(dfTime = data$TowInfo, cdec = cdec, metadata = metadata)) %>% 
  setNames(c("Temp", "TempMonth", "TopEC", "TopECMonth", "BottomEC", "BottomECMonth", "Turbidity", "TurbidityMonth"))

# Saving all outlier DFs to an excel sheet for analysis -------------------

# This is a temp name
# Adding the 3 columns that Adam requested for the ES in charge to fill in to document result of analysis
# The use of lapply here will run this mutate operation to all elements of list "outliers" at once
saveSheet <- lapply(outliers, function(x) mutate(x, IsOutlier = NA, ChangedTo = NA, CommentsOutlier = NA,
                                                 # Strange XML character in some rows of the comments column in the
                                                 # Water Info table; removing this so that the excel file saves correctly
                                                 across(where(is.character), ~str_replace_all(.x, "\uFFFD", ""))))

# Saving the file; this will save to the current directory, which by default is in the parent folder; will change this later
writexl::write_xlsx(saveSheet, file.path("data-raw", "Outliers", "SLS", paste0("SLS_outliers_", gsub(" |:", "_", now()), ".xlsx")))
