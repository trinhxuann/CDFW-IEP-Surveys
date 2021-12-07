# This script contains ALL requested QAQC steps for the SLS survey. The product of this
# script will be a data table with all of the outliers for the ES in charge
# to check over. Fixing these potential outliers is up to the ES in charge and is a manual process.

# libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(stringr)
library(leaflet)

# Reading in the base tables ----------------------------------------------
# This script should be ran after creating the databases script. I will assume
# that the rds file is also created during that process and will only import
# that file here. It contains all of the individual base .csv files in one

data <- readRDS(file.path("data-raw", "SLS", "SLSTables.rds"))

# Empty list to house the outliers in
outliers <- list()

# When this analysis is up to date, will likely only want to see data from
# the current season. Currently, the season for SLS spans from Dec-Mar of each WY
# Also adding a second requirement that the latest data entry is later than Dec 1 of current year

yearOfInterest <- year(today()) + (month(today()) > 11 & max(data$`Water Info`$Date) > as.Date(paste0(year(today()), "-12-01")))

# Now moving on to checking outliers 
# The calculations below are those that were requested by Adam on 11-9-2021

# The "NAFlag" column will flag values with NAs;
# However, these NAs will be ignored when calculating mean and sd, as is default behavior in Access
# NAFlag will be present across all "queries"
# Outlier column will also be present across all "queries"

# Cable outliers ----------------------------------------------------------

# Equivalent to "Edit - Cable Out (outliers)

# What the script does: If CableOut is NOT standard during a tow with bottom depth > 5, return as outlier
# Removing this entire query as it is redundant compared to the Cable out VS depth query (approved by Adam)

# Equivalent to "Edit - Cable out VS depth"

# What the script does: For each CableOut, there is a range of accompanying BottomDepth. If outside that combo,
# return as an outlier

outliers$CableDepth <- full_join(data$`Water Info`, data$`Tow Info`, 
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

outliers$MeterReading <- full_join(data$`Water Info`, data$`Tow Info`,
                            by = c("Date", "Station")) %>% 
  select(Date, Survey, Station, Tow, Duration, 
         CBMeterCheck, NetMeterCheck, contains("Comments")) %>% 
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

outliers$NetMeterSerial <- full_join(data$`Water Info`, data$`Tow Info`,
                            by = c("Date", "Station")) %>% 
  transmute(Year = year(Date), 
            Survey, 
            NetMeterSerial) %>% 
  filter(Year %in% yearOfInterest) %>% 
  group_by(Year, Survey, NetMeterSerial) %>% 
  count(name = "CountOfNetMeterSerial")
# I believe this script is used for other analyses by Native Fishes. Will keep for now
# Checked 11-24-21 by TN that this matches the DB query, small enough to check that all is equal
# # From conversations with Adam, these meter values are used to potentially plot out when a meter
# # goes bad in the season. The following code attempts to plot that
# full_join(data$`Water Info`, data$`Tow Info`,
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

outliers$TowDuration <- full_join(data$`Water Info`, data$`Tow Info`,
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

outliers$BottomDepth <- data$`Tow Info` %>% 
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

outliers$BottomDepthMonth <- data$`Tow Info` %>% 
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
  arrange(Date) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for the NAFlag column
  select(Date, SeasonYear, SeasonYear, Month, Station, BottomDepth, meanBottomDepth, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Temp III
outliers$Temp <- data$`Water Info` %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTemp = mean(Temp, na.rm = T),
         sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
         sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
         NAFlag = ifelse(is.na(Temp), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, Temp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# TempMonth
outliers$TempMonth <- data$`Water Info` %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTemp = mean(Temp, na.rm = T),
         sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
         sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
         NAFlag = ifelse(is.na(Temp), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, Temp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Top EC III
outliers$TopEC <- data$`Water Info` %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTopEC = mean(TopEC, na.rm = T),
         sd2down = meanTopEC - 2 * sd(TopEC, na.rm = T),
         sd2up = meanTopEC + 2 * sd(TopEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < sd2down | TopEC > sd2up), T, F),
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, TopEC, meanTopEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# TopECMonth
outliers$TopECMonth <- data$`Water Info` %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTopEC = mean(TopEC, na.rm = T),
         sd2down = meanTopEC - 2 * sd(TopEC, na.rm = T),
         sd2up = meanTopEC + 2 * sd(TopEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < sd2down | TopEC > sd2up), T, F),
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, TopEC, meanTopEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Bottom EC III
outliers$BottomEC <- data$`Water Info` %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanBottomEC = mean(BottomEC, na.rm = T),
         sd2down = meanBottomEC - 2 * sd(BottomEC, na.rm = T),
         sd2up = meanBottomEC + 2 * sd(BottomEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < sd2down | BottomEC > sd2up), T, F),
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, BottomEC, meanBottomEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# BottomECMonth
outliers$BottomECMonth <- data$`Water Info` %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanBottomEC = mean(BottomEC, na.rm = T),
         sd2down = meanBottomEC - 2 * sd(BottomEC, na.rm = T),
         sd2up = meanBottomEC + 2 * sd(BottomEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < sd2down | BottomEC > sd2up), T, F),
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, BottomEC, meanBottomEC, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Secchi III
outliers$Secchi <- data$`Water Info` %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanSecchi = mean(Secchi, na.rm = T),
         sd2down = meanSecchi - 2 * sd(Secchi, na.rm = T),
         sd2up = meanSecchi + 2 * sd(Secchi, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < sd2down | Secchi > sd2up), T, F),
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, Secchi, meanSecchi, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# SecchiMonth
outliers$SecchiMonth <- data$`Water Info` %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanSecchi = mean(Secchi, na.rm = T),
         sd2down = meanSecchi - 2 * sd(Secchi, na.rm = T),
         sd2up = meanSecchi + 2 * sd(Secchi, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < sd2down | Secchi > sd2up), T, F),
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, Secchi, meanSecchi, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Edit – Turbidity III
outliers$Turbidity <- data$`Water Info` %>% 
  group_by(Station) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTurbidity = mean(Turbidity, na.rm = T),
         sd2down = meanTurbidity - 2 * sd(Turbidity, na.rm = T),
         sd2up = meanTurbidity + 2 * sd(Turbidity, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < sd2down | Turbidity > sd2up), T, F),
         NAFlag = ifelse(is.na(Turbidity), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Station, Turbidity, meanTurbidity, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# TurbidityMonth
outliers$TurbidityMonth <- data$`Water Info` %>% 
  group_by(Station, Month = month(Date)) %>% 
  mutate(SeasonYear = year(Date) + (month(Date) > 11),
         # This is part 1 of the query
         meanTurbidity = mean(Turbidity, na.rm = T),
         sd2down = meanTurbidity - 2 * sd(Turbidity, na.rm = T),
         sd2up = meanTurbidity + 2 * sd(Turbidity, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < sd2down | Turbidity > sd2up), T, F),
         NAFlag = ifelse(is.na(Turbidity), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, SeasonYear, Month, Station, Turbidity, meanTurbidity, sd2down, sd2up, Comments, Outlier, NAFlag) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T | NAFlag == T,
         SeasonYear %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, Date)

# Various plots for station vs station/months -----------------------------
# # These are simply exploratory plots to see how the two variants differ from one another
# # Does one variant yield much more "outliers" than others?
# 
# data$`Water Info` %>%
#   group_by(Station, Month = month(Date)) %>%
#   mutate(# This is part 1 of the query
#     meanTemp = mean(Temp, na.rm = T),
#     sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
#     sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
#     # This is part 2 of the query
#     Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
#     NAFlag = ifelse(is.na(Temp), T, F)) %>%
#   arrange(Date) %>%
#   # This is part 3 of the query
#   select(Date, Station, Temp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>%
#   mutate(Date = as.Date(Date)) %>%
#   ggplot(aes(Date, Temp, color = Outlier)) +
#   geom_point() +
#   labs(title = "Grouped by station and month")
# # Seems like for temp, it might be worth it to use a "region" to calculate the mean/sd or exclude certain stations
# # Alot of these outliers also appear to have occurred during the drought, with the higher temps being a bit outlying
# 
# data$`Water Info` %>%
#   group_by(Station) %>%
#   mutate(# This is part 1 of the query
#     meanTemp = mean(Temp, na.rm = T),
#     sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
#     sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
#     # This is part 2 of the query
#     Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
#     NAFlag = ifelse(is.na(Temp), T, F)) %>%
#   arrange(Date) %>%
#   # This is part 3 of the query
#   select(Date, Station, Temp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>%
#   mutate(Date = as.Date(Date)) %>%
#   ggplot(aes(Date, Temp, color = Outlier)) +
#   geom_point() +
#   labs(title = "Grouped by station Only")
# 
# # As expected, the "only station" variant will flag extreme values across all years, so mostly during the drought, while
# # the Month variant will have more subtle outliers that appears to be similar to other datapoints around it when plotted
# # this way
# 
# # How many more outliers are in the monthly variant?
# purrr::map_df(outliers, function(x) data.frame = c(n = nrow(x)), .id = "parameter") %>% 
#   mutate(scale = ifelse(str_detect(parameter, "Month"), "Station, Month", "Station"),
#          parameter = str_remove(parameter, "Month"),
#          parameter = factor(parameter, levels = c(.$parameter))) %>% 
#   ggplot(aes(parameter, n, fill = scale)) + 
#   geom_col(position = "dodge") +
#   labs(title = "Number of outliers similar, more varied on location of outliers")
# # About the same amount of outliers either way; personally think it's very slightly more robust to at least
# # try and account for seasonality if this comparison to the 95% of all values approach is to be kept. There
# # doesn't seem to be to many more flagged values

# Plotting the GPS coordinates --------------------------------------------

# This section is in response to the "Edit – GPS Coordinates" query. Adam stated that this query is used to allow
# the ES in charge to quickly visualize the locations of the tows for the season.
# Overall motivation of this section: plot out the locations to visually help determine outliers; also, use
# geodistance calculations to cluster potential outliers to potentially flag outliers

GPSDF <- data$`Water Info` %>% 
  # Converting the GPS coordinates from H/M/S to degrees
  mutate(across(c(Lat, Long), ~str_remove(.x, "\\.") %>% str_remove_all("\\-")),
         LatD = str_sub(Lat, start = 1, end = 2),
         LatM = str_sub(Lat, start = 3, end = 4),
         LatS = sub("(.{2})(.*)", "\\1.\\2", str_sub(Lat, start = 5)),
         LonD = str_sub(Long, start = 1, end = 3),
         LonM = str_sub(Long, start = 4, end = 5),
         LonS = sub("(.{2})(.*)", "\\1.\\2", str_sub(Long, start = 6)),
         # Converting Lat/Long to numeric
         across(c(LatD, LatM, LatS, LonD, LonM, LonS), ~as.numeric(.x)),
         # Now finally converting to lat/long in 
         Lat = LatD + LatM/60 + LatS/3600,
         Long = -(LonD + LonM/60 + LonS/3600),
         # Creating season year to help with the plotting function below
         SeasonYear = year(Date) + (month(Date) > 11),
         group = "Tow") %>% 
  # Now binding to lon/lat of the 20 mm stations; these will serve as the "average"
  # coordinates that these stations potentially should be at and will serve as a visual
  # comparison to where the tow coordinates are
  
  # CHANGE THIS TABLE TO STATION LOOKUP TABLE# ####
  bind_rows(data$`20mm Stations` %>% 
              transmute(Station,
                        Lat = LatD + LatM/60 + LatS/3600,
                        Long = -(LonD + LonM/60 + LonS/3600),
                        group = "20mm Stations"))

# This step will be in two parts, using two main functions: plotGPS and findOutlierGPS below:

# Use plotGPS to visually determine where outliers may be
plotGPS <- function(df, station = NULL, Year = NULL, ...) {
  
  pal <- colorFactor("viridis", domain = c(df$group))
  
  if (is.null(Year)) {
    warning("This will plot all tows in the requested data table and may take a substantial amount of resources to complete.", call. = F)
  } else {
    df <- df %>% 
      filter(SeasonYear == Year | group == "20mm Stations")
  }
  
  leaflet(df,
          width = "100%",
          height = "500") %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(~Long, ~Lat,
                     label = ~as.character(Station),
                     color = ~pal(group),
                     radius = 4,
                     stroke = F, fillOpacity = 0.8,
                     labelOptions = labelOptions(noHide = T,
                                                 offset = c(18,0),
                                                 textOnly = T,
                                                 textsize = "12px",
                                                 direction = "center")) %>% 
    addLegend(pal = pal, values = ~group, opacity = 1, ...)
}

# Example: this plots ALL stations across a year of interest; Year can be left blank if
# you want all stations across all years plotted (will be slow though)
plotGPS(GPSDF, Year = 2021, title = "Source")

# Use findOutlierGPS after running plotGPS and visually determining which stations/year may have 

# Finding distance to run the clustering analysis on
# Function taken from https://stackoverflow.com/questions/21095643/approaches-for-spatial-geodesic-latitude-longitude-clustering-in-r-with-geodesic
geo.dist = function(df) {
  require(geosphere)
  d <- function(i,z){         # z[1:2] contain long, lat columns
    dist <- rep(0,nrow(z))    # Creates placeholder to begin populating array
    dist[i:nrow(z)] <- distVincentyEllipsoid(z[i:nrow(z),1:2],z[i,1:2]) # Calculates distances between each points
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(df),d,df)) # Building the array of distances between each points
  return(as.dist(dm))
}

# Function to analyze for outliers based on spatial distances and hierarchical clustering;
# if k is specified, will also plot the clusters 
findOutlierGPS <- function(df, station = NULL, Year = NULL, 
                           k = NULL, print = T) {
  
  if (!is.null(station)) df <- filter(df, Station %in% station)
  
  if (!is.null(Year)) df <- filter(df, SeasonYear %in% Year)
  
  d <- geo.dist(select(df, Long, Lat) %>% 
                  na.omit())
  hc <- hclust(d)
  if (print) plot(hc)
  
  if (!is.null(k)) {
    # Specifying the cluster
    dfClust <- df %>% 
      filter(!is.na(Long), !is.na(Lat)) %>% 
      mutate(group = cutree(hc, k))
    
    if (print) print(plotGPS(dfClust, station = station, Year = Year, title = "Cluster"))
    
    dfClust %>% 
      arrange(-group)
  }
}

# Edit – GPS Coordinates
# These are stations that I see that may be outlying; 
# k was chosen after visual inspection of the produced dendogram
# print = F used AFTER confirming with the visualizations that this is what you want

GPSOutlying <- list()
GPSOutlying[[1]] <- findOutlierGPS(GPSDF, station = 912, k = 2, print = F)
GPSOutlying[[2]] <- findOutlierGPS(GPSDF, station = 609, k = 2, print = F)
GPSOutlying[[3]] <- findOutlierGPS(GPSDF, station = 606, k = 2, print = F)
GPSOutlying[[4]] <- findOutlierGPS(GPSDF, station = 610, k = 5, print = F)
GPSOutlying[[5]] <- findOutlierGPS(GPSDF, station = 520, k = 2, print = F)
GPSOutlying[[6]] <- findOutlierGPS(GPSDF, station = 915, k = 3, print = F)

# For multiple stations, can bind these together into a singular data frame
# Would envision all clusters other than 1 would be classified as outliers, something like:
outliers$GPSCoordinates <- lapply(GPSOutlying, function(x) filter(x, group != 1)) %>% 
  bind_rows() %>% 
  mutate(Outlier = T) %>% 
  bind_rows(data$`Water Info` %>% 
              # This gives a warning of NAs introduced by coercion; can ignore given that these are NAs to start with
              # Will simply supress this because it is not a valid warning
              filter(is.na(Lat) | is.na(Long)) %>% 
              mutate(across(c(Lat, Long), ~suppressWarnings(as.numeric(.x))),
                     NAFlag = T,
                     Outlier = F)) %>% 
  mutate(NAFlag = ifelse(is.na(NAFlag), F, NAFlag))

# # QAQC FL -----------------------------------------------------------------
# 
# data$Lengths %>% 
#   group_by(Month = month(Date),
#            FishCode) %>% 
#   mutate(# This is part 1 of the query
#     meanLengths = mean(Length, na.rm = T),
#     sd2down = meanLengths - 2 * sd(Length, na.rm = T),
#     sd2up = meanLengths + 2 * sd(Length, na.rm = T),
#     # This is part 2 of the query
#     # Why this 12-31 cut off...? Ask Adam ####
#     Outlier = ifelse((Length < sd2down | Length > sd2up) & Date > as.Date("2012-12-31"), T, F),
#     NAFlag = ifelse(is.na(Length), T, F)) %>% 
#   arrange(Date) %>% 
#   # This is part 3 of the query
#   select(Date, Month, FishCode, Length, meanLengths, sd2down, sd2up, Outlier, NAFlag) %>% 
#   mutate(Date = as.Date(Date)) %>% 
#   filter(Outlier == T | NAFlag == T) %>% 
#   arrange(NAFlag, Outlier, Date) %>% 
#   View()
# 
# # Turkey's fence, 1.5 and 3.0
# data$Lengths %>% 
#   group_by(Month = month(Date),
#            FishCode) %>% 
#   mutate(# This is part 1 of the query
#     quant25 = quantile(Length, probs = 0.25, na.rm = T),
#     quant75 = quantile(Length, probs = 0.75, na.rm = T),
#     iqr = quant75 - quant25,
#     Outlier = ifelse((Length < quant25 - (iqr * 3) | Length > quant75 + (iqr * 3)) & Date > as.Date("2012-12-31"), T, F),
#     NAFlag = ifelse(is.na(Length), T, F)) %>% 
#   arrange(Date) %>% 
#   # This is part 3 of the query
#   select(Date, Month, FishCode, Length, quant25, quant75, iqr, Outlier, NAFlag) %>% 
#   mutate(Date = as.Date(Date)) %>% 
#   filter(Outlier == T | NAFlag == T) %>% 
#   arrange(NAFlag, Outlier, Date) %>% 
#   View()
# # Much harder with fish data lol


# Saving all outlier DFs to an excel sheet for analysis -------------------

# This is a temp name
# Adding the 3 columns that Adam requested for the ES in charge to fill in to document result of analysis
saveSheet <- lapply(outliers, function(x) mutate(x, IsOutlier = NA, ChangedTo = NA, CommentsOutlier = NA,
                                               # Strange XML character in some rows of the comments column in the
                                               # Water Info table; removing this so that the excel file saves correctly
                                               across(where(is.character), ~str_replace_all(.x, "\uFFFD", ""))))

# Saving the file; this will save to the current directory, which by default is in the parent folder; will change this later
writexl::write_xlsx(saveSheet, file.path("data-raw", "Outliers", "SLS", paste0("SLS_outliers_", today(), ".xlsx")))
