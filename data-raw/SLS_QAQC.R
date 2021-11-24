# This script contains ALL QAQC steps for the SLS survey. The product of this
# script will be a datatable with all of the outliers for the ES in charge
# to check over. Fixing these potential outliers are up to the ES in charge and is manual.

# libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)
library(stringr)

# Reading in the base tables ----------------------------------------------
# This script should be ran after creating the databases script. I will assume
# that the rds file is also created during that process and will only import
# that file here. It contains all of the individual base .csv files in one

data <- readRDS(file.path("data-raw", "SLS", "SLSTables.rds"))

# Empty list to house the outliers in
outliers <- list()

# Going to assume that if the survey is currently running, will want to look at the current
# season's data. Season for SLS will span from Dec-Mar of each WY

yearOfInterest <- year(today()) + (month(today()) > 11)

# Now moving on to checking outliers 
# Cable outliers ----------------------------------------------------------

# Equivalent to "Edit - Cable Out (outliers)

# If CableOut is NOT standard during a tow with bottom depth > 5, return as outlier
# There is limited to the most recent season year only

# Ctrl + shift + c to comment a block

# outliers$CableOut <- full_join(data$`Water Info`, data$`Tow Info`,
#                    by = c("Date", "Station")) %>%
#   # Need to create a seasonYear column to look at data from current season
#   # This historically was not needed since surveys only ran from Jan-Mar, but not
#   # SLS also surveys in Dec
#   transmute(Date = as.Date(Date),
#             Survey, Station, Tow, BottomDepth, CableOut) %>%
#   filter(!CableOut %in% c(45, 60, 75, 90, 110, 125, 140, 155),
#          BottomDepth > 5)

# Equivalent to "Edit - Cable out VS depth"

# For each CableOut, there is a range of accompnaying BottomDepth. If outside that combo,
# return as an outlier
# This is limited to the most recent season year only

outliers$CableDepth <- full_join(data$`Water Info`, data$`Tow Info`, 
                        by = c("Date", "Station")) %>% 
  transmute(Date = as.Date(Date),
            Survey, Station, Tow, BottomDepth, CableOut) %>% 
  arrange(Date) %>% 
  # Why are the ranges overlapping...?
  # Ask Adam ####
  filter(BottomDepth > 39 & !CableOut %in% c(140, 155) |
           BottomDepth > 34 & BottomDepth < 40 & !CableOut %in% c(140, 125, 155) |
           BottomDepth > 29 & BottomDepth < 35 & !CableOut %in% c(140, 125, 110) |
           BottomDepth > 24 & BottomDepth < 30 & !CableOut %in% c(125, 110, 90) |
           BottomDepth > 19 & BottomDepth < 25 & !CableOut %in% c(110, 90, 75) |
           BottomDepth > 14 & BottomDepth < 20 & !CableOut %in% c(90, 75, 60) |
           BottomDepth > 9 & BottomDepth < 15 & !CableOut %in% c(75, 60, 45) |
           BottomDepth > 4 & BottomDepth < 10 & !CableOut %in% c(60, 45))

# Meter readings ----------------------------------------------------------

# Equivalent to "Edit - Meter Reading Out of Range"
# Ask Adam ####

# Values = field tested
outliers$MeterReading <- full_join(data$`Water Info`, data$`Tow Info`,
                            by = c("Date", "Station")) %>% 
  select(Date, Survey, Station, Tow, Duration, 
         CBMeterCheck, NetMeterCheck, contains("Comments")) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter((Duration %in% 2.5 & (NetMeterCheck < 2500 | NetMeterCheck > 12000)) |
           (Duration %in% 5 & (NetMeterCheck < 5000 | NetMeterCheck > 15000)) |
           (Duration %in% 10 & (NetMeterCheck < 10000 | NetMeterCheck > 30000))) %>% 
  arrange(Date)

# Equivalent to "Edit - Net Meter Serial"
# What's the purpose of this script...?
# Ask Adam ####

outliers$NetMeterSerial <- full_join(data$`Water Info`, data$`Tow Info`,
                            by = c("Date", "Station")) %>% 
  transmute(Year = year(Date), 
            Survey, 
            NetMeterSerial) %>% 
  filter(Year %in% yearOfInterest) %>% 
  group_by(Year, Survey, NetMeterSerial) %>% 
  count(name = "CountOfNetMeterSerial")

# Equivalent to "Edit - Tow duration (outliers)"
# Why join to Water Info?
# Ask Adam ####

outliers$TowDuration <- full_join(data$`Water Info`, data$`Tow Info`,
                         by = c("Date", "Station")) %>% 
  filter(!Duration %in% c(2.5, 5, 10)) %>% 
  mutate(Date = as.Date(Date))

# Physical parameters -----------------------------------------------------

# Equivalent to Edit – Bottom Depth III

# First part is Bottom Depth I, where the 2*(standard deviation) is calculated for bottom depth
# NAs in: 340, 602, 711, 716, 723, 812, 815, 906, 910, 912, 919
# Ask Adam ####
outliers$BottomDepth <- data$`Tow Info` %>% 
  group_by(Station) %>% 
  mutate(# This is part 1 of the query
         meanBottomDepth = mean(BottomDepth, na,rm = T), ########### fix this #############
         sd2down = meanBottomDepth - 2 * sd(BottomDepth),
         sd2up = meanBottomDepth + 2 * sd(BottomDepth),
         countStation = n(),
         # This is part 2 of the query
         # Why this 12-31 cut off...? Ask Adam ####
         Outlier = ifelse((BottomDepth < sd2down | BottomDepth > sd2up) & Date > as.Date("2012-12-31"), T, F)) %>% 
  arrange(Date) %>% 
  # This is part 3 of the query
  select(Date, Station, BottomDepth, meanBottomDepth, sd2down, sd2up, Outlier) %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Outlier == T)
# Create a flag column for NAs

# Change to per annotation
# Edit – Temp III
# Edit – Top EC III
# Edit – Bottom EC III
# Edit – Secchi III
# Going to just do bottom EC, top EC, Temp, and Secchi in the same data frame here
outliers <- data$`Water Info` %>% 
  group_by(Station) %>% 
  mutate(Date = as.Date(Date),
         # This is step 1
         across(c(Temp, TopEC, BottomEC, Secchi, Turbidity), ~mean(.x), .names = "mean{.col}"),
         across(c(Temp, TopEC, BottomEC, Secchi, Turbidity), ~mean(.x) - 2*sd(.x), .names = "sd2down{.col}"),
         across(c(Temp, TopEC, BottomEC, Secchi, Turbidity), ~mean(.x) + 2*sd(.x), .names = "sd2up{.col}"),
         # May need to check this n; right for some entries but wrong for others, like due to NAs
         across(c(Temp, TopEC, BottomEC, Secchi, Turbidity), ~n(), .names = "count{.col}"),
         TempOutlier = ifelse((Temp < sd2downTemp | Temp > sd2upTemp) & Date > as.Date("2017-12-31"), T, F),
         TopECOutlier = ifelse((TopEC < sd2downTopEC | TopEC > sd2upTopEC) & Date > as.Date("2018-12-31"), T, F),
         BottomECOutlier = ifelse((BottomEC < sd2downBottomEC | BottomEC > sd2upBottomEC) & Date > as.Date("2019-12-31"), T, F),
         SecchiOutlier = ifelse((Secchi < sd2downSecchi | Secchi > sd2upSecchi) & Date > as.Date("2017-1-1"), T, F),
         # There is no turbidity outlier in SLS
         TurbidityOutlier = ifelse((Turbidity < sd2downTurbidity | Turbidity > sd2upTurbidity) & Date > as.Date("2012-12-31"), T, F)) %>% 
  filter(if_any(contains("Outlier"), ~.x == T)) %>% 
  {lapply(c("Temp", "TopEC", "BottomEC", "Secchi", "Turbidity"), 
          function(x) select(., Date, Survey, Station, contains(x)) %>% 
            rename(Outlier = contains("Outlier")) %>% 
            filter(Outlier == T))} %>% 
  setNames(c("Temp", "TopEC", "BottomEC", "Secchi", "Turbidity")) %>% 
  c(outliers, .) 

# Edit – GPS Coordinates
# Try to map this out
  # Station label, survey #

# Are there no catch queries?
# QAQC FL
# Any other columns?


# Why not find outliers based on survey/Station? This would provide more resolution per survey
# Try per survey/station
# are these too many flags? how many?

# Do you want just the filters or no? Relevant for the physical variables
stop()

writexl::write_xlsx(outliers, "outliers.xlsx")
