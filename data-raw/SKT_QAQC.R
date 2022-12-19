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

# Prevent scientific notation
options(scipen = 99999999)
        
# SKT data has been collected for the season. I am now sitting down to do the end of the year QAQC.
# This involves several sequential steps:

# 1. Output the individual relational tables from Access to R to prep the queries
# 2. Run the queries to output the potential outliers
# 3. Fix outleirs. This has to be done manually by the ES****
# 4. Output the individual relational tables to a flat file format again. All outliers are fixed at this point
# 5. Prep for publication to EDI

# Connecting to Access db -------------------------------------------------

source("~/Git/CDFW-IEP-Surveys/R/bridgeAccess.R")

# If you are working from the UDrive file, make sure you are on VPN if not in the office.
# bridgeAccess(file = "C:\\Users\\txnguyen\\Desktop\\SKT_Query.accdb", 
#              script = "~/Git/CDFW-IEP-Surveys/R/connectAccess.R")

data <- bridgeAccess(file = "C:\\Users\\txnguyen\\Desktop\\SKT_Query.accdb", 
                     tables = c("tblSample", "tblCatch", "tblFishInfo", 
                                "tblOrganismCodes", "tblSexLookUp", 
                                "SKT Station Sort Order for Reporting"),
                     script = "~/Git/CDFW-IEP-Surveys/R/connectAccess.R")

# Recreating QAQC scripts -------------------------------------------------
dateOfInterest <- paste0(((as.numeric(format(Sys.time(), "%Y"))) - 1) + 
  (as.numeric(format(Sys.time(), "%m")) > 11 & 
     max(data$tblSample$SampleDate) > as.Date(paste0((as.numeric(format(Sys.time(), "%Y"))), "-11-01"))),
  "-11-01") %>% 
  as.Date()

yearOfInterest <- ifelse(as.numeric(format(Sys.time(), "%m")) >= 11 & 
                      as.numeric(format(max(data$tblSample$SampleDate), "%m")) >= 11,
                      as.numeric(format(Sys.time(), "%Y")) + 1, as.numeric(format(Sys.time(), "%Y")))

outliers <- list()

# EDIT METER CHECK IIII ---------------------------------------------------
outliers$meterCheck <- data$tblSample %>% 
  transmute(SampleDate, StationCode, SampleTimeStart, SampleTimeEnd,
            MeterStart, MeterEnd, 
            RawMeterCheck = MeterEnd - MeterStart, SampleComments) %>% 
  # As of 2022, this filter does not do anything as no MeterEnd is <= 0
  filter(MeterEnd > 0) %>% 
  mutate(Meter.Check = ifelse(RawMeterCheck < 0, 
                              MeterEnd - MeterStart + 1000000, RawMeterCheck)) %>% 
  group_by(StationCode) %>% 
  mutate(AvgOfMeter.Check = mean(Meter.Check, na.rm = T),
         Lower.2sd = AvgOfMeter.Check - 2*sd(Meter.Check, na.rm = T),
         Upper.2sd = AvgOfMeter.Check + 2*sd(Meter.Check, na.rm = T),
         Outlier = ifelse(Meter.Check < Lower.2sd | Meter.Check > Upper.2sd,
                          -1, NA)) %>% 
  filter(SampleDate > as.Date("2016-12-01")) %>% 
  select(SampleDate, StationCode, SampleTimeStart, SampleTimeEnd, MeterStart,
         MeterEnd, Meter.Check, Outlier) %>% 
  arrange(Outlier) %>% 
  # The produces the same data frame as the query (Checked 12082022 by TN)
  # Fixing sampling time to have the correct date-time structure
  mutate(SampleTimeStart = as.POSIXct(paste0(SampleDate, " ", format(SampleTimeStart, "%H:%M:%S"))),
         SampleTimeEnd = as.POSIXct(paste0(SampleDate, " ", format(SampleTimeEnd, "%H:%M:%S"))))

# Edit Duration-VM --------------------------------------------------------
outliers$duration <- data$tblSample %>% 
  transmute(SampleDate, SurveyNumber, StationCode, SampleTimeStart, SampleTimeEnd,
            Minutes = as.numeric(difftime(SampleTimeEnd, SampleTimeStart, units = "min")),
            SampleComments,
            Year = as.numeric(format(SampleDate, "%Y"))) %>% 
  filter(!Minutes %in% c(2.5, 5, 10), !is.na(Minutes))

# Edits-EC_III ------------------------------------------------------------
outliers$topEC <- data$tblSample %>% 
  group_by(StationCode) %>% 
  mutate(Mean = mean(ConductivityTop, na.rm = T),
            Lower.2sd = Mean - 2*sd(ConductivityTop, na.rm = T),
            Upper.2sd = Mean + 2*sd(ConductivityTop, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(ConductivityTop < Lower.2sd | ConductivityTop > Upper.2sd, -1, NA),
            SampleDate, StationCode, SampleTimeStart, ConductivityTop, Mean, 
            Lower.2sd, Upper.2sd) %>% 
  arrange(Outlier, SampleDate, StationCode, SampleTimeStart) %>% 
  filter(SampleDate > as.Date("2016-12-01"))

# Monthly
outliers$topECMonthly <- data$tblSample %>% 
  group_by(StationCode, Month = format(SampleDate, "%m")) %>% 
  mutate(Mean = mean(ConductivityTop, na.rm = T),
         Lower.2sd = Mean - 2*sd(ConductivityTop, na.rm = T),
         Upper.2sd = Mean + 2*sd(ConductivityTop, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(ConductivityTop < Lower.2sd | ConductivityTop > Upper.2sd, -1, NA),
            SampleDate, Month, StationCode, SampleTimeStart, ConductivityTop, Mean, 
            Lower.2sd, Upper.2sd) %>% 
  arrange(Outlier, SampleDate, StationCode, SampleTimeStart) %>% 
  filter(SampleDate > as.Date("2016-12-01"))

# Edits-Secchi_III --------------------------------------------------------
outliers$secchi <- data$tblSample %>% 
  group_by(StationCode) %>% 
  mutate(Mean = mean(Secchi, na.rm = T),
         Lower.2sd = Mean - 2*sd(Secchi, na.rm = T),
         Upper.2sd = Mean + 2*sd(Secchi, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(Secchi < Lower.2sd | Secchi > Upper.2sd, -1, NA),
            SampleDate, StationCode, Secchi, Mean, Lower.2sd, Upper.2sd) %>% 
  filter(format(SampleDate, ("%Y")) == 2020) %>% 
  arrange(desc(Outlier), SampleDate, StationCode)

# Monthly
outliers$secchiMonthly <- data$tblSample %>% 
  group_by(StationCode, Month = format(SampleDate, "%m")) %>% 
  mutate(Mean = mean(Secchi, na.rm = T),
         Lower.2sd = Mean - 2*sd(Secchi, na.rm = T),
         Upper.2sd = Mean + 2*sd(Secchi, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(Secchi < Lower.2sd | Secchi > Upper.2sd, -1, NA),
            SampleDate, Month, StationCode, SampleTimeStart, Secchi, Mean, 
            Lower.2sd, Upper.2sd) %>% 
  arrange(Outlier, SampleDate, StationCode, SampleTimeStart) %>% 
  filter(SampleDate > as.Date("2016-12-01"))

# Edits-WaterTemp_III -----------------------------------------------------
outliers$waterTemp <- data$tblSample %>% 
  group_by(StationCode) %>% 
  mutate(Mean = mean(WaterTemperature, na.rm = T),
         Lower.2sd = Mean - 2*sd(WaterTemperature, na.rm = T),
         Upper.2sd = Mean + 2*sd(WaterTemperature, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(WaterTemperature < Lower.2sd | WaterTemperature > Upper.2sd, -1, NA),
            SampleDate, SampleTimeStart, StationCode, WaterTemperature, Mean, Lower.2sd, Upper.2sd)%>% 
  filter(format(SampleDate, ("%Y")) == 2019) %>%
  arrange(desc(Outlier), SampleDate, StationCode)
# all.equal does not give perfect match here. There is some floating point error between the two
# Deeming this as not a problem 12-12-2022 TN

# Monthly
outliers$waterTempMonthly <- data$tblSample %>% 
  group_by(StationCode, Month = format(SampleDate, "%m")) %>% 
  mutate(Mean = mean(WaterTemperature, na.rm = T),
         Lower.2sd = Mean - 2*sd(WaterTemperature, na.rm = T),
         Upper.2sd = Mean + 2*sd(WaterTemperature, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(WaterTemperature < Lower.2sd | WaterTemperature > Upper.2sd, -1, NA),
            SampleDate, Month, StationCode, SampleTimeStart, WaterTemperature, Mean, 
            Lower.2sd, Upper.2sd) %>% 
  arrange(Outlier, SampleDate, StationCode, SampleTimeStart) %>% 
  filter(SampleDate > as.Date("2016-12-01"))

# Edits-Turbidty III ------------------------------------------------------
outliers$turbidity <- data$tblSample %>% 
  group_by(StationCode) %>% 
  mutate(Mean = mean(Turbidity, na.rm = T),
         Lower.2sd = Mean - 2*sd(Turbidity, na.rm = T),
         Upper.2sd = Mean + 2*sd(Turbidity, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(Turbidity < Lower.2sd | Turbidity > Upper.2sd, -1, NA),
            SampleDate, SampleTimeStart, StationCode, Turbidity, Mean, Lower.2sd, Upper.2sd)%>% 
  # filter(format(SampleDate, ("%Y")) == 2020) %>%
  arrange(desc(Outlier), SampleDate, StationCode)
# all.equal shows mismatch in number of NA values, despite showing the same #s.
# Deeming this as not a problem 12-12-2022 TN

# Monthly
outliers$turbidityMonthly <- data$tblSample %>% 
  group_by(StationCode, Month = format(SampleDate, "%m")) %>% 
  mutate(Mean = mean(Turbidity, na.rm = T),
         Lower.2sd = Mean - 2*sd(Turbidity, na.rm = T),
         Upper.2sd = Mean + 2*sd(Turbidity, na.rm = T)) %>% 
  ungroup() %>% 
  transmute(Outlier = ifelse(Turbidity < Lower.2sd | Turbidity > Upper.2sd, -1, NA),
            SampleDate, Month, StationCode, SampleTimeStart, Turbidity, Mean, 
            Lower.2sd, Upper.2sd) %>% 
  arrange(Outlier, SampleDate, StationCode, SampleTimeStart) %>% 
  filter(SampleDate > as.Date("2016-12-01"))

# GPS coordinates ---------------------------------------------------------
source("R/gpsHelpers.R")

# First, create a data frame of the coordinates with the coordinates taken during the tows
# These are the ones that will be flagged as outliers if they are more than half a mile from
# the theoretical coordinates
GPSDF <- data$tblSample %>%
  mutate(across(c(LatDeg, LatMin, LatSec, LongDeg, LongMin, LongSec), ~as.numeric(.x)),
         # Defining anything AFTER November as being part of the next year
         Year = ifelse(as.numeric(format(SampleDate, "%m")) %in% c(11, 12),
                       as.numeric(format(SampleDate, "%Y")) + 1, as.numeric(format(SampleDate, "%Y")))) %>% 
  transmute(SampleRowID, SampleDate, 
            Survey = SurveyNumber, Year = as.numeric(format(SampleDate, "%Y")), Station = StationCode,
            SampleTimeStart, SampleTimeEnd,
            Lat = LatDeg + LatMin/60 + LatSec/3600,
            Long = -(LongDeg + LongMin/60 + LongSec/3600),
            group = "Tow") %>% 
  # Now combining in the data frame with the theoretical coordinates per stations
  bind_rows(data$`SKT Station Sort Order for Reporting` %>% 
              mutate(across(c(LatDeg, LatMin, LatSec, LongDec, LongMin, LatSec), ~as.numeric(.x))) %>% 
              transmute(Station, 
                        Lat = LatDeg + LatMin/60 + LatSec/3600,
                        Long = -(LongDec + LongMin/60 + LongSec/3600),
                        group = "TheoreticalCoords"))

plotGPS(GPSDF, 2003)
outliers$GPS <- gpsOutlier(GPSDF, d = 0.5, year = yearOfInterest, returnDF = T)

# Pulling cdec readings for environmental predictors that are outliers ----
source("R/cdecHelpers.R")
# This RData file is created from the cdecMetadata.R file
load("cdecStations_20221215.RData")
# stop()
closestMetadata <- data$`SKT Station Sort Order for Reporting` %>% 
  mutate(lat = LatDeg + LatMin/60 + LatSec/3600,
         long = -(LongDec + LongMin/60 + LongSec/3600)) %>% 
  calcNearestCDEC(cdecStations, 
                  cdecMetadata = cdecMetadata %>% 
                    bind_rows(),
                  variable = "ec", waterColumn = "top")

closestMetadata <- mapply(calcNearestCDEC,
                          variable = c("ec", "temp", "turbidity"),
                          MoreArgs = list(df = data$`SKT Station Sort Order for Reporting` %>% 
                                            mutate(lat = LatDeg + LatMin/60 + LatSec/3600,
                                                   long = -(LongDec + LongMin/60 + LongSec/3600)),
                                          cdecGPS = cdecStations,
                                          cdecMetadata = bind_rows(cdecMetadata),
                                          waterColumn = "top"),
                          SIMPLIFY = F)

cdec <- lapply(closestMetadata, function(x) bind_rows(x) %>% 
                 distinct(Station, gage) %>% 
                 transmute(Station,
                           first = gage,
                           second = NA,
                           third = NA)) %>% 
  setNames(names(closestMetadata))

stop()

# To get popCDEC to work right: need "Station" and "TowTime". 
# Date must be a DATE formatted; TowTime must be date-time

# Now adding cdec values to the list of outliers. For nwo, this only keeps the outliers, i.e.,
# "Outlier" == -1. I'd like to change this to TRUE/FALSE to keep similar to 20mm/SLS scripts. Will leave for now.
outliers[which(names(outliers) %in% c("topEC", "topECMonthly", "waterTemp", "waterTempMonthly",
                                      "turbidity", "turbidityMonthly"))] <- 
  mapply(popCDEC, 
         df = list(outliers$topEC, outliers$topECMonthly, outliers$waterTemp, outliers$waterTempMonthly, 
                   outliers$turbidity, outliers$turbidityMonthly) %>% 
           lapply(function(x) {
             x %>% mutate(SampleDate = as.Date(SampleDate),
                          TowTime = as.POSIXct(paste(SampleDate, format(SampleTimeStart, 
                                                                        format = "%H:%M:%S")), 
                                               format = "%Y-%m-%d %H:%M:%S")) %>% 
               rename(Station = StationCode) %>% 
               filter(Outlier == -1)
           }),
         variable = c("ec", "ec", "temp", "temp", "turbidity", "turbidity"),
         waterColumn = c("top", "top", "top", "top", "top", "top"),
         cdec = list(cdec$ec, cdec$ec, cdec$temp, cdec$temp, cdec$turbidity, cdec$turbidity),
         metadata = list(closestMetadata$ec, closestMetadata$ec, 
                         closestMetadata$temp, closestMetadata$temp,
                         closestMetadata$turbidity, closestMetadata$turbidity))

# Saving all outlier DFs to an excel sheet for analysis -------------------

# Adding the 3 columns for ES to fill out for record keeping on the excel sheet
saveSheet <- lapply(outliers, function(x) mutate(x,
                                                 IsOutlier = NA, ChangedTo = NA, CommentsOutlier = NA,
                                                 # Strange XML character in some rows of the comments column in the
                                                 # Water Info table; removing this so that the excel file saves
                                                 # correctly
                                                 across(where(is.character), ~str_replace_all(.x, "\uFFFD", ""))))

# Saving the file; this will save to the current directory, which by default is in the parent folder; will change this later
writexl::write_xlsx(saveSheet, file.path("data-raw", "Outliers", "SKT", paste0("SKT_outliers_", gsub(" |:", "_", now()), ".xlsx")))

