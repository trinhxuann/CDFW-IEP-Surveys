# This script will read in the rds file or individual csvs (if rds is not there)
# and output the joined file, SLS.csv

# Libraries needed
library(readr)
library(dplyr)
library(tidyr)
library(wql)

# Reading the data --------------------------------------------------------

# Specify file path here to the file  
filePath <- file.path("data-raw", "SLS")

# RDS or csvs to use
if (length(list.files(path = filePath, pattern = ".rds$")) > 0) {
  
  SLSTables <- readRDS(file.path(filePath, list.files(path = filePath, pattern = ".rds$"))) 
  
  # For tables with the Station column, conver them to character (for LTMRData)
  stationColumnIndex <- sapply(SLSTables, function(x) {
    any(grepl("Station", names(x)))
  }) %>% 
    which() %>% 
    names()
  
  for (i in stationColumnIndex) {
    SLSTables[[i]] <- dplyr::mutate(SLSTables[[i]], Station = as.character(Station))
  }
} else {
  SLSTables <- list()
  
  SLSTables$Catch <- readr::read_delim(file.path("data-raw", "SLS", "Catch.csv"), delim = ",",
                                       col_types =
                                         readr::cols_only(
                                           # Using col_date() here as the format must be
                                           # very specific or an error should be thrown
                                           # and fixed
                                           Date = readr::col_date("%Y-%m-%d"),
                                           Station = readr::col_character(),
                                           Tow = readr::col_integer(),
                                           FishCode = readr::col_integer(),
                                           Catch = readr::col_integer(),
                                           CatchID = readr::col_integer()
                                         )
  )
  
  SLSTables$Lengths <- readr::read_delim(file.path("data-raw", "SLS", "Lengths.csv"), delim = ",",
                                         col_types =
                                           readr::cols_only(
                                             Date = readr::col_date("%Y-%m-%d"),
                                             Station = readr::col_character(),
                                             Tow = readr::col_integer(),
                                             FishCode = readr::col_integer(),
                                             Length = readr::col_integer(),
                                             EntryOrder = readr::col_integer()
                                           )
  )
  
  SLSTables$MeterCorrections <- readr::read_delim(file.path("data-raw", "SLS", "MeterCorrections.csv"), delim = ",",
                                                  col_types =
                                                    readr::cols_only(
                                                      StudyYear = readr::col_double(),
                                                      MeterSerial = readr::col_integer(),
                                                      CalibrationDate = readr::col_date("%Y-%m-%d"),
                                                      kFactor = readr::col_double(),
                                                      Notes = readr::col_character()
                                                    )
  )
  
  SLSTables$TowInfo <- readr::read_delim(file.path("data-raw", "SLS", "TowInfo.csv"), delim = ",",
                                         col_types =
                                           readr::cols_only(
                                             Date = readr::col_date("%Y-%m-%d"),
                                             Station = readr::col_character(),
                                             Tow = readr::col_integer(),
                                             Time = readr::col_character(),
                                             Tide = readr::col_character(),
                                             BottomDepth = readr::col_integer(),
                                             CableOut = readr::col_integer(),
                                             Duration = readr::col_double(),
                                             NetMeterSerial = readr::col_integer(),
                                             NetMeterStart = readr::col_integer(),
                                             NetMeterEnd = readr::col_integer(),
                                             NetMeterCheck = readr::col_integer(),
                                             Comments = readr::col_character()
                                           )
  )
  
  SLSTables$WaterInfo <- readr::read_delim(file.path("data-raw", "SLS", "WaterInfo.csv"), delim = ",",
                                           col_types =
                                             readr::cols_only(
                                               Survey = readr::col_integer(),
                                               Date = readr::col_date("%Y-%m-%d"),
                                               Station = readr::col_character(),
                                               Temp = readr::col_double(),
                                               TopEC = readr::col_integer(),
                                               BottomEC = readr::col_integer(),
                                               Secchi = readr::col_integer(),
                                               Turbidity = readr::col_double(),
                                               Comments = readr::col_character()
                                             )
  )
  
  SLSTables$Station_Lookup <- readr::read_delim(file.path("data-raw", "SLS", "Station_Lookup.csv"), delim = ",",
                                                col_types =
                                                  readr::cols_only(
                                                    ID = readr::col_double(),
                                                    Station = readr::col_character(),
                                                    Description = readr::col_character(),
                                                    Lat = readr::col_character(),
                                                    Long = readr::col_character())) 
  
  # Were there any problems with the reading process?
  if (sum(sapply(SLSTables, function(x) nrow(readr::problems(x)))) > 0) {
    warning("Problems were detected during the read phase. Check the code")
    browser()
  }
}

# Manipulating the data tables --------------------------------------------

waterInfo <- SLSTables$WaterInfo %>%
  dplyr::mutate(
    # Converting secchi from cm to m
    Secchi = Secchi/100,
    # Converting EC to salinity
    # Per SOP normalized at temp = 25C; units are in millisiemens
    Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
    Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
    # This is to take care of how the floats are read between Access and readr methods...
    Temp_surf = round(Temp, 2)) %>% 
  dplyr::rename(Notes_env = Comments)

towInfo <- SLSTables$TowInfo %>%
  # manipulation of time should not be required if pulling from the RDS file, 
  # but won't change so leaving to cover when you are using the csv files
  dplyr::mutate(Time = strptime(Time, format = "%Y-%m-%d %H:%M:%S",
                                tz = "America/Los_Angeles"),
                StudyYear = as.numeric(format(Date, format = "%Y"))) %>% 
  dplyr::rename(Notes_tow = Comments) %>% 
  # 1 parsing error because time was not recorded for that row
  # Now, joining to the Meter Corrections table to calculate tow volume later
  # This is based on the "WEB_Raw_Catch_Volum_Info" query in the SLS Access query database
  # First need to create a study year column; from the query above, the "WEB_VolumeInfo" query calculates year from the
  # towInfo table.
  dplyr::left_join(SLSTables$MeterCorrections %>%
                     # There are duplicated values here in this table; will simply distinct() them
                     # Confirmed via email with Adam, ES of Native Fish unit as of 10-27-2021
                     dplyr::distinct(),
                   by = c("StudyYear", "NetMeterSerial" = "MeterSerial")) %>%
  # Moving on to various required calculations
  dplyr::mutate(Datetime = as.POSIXct(paste(Date, format(Time, "%H:%M:%S")),
                                      format = "%Y-%m-%d %H:%M:%S",
                                      tz = "America/Los_Angeles"),
                # Now to turn tide into factors as outlined in the metadata file
                Tide = dplyr::case_when(Tide == 1 ~ "High Slack",
                                        Tide == 2 ~ "Ebb",
                                        Tide == 3 ~ "Low Slack",
                                        Tide == 4 ~ "Flood",
                                        TRUE ~ NA_character_),
                # Converting bottom depth to meters
                Depth = BottomDepth * 0.3048,
                # Convert cable out from feet to meters
                CableOut = CableOut * 0.3048,
                # Calculating tow volume for the fish net, 0.37 as the area of the mout of the net (per Access + SLS SOP document)
                Tow_volume = NetMeterCheck * kFactor * 0.37)

stationLookup <- SLSTables$Station_Lookup %>% 
  # Removing 2 rows of trailing white space
  dplyr::mutate(across(c(Lat, Long), trimws)) %>% 
  tidyr::separate(Lat, into = c("LatD", "LatM", "LatS"), sep = " ") %>%
  tidyr::separate(Long, into = c("LonD", "LonM", "LonS"), sep = " ") %>%
  dplyr::mutate(across(c(LatD, LatM, LatS, LonD, LonM, LonS), ~as.numeric(.x)),
                Latitude = (LatD + LatM/60 + LatS/3600),
                Longitude = -(LonD + LonM/60 + LonS/3600))

catch <- SLSTables$Catch 

lengths <- SLSTables$Lengths %>%
  # Calculating total number of fish measured (across all lengths) and # of fish measured
  # per Date, Station, Tow, and FishCode
  # This is to calculate plus counts later in dfFin
  dplyr::group_by(Date, Station, Tow, FishCode, Length)%>%
  dplyr::summarise(LengthFrequency = dplyr::n(), .groups = "drop") %>%
  dplyr::group_by(Date, Station, Tow, FishCode) %>%
  dplyr::mutate(TotalLengthMeasured = sum(LengthFrequency)) %>%
  dplyr::ungroup()

# Joining the tables ------------------------------------------------------

# After joining, will want to check to see if values have been deleted or added
# during the joining process

# Now to combine the datasets together, following the relationship table in Access
# The tables go waterInfo -> towInfo -> Catch -> lengths

# This function will be used to compare the joined df to the base tables at every step
# to identify what is being deleted/added to the database
# should only be additions here due to the use of full_join
compareDF <- function(baseDF, targetDF, 
                      distinctTarget = T, 
                      arrangeKey = c("Date", "Station")) {
  
  # Base table is here the relational table from Access,
  # arrange the dataset by columns in arrangeKey
  # This is to ensure that all.equal works since order or the rows is also checked
  baseDF <- baseDF %>% 
    dplyr::arrange(dplyr::across(dplyr::all_of(arrangeKey))) %>% 
    data.frame()
  
  # This optionality is currently in here because I may be missing some edge cases
  # But, I don't think this should ever be not TRUE
  if (distinctTarget) {
    # Since the full join will duplicate data, will need to remove these duplicates
    # in order to remake the base table
    targetDF <- targetDF %>%
      # As the full join may introduce new columns from other datasets, select only
      # the columns in the relational table that you want to recreate
      dplyr::select(names(baseDF)) %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(dplyr::across(dplyr::all_of(arrangeKey))) %>% 
      data.frame()
  } else {
    targetDF <- targetDF %>% 
      dplyr::select(names(baseDF)) %>% 
      dplyr::arrange(dplyr::across(dplyr::all_of(arrangeKey))) %>% 
      data.frame()
  }
  
  all.equal(baseDF, targetDF)
}

joinCheckSteps <- list()
# Now begin joining

# waterInfo and towInfo together
waterTowJoin <- waterInfo %>% 
  dplyr::full_join(towInfo,
                   c("Date", "Station"))

joinCheckSteps$waterTowWater <- compareDF(waterInfo, waterTowJoin)
joinCheckSteps$waterTowTow <- compareDF(towInfo, waterTowJoin)
# Ok

# Now joining catch to waterInfo + towInfo joined table
waterTowCatchJoin <- waterTowJoin %>% 
  dplyr::full_join(catch,
                   by = c("Date", "Station", "Tow"))

joinCheckSteps$waterTowCatchWater <- compareDF(waterInfo, waterTowCatchJoin)
joinCheckSteps$waterTowCatchTow <- compareDF(towInfo, waterTowCatchJoin)
compareDF(catch, waterTowCatchJoin)
# Check catch (+X NAs)
joinCheckSteps$waterTowCatchCatch <- compareDF(catch, waterTowCatchJoin %>% 
                                                 dplyr::filter(!is.na(Catch)))
# NOTE HERE; there are instances when tows do not catch any
# fish at all and these instances are NOT recorded in the catch
# table. If you use inner_join here, you run into the trouble
# of losing environmental data; this is because environmental data
# is still recorded even if catch is 0 but this only shows up
# in the waterInfo table. Use of full_join is more appropriate here

# Now join to length table
waterTowCatchLengthJoin <- waterTowCatchJoin %>% 
  dplyr::full_join(lengths,
                   by = c("Date", "Station", "Tow", "FishCode"))

joinCheckSteps$waterTowCatchLengthWater <- compareDF(waterInfo, waterTowCatchLengthJoin)
joinCheckSteps$waterTowCatchLengthTow <- compareDF(towInfo, waterTowCatchLengthJoin)
compareDF(catch, waterTowCatchLengthJoin)
compareDF(lengths, waterTowCatchLengthJoin)
# Similar problem from catch propagated
joinCheckSteps$waterTowCatchLengthCatch <- compareDF(catch, waterTowCatchLengthJoin %>% 
                                                       dplyr::filter(!is.na(Catch)))
# Check lengths (+y NAs, more values than catch if catch has a value but lengths were not recorded)
joinCheckSteps$waterTowCatchLengthLengths <- compareDF(lengths, waterTowCatchLengthJoin %>% 
                                                         dplyr::filter(!is.na(Length)))
# The +1 error that is propagated via length table happened on 
# This occurred on 2009-02-18, station 508 during which there is 1 entry of
# FishCode == 99 without a length or catch. When removed.

# Now the last joining steps, to species code and station #
# This step should never add or delete rows given that it's a left_join only
finJoin <- waterTowCatchLengthJoin %>% 
  dplyr::left_join(LTMRdata::Species %>%
                     dplyr::select(TMM_Code,
                                   Taxa) %>%
                     dplyr::filter(!is.na(TMM_Code)),
                   by = c("FishCode" = "TMM_Code")) %>%
  dplyr::left_join(stationLookup,
                   by = "Station")

# Final comparison between length and final table:
# Length chosen here because it is the finest resolution table to be
# joined. Everything else will have duplications to accomodate per
# individual lengths

lengthCheckDF <- lengths %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), ~sum(.x, na.rm = T)))
lengthCheckJoinedDF <- finJoin %>% 
  dplyr::select(names(lengths)) %>% 
  dplyr::summarise(dplyr::across(where(is.numeric), ~sum(.x, na.rm = T)))

joinCheckSteps$length <- all.equal(lengthCheckDF %>% 
                                     dplyr::select(Length, LengthFrequency, TotalLengthMeasured) %>% 
                                     as.data.frame(),
                                   lengthCheckJoinedDF %>% 
                                     dplyr::select(Length, LengthFrequency, TotalLengthMeasured) %>% 
                                     as.data.frame())

# Final output ------------------------------------------------------------

SLS <- finJoin %>%
  # Merging the two comment columns together; they both have data in them
  dplyr::mutate(Notes_tow = paste(Notes_tow, Notes_env, sep = "; ")) %>%
  dplyr::arrange(Date, Datetime, Survey, Station, Tow, Taxa, Length) %>%
  dplyr::mutate(Source = "SLS",
                SampleID = paste(Source, Date, Station, Tow), # Creating SampleID index
                Count = dplyr::if_else(is.na(Length), 
                                       as.numeric(Catch), 
                                       (LengthFrequency/TotalLengthMeasured) * Catch),
                # Creating Length_NA_flag to parallel the other survey datasets in LTMR
                Length_NA_flag = dplyr::if_else(is.na(Count), "No fish caught", NA_character_),
                # Creating Method column; Adam described this as an "Olbique tow", significantly diff from WMT
                Method = "Oblique tow",
                Station = as.character(Station)) %>% 
  # Initial 0 filling: only fill in 0s when sampling occurred but NO catch of any spp occurred
  dplyr::mutate(Count = ifelse(Length_NA_flag %in% "No fish caught",
                               0,
                               Count))

# One last step to QAQC; can we get back to the base tables from this joined table?
# waterInfo
joinCheckSteps$waterInfoFin <- compareDF(SLS %>% 
                                           dplyr::select(names(waterInfo)) %>% 
                                           dplyr::distinct(),
                                         waterInfo, distinctTarget = F)
# towInfo
joinCheckSteps$towInfoFin <- compareDF(SLS %>% 
                                         # Notes_tow column here is slightly modified from original
                                         # This column pastes 2 columns together, resulting in no NAs
                                         dplyr::select(names(towInfo), -Notes_tow) %>% 
                                         dplyr::distinct(),
                                       towInfo,
                                       distinctTarget = F)
# catch
joinCheckSteps$catchFin <- compareDF(SLS %>% 
                                       dplyr::select(names(catch)) %>% 
                                       dplyr::distinct() %>% 
                                       dplyr::filter(!is.na(Catch)) %>% 
                                       dplyr::arrange(Date, Station, Tow, FishCode),
                                     catch %>% 
                                       dplyr::arrange(Date, Station, Tow, FishCode),
                                     distinctTarget = F)
# length
joinCheckSteps$lengthFin <- compareDF(SLS %>% 
                                        dplyr::select(names(lengths)) %>% 
                                        dplyr::distinct() %>% 
                                        dplyr::filter(!is.na(Length)) %>% 
                                        dplyr::arrange(Date, Station, Tow, FishCode),
                                      lengths %>% 
                                        dplyr::arrange(Date, Station, Tow, FishCode),
                                      distinctTarget = F)

# If any of the QAQC steps to check for data joining errors has an error, you will need to check
# what happened here
if (any(!unlist(joinCheckSteps))) {
  warning("There are unaccounted for additions/deletions to the joining process. Check steps.", call. = F)
  browser()
} else {
  SLS <- SLS %>% 
    # Removing CatchID and entryorder as they are not relevant to the dataset
    # Removing TopEC, BottomEC as they have been converted over the salinity already
    # Removing CBMeterSerial, CBMeterStart, CBMeterEnd, CBMeterCheck as CB not ran on the SLS
    dplyr::select(Source, Station, Latitude, Longitude,
                  Date, Datetime, Survey, Depth, SampleID, Method,
                  Tide, Sal_surf, Sal_bot, Temp_surf, Secchi, Turbidity, Tow_volume,
                  Cable_length = CableOut, Tow_duration = Duration,
                  Taxa, Length, Count, Length_NA_flag,
                  Notes_tow, Notes_flowmeter = Notes) %>% 
    data.frame()
}

# Filling in additional zeroes --------------------------------------------

# Zeroes are defined as sampling events that did not catch any individuals of a species. Currently,
# these are not defined in the dataset itself. The following code will fill in these zeroes for all
# species and sampling events:

# SLSZeroesFull <- SLS %>%
#   dplyr::mutate(Taxa = factor(Taxa)) %>%
#   # Fill in rows for all columns other than taxa, length, and count
#   # Taxa = filled with the factors in taxa
#   # Length = filled with NAs if missing
#   # Count = filled with 0
#   dplyr::group_by(Source, Station, Latitude, Longitude, Date, Datetime, Survey, Depth,
#                   SampleID, Method, Tide, Sal_surf, Sal_bot, Temp_surf, Secchi, Turbidity,
#                   Tow_volume, Cable_length, Tow_duration, Length_NA_flag,
#                   Notes_tow, Notes_flowmeter) %>%
#   # Fill in rows for all Station, Date, Datetime, Survey, Method
#   # To use this function, you may need to be in 64-bit R for the memory requirement
#   # There will be a noticable lag in running complete()
#   tidyr::complete(Taxa,
#                   fill = list(Count = 0)) %>%
#   data.frame()
# 
# # Check to see if duplications/deletions ocurred in the length/catch columns
# # The other columns are not checked because they will be duplicated to accomodate the additional taxa
# all.equal(SLS %>%
#             dplyr::select(Length, Count) %>%
#             dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(.x, na.rm = T))),
#           SLSZeroesFull %>%
#             dplyr::select(Length, Count) %>%
#             dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(.x, na.rm = T))))

# Writing the final dataset -----------------------------------------------

# Change the object if you want the fully zero-filled dataset.
write.csv(SLS, file = file.path("data-raw", "SLS", "SLS.csv"), row.names = F)
