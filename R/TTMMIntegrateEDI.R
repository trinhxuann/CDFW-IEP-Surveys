#' Integration of 20mm, TTMM, Access base tables into a long format data frame
#'
#' This function will read in the individual base tables from the SLS Access
#' database and integrate them into a singular long data format. The required
#' tables are "Catch", "Lengths", "Tow Info", "Water Info", Meter Corrections,
#' and "Station_Lookup" from the Access database.
#'
#' @param filePath Location of the base Access tables to be integrated. 
#' Default behavior is to simply read from the file path within data-raw/SLS folder of this package.
#' The function can also take two input files, an RDS file containing all of the
#' dataframes or the individual csv's themselves. The default behavior will be 
#' to use the RDS file if filePath is NULL, if the file is available.
#' @param fillZeroes Logical, should all tows be filled with count = 0 for species
#' not caught during the trawl?
#' 
#' @importFrom magrittr %>%
#' @return An integrated fish dataset of the CDFW 20mm Survey.
#' @export TTMMIntegrate

TTMMIntegrate <- function(filePath = NULL, fillZeroes = F) {
  #NOTE: this function is based off the workflow from Lara Mitchell and her work on 
  # the 20mm. I have reformatted the workflow to match the SLS workflow
  # There are some main differences between her version and this one that users may
  # pick up on:
  # 1) the lat/lon used here are those recorded from the sample relational tables, which means
  # they reflect the coordinates at the time of sample. Lara used the theoretical coords only
  # Here, sampling coords are used by default and supplemented with the theoretical if missing
  # 2) tow_volume is slightly different. Lara did not calculate kfactor_avg for
  # meterserial 20780 which had an NA; the NA is ignored here to calculate an average
  
  # Reading the data --------------------------------------------------------
  
  if (is.null(filePath)) {
    filePath <- file.path("data-raw", "20mm")
  }
  
  # RDS or csvs to use
  if (length(list.files(path = filePath, pattern = ".rds$")) > 0) {
    
    TTMMTables <- readRDS(file.path(filePath, list.files(path = filePath, pattern = ".rds$"))) 
    
    # For tables with the Station column, conver them to character (for LTMRData)
    stationColumnIndex <- sapply(TTMMTables, function(x) {
      # The grepl() function here uses regular expression, which can be abstract. The
      # ^ symbol here indicates the start of the string and the $ represents the end.
      # Translated, this means find columns that exactly match Station (e.g., StationID won't be matched)
      any(grepl("^Station$", names(x)))
    }) %>% 
      which() %>% 
      names()
    
    for (i in stationColumnIndex) {
      TTMMTables[[i]] <- dplyr::mutate(TTMMTables[[i]], Station = as.character(Station))
    }
  } else {
    # If you do not have an RDS file, then you will have to read in the files directly
    # There are 9 csv's
    TTMMTables <- list()
    
    TTMMTables$Survey <- readr::read_delim(file.path("data-raw", "20mm", "Survey.csv"), delim = ",",
                                         col_types =
                                           readr::cols_only(
                                             SurveyID = col_integer(),
                                             SampleDate = col_date(format = "%Y-%m-%d"),
                                             Survey = col_integer(),
                                             Comments = col_character()
                                           )
    )
    
    TTMMTables$Station <- readr::read_delim(file.path("data-raw", "20mm", "Station.csv"), delim = ",",
                                           col_types =
                                             readr::cols_only(
                                               StationID = col_integer(),
                                               SurveyID = col_integer(),
                                               Station = col_character(),
                                               LatDeg = col_double(),
                                               LatMin = col_double(),
                                               LatSec = col_double(),
                                               LonDeg = col_double(),
                                               LonMin = col_double(),
                                               LonSec = col_double(),
                                               Temp = col_double(),
                                               TopEC = col_integer(),
                                               BottomEC = col_integer(),
                                               Secchi = col_integer(),
                                               Turbidity = col_double(),
                                               Comments = col_character()
                                             )
    )
    
    TTMMTables$Tow <- readr::read_delim(file.path("data-raw", "20mm", "Tow.csv"), delim = ",",
                                                    col_types =
                                                      readr::cols_only(
                                                        TowID = col_integer(),
                                                        StationID = col_integer(),
                                                        TowNum = col_integer(),
                                                        TowTime = col_character(),
                                                        Tide = col_character(),
                                                        BottomDepth = col_integer(),
                                                        CableOut = col_integer(),
                                                        Duration = col_double()
                                                      )
    ) %>% 
      # Changing TowTime to posixct format to match what is in the RDS version
      mutate(TowTime = as.POSIXct(TowTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"))
    
    TTMMTables$Gear <- readr::read_delim(file.path("data-raw", "20mm", "Gear.csv"), delim = ",",
                                            col_types =
                                              readr::cols_only(
                                                GearID = col_integer(),
                                                TowID = col_integer(),
                                                GearCode = col_integer(),
                                                MeterSerial = col_integer(),
                                                MeterStart = col_integer(),
                                                MeterEnd = col_integer(),
                                                MeterCheck = col_integer(),
                                                Comments = col_character()
                                              )
    )
    
    TTMMTables$GearCodesLkp <- readr::read_delim(file.path("data-raw", "20mm", "GearCodesLkp.csv"), delim = ",",
                                             col_types =
                                               readr::cols_only(
                                                 GearCode = col_integer(),
                                                 Gear = col_character(),
                                                 GearDescription = col_character(),
                                                 Order = col_integer(),
                                                 Active = col_logical()
                                               )
    )
    
    TTMMTables$MeterCorrections <- readr::read_delim(file.path("data-raw", "20mm", "MeterCorrections.csv"), delim = ",",
                                                     col_types =
                                                       readr::cols_only(
                                                         StudyYear = col_integer(),
                                                         MeterSerial = col_integer(),
                                                         CalibrationDate = col_date(format = ""),
                                                         kFactor = col_double(),
                                                         Notes = col_character()
                                                       )
    )
    
    TTMMTables$StationCords <- readr::read_delim(file.path("data-raw", "20mm", "StationCords.csv"), delim = ",",
                                                 col_types =
                                                   readr::cols_only(
                                                     ID = col_integer(),
                                                     Station = col_character(),
                                                     Lat = col_character(),
                                                     Long = col_character()
                                                   )
    )
    
    TTMMTables$FishSample <- readr::read_delim(file.path("data-raw", "20mm", "FishSample.csv"), delim = ",",
                                                 col_types =
                                                   readr::cols_only(
                                                     FishSampleID = col_integer(),
                                                     GearID = col_integer(),
                                                     SampleCode = col_integer(),
                                                     FishCode = col_integer(),
                                                     Catch = col_integer()
                                                   )
    )
    
    TTMMTables$FishLength <- readr::read_delim(file.path("data-raw", "20mm", "FishLength.csv"), delim = ",",
                                               col_types =
                                                 readr::cols_only(
                                                   FishLengthID = col_integer(),
                                                   FishSampleID = col_integer(),
                                                   Length = col_integer(),
                                                   AdFinPresent = col_logical(),
                                                   ReleasedAlive = col_logical(),
                                                   FieldRace = col_character(),
                                                   FinalRace = col_character()
                                                 )
    )
    
    # As of 4-28-22, there ARE issues with the comments in the "Gear" table and how those are read
    # into R. These entries use line breaks and made exports to a flat file difficult...
    # Will leave for now.
    
    # Were there any problems with the reading process?
    if (sum(sapply(TTMMTables, function(x) nrow(readr::problems(x)))) > 0) {
      warning("Problems were detected during the read phase. Check the code")
      browser()
    }
  }

  # Manipulating the data tables --------------------------------------------
  
  # Changing some names to keep comments across the relational tables in tact
  survey <- TTMMTables$Survey %>% 
    rename(Date = SampleDate,
           Notes_survey = Comments)
  
  # This 20 mm station table is not in the 20 mm back end for some reason
  TmmStations <- read.csv(file.path("data-raw", "20mm", "20mmStations.csv"), 
                          stringsAsFactors = FALSE) %>% 
    transmute(Station = as.character(Station), 
              LatitudeTheoretical = LatD + LatM/60 + LatS/3600,
              LongitudeTheoretical = -(LonD + LonM/60 + LonS/3600))
  
  station <- TTMMTables$Station %>% 
    rename(Temp_surf = Temp,
           Notes_station = Comments) %>% 
    # Joining to the theoretical tmm station coordiantes
    left_join(TmmStations, by = "Station") %>% 
    # Convert conductivity to salinity; TopEC is in micro-S/cm; 
    # input should be in milli-S/cm:
    mutate(Sal_surf = wql::ec2pss(TopEC/1000, t = 25),
           Sal_bot = wql::ec2pss(BottomEC/1000, t = 25),
           # Converting secchi from cm to m
           Secchi = Secchi/100,
           # Converting lat and longs into degrees
           LatitudeStation = LatDeg + LatMin/60 + LatSec/3600,
           LongitudeStation = -(LonDeg + LonMin/60 + LonSec/3600),
           # Now, if we have coordinates from the station table itself, use that
           # if not, then use the theoretical station values
           Latitude = ifelse(is.na(LatitudeStation), LatitudeTheoretical, LatitudeStation),
           Longitude = ifelse(is.na(LongitudeStation), LongitudeTheoretical, LongitudeStation))
  
  gear <- TTMMTables$Gear %>% 
    rename(Notes_gear = Comments) %>% 
    # want to pull only the fish net for now
    filter(GearCode == 2)
  
  tow <- TTMMTables$Tow %>% 
    mutate(Tide = case_when(Tide == 1 ~ "High Slack",
                            Tide == 2 ~ "Ebb",
                            Tide == 3 ~ "Low Slack",
                            Tide == 4 ~ "Flood"),
           # Pulling out just the time
           TowTime = format(TowTime, format = "%H:%M:%S"),
           # Converting bottom depth from feet to meters
           Depth = BottomDepth * 0.3048,
           # Same for cable length
           Cable_length = CableOut * 0.3048)
  
  # Changing gear name in the GearCodesLkp table; just cleaning up the names
  # I think this may be dropped, but will wait until I hear officially
  gearCode <- TTMMTables$GearCodesLkp %>% 
    mutate(Method = case_when(GearDescription == "mesozooplankton (CB) net" ~ "Mesozooplankton_CB_Net",
                               GearDescription == "20mm Net" ~ "20mm_Net"))
  
  # Creating table to fill in instances when a meter does not have
  # correction factors. This is filled with the average for that meter
  # across ALL years. There is only 1 instance when this occurred, meter
  # 20780 that was calibrated on 2012-12-27
  meterCorrection <- TTMMTables$MeterCorrections %>%
    dplyr::group_by(MeterSerial) %>%
    dplyr::summarize(kFactor_avg = mean(kFactor, na.rm = T), 
                     .groups="drop") %>% 
    # Now, joining back to original meter corrections DF to fill in missing 
    # with meter average
    full_join(TTMMTables$MeterCorrections, by = "MeterSerial") %>% 
    mutate(kFactor_final = ifelse(is.na(kFactor), 
                                  kFactor_avg, 
                                  kFactor))
  
# Combining ---------------------------------------------------------------
  
  # This function will be used to compare the joined df to the base tables at every step
  # to identify what is being deleted/added to the database
  # should only be additions here due to the use of full_join
  compareDF <- function(baseDF, targetDF, 
                        distinctTarget = T, 
                        arrangeKey) {
  
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
  
  # Now, combining the relational tables together. This follows the relationships
  # as outlined in the Access database: Survey -> Station -> Tow -> 
  # Gear -> FishSample -> FishLength
  # Focusing only on the fish stuff for now
  surveyStation <- survey %>% 
    full_join(station, by = "SurveyID")
  
  joinCheckSteps$survey <- compareDF(survey, surveyStation, arrangeKey = "SurveyID")
  joinCheckSteps$station <- compareDF(station, surveyStation, arrangeKey = "SurveyID")
  
  # next is tow
  surveyStationTow <- surveyStation %>% 
    full_join(tow, by = "StationID")
  
  joinCheckSteps$tow <- compareDF(tow, surveyStationTow, arrangeKey = "StationID")
  
  # next is gear
  surveyStationTowGear <- surveyStationTow %>% 
    full_join(gear, by = "TowID")
  
  joinCheckSteps$gear <- compareDF(gear, surveyStationTowGear, arrangeKey = "TowID")
  # 1 additional column added in the tow table at towID == 29970
  # Looking at the tow table, this seemed to have been a 4th tow but data was not recorded
  # there are no other 4th tows in the entire dataset
  joinCheckSteps$gear <- compareDF(gear, 
                                   surveyStationTowGear %>% 
                                     filter(TowID != 29970), 
                                   arrangeKey = "TowID")
  # Removing this 1 row works
  
  # next is FishSample
  # First step is to summarize the length table to include length frequencies
  # This step is required to calculate adjusted count later; this format is
  # found in most studies, in which you have a length frequency column and not
  # every measured fish having its own row. Duplications like this will cause you
  # to calculate adjusted count wrong
  LengthFrequency <- TTMMTables$FishLength %>% 
    dplyr::filter(!is.na(Length)) %>%
    # Don't want to group by FishLengthID here because we want to summarize into length Freq
    # Also, removing AdFinPresent, ReleasedAlive, FieldRace, and FinalRace from here
    # this is because these can cause "duplicates" to appear in the summary dataset
    # since these columns are removed in the final dataset
    # FishSampleID are based on FishCode and Catch
    dplyr::group_by(FishSampleID, Length) %>%
    dplyr::summarize(LengthFrequency = n(), 
                     .groups = "drop")
  
  # now combine this to the FishSample table to get frequency distribution per catch obs
  FishSampledLength <- full_join(LengthFrequency, 
                                 TTMMTables$FishSample, 
                                 by = "FishSampleID") 
  
  joinCheckSteps$catchSample <- compareDF(TTMMTables$FishSample, 
                                          FishSampledLength,
                                          arrangeKey = "FishSampleID")
  joinCheckSteps$catchLength <- compareDF(LengthFrequency, 
                                          FishSampledLength, 
                                          arrangeKey = "FishSampleID")
  # There are 17 instances of fish catch but without a FishLengthID; I do not know why
  # this is the case as there are catches during those entries. There are some entries
  # with FishCode == 99, but not all
  joinCheckSteps$catchLength <- compareDF(LengthFrequency, 
                                          FishSampledLength %>% 
                                            filter(!is.na(Length)), 
                                          arrangeKey = "FishSampleID")
  
  # Calculating adjusted count from the combined fish sample + length table
  catch <- FishSampledLength %>% 
    # Calculating total measured
    group_by(FishSampleID, GearID, FishCode) %>% 
    mutate(TotalMeasured = sum(LengthFrequency, na.rm = T),
           # 1 instance of catch < # of fish measured. Replacing catch with
           # the number of fish measured here: FishSampleID = 37049
           CatchNew = ifelse(TotalMeasured > Catch, TotalMeasured, Catch)) %>% 
    group_by(FishSampleID, GearID, FishCode, Length) %>% 
    # Now to calculate adjusted count
    mutate(Count = (LengthFrequency/TotalMeasured) * CatchNew)
  
  
  # Joining catch data to environmental data from earlier:
  finJoin <- full_join(surveyStationTowGear, catch,
                       by = "GearID") 
  
  joinCheckSteps$surveyStationTowGear <- compareDF(surveyStationTowGear, 
                                                   finJoin, 
                                                   arrangeKey = "GearID")
  joinCheckSteps$catch <- compareDF(catch, 
                                    finJoin, 
                                    arrangeKey = "GearID")
  # 2996 instances of no fish catch
  # 1 instance of no catch in the catch DF at gearID 40026
  joinCheckSteps$catch <- compareDF(catch %>% 
                                      filter(!is.na(Catch)), 
                                    finJoin %>% 
                                      filter(!is.na(Catch)), 
                                    arrangeKey = "GearID")
  
  # Final comparison between length and final table:
  # Length chosen here because it is the finest resolution table to be
  # joined. Everything else will have duplications to accomodate per
  # individual lengths
  
  lengthCheckDF <- TTMMTables$FishLength %>% 
    dplyr::summarise(Length = sum(Length, na.rm = T))
  
  # Mind the LengthFrequency here; the original df lists out each fish while
  # the finJoin DF summarizes them by length freq. To get the same number,
  # you have to expand the count again.
  lengthCheckJoinedDF <- finJoin %>% 
    dplyr::summarise(Length = sum(Length * LengthFrequency, na.rm = T))
  
  joinCheckSteps$length <- all.equal(lengthCheckDF %>% 
                                       as.data.frame(),
                                     lengthCheckJoinedDF %>% 
                                       as.data.frame())
  
  # Final output ------------------------------------------------------------

  # Some final manipulations; these are here because they are directly associated with the
  # final data frame. I've tried to keep manipulations to their relevant data frame for
  # easy reading
  TTMM <- finJoin %>%
    mutate(StudyYear = as.numeric(format(Date, "%Y"))) %>% 
    # Joining MeterSerial table
    left_join(meterCorrection, by = c("StudyYear", "MeterSerial")) %>% 
    # Should only be the 20 mm fish net for now; filtered earlier
    left_join(gearCode, by = "GearCode") %>% 
    # Adding in taxa data base from LTMR
    left_join(LTMRdata::Species %>% 
                select(TMM_Code, Taxa) %>% 
                filter(!is.na(TMM_Code)), 
              by = c("FishCode" = "TMM_Code")) %>% 
    dplyr::arrange(Date, Survey, Station, TowNum) %>%
    dplyr::mutate(Source = "20mm",
                  SampleID = paste(Source, Date, Station, TowNum), # Creating SampleID index
                  # Converting time to a date-time column
                  Datetime = as.POSIXct(paste(Date, TowTime),
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = "America/Los_Angeles"),
                  # Creating Length_NA_flag to parallel the other survey datasets in LTMR
                  # Creating an "Unknown Length" classifier when count is NA but catch isnt
                  Length_NA_flag = case_when(is.na(Count) & !is.na(Catch) ~ "Unknown length",
                                             is.na(Count) ~ "No fish caught",
                                             TRUE ~ NA_character_),
                  # For instances of unknown length, simply use the catch number if present
                  Count = ifelse(!is.na(Catch) & is.na(Count),
                                 Catch, Count),
                  # Calculating tow volume using net area of 1.51
                  Tow_volume = 1.51 * kFactor_final * MeterCheck)
  
  # There are two instances in which subsampling of Delta Smelt occurred. I will
  # simply flag these two instances
  subsamplingDF <- TTMM %>% 
    filter(FishCode == 3,
           Catch != TotalMeasured) %>% 
    mutate(Length = NA,
           Count = Catch - TotalMeasured,
           Length_NA_flag2 = "Subsampling issue") %>% 
    distinct(Source, Station, Latitude, Longitude, Date, Datetime, Survey,
             TowNum, Depth, SampleID, Method, Tide, Sal_surf, Temp_surf, Secchi,
             Tow_volume, Cable_length, Taxa, Length, Count,
             Length_NA_flag2, Notes_survey, Notes_station, Notes_gear,
             SurveyID, StationID, TowID, GearID, FishSampleID) %>% 
    select(Station, Date, Survey, TowNum, Taxa, Length_NA_flag2)
  
  # Joining both TTMM dataframes together, basically adding the "subsamplingDF"
  TTMM <- TTMM %>% 
    left_join(subsamplingDF, 
              # Everything except Length_NA_flag
              by = c("Date", "Survey", "Station", "TowNum", "Taxa")) %>% 
    mutate(Length_NA_flag = ifelse(!is.na(Length_NA_flag2), Length_NA_flag2, Length_NA_flag)) %>% 
    select(-Length_NA_flag2)
  
  # Finish up with rest of the code from Lara, from 159 on
  
  # TTMM %>% 
  #   filter(Date %in% as.Date("2021-03-23"), 
  #          Station == 610,
  #          TowNum == 1,
  #          Taxa == "Gasterosteus aculeatus",
  #          is.na(Catch))
  # # This was code from Lara; it seems as thought this was fixed in the actual database so this shoudl return a 0

  # If any of the QAQC steps to check for data joining errors has an error, you will need to check
  # what happened here
  if (any(!sapply(unlist(joinCheckSteps), function(x) x == T))) {
    warning("There are unaccounted for additions/deletions to the joining process. Check steps.", call. = F)
    # browser()
    TMM <- TTMM %>% 
      # Doing initial 0 fillings, which will only be for instances when a row catches 0 fish at all
      # Initial 0 filling: only fill in 0s when sampling occurred but NO catch of any spp occurred
      dplyr::mutate(Count = ifelse(Length_NA_flag %in% "No fish caught",
                                   0,
                                   Count)) %>% 
      arrange(SurveyID, StationID, TowID, GearID, FishSampleID) %>% 
      select(Source, Station, Latitude, Longitude, Date, Datetime, Survey,
             TowNum, Depth, SampleID, Method, Tide, Sal_surf, Temp_surf, Secchi,
             Tow_volume, Cable_length, Taxa, Length, Count,
             Length_NA_flag, Notes_survey, Notes_station, Notes_gear) %>% 
      data.frame()
  } else {
    TMM <- TTMM %>% 
      # Doing initial 0 fillings, which will only be for instances when a row catches 0 fish at all
      # Initial 0 filling: only fill in 0s when sampling occurred but NO catch of any spp occurred
      dplyr::mutate(Count = ifelse(Length_NA_flag %in% "No fish caught",
                                   0,
                                   Count)) %>% 
      arrange(SurveyID, StationID, TowID, GearID, FishSampleID) %>% 
      select(Source, Station, Latitude, Longitude, Date, Datetime, Survey,
           TowNum, Depth, SampleID, Method, Tide, Sal_surf, Temp_surf, Secchi,
           Tow_volume, Cable_length, Taxa, Length, Count,
           Length_NA_flag, Notes_survey, Notes_station, Notes_gear) %>% 
      data.frame()
  }

  # Filling in additional zeroes --------------------------------------------
 
 if (fillZeroes) {
   TMMZeroesFull <- TMM %>%
     dplyr::mutate(Taxa = factor(Taxa)) %>%
     # Fill in rows for all columns other than taxa, length, and count
     # Taxa = filled with the factors in taxa
     # Length = filled with NAs if missing
     # Count = filled with 0
     dplyr::group_by(Source, Station, Latitude, Longitude, Date, Datetime, Survey,
                     TowNum, Depth, SampleID, Method, Tide, Sal_surf, Temp_surf, Secchi,
                     Tow_volume, Cable_length,
                     Length_NA_flag, Notes_survey, Notes_station, Notes_gear) %>%
     # Fill in rows for all Station, Date, Datetime, Survey, Method
     # To use this function, you may need to be in 64-bit R for the memory requirement
     # There will be a noticable lag in running complete()
     tidyr::complete(Taxa,
                     fill = list(Count = 0)) %>%
     data.frame()
   
   # Check to see if duplications/deletions ocurred in the length/catch columns
   # The other columns are not checked because they will be duplicated to accomodate the additional taxa
   zeroesPassed <- all.equal(TMM %>%
                               dplyr::select(Length, Count) %>%
                               dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(.x, na.rm = T))),
                             TMMZeroesFull %>%
                               dplyr::select(Length, Count) %>%
                               dplyr::summarise(dplyr::across(dplyr::everything(), ~sum(.x, na.rm = T))))
   
   if (isTRUE(zeroesPassed)) {
     TMMZeroesFull
   } else {
     warning("Duplications/deletions occurred in the final zero-filling step", call. = F)
     browser()
   }
 } else {
   # If the user does not want zero-filling, then simply return the TMM dataset. This one has 
   # only the preliminary zero filling (tows when 0 fish caught but water samples were taken)
   TMM
 }
}

TTMM <- TTMMIntegrate(filePath = file.path("data-raw", "20mm"))
TTMM <- TTMM %>% 
  filter(Date < as.Date("2021-12-01"))
write.csv(TTMM, file = file.path("data-raw", "20mm", "TMM.csv"), row.names = F)
