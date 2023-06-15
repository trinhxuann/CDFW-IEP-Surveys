# First, connect to the database
# Then, pull the respective tables to be manipulated
# Finally, integrate the tables into a long format
library(dm)
library(dplyr)
library(tidyr)

# Sourcing the required functions -----------------------------------------
source("~/Git/CDFW-IEP-Surveys/R/bridgeAccess.R")

# Connect -----------------------------------------------------------------
data <- bridgeAccess(file = "U:/NativeFish/SmeltData/DS-DATA/SKT_Query.accdb", 
                     tables = c("tblSample", "tblCatch", "tblOrganismCodes", 
                                "tblFishInfo", "SKT Station Sort Order for Reporting"), 
                     script = "~/Git/CDFW-IEP-Surveys/R/connectAccess.R") %>% 
  setNames(c("tblSample", "tblCatch", "tblOrganismCodes", 
             "tblFishInfo", "tblStationCoordinates"))

# Relational schema -------------------------------------------------------

relationalSchema <- as_dm(data) %>% 
  dm_add_pk(tblSample, SampleRowID) %>% 
  dm_add_pk(tblCatch, CatchRowID) %>% 
  dm_add_pk(tblOrganismCodes, OrganismCode) %>% 
  dm_add_pk(tblFishInfo, LengthRowID) %>% 
  dm_add_pk(tblStationCoordinates, Station) %>% 
  dm_add_fk(tblSample, StationCode, tblStationCoordinates) %>% 
  dm_add_fk(tblCatch, OrganismCode, tblOrganismCodes) %>% 
  dm_add_fk(tblCatch, SampleRowID, tblSample) %>% 
  dm_add_fk(tblFishInfo, CatchRowID, tblCatch) %>%
  dm_set_colors("#344D67" = c(tblFishInfo, tblCatch, tblOrganismCodes),
                "#6ECCAF" = tblSample,
                "#ADE792" = tblStationCoordinates) %>% 
  dm_draw()

# Now save it
htmlwidgets::saveWidget(relationalSchema, 
                        file = file.path("data-raw", "SKT", "sktRelationalSchema.html"), 
                        libdir = 'html_support_files')
pagedown::chrome_print(file.path("data-raw", "SKT", "sktRelationalSchema.html"), 
                       selector = "#a_graph0",
                       format = "png", scale = 5)

# Joining the relational tables together ----------------------------------

# meter number...?

joined <- data$tblSample %>%
  select(-JustEdited) %>% 
  full_join(data$tblCatch, "SampleRowID") %>% 
  full_join(data$tblFishInfo %>% 
              select(-c(FishID1, FishID2, ReleasedAlive, LengthRace, CWTRace, CWTNumber)), 
            "CatchRowID") %>% 
  left_join(data$tblOrganismCodes %>% 
              select(OrganismCode, CommonName), "OrganismCode") %>% 
  left_join(data$tblStationCoordinates %>% 
              transmute(Station,
                        LatitudeTheoretical = LatDeg + LatMin/60 + LatSec/3600,
                        LongitudeTheoretical = -(LongDeg + LongMin/60 + LongSec/3600)),
            by = c("StationCode" = "Station")) %>% 
  mutate(across(c(LatDeg, LatMin, LatSec, LongDeg, LongMin, LongSec),
                ~as.numeric(.x))) %>% 
  group_by(SampleRowID, CatchRowID) %>% 
  add_tally(name = "TotalMeasured") %>% 
  ungroup() %>% 
  transmute(SampleDate = as.Date(SampleDate), 
            SurveyNumber, StationCode, 
            Latitude = LatDeg + LatMin/60 + LatSec/3600,
            Longitude = -(LongDeg + LongMin/60 + LongSec/3600),
            LatitudeTheoretical, LongitudeTheoretical,
            SampleTimeStart = as.POSIXct(paste0(SampleDate, format(SampleTimeStart, "%H:%M:%M")), 
                                         format = "%Y-%m-%d %H:%M:%M", tz = "America/Los_Angeles"),
            SampleTimeEnd = as.POSIXct(paste0(SampleDate, format(SampleTimeEnd, "%H:%M:%M")), 
                                       format = "%Y-%m-%d %H:%M:%M", tz = "America/Los_Angeles"),
            TowTime = as.numeric(difftime(SampleTimeEnd, SampleTimeStart, units = "mins")),
            # secchi from cm to m
            Secchi = Secchi/100, 
            ConductivityTop, 
            # Salinity to be calculated from conductivity
            SalinityTop = wql::ec2pss(ConductivityTop/1000, t = 25),
            WaterTemperature, Turbidity, 
            # Depth from ft to m
            DepthBottom = DepthBottom * 0.3048,
            MeterDifference = MeterEnd - MeterStart,
            TowVolume = 13.9446 * 0.026873027 * MeterDifference,
            TideCode = case_when(TideCode == 1 ~ "high",
                                 TideCode == 2 ~ "ebb",
                                 TideCode == 3 ~ "low",
                                 TideCode == 4 ~ "flood"), 
            TowDirectionCode = case_when(TideCode == 1 ~ "with",
                                         TideCode == 2 ~ "against",
                                         TideCode == 3 ~ "unknown"),
            OrganismCode, CommonName, ForkLength, 
            # Catch is some times designated as -1 for no catch. Replacing with 0 to be consistent
            Catch = ifelse(Catch == -1 & CommonName == "No Catch", 0, Catch), 
            TotalMeasured, 
            # If length frequency > catch, then just use length frequency. However, as of 2023-01-25,
            # there are no instances of this in the SKT dataset. Note in the SKT database, there is no
            # LengthFrequency column as EACH fish is recorded, even if other fish are of the same FL
            AdjustedCount = (1/TotalMeasured) * (ifelse(Catch < TotalMeasured, TotalMeasured, Catch)),
            Length_NA_flag = case_when(is.na(ForkLength) & CommonName == "No Catch" ~ "noCatch",
                                       is.na(ForkLength) & CommonName %in% c("Siberian prawn", "palaemon shrimp", "crangon shrimp",
                                                                             "Maeotias", "Jellyfish", "Pleurobrachia", 
                                                                             "Mississippi Grass Shrimp",
                                                                             "Bell jelly", "Blackfordia") ~ "invertebrate"),
            # Only osmeridae are sexed; removing sex data for all other species (all labeled as "unknown")
            Sex = case_when(Sex == 0 & CommonName %in% c("Delta Smelt", "wakasagi", "longfin smelt") ~ "unknown",
                            Sex == 1 ~ "male",
                            Sex == 2 ~ "female"),
            ReproductiveStage, 
            ReproductiveStageSecond = X2ndStage,
            AdFinPresent, VIE, 
            SampleComments, CatchComments,
            SampleRowID, CatchRowID, LengthRowID) %>% 
  arrange(SampleRowID, CatchRowID, LengthRowID) %>% 
  # Renaming to DUWG recommended standards; not all variables will have a recommendation/need to be changed
  rename(StartTime = SampleTimeStart, EndTime = SampleTimeEnd,
         Duration = TowTime, SpecificConductanceTop = ConductivityTop,
         WaterTempTop = WaterTemperature, TurbidityTop = Turbidity,
         BottomDepth = DepthBottom, FlowMeterDifference = MeterDifference,
         Volume = TowVolume, )
# How to deal with when catch < total measured esp for instances where NAs are present (which increases total measured)

# Code to zero fill across ALL species and all samples. Not going to be included as a product with EDI but code is here to use:
# joinedZeroes <- joined %>%
#   mutate(across(c(OrganismCode), ~factor(.x)),
#          zeroFilled = F) %>%
#   group_by(SampleDate, SurveyNumber, StationCode, Latitude, Longitude,
#            SampleTimeStart, SampleTimeEnd, TowTime, Secchi, ConductivityTop,
#            SalinityTop, WaterTemperature, Turbidity, DepthBottom,
#            MeterDifference, TowVolume, TideCode, TowDirectionCode,
#            SampleRowID, CatchRowID) %>%
#   tidyr::complete(OrganismCode,
#                   fill = list(Catch = 0, AdjustedCount = 0, TotalMeasured = 0, zeroFilled = T)) %>%
#   select(-CommonName) %>%
#   left_join(data$tblOrganismCodes %>%
#               transmute(OrganismCode = factor(OrganismCode),
#                         CommonName),
#             by = "OrganismCode") %>%
#   relocate(CommonName, .after = OrganismCode) %>%
#   relocate(zeroFilled, .after = Length_NA_flag) %>%
#   relocate(contains("ID"), .after = last_col()) %>% 
#   # Don't want the zerofilled no catches
#   filter(CommonName == "No Catch" & zeroFilled == F |
#            CommonName != "No Catch") %>%
#   arrange(SampleRowID, CatchRowID, OrganismCode, LengthRowID)

# Writing the file --------------------------------------------------------
dataFin <- list(data$tblSample %>% 
                  arrange(SampleRowID),
                data$tblCatch %>% 
                  arrange(CatchRowID),
                data$tblFishInfo %>% 
                  arrange(LengthRowID),
                data$tblOrganismCodes %>% 
                  arrange(OrganismCode),
                data$tblStationCoordinates) %>% 
  setNames(names(data))

currentTime <- Sys.time()

# Relational tables
lapply(seq_along(dataFin), function(i) {
         filePath <- paste0("data-raw/SKT/filesToUpload", names(dataFin)[[i]], ".csv")
         write.csv(dataFin[[i]], 
                   filePath,
                   row.names = F)
       })

# Joined file
joined %>% 
  filter(SampleDate < if_else(as.numeric(format(currentTime, "%m")) < 11, 
                              as.Date(paste0(as.numeric(format(currentTime, "%Y")) - 1, "-10-01")),
                              as.Date(paste0(as.numeric(format(currentTime, "%Y")), "-10-01")))) %>% 
  write.csv("data-raw/SKT/filesToUpload/sktJoined.csv", 
            row.names = F)

