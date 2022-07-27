# This is a slight modificaiton of the LTMRdata 20 mm code by Lara Mitchell
# https://github.com/sbashevkin/LTMRdata/blob/master/data-raw/20mm.R

## Save select tables:
keepTables <- c("Tow","FishSample","FishLength","Survey","Station",
                "20mmStations","Gear","GearCodesLkp","MeterCorrections","SampleCode")

#########################################################################################
## Create and save compressed data files using raw tables.

library(dplyr)
library(lubridate)
library(wql)
require(LTMRdata)

data <- readRDS(file.path("data-raw", "20mm", "TTmmTables.rds"))


# # Some changes between what is currently in the database (02092022) vs what's shown on LTMRdata:
# Survey <- read.csv(file.path("data-raw", "20mm", "backup", "Survey.csv"), stringsAsFactors=FALSE)
# Station <- read.csv(file.path("data-raw", "20mm", "backup", "Station.csv"), stringsAsFactors=FALSE)
# # 12 mismatch between Comments column; not a problem. Simply based on 
# # how unicode characters are read in, specifically, <U+0092>
# Tow <- read.csv(file.path("data-raw", "20mm", "backup", "Tow.csv"), stringsAsFactors=FALSE)
# Gear <- read.csv(file.path("data-raw", "20mm", "backup", "Gear.csv"), stringsAsFactors=FALSE)
# # Comments read differences
# GearCodesLkp <- read.csv(file.path("data-raw", "20mm", "backup", "GearCodesLkp.csv"), stringsAsFactors=FALSE)
# MeterCorrections <- read.csv(file.path("data-raw", "20mm", "backup", "MeterCorrections.csv"), stringsAsFactors=FALSE)
# # 3 additional entries for 2022 recorded
# TmmStations <- read.csv(file.path("data-raw", "20mm", "backup", "20mmStations.csv"), stringsAsFactors=FALSE)
# # This table is not specifically in 20 mm back-end database...will have to follow up with Adam
# FishSample <- read.csv(file.path("data-raw", "20mm", "backup", "FishSample.csv"), stringsAsFactors=FALSE)
# # FishSampleID: 79815, 79816 ADDED; 3 FishSampleIDs REMOVED, 77487, 72447, 72446
# # Removal of these IDs gives the same data.frame
# FishLength <- read.csv(file.path("data-raw", "20mm", "backup", "FishLength.csv"), stringsAsFactors=FALSE)
# # Differences due to the FishSampleID added/removed identified above

# Some changes between what is currently in the database (02092022) vs what's shown on LTMRdata:
Survey <- data$Survey %>% 
  rename(Notes_survey = Comments)
Station <- data$Station %>% 
  rename(Notes_station = Comments)
# 12 mismatch between Comments column; not a problem. Simply based on 
# how unicode characters are read in, specifically, <U+0092>
Tow <- data$Tow
Gear <- data$Gear %>% 
  rename(Notes_gear = Comments)
# Comments read differences
GearCodesLkp <- data$GearCodesLkp
MeterCorrections <- data$MeterCorrections
# 3 additional entries for 2022 recorded
TmmStations <- read.csv(file.path("data-raw", "20mm", "20mmStations.csv"), stringsAsFactors=FALSE)
# This table is not specifically in 20 mm back-end database...will have to follow up with Adam
FishSample <- data$FishSample
# FishSampleID: 79815, 79816 ADDED; 3 FishSampleIDs REMOVED, 77487, 72447, 72446
# Removal of these IDs gives the same data.frame
FishLength <- data$FishLength
# Differences due to the FishSampleID added/removed identified above

# This table will fill in meters that do not have correction factors with
# the average for that meter across all years
# There was only 1 entry that was too high to enter and was left as NA
# meter 20780 calibrated on 2012-12-27
MeterCorrections_avg <- data$MeterCorrections %>%
  dplyr::group_by(MeterSerial) %>%
  dplyr::summarize(kFactor_avg=mean(kFactor), .groups="drop")

sample20mm <- Survey %>%
  dplyr::inner_join(Station, by="SurveyID") %>%
  dplyr::inner_join(Tow, by="StationID") %>%
  dplyr::inner_join(Gear, by="TowID") %>%
  dplyr::left_join(GearCodesLkp, by="GearCode") %>%
  dplyr::left_join(TmmStations, by="Station") %>%
  dplyr::mutate(StudyYear=lubridate::year(SampleDate)) %>%
  dplyr::left_join(MeterCorrections, by=c("StudyYear","MeterSerial")) %>%
  dplyr::left_join(MeterCorrections_avg, by="MeterSerial") %>%
  # Only want the fish net, NO zoop data here
  dplyr::filter(GearCode == 2) %>%
  # Net area 1.51 meter?
  dplyr::mutate(kFactor_final=ifelse(!is.na(kFactor), kFactor, kFactor_avg),
                Tow_volume=1.51*kFactor_final*MeterCheck) %>%
  dplyr::arrange(SampleDate, Survey, Station, TowNum) %>%
  dplyr::mutate(Source="20mm",
                SampleID=paste(Source, 1:nrow(.)),
                Tow_direction=NA,
                ## Tide codes from 20mmDataFileFormat_New_102621.pdf on the CDFW ftp site:
                Tide=recode(Tide, `1`="High Slack", `2`="Ebb", `3`="Low Slack", `4`="Flood"),
                TowTime=substring(TowTime,12),
                Datetime=paste(SampleDate, TowTime),
                # TN: opting to have this NOT in posixct format. That format will be 
                # reflected in the datetime category already. Will leave this as a "Date"
                # specification
                Date=SampleDate,
                Datetime=parse_date_time(if_else(is.na(TowTime), NA_character_, Datetime),
                                         "%Y-%m-%d %%H:%M:%S", tz="America/Los_Angeles"),
                Depth=BottomDepth*0.3048, # Convert depth to m from feet
                Cable_length=CableOut*0.3048, # Convert to m from feet
                Method=GearDescription,
                Temp_surf=Temp,
                ## Convert conductivity to salinity; TopEC is in micro-S/cm; input should
                ##		be in milli-S/cm:
                Sal_surf=wql::ec2pss(TopEC/1000, t=25),
                Latitude=(LatD + LatM/60 + LatS/3600),
                Longitude= -(LonD + LonM/60 + LonS/3600)) %>%
  dplyr::select(GearID, Source, Station, Latitude, Longitude, Date, Datetime, Survey,
                TowNum, Depth, SampleID, Method, Tide, Sal_surf, Temp_surf,
                Secchi, Tow_volume, Tow_direction, Cable_length,
                Duration, Turbidity, Notes_survey, Notes_station, Notes_gear)

## Note:
## GearID 33397 has two entries for fish code 10 in FishSample
## GearID 35766 has two entries for fish code 2 in FishSample
## Based on lengths, probably a matter of being ID'd in the field vs. in the lab.

fish20mm_totalCatch <- FishSample %>%
  dplyr::select(GearID, FishSampleID, FishCode, Catch)

fish20mm_individLength <- FishSample %>%
  dplyr::inner_join(FishLength, by="FishSampleID") %>%
  dplyr::select(GearID, FishSampleID, FishCode, Length, Catch)

fish20mm_lengthFreq_measured <- fish20mm_individLength %>%
  dplyr::filter(!is.na(Length)) %>%
  dplyr::group_by(GearID, FishSampleID, FishCode, Length) %>%
  dplyr::summarize(LengthFrequency=n(), .groups="drop")

fish20mm_adjustedCount <- fish20mm_totalCatch %>%
  dplyr::left_join(fish20mm_lengthFreq_measured,
                   by=c("GearID","FishSampleID","FishCode")) %>%
  dplyr::group_by(GearID, FishSampleID, FishCode) %>%
  dplyr::mutate(TotalMeasured=sum(LengthFrequency, na.rm=T)) %>%
  dplyr::ungroup() %>%
  ## Add total catch numbers:
  ## There are some cases where the number of fish measured is greater than the
  ## catch value in the FishSample table. In these cases, use the number measured.
  dplyr::mutate(CatchNew=ifelse(TotalMeasured > Catch, TotalMeasured, Catch)) %>%
  ## Calculate length-frequency-adjusted counts:
  dplyr::mutate(Count=(LengthFrequency/TotalMeasured)*CatchNew) %>%
  dplyr::left_join(LTMRdata::Species %>% ## Add species names
                     dplyr::select(TMM_Code, Taxa) %>%
                     filter(!is.na(TMM_Code)),
                   by=c("FishCode"="TMM_Code"))

## Examine the cases where number measured > catch:
count_mismatch <- subset(fish20mm_adjustedCount, CatchNew != Catch)
# nrow(count_mismatch)


## Join sample and fish info:
TMM <- sample20mm %>%
  dplyr::left_join(fish20mm_adjustedCount %>%
                     dplyr::select(GearID, FishCode, Taxa, Length,
                                   LengthFrequency, Catch, CatchNew, Count),
                   by="GearID") %>%
  ## Add reasoning for any NA lengths:
  dplyr::mutate(Length_NA_flag=if_else(is.na(Catch), "No fish caught", NA_character_),
                Station=as.character(Station))

## There are some cases where:
##  -positive catch is indicated in FishSample, but there are no corresponding records
##		in FishLength.
## In these cases, use the Catch value from FishSample as Count and change
##	Length_NA_flag:
index_1 <- which(!is.na(TMM$Catch) & is.na(TMM$Count))
# TMM[index_1, ]
TMM$Count[index_1] <- TMM$Catch[index_1]
TMM$Length_NA_flag[index_1] <- "Unknown length"
# TMM[index_1, ]

# ## 2021-10-25: Adam Chorazyczewski of CDFW confirmed that catch for this
# ## record is 1:
# TN: This has now been corrected in the database itself, but will leave this code here although
# index_2 should be 0L
index_2 <- which(as.character(TMM$Date) == "2021-03-23" & TMM$Station == 610 &
                   TMM$TowNum == 1 & TMM$Taxa == "Gasterosteus aculeatus" &
                   is.na(TMM$Catch))

TMM[index_2,c("Catch","Count")] <- 1

TMM[index_2,"Length_NA_flag"] <- "Unknown length"

# colSums(is.na(TMM))[which(colSums(is.na(TMM)) == nrow(TMM))]
# # Tow_Direction and Notes_survey are all NAs

# Not needed for 20 mm EDI page
# ## Create final measured lengths data frame:
# TMM_measured_lengths <- TMM %>%
#   dplyr::select(SampleID, Taxa, Length, LengthFrequency) %>%
#   dplyr::rename(Count=LengthFrequency)
# nrow(TMM_measured_lengths)
# ncol(TMM_measured_lengths)
# names(TMM_measured_lengths)

## Now remove extra fields:
TMM <- TMM %>%
  # Joining all comments here in case each has data
  dplyr::mutate(Notes_tow = paste(Notes_station, Notes_gear, sep = "; ")) %>% 
  dplyr::select(-GearID, -Duration, -Turbidity, -Notes_station, -Notes_gear,
                -FishCode, -Catch, -CatchNew,
                -LengthFrequency) %>% 
  relocate(Notes_survey, Notes_tow, .after = last_col())

## Save compressed data to /data:
# usethis::use_data(TMM, TMM_measured_lengths, overwrite=TRUE)
write_csv(TMM, file = file.path("data-raw", "20mm", "20mm.csv"))
