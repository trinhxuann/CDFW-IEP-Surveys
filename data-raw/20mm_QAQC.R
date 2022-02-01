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

data <- readRDS(file.path("data-raw", "20mm", "TTmmTables.rds"))

# Empty list to house the outliers in
outliers <- list()

# When this analysis is up to date, will likely only want to see data from
# the current season. Currently, the season for 20 mm spans from Mar to ~July
# Also adding a second requirement that the latest data entry is later than Mar of current year

yearOfInterest <- (year(today()) - 1) + (month(today()) > 3 & max(data$Survey$SampleDate) > as.SampleDate(paste0(year(today()), "-03-01")))

# Make sure that the file names/table names are the same as what will be called here. They NEED to be:
# "Catch", "Lengths", "MeterCorrections", "Station_Lookup", "TowInfo", and "WaterInfo"
# Order of names listed in expectedNames variable does not matter
expectedNames <- c("Catch", "Lengths", "MeterCorrections", "TowInfo", "WaterInfo", "Station_Lookup")

if (!all(names(data) %in% expectedNames)) {
  stop("Table name(s) ", shQuote(names(data)[which(!names(data) %in% expectedNames)]),
          " do/does not have the defaults.")
}

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

# This step will be in two parts, using two main functions: plotGPS and findOutlierGPS below:

# Use plotGPS to visually determine where outliers may be
plotGPS <- function(df, station = NULL, Year = NULL, ...) {
  
  pal <- colorFactor("viridis", domain = c(df$group))
  
  if (is.null(Year)) {
    warning("This will plot all tows in the requested data table and may take a substantial amount of resources to complete.", call. = F)
  } else {
    df <- df %>% 
      filter(Year == Year | group == "TheoreticalCoords" | is.numeric(group))
  }
  
  if (!is.null(station)) {
    df <- df %>% 
      filter(Station == station)
  }
  
  leaflet(df,
          width = "100%",
          height = "800") %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addCircleMarkers(~Long, ~Lat,
                     label = ~as.character(Station),
                     color = ~pal(group),
                     radius = 6,
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
# 2001 looks good
plotGPS(GPSDF, Year = 2021, title = "Source")
plotGPS(GPSDF, Year = 2021, station = 809, title = "Source")

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
  
  # need the TheoreticalCoords filter because those do not have a year associated with them; ok to do it here
  # since the station filter goes first. When year is specified and no station, will display all theoretical stations
  # the is numeric group is for AFTER cluster have been chosen, so for dfClust df in the findOutlierGPS() function
  if (!is.null(Year)) df <- filter(df, Year %in% Year | group %in% "TheoreticalCoords")
  
  d <- geo.dist(select(df, Long, Lat) %>% 
                  na.omit())
  hc <- hclust(d)
  
  if (print & length(hc$order) > 2) plot(hc)
  
  if (!is.null(k)) {
    clusters <- cutree(hc, k)
    
    # Specifying the cluster
    dfClust <- df %>% 
      filter(!is.na(Long), !is.na(Lat)) %>% 
      # A bit of coding to get the theoretical coords to always be cluster 1
      # First, convert group into a factor with levels to arrange correctly
      # Second, create groupOrder as a factor and pull the numeric version of it to assign as clusters
      mutate(source = factor(group, levels = c("TheoreticalCoords", "Tow")),
             groupOrder = ifelse(group == "TheoreticalCoords", group, clusters)) %>% 
      arrange(source) %>% 
      mutate(groupOrder = factor(groupOrder, levels = unique(.$groupOrder)),
             group = as.numeric(groupOrder)) %>% 
      group_by(group) %>% 
      add_tally() %>% 
      ungroup()
    
    # Making the assumption here that if it is NOT in the same group as the theoretical coordinates,
    # then it is outlying
    theoreticalCluster <- dfClust %>% 
      filter(is.na(SampleDate), is.na(Temp), is.na(LonD)) %>% 
      pull(group)
    
    dfClust <- dfClust %>% 
      mutate(Outlier = ifelse(group == theoreticalCluster, F, T))
    
    if (print) print(plotGPS(dfClust, station = station, Year = Year, title = "Cluster"))
    
    dfClust %>% 
      select(SampleDate, Year, Survey, Station, Temp, TopEC, BottomEC, Secchi, Turbidity,
             Lat, Long, contains("Comments"), Cluster = group, Outlier) %>% 
      arrange(-Outlier)
  } else {
    cat("Pick a the number of clusters, k using the map displayed. \n")
    plotGPS(df, station = station, Year = Year, title = "Source")
  }
}

# Edit – GPS Coordinates
# These are stations that I see that may be outlying across ALL years; 
# IMPORTANT:
# k was chosen after visual inspection of the produced dendogram
# print = F used AFTER confirming with the visualizations that this is what you want

GPSOutlying <- list()
# Example:
# GPSOutlying[[1]] <- findOutlierGPS(GPSDF, station = 809, Year = 2022)

# Might be useful to plot all years data for the particular station of interest?
# Here are 4 stations that are grossly outlier when plotting the entire dataset
# Note all k = 3 but that's just coincidence
findOutlierGPS(GPSDF, station = 323)
findOutlierGPS(GPSDF, station = 323, k = 3)

findOutlierGPS(GPSDF, station = 812, k = 3)
findOutlierGPS(GPSDF, station = 340, k = 3)
findOutlierGPS(GPSDF, station = 719, k = 3)

# Now placing them into outlier table to be exported later
GPSOutlying[[1]] <- findOutlierGPS(GPSDF, station = 323, k = 3, print = F)
GPSOutlying[[2]] <- findOutlierGPS(GPSDF, station = 812, k = 3, print = F)
GPSOutlying[[3]] <- findOutlierGPS(GPSDF, station = 340, k = 3, print = F)
GPSOutlying[[4]] <- findOutlierGPS(GPSDF, station = 719, k = 3, print = F)

# For multiple stations, can bind these together into a singular data frame
# Would envision all clusters other than 1 would be classified as outliers, something like:
outliers$GPSCoordinates <- lapply(GPSOutlying, function(x) filter(x, Outlier == T)) %>% 
  bind_rows() %>% 
  bind_rows(data$Station %>% 
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
              filter(is.na(Lat) | is.na(Long)) %>% 
              mutate(across(c(Lat, Long), ~suppressWarnings(as.numeric(.x))),
                     NAFlag = T,
                     Outlier = F)) %>% 
  mutate(NAFlag = ifelse(is.na(NAFlag), F, NAFlag),
         Year = ifelse(is.na(Year), year(SampleDate), Year))
# Note, this does NOT have a Year %in% yearOfInterest filter; this step should of been taken care of
# when manually surveying which station is outlying

# Cable outliers ----------------------------------------------------------

# Equivalent to "Edit - Cable Out (outliers)

# What the script does: If CableOut is NOT standard during a tow with bottom depth > 5, return as outlier
# Removing this entire query as it is redundant compared to the Cable out VS depth query (approved by Adam)

# Equivalent to "Edit - Cable out VS depth"

# What the script does: For each CableOut, there is a range of accompanying BottomDepth. If outside that combo,
# return as an outlier

outliers$CableDepth <- full_join(data$Survey,
                                 data$Station,
                                 by = "SurveyID") %>% 
  full_join(data$Tow, 
            by = c("StationID")) %>% 
  transmute(Year = year(SampleDate),
            SampleDate = as.SampleDate(SampleDate),
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
# Join to the water info table is needed to get "survey"
# Checked 11-24-21 by TN that this matches the DB query, by summing each column of resulting DF
# 12-6-21; changing this to Outlier column format == still same output as 11-24-21 BEFORE adding in values with
# NAFlag; Adding in the NAFlag column will mean the final table will have more flagged values than on 11-24-21 (thus on Access DB)

# Meter readings ----------------------------------------------------------

# ASK ADAM Not a part of 20 mm query DB ####

# Equivalent to "Edit - Meter Reading Out of Range"
# What the script does: Check values of duration and flow meter readings; those beyond these thresholds
# are outliers; these values thresholds = field tested

outliers$MeterReading <- full_join(data$Survey, data$Station,
                                   by = "SurveyID") %>%
  full_join(data$Tow, by = "StationID") %>% 
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
# Checked 11-24-21 by TN that this matches the DB query, small enough to check that all is equal
# Checked 12-06-21 by TN that addition of Outlier and NAFlag does not break code

# Equivalent to "Edit - Meter Reading Out of Range"
# What the script does: Check values of duration and flow meter readings; those beyond these thresholds
# are outliers; these values thresholds = field tested

outliers$CB_MeterReading <- full_join(data$Survey, data$Station,
                                   by = "SurveyID") %>%
  full_join(data$Tow, by = "StationID") %>% 
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
# Checked 11-24-21 by TN that this matches the DB query, small enough to check that all is equal
# Checked 12-06-21 by TN that addition of Outlier and NAFlag does not break code

# Equivalent to "Edit - Net Meter Serial"
# plot out flow meter checks, by diff flow meter serials; to determine when a certain flow meter failed during a season

outliers$NetMeterSerial <- full_join(data$Survey, data$Station,
                                     by = "SurveyID") %>%
  full_join(data$Tow, by = "StationID") %>% 
  full_join(data$Gear, by = "TowID") %>% 
  transmute(Year = year(SampleDate), 
            Survey, 
            MeterSerial,
            GearCode) %>% 
  filter(GearCode %in% 2,
         Year %in% yearOfInterest) %>% 
  group_by(Year, Survey, MeterSerial, GearCode) %>% 
  count(name = "CountOfMeterSerial")
# I believe this script is used for other analyses by Native Fishes. Will keep for now
# Checked 11-24-21 by TN that this matches the DB query, small enough to check that all is equal

# Equivalent to "Edit - Tow duration (outliers)"

# What the script does: find data points where duration is NOT equal to 2.5, 5, or 10 min; these are
# the standard tow lengths for this survey

outliers$TowDuration <- full_join(data$Survey, data$Station,
                                  by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  mutate(Year = year(SampleDate),
         Outlier = ifelse(!Duration %in% c(2.5, 5, 10),
                          T, F),
         NAFlag = ifelse(is.na(Duration),
                         T, F)) %>% 
  select(SampleDate, Survey, Year, ends_with("ID"), Station,
         TowNum, Duration, contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest)
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

outliers$BottomDepth <- full_join(data$Survey, data$Station,
                                  by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanBottomDepth = mean(BottomDepth, na.rm = T),
         sd2down = meanBottomDepth - 2 * sd(BottomDepth, na.rm = T),
         sd2up = meanBottomDepth + 2 * sd(BottomDepth, na.rm = T),
         # countStation = n(), # Removing this portion of the code; it is a part of query 1 but not used elsewhere
         # This is part 2 of the query
         Outlier = ifelse((BottomDepth < sd2down | BottomDepth > sd2up), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomDepth), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, ends_with("ID"), Year, Station, BottomDepth, meanBottomDepth, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)
# Checked 11-24-21 by TN that this matches the DB query, summation of the numeric columns equal

# BottomDepthMonth

outliers$BottomDepthMonth <- full_join(data$Survey, data$Station,
                                       by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station, Month = month(SampleDate)) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanBottomDepth = mean(BottomDepth, na.rm = T),
         sd2down = meanBottomDepth - 2 * sd(BottomDepth, na.rm = T),
         sd2up = meanBottomDepth + 2 * sd(BottomDepth, na.rm = T),
         # countStation = n(), # Removing this portion of the code; it is a part of query 1 but not used elsewhere
         # This is part 2 of the query
         Outlier = ifelse((BottomDepth < sd2down | BottomDepth > sd2up), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomDepth), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, ends_with("ID"), Year, Month,
         Station, BottomDepth, meanBottomDepth, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Temp III
outliers$Temp <- full_join(data$Survey, data$Station,
                           by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanTemp = mean(Temp, na.rm = T),
         sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
         sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
         NAFlag = ifelse(is.na(Temp), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"), Year, Station, Temp, meanTemp, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# TempMonth
outliers$TempMonth <- full_join(data$Survey, data$Station,
                                by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station, Month = month(SampleDate)) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanTemp = mean(Temp, na.rm = T),
         sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
         sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
         NAFlag = ifelse(is.na(Temp), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"), Year, Month,
         Station, Temp, meanTemp, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Top EC III
outliers$TopEC <- full_join(data$Survey, data$Station,
                            by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanTopEC = mean(TopEC, na.rm = T),
         sd2down = meanTopEC - 2 * sd(TopEC, na.rm = T),
         sd2up = meanTopEC + 2 * sd(TopEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < sd2down | TopEC > sd2up), T, F),
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"),
         Year, Station, TopEC, meanTopEC, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# TopECMonth
outliers$TopECMonth <- full_join(data$Survey, data$Station,
                                 by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station, Month = month(SampleDate)) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanTopEC = mean(TopEC, na.rm = T),
         sd2down = meanTopEC - 2 * sd(TopEC, na.rm = T),
         sd2up = meanTopEC + 2 * sd(TopEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < sd2down | TopEC > sd2up), T, F),
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"),
         Year, Month, Station, TopEC, meanTopEC, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Bottom EC III
outliers$BottomEC <- full_join(data$Survey, data$Station,
                               by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanBottomEC = mean(BottomEC, na.rm = T),
         sd2down = meanBottomEC - 2 * sd(BottomEC, na.rm = T),
         sd2up = meanBottomEC + 2 * sd(BottomEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < sd2down | BottomEC > sd2up), T, F),
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"),
         Year, Station, BottomEC, meanBottomEC, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# BottomECMonth
outliers$BottomECMonth <- full_join(data$Survey, data$Station,
                                    by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station, Month = month(SampleDate)) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanBottomEC = mean(BottomEC, na.rm = T),
         sd2down = meanBottomEC - 2 * sd(BottomEC, na.rm = T),
         sd2up = meanBottomEC + 2 * sd(BottomEC, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < sd2down | BottomEC > sd2up), T, F),
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"), 
         Year, Month, Station, BottomEC, meanBottomEC, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Secchi III
outliers$Secchi <- full_join(data$Survey, data$Station,
                             by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanSecchi = mean(Secchi, na.rm = T),
         sd2down = meanSecchi - 2 * sd(Secchi, na.rm = T),
         sd2up = meanSecchi + 2 * sd(Secchi, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < sd2down | Secchi > sd2up), T, F),
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"), 
         Year, Station, Secchi, meanSecchi, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# SecchiMonth
outliers$SecchiMonth <- full_join(data$Survey, data$Station,
                                  by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station, Month = month(SampleDate)) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanSecchi = mean(Secchi, na.rm = T),
         sd2down = meanSecchi - 2 * sd(Secchi, na.rm = T),
         sd2up = meanSecchi + 2 * sd(Secchi, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < sd2down | Secchi > sd2up), T, F),
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"), 
         Year, Month, Station, Secchi, meanSecchi, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Turbidity III
outliers$Turbidity <- full_join(data$Survey, data$Station,
                                by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanTurbidity = mean(Turbidity, na.rm = T),
         sd2down = meanTurbidity - 2 * sd(Turbidity, na.rm = T),
         sd2up = meanTurbidity + 2 * sd(Turbidity, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < sd2down | Turbidity > sd2up), T, F),
         NAFlag = ifelse(is.na(Turbidity), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"),
         Year, Station, Turbidity, meanTurbidity, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# TurbidityMonth
outliers$TurbidityMonth <- full_join(data$Survey, data$Station,
                                     by = "SurveyID") %>% 
  full_join(data$Tow, by = "StationID") %>% 
  group_by(Station, Month = month(SampleDate)) %>% 
  mutate(Year = year(SampleDate),
         # This is part 1 of the query
         meanTurbidity = mean(Turbidity, na.rm = T),
         sd2down = meanTurbidity - 2 * sd(Turbidity, na.rm = T),
         sd2up = meanTurbidity + 2 * sd(Turbidity, na.rm = T),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < sd2down | Turbidity > sd2up), T, F),
         NAFlag = ifelse(is.na(Turbidity), T, F)) %>% 
  arrange(SampleDate) %>% 
  # This is part 3 of the query
  select(SampleDate, ends_with("ID"),
         Year, Month, Station, Turbidity, meanTurbidity, sd2down, sd2up, 
         contains("Comments"), Outlier, NAFlag) %>% 
  mutate(SampleDate = as.Date(SampleDate)) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Various plots for station vs station/months -----------------------------
# # These are simply exploratory plots to see how the two variants differ from one another
# # Does one variant yield much more "outliers" than others?
# 
# data$WaterInfo %>%
#   group_by(Station, Month = month(SampleDate)) %>%
#   mutate(# This is part 1 of the query
#     meanTemp = mean(Temp, na.rm = T),
#     sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
#     sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
#     # This is part 2 of the query
#     Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
#     NAFlag = ifelse(is.na(Temp), T, F)) %>%
#   arrange(SampleDate) %>%
#   # This is part 3 of the query
#   select(SampleDate, Station, Temp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>%
#   mutate(SampleDate = as.SampleDate(SampleDate)) %>%
#   ggplot(aes(SampleDate, Temp, color = Outlier)) +
#   geom_point() +
#   labs(title = "Grouped by station and month")
# # Seems like for temp, it might be worth it to use a "region" to calculate the mean/sd or exclude certain stations
# # Alot of these outliers also appear to have occurred during the drought, with the higher temps being a bit outlying
# 
# data$WaterInfo %>%
#   group_by(Station) %>%
#   mutate(# This is part 1 of the query
#     meanTemp = mean(Temp, na.rm = T),
#     sd2down = meanTemp - 2 * sd(Temp, na.rm = T),
#     sd2up = meanTemp + 2 * sd(Temp, na.rm = T),
#     # This is part 2 of the query
#     Outlier = ifelse((Temp < sd2down | Temp > sd2up), T, F),
#     NAFlag = ifelse(is.na(Temp), T, F)) %>%
#   arrange(SampleDate) %>%
#   # This is part 3 of the query
#   select(SampleDate, Station, Temp, meanTemp, sd2down, sd2up, Comments, Outlier, NAFlag) %>%
#   mutate(SampleDate = as.SampleDate(SampleDate)) %>%
#   ggplot(aes(SampleDate, Temp, color = Outlier)) +
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

# # QAQC FL -----------------------------------------------------------------
# 
# data$Lengths %>% 
#   group_by(Month = month(SampleDate),
#            FishCode) %>% 
#   mutate(# This is part 1 of the query
#     meanLengths = mean(Length, na.rm = T),
#     sd2down = meanLengths - 2 * sd(Length, na.rm = T),
#     sd2up = meanLengths + 2 * sd(Length, na.rm = T),
#     # This is part 2 of the query
#     # Why this 12-31 cut off...? Ask Adam ####
#     Outlier = ifelse((Length < sd2down | Length > sd2up) & SampleDate > as.SampleDate("2012-12-31"), T, F),
#     NAFlag = ifelse(is.na(Length), T, F)) %>% 
#   arrange(SampleDate) %>% 
#   # This is part 3 of the query
#   select(SampleDate, Month, FishCode, Length, meanLengths, sd2down, sd2up, Outlier, NAFlag) %>% 
#   mutate(SampleDate = as.SampleDate(SampleDate)) %>% 
#   filter(Outlier == T | NAFlag == T) %>% 
#   arrange(NAFlag, Outlier, SampleDate) %>% 
#   View()
# 
# # Turkey's fence, 1.5 and 3.0
# data$Lengths %>% 
#   group_by(Month = month(SampleDate),
#            FishCode) %>% 
#   mutate(# This is part 1 of the query
#     quant25 = quantile(Length, probs = 0.25, na.rm = T),
#     quant75 = quantile(Length, probs = 0.75, na.rm = T),
#     iqr = quant75 - quant25,
#     Outlier = ifelse((Length < quant25 - (iqr * 3) | Length > quant75 + (iqr * 3)) & SampleDate > as.SampleDate("2012-12-31"), T, F),
#     NAFlag = ifelse(is.na(Length), T, F)) %>% 
#   arrange(SampleDate) %>% 
#   # This is part 3 of the query
#   select(SampleDate, Month, FishCode, Length, quant25, quant75, iqr, Outlier, NAFlag) %>% 
#   mutate(SampleDate = as.SampleDate(SampleDate)) %>% 
#   filter(Outlier == T | NAFlag == T) %>% 
#   arrange(NAFlag, Outlier, SampleDate) %>% 
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
