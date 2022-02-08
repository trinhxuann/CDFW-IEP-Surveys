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


# # Example: this plots ALL stations across a year of interest; Year can be left blank if
# # you want all stations across all years plotted (will be slow though)
# # 2001 looks good
# plotGPS(GPSDF, Year = 2021, title = "Source")
# plotGPS(GPSDF, Year = 2021, station = 809, title = "Source")

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
# IMPORTANT:
# k was chosen after visual inspection of the produced dendogram
# print = F used AFTER confirming with the visualizations that this is what you want

GPSOutlying <- list()
# Example:
# GPSOutlying[[1]] <- findOutlierGPS(GPSDF, station = 809, Year = 2022)

# # Might be useful to plot all years data for the particular station of interest?
# # Example of workflow through this function:
# # Here are 4 stations that are grossly outlier when plotting the entire dataset
# # Note all k = 3 but that's just coincidence
# findOutlierGPS(GPSDF, station = 323)
# findOutlierGPS(GPSDF, station = 323, k = 3)
# 
# findOutlierGPS(GPSDF, station = 812, k = 3)
# findOutlierGPS(GPSDF, station = 340, k = 3)
# findOutlierGPS(GPSDF, station = 719, k = 3)

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

# For 20mm, the joining keys are autonumbered ID columns. To get date, the Survey dataset has to be joined.
# Since this step is repeated before every query, will simply write a short function to join the
# Survey, Station, and Tow dataframes together

joinSurveyStationTow <- function(skipTow = F) {
  
  df <- full_join(data$Survey,
            data$Station,
            by = "SurveyID") 
  
  if (anyDuplicated(df) != 0) {
    stop("Join of tables Survey and Station produced duplicated data when it should not.")
  }
  
  if (!skipTow) {
    # Final step, this will cause duplicates as intended since env data only recorded once per station
    full_join(df, data$Tow, 
              by = c("StationID"))
  } else {
    df
  }
}

# Each of query is a replication of the access version. As such, what is outputted here should be the same
# as the Access version; this following function will compare that
# Need to be connected to access for this

connectAccess <- function(file,
                          exdir = tempdir(),
                          surveyName = "20mm") {
  
  if (exists("con")) {
    test <- try(dbIsValid(con))
    
    if (!inherits(test, "try-error")) {
      warning("Access connection already established", call. = F)
      return()
    }
  }
  
  # In order to use this correctly, you need to have the 32-bit version of R installed
  # This function is used with system() below to create an rds file 
  # required by the rest of the script
  
  # If the download20mm() function was used to download the 20mm files, then it will be stored in the 
  # temp directory, of which will pull here
  if (is.na(file) | file == shQuote("NA") | file == "NA") {
    tempFile <- list.files(tempdir())[grep(paste0(surveyName, "*.+zip"), list.files(tempdir()))]
    
    # Extracting the downloaded file from download20mm()
    if (grepl(".zip", tempFile)) {
      localDbFile <- unzip(zipfile = file.path(exdir, tempFile), exdir = exdir)
    }
  } else {
    localDbFile <- file
    
    if (!file.exists(localDbFile)) stop("File path not found. Did you specify right? Are you on VPN?", call. = F)
  }
  
  # Driver and path required to connect from RStudio to Access
  dbString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                     "Dbq=", localDbFile)
  
  # Connection variable itself
  con <- tryCatch(DBI::dbConnect(drv = odbc::odbc(), .connection_string = dbString),
                  error = function(cond) {
                    if (all(stringr::str_detect(cond$message, c("IM002", "ODBC Driver Manager")))) {
                      message(cond, "\n")
                      message("IM002 and ODBC Driver Manager error generally means a 32-bit R needs to be installed or used.")
                    } else {
                      message(cond)
                    }
                  })
}

con <- connectAccess("U:\\NativeFish\\SmeltData\\DS-DATA\\20-mm local\\20-mm_FishZooData_Backup.accdb")

checkQueries <- function(rQueryDF, accessQueryName, conn = con,
                         meanVar, sdDownName, sdUpName) {
  
  accessQueryCompare <- DBI::dbReadTable(name = accessQueryName, conn = conn)
  
  if (any(names(accessQueryCompare) %in% "Outlier")) {
    if (any(c(missing(meanVar), missing(sdDownName), missing(sdUpName)))) {
      message("Specify meanVar, sdDownName, sdUpName arguments: \n")
    
      return(print(names(accessQueryCompare)))
    }
    
    
    
    accessQueryCompare <- accessQueryCompare %>% 
      filter(Outlier == -1) %>% 
      rename(Station = Station.Station,
             Mean = sym(meanVar),
             SdTwoDown = sym(sdDownName),
             SdTwoUp = sym(sdUpName)) %>%
      mutate(Outlier = ifelse(Outlier == -1, T, F))
  } 
  
  if (any(names(rQueryDF) %in% "Outlier")) {
    rQueryDFCompare <- rQueryDF %>% 
      filter(Outlier %in% "TRUE")
  } else {
    rQueryDFCompare <- rQueryDF
  }
  
  # mutate(SampleDate = as.Date(SampleDate))
  # else {
  #   accessQueryCompare <- accessQueryCompare %>% 
  #     arrange(SampleDate, Station)
  # }
  
  # rQueryDFCompare <- rQueryDF %>% 
  #   filter(Outlier %in% "TRUE")
  
  if (any(names(accessQueryCompare) %in% "SampleDate")) {
    accessQueryCompare <- accessQueryCompare %>% 
      mutate(SampleDate = as.Date(SampleDate)) %>% 
      arrange(SampleDate, Station)
    
    rQueryDFCompare <- rQueryDFCompare %>% 
      mutate(SampleDate = as.Date(SampleDate)) %>% 
      arrange(SampleDate, Station)
  }
    
  if (any(names(rQueryDFCompare) %in% c("Comments.x", "Comments.y"))) {
    
    commentVector <- syms(names(select(rQueryDFCompare, contains("Comments"))))
    
    rQueryDFCompare <- rQueryDFCompare %>% 
      unite("Comments", !!!commentVector, sep = "") %>%
      mutate(Comments = gsub("NA", "", Comments),
             Comments = ifelse(Comments %in% "", NA_character_, Comments))
  }
  
  namesCompare <- names(rQueryDFCompare)[names(rQueryDFCompare) %in% names(accessQueryCompare)]
  
  compare <- all.equal(rQueryDFCompare %>% 
                         select(namesCompare) %>% 
                         data.frame(),
                       accessQueryCompare %>% 
                         select(namesCompare) %>% 
                         data.frame(),
                       # NOTE: this tolerance is here because of single -> double conversion float issue
                       # The diff between single and double = 7 vs 14 digits
                       tolerance = 1e-7)
  
  if (!isTRUE(compare)) {
    print(compare)
    warning("The two queries are not the same.")
    browser()
  }
  
  cat("Check complete. The two queries are the same. \n")
  T
}

# IMPORTANT NOTE: this checkQueries function is performed on the rQuery that is BEFORE joining the Tow
# data table. After joining, data is duplicated for the 20 mm. So, the FINAL output of the queries
# built in R will NOT be the same as the ones in Access since the Access ones do not 

# Equivalent to "Edit - Cable Out (outliers)

# What the script does: If CableOut is NOT standard during a tow with bottom depth > 5, return as outlier
# Removing this entire query as it is redundant compared to the Cable out VS depth query (approved by Adam)

# Equivalent to "Edit - Cable out vs Depth"

# What the script does: For each CableOut, there is a range of accompanying BottomDepth. If outside that combo,
# return as an outlier

outliers$CableDepth <- joinSurveyStationTow() %>% 
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

outliers$MeterReading <- joinSurveyStationTow() %>% 
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

# Equivalent to "Edit - CB meter out of range"
# What the script does: Check values of duration and flow meter readings; those beyond these thresholds
# are outliers; these values thresholds = field tested

outliers$CB_MeterReading <- joinSurveyStationTow() %>% 
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

# Equivalent to "Edit - Net Meter Serial"
# plot out flow meter checks, by diff flow meter serials; to determine when a certain flow meter failed during a season

outliers$NetMeterSerial <- joinSurveyStationTow() %>% 
  full_join(data$Gear, by = "TowID") %>% 
  transmute(Year = year(SampleDate), 
            Survey, 
            MeterSerial,
            GearCode) %>% 
  filter(GearCode %in% 2,
         Year %in% yearOfInterest) %>% 
  group_by(Year, Survey, MeterSerial, GearCode) %>% 
  count(name = "CountOfMeterSerial")

# Equivalent to "Edit - Tow duration (outliers)"

# What the script does: find data points where duration is NOT equal to 2.5, 5, or 10 min; these are
# the standard tow lengths for this survey

outliers$TowDuration <- joinSurveyStationTow() %>% 
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
findMeanAndStdev <- function(data, variable, byMonth = F) {

  # This is equivalent to step 1 in the edit queries, finding mean and std of data
  variable <- enquo(variable)

  if (byMonth) {
    data %>%
      group_by(Station, 
               Month = month(SampleDate)) %>%
      summarise(Mean = mean(!!variable, na.rm = T),
                SdTwoDown = Mean - 2 * sd(!!variable, na.rm = T),
                SdTwoUp = Mean + 2 * sd(!!variable, na.rm = T),
                .groups = "drop")
  } else {
    data %>%
      group_by(Station) %>%
      summarise(Mean = mean(!!variable, na.rm = T),
                SdTwoDown = Mean - 2 * sd(!!variable, na.rm = T),
                SdTwoUp = Mean + 2 * sd(!!variable, na.rm = T),
                .groups = "drop")
  }
  # countStation = n(), # Removing this portion of the code; it is a part of query 1 but not used elsewhere
}

# Equivalent to "Edit - Bottom Depth (outliers)"

outliers$BottomDepth <- joinSurveyStationTow() %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(), 
                             variable = BottomDepth),
            by = "Station") %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((BottomDepth < SdTwoDown | BottomDepth > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomDepth), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Station, BottomDepth, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)
# BottomDepthMonth

outliers$BottomDepthMonth <- joinSurveyStationTow() %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(), 
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
  select(SampleDate, SurveyID, StationID, Year, Month, Station, BottomDepth, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit - Temp (outliers)
outliers$Temp <- joinSurveyStationTow() %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = Temp),
            by = "Station") %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Temp < SdTwoDown | Temp > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Temp), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Station, Temp, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# TempMonth
outliers$TempMonth <- joinSurveyStationTow() %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = Temp,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Temp < SdTwoDown | Temp > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Temp), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Month, Station, Temp, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit - Top EC (outliers)
outliers$TopEC <- joinSurveyStationTow() %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = TopEC),
            by = "Station") %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < SdTwoDown | TopEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Station, TopEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# TopECMonth
outliers$TopECMonth <- joinSurveyStationTow() %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = TopEC,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((TopEC < SdTwoDown | TopEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(TopEC), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Month, Station, TopEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Bottom EC III
outliers$BottomEC <- joinSurveyStationTow() %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = BottomEC),
            by = "Station") %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < SdTwoDown | BottomEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Station, BottomEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# BottomECMonth
outliers$BottomECMonth <- joinSurveyStationTow() %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = BottomEC,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((BottomEC < SdTwoDown | BottomEC > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(BottomEC), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Month, Station, BottomEC, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Edit – Secchi III
outliers$Secchi <- joinSurveyStationTow() %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = Secchi),
            by = "Station") %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < SdTwoDown | Secchi > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Station, Secchi, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# SecchiMonth
outliers$SecchiMonth <- joinSurveyStationTow() %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = Secchi,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Secchi < SdTwoDown | Secchi > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Secchi), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Month, Station, Secchi, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# There is no equivalent Turbidity script in Access for 20 mm
outliers$Turbidity <- joinSurveyStationTow() %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = Turbidity),
            by = "Station") %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < SdTwoDown | Turbidity > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Turbidity), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Station, Turbidity, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# checks$Turbidity <- checkQueries(outliers$Turbidity, "Edit - Turbidity (outliers)")

# TurbidityMonth
outliers$TurbidityMonth <- joinSurveyStationTow() %>% 
  mutate(Month = month(SampleDate)) %>% 
  full_join(findMeanAndStdev(joinSurveyStationTow(skipTow = T), 
                             variable = Turbidity,
                             byMonth = T),
            by = c("Station", "Month")) %>% 
  mutate(Year = year(SampleDate),
         # This is part 2 of the query
         Outlier = ifelse((Turbidity < SdTwoDown | Turbidity > SdTwoUp), T, F),
         # This is new to the code; will create a column that flags NAs and those will also be returned
         NAFlag = ifelse(is.na(Turbidity), T, F)) %>% 
  # This is part 3 of the query
  # Note that the Comments column from the table is preserved here; this is useful for diagnosing the NAFlag column
  select(SampleDate, SurveyID, StationID, Year, Month, Station, Turbidity, 
         Mean, SdTwoDown, SdTwoUp, 
         contains("Comments"), Outlier, NAFlag) %>% 
  filter(Outlier == T | NAFlag == T,
         Year %in% yearOfInterest) %>% 
  distinct() %>% 
  arrange(NAFlag, Outlier, SampleDate)

# Technical coding section ------------------------------------------------
# This section is currently only for checking if my coding is correct. It can be adapted to 
# run by the ES's, but the year filters in Access have to be changed as well, so not making
# this part of the normal code yet. 
# Have checked all queries with the original and they have all matched so far: 02-07-22, TN

checks <- list()

checks$CableDepth <- checkQueries(outliers$CableDepth, "Edit - Cable out vs Depth")

checks$MeterReading <- checkQueries(outliers$MeterReading, "Edit - Net meter reading out of range")

checks$CB_MeterReading <- checkQueries(outliers$CB_MeterReading, "Edit - CB meter out of range")

checks$NetMeterSerial <- checkQueries(outliers$NetMeterSerial, "Edit - Net Meter Serial")

checks$BottomDepth <- checkQueries(outliers$BottomDepth, "Edit - Bottom Depth (outliers)",
                                   meanVar = "Mean", 
                                   sdDownName = "X2...Standard.Devations",
                                   sdUpName = "X2...Standard.Devations.1")

checks$Temp <- checkQueries(outliers$Temp, "Edit - Temp (outliers)",
                            meanVar = "Mean", 
                            sdDownName = "X2...Standard.Devations",
                            sdUpName = "X2...Standard.Devations.1")

checks$TopEC <- checkQueries(outliers$TopEC, "Edit - Top EC (outliers)",
                             meanVar = "AvgOfTopEC",
                             sdDownName = "Lower.Bound",
                             sdUpName = "Upper.Bound")

checks$BottomEC <- checkQueries(outliers$BottomEC, "Edit - Bottom EC (outliers)",
                                meanVar = "AvgOfBottomEC",
                                sdDownName = "Lower.Bound",
                                sdUpName = "Upper.Bound")

checks$Secchi <- checkQueries(outliers$Secchi, "Edit - Secchi (outliers)",
                              meanVar = "AvgOfSecchi",
                              sdDownName = "Lower.Bound",
                              sdUpName = "Upper.Bound")
# There isn't a check for turbidity as turbidity in the 20 mm db does not exist
# All checks should be TRUE, indicating the same output from the script and the query
if (!all(unlist(checks))) stop("Checks are not all equal")

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
#   mutate(SampleDate = as.Date(SampleDate)) %>%
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
#   mutate(SampleDate = as.Date(SampleDate)) %>%
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
#     Outlier = ifelse((Length < sd2down | Length > sd2up) & SampleDate > as.Date("2012-12-31"), T, F),
#     NAFlag = ifelse(is.na(Length), T, F)) %>% 
#   arrange(SampleDate) %>% 
#   # This is part 3 of the query
#   select(SampleDate, Month, FishCode, Length, meanLengths, sd2down, sd2up, Outlier, NAFlag) %>% 
#   mutate(SampleDate = as.Date(SampleDate)) %>% 
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
#     Outlier = ifelse((Length < quant25 - (iqr * 3) | Length > quant75 + (iqr * 3)) & SampleDate > as.Date("2012-12-31"), T, F),
#     NAFlag = ifelse(is.na(Length), T, F)) %>% 
#   arrange(SampleDate) %>% 
#   # This is part 3 of the query
#   select(SampleDate, Month, FishCode, Length, quant25, quant75, iqr, Outlier, NAFlag) %>% 
#   mutate(SampleDate = as.Date(SampleDate)) %>% 
#   filter(Outlier == T | NAFlag == T) %>% 
#   arrange(NAFlag, Outlier, SampleDate) %>% 
#   View()
# # Much harder with fish data lol


# Saving all outlier DFs to an excel sheet for analysis -------------------

# This is a temp name
# Adding the 3 columns that Adam requested for the ES in charge to fill in to document result of analysis
saveSheet <- lapply(outliers, function(x) mutate(x,
                                                 IsOutlier = NA, ChangedTo = NA, CommentsOutlier = NA,
                                                 # Strange XML character in some rows of the comments column in the
                                                 # Water Info table; removing this so that the excel file saves correctly
                                                 across(where(is.character), ~str_replace_all(.x, "\uFFFD", ""))))

# Saving the file; this will save to the current directory, which by default is in the parent folder; will change this later
writexl::write_xlsx(saveSheet, file.path("data-raw", "Outliers", "20mm", paste0("20mm_outliers_", today(), ".xlsx")))
