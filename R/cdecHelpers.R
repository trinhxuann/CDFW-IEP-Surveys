# Reading nearby CDEC stations --------------------------------------------
# Function to pull data from CDEC gages into R
pullCDEC <- function(station, sensor = NULL, 
                     duration = c("event", "hourly", "daily"), 
                     dateStart, dateEnd = NULL,
                     verbose = T) {
  
  if (is.null(sensor)) {
    webpage <- GET("http://cdec.water.ca.gov/dynamicapp/staMeta",
                   query = list(station_id = station))
    
    table <- webpage %>% 
      read_html() %>% 
      html_elements("table") 
    
    if (length(table) != 0) {
      session <- table %>% 
        {.[[which(grepl("Sensor Description", .))]]}
    } else {
      stop("Check station name. No table was found.")
    }
    
    tableNames <- session %>% 
      html_elements("th") %>% 
      html_text() %>% 
      gsub("\\s", "", .) %>% 
      gsub("(^[A-Z])", "\\L\\1", ., perl = T)
    
    availableData <- session %>% 
      html_table() %>% 
      setNames(tableNames) %>% 
      arrange(sensorNumber) %>% 
      mutate(duration = gsub("\\(|\\)", "", duration),
             gage = station)
    
    if (is.null(sensor)) {
      if (isTRUE(verbose)) {
        print(availableData, n = Inf, width = Inf)
        message("Please provide sensor # and duration")
      } 
      return(invisible(availableData))
    }
    
    if (length(duration) > 1) {
      returnDF <- filter(availableData, sensorNumber == sensor)
      
      if (isTRUE(verbose)) {
        returnDF %>% 
          print(n = Inf, width = Inf)
        message("Select a duration.")
      }
      return(invisible(returnDF))
    }
    
    if (missing(dateStart)) {
      returnDF <- filter(availableData, sensorNumber == sensor)
      
      if (isTRUE(verbose)) {
        returnDF %>% 
          print(n = Inf, width = Inf)
        message("Select a start date")
      } 
      return(invisible(returnDF))
    }
  }
  
  duration <- match.arg(duration)
  
  if (duration == "event") {
    duration <- "E"
  } else {
    if (duration == "hourly") {
      duration <- "H"
    } else {
      duration <- "D"
    }
  }
  
  dateStart <- as.Date(dateStart, tryFormats = c("%Y/%m%/%d", "%m/%d/%Y", "%Y-%m-%d", "%m-%d-%Y"))
  
  # For dateEnding date. If provided, use it; if not, use today
  if (is.null(dateEnd)) {
    dateEnd <- Sys.Date()
  } else {
    dateEnd <- as.Date(dateEnd, tryFormats = c("%Y/%m%/%d", "%m/%d/%Y", "%Y-%m-%d", "%m-%d-%Y"))
  }
  
  csv <- GET("https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet",
             query = list(Stations = paste(station, collapse = ","),
                          SensorNums = sensor,
                          dur_code = duration,
                          Start = dateStart,
                          End = dateEnd))
  
  df <- suppressWarnings(content(csv, type = "text/csv", 
                                 col_types = 
                                   readr::cols(
                                     STATION_ID = readr::col_character(),
                                     DURATION = readr::col_character(),
                                     SENSOR_NUMBER = readr::col_double(),
                                     SENSOR_TYPE = readr::col_character(),
                                     `DATE TIME` = readr::col_character(),
                                     `OBS DATE` = readr::col_character(),
                                     VALUE = readr::col_double(),
                                     DATA_FLAG = readr::col_character(),
                                     UNITS = readr::col_character()
                                   ),
                                 encoding = "ISO-8859-1")) %>% 
    rename_all(~gsub("((?<=[_\\s])+.)", "\\U\\1", tolower(.x), perl = T)) %>% 
    rename_all(~gsub("_|\\s", "", .x)) %>% 
    mutate(dateTime = as.POSIXct(dateTime, format = "%Y%m%d %H%M", tz = "America/Los_Angeles"),
           obsDate = as.Date(obsDate, format = "%Y%m%d %H%M"))
  # Suppressing warning of "parsing failures" here. 
  # This is caused by "---" in the VALUE column; want this as NA
  
  if (nrow(df) == 0) {
    warning("No data available for station ", station, " as specified.")
    return()
  }
  
  # Need to convert temperature data into C.
  if (unique(df$units) %in% "DEG F" & mean(df$value, na.rm = T) > 32) {
    df <- dplyr::mutate(df, 
                        value = (value - 32)*5/9,
                        units = "DEG C")
  }
  df
}

# Function to pull in metadata of all stations in the CDEC data table
pullMetadataCDEC <- function(station, list = T) {
  metadata <- mapply(pullCDEC, station = station, MoreArgs = list(verbose = F), SIMPLIFY = F)
  
  if (!isTRUE(list)) {
    return(do.call(rbind.data.frame, metadata))
  }
  metadata
}

popCDEC <- function(df, 
                    dfTime = NULL,
                    cdec,
                    metadata,
                    variable = c("temp", "turbidity", "ec"), 
                    waterColumn = c("top", "lower")) {

  if (nrow(df) == 0) {
    message("Data frame has no data.")
    return(data.frame())
  }
  
  # Are there dates and time variables in the DF?
  dateVariable <- names(df)[which(sapply(df, function(x) inherits(x, "Date")))]
  timeVariable <- names(df)[which(sapply(df, function(x) inherits(x, "POSIXct")) & !names(df) %in% "dateTime")]
  
  if (length(dateVariable) > 0) {
    dateVariable <- sym(dateVariable)
  } else {
    stop("A date variable (Date format) is missing.", call. = F)
  }
  
  if (length(timeVariable) > 0) {
    timeVariable <- sym(timeVariable)
  } else {
    if (is.null(dfTime)) {
      stop("A time variable (POSIXct format) is missing. Specify `dfTime` or join time to your data frame.", call. = F)
    }
    
    # Join dfTime to grab the time variable
    df <- df %>% 
      left_join(dfTime %>% 
                  select(!!dateVariable, Station, Tow, where(is.POSIXct)), 
                by = c(as.character(dateVariable), "Station")) %>% 
      mutate(Time = as.POSIXct(paste(Date, format(Time, format = "%H:%M:%S"))))
    
    timeVariable <- sym(names(df)[which(sapply(df, function(x) inherits(x, "POSIXct")) & !names(df) %in% "dateTime")])
  }
  
  variableWanted <- ifelse(variable %in% "ec", "ELEC.* CONDUCT.* MICRO", variable)
  waterColumnWanted <- match.arg(waterColumn)
  
  joinedDF <- left_join(df, cdec, by = "Station") %>% 
    data.frame() %>% 
    pivot_longer(c(first, second, third), 
                 names_to = "priority", values_to = "cdecGage")
  
  gagesOfInterest <- na.omit(unique(joinedDF$cdecGage))
  
  if (length(gagesOfInterest) > 0) {
   
    filteredMetadata <- lapply(na.omit(unique(joinedDF$cdecGage)), function(x) {
      
      dfFiltered <- metadata[[x]] %>%
        filter(grepl(variableWanted, sensorDescription, ignore.case = T))
      
      if (nrow(dfFiltered) > 1) {
        if (waterColumnWanted == "lower") {
          dfFilteredWaterColumn <- dfFiltered %>%
            filter(grepl(waterColumnWanted, sensorDescription, ignore.case = T))
          if (nrow(dfFilteredWaterColumn) != 0) {
            dfFiltered <- dfFilteredWaterColumn
          }
        } else {
          dfFilteredWaterColumn <- dfFiltered %>%
            filter(!grepl("lower", sensorDescription, ignore.case = T))
          if (nrow(dfFilteredWaterColumn) != 0) {
            dfFiltered <- dfFilteredWaterColumn
          }
        }
      }
      
      duration <- pull(dfFiltered, duration) %>%
        factor(levels = c("event", "hourly", "daily")) %>%
        sort()
      
      if (length(duration) == 0) {
        message("Sensor ", dQuote(x), " does not have sensor data for ", dQuote(variable), ".")
        return()
      }
      
      duration <- duration[[1]]
      
      dfFiltered %>%
        filter(duration == !!duration, !grepl("AIR", sensorDescription)) %>%
        transmute(cdecGage = gage,
                  SensorNumber = sensorNumber,
                  Duration = duration)
    }) %>% 
      bind_rows() %>% 
      right_join(joinedDF, by = "cdecGage") %>% 
      relocate(c(cdecGage, SensorNumber, Duration), .after = everything()) %>% 
      arrange(!!timeVariable, priority) %>%
      mutate(cdecGage = ifelse(is.na(SensorNumber) | is.na(Duration),
                               NA, cdecGage))
  } else {
    return(df %>% 
             mutate(valueCDEC = "No applicable CDEC stations.") %>% 
             relocate(contains("valueCDEC"), .before = contains("mean")))
  }
  
  joinedDFCDEC <- filteredMetadata %>% 
    filter(!is.na(cdecGage)) %>% 
    mutate(rowNumber = row_number())
  
  splitGroups <- joinedDFCDEC %>% 
    mutate(group = paste(SensorNumber, Duration, sep = "_")) %>% 
    group_by(group) %>% 
    split(f = .$group)

  pulledData <- lapply(splitGroups, function(x) {

    dates <- pull(x, which(sapply(x, function(x) inherits(x, "Date"))))
    
    cdecStations <- unique(x$cdecGage)
    
    df <- pullCDEC(station = cdecStations, 
                   sensor = unique(x$SensorNumber), 
                   duration = unique(x$Duration), 
                   dateStart = min(dates), 
                   dateEnd = max(dates) + 1) %>% 
      right_join(x, by = c("obsDate" = as.character(dateVariable), "stationId" = "cdecGage"), 
                 relationship = "many-to-many")
    
    if (is.null(df)) return(data.frame(valueCDEC = NA,
                                       closestTime = NA))
    
    df <- df %>% 
      mutate(closestTime = abs(difftime(!!timeVariable, dateTime, units = "mins"))) %>% 
      group_by(rowNumber) %>% 
      slice(which.min(closestTime)) %>% 
      transmute(stationId,
                valueCDEC = value,
                closestTime) %>% 
      ungroup() %>% 
      right_join(x, by = "rowNumber") %>% 
      select(rowNumber, valueCDEC, closestTime)
    
    if (nrow(df) != nrow(x)) warning("Could not pull CDEC data for some outlying datapoints.")
    df
  }) %>% 
    bind_rows() %>% 
    arrange(rowNumber) %>% 
    select(-rowNumber)
  
  joinedDFCDEC %>% 
    select(-rowNumber) %>% 
    bind_cols(pulledData) %>% 
    bind_rows(joinedDF %>% 
                filter(is.na(cdecGage))) %>% 
    pivot_wider(names_from = "priority", 
                values_from = c("cdecGage", "SensorNumber", "Duration", "valueCDEC", "closestTime")) %>% 
    relocate(contains("valueCDEC"), .before = contains("mean"))
}

parPopCDEC <- function(df, 
                       cdec,
                       metadata,
                       variable = c("temp", "turbidity", "ec"), 
                       waterColumn = c("top", "lower")) {
  library(parallel)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, {library(dplyr); library(httr)})
  clusterExport(cl, varlist = c("pullCDEC", "metadata"))
  
  variableWanted <- ifelse(variable %in% "ec", "ELEC.* CONDUCT.* MICRO", variable)
  waterColumnWanted <- match.arg(waterColumn)
  
  joinedDF <- left_join(df, cdec, by = "Station") %>% 
    data.frame() %>% 
    pivot_longer(c(first, second, third), 
                 names_to = "priority", values_to = "cdecGage")
  
  filteredMetadata <- parLapply(cl, na.omit(unique(joinedDF$cdecGage)), function(x) {
    
    dfFiltered <- metadata[[x]] %>%
      filter(grepl(variableWanted, sensorDescription, ignore.case = T))
    
    if (nrow(dfFiltered) > 1) {
      if (waterColumnWanted == "lower") {
        dfFilteredWaterColumn <- dfFiltered %>%
          filter(grepl(waterColumnWanted, sensorDescription, ignore.case = T))
        if (nrow(dfFilteredWaterColumn) != 0) {
          dfFiltered <- dfFilteredWaterColumn
        }
      } else {
        dfFilteredWaterColumn <- dfFiltered %>%
          filter(!grepl("lower", sensorDescription, ignore.case = T))
        if (nrow(dfFilteredWaterColumn) != 0) {
          dfFiltered <- dfFilteredWaterColumn
        }
      }
    }
    
    duration <- pull(dfFiltered, duration) %>%
      factor(levels = c("event", "hourly", "daily")) %>%
      sort()
    
    if (length(duration) == 0) {
      message("Sensor ", dQuote(x), " does not have sensor data for ", dQuote(variable), ".")
      return(data.frame())
    }
    
    duration <- duration[[1]]
    
    dfFiltered %>%
      filter(duration == !!duration) %>%
      transmute(cdecGage = gage,
                SensorNumber = sensorNumber,
                Duration = duration)
  }) %>% 
    bind_rows() %>% 
    right_join(joinedDF, by = "cdecGage") %>% 
    relocate(c(cdecGage, SensorNumber, Duration), .after = everything()) %>% 
    arrange(TowTime, priority) %>%
    mutate(cdecGage = ifelse(is.na(SensorNumber) | is.na(Duration),
                             NA, cdecGage))
  
  joinedDFCDEC <- filteredMetadata %>% 
    filter(!is.na(cdecGage)) %>% 
    mutate(rowNumber = row_number())
  
  splitGroups <- joinedDFCDEC %>%
    mutate(group = paste(Station, SensorNumber, Duration, sep = "_")) %>%
    group_by(group) %>%
    split(f = .$group)
  
  pulledData <- parLapply(cl, splitGroups, function(x) {
    
    dates <- pull(x, which(sapply(x, function(x) inherits(x, "Date"))))
    
    cdecStations <- unique(x$cdecGage)
    
    df <- pullCDEC(station = cdecStations,
                   sensor = unique(x$SensorNumber),
                   duration = unique(x$Duration),
                   dateStart = min(dates),
                   dateEnd = max(dates) + 1) %>%
      right_join(x, by = c("obsDate" = "SampleDate", "stationId" = "cdecGage"))
    
    if (is.null(df)) return(data.frame(valueCDEC = NA,
                                       closestTime = NA))
    
    towTimeIndex <- sym(names(df)[which(sapply(df, function(x) inherits(x, "POSIXct")) & !names(df) %in% "dateTime")])
    
    df <- df %>%
      mutate(closestTime = abs(difftime(!!towTimeIndex, dateTime, units = "mins"))) %>%
      group_by(rowNumber) %>%
      slice(which.min(closestTime)) %>%
      transmute(stationId,
                valueCDEC = value,
                closestTime) %>%
      ungroup() %>%
      right_join(x, by = "rowNumber") %>%
      select(rowNumber, valueCDEC, closestTime)
    
    if (nrow(df) != nrow(x)) warning("Could not pull CDEC data for some outlying datapoints.")
    df
  }) %>%
    bind_rows() %>%
    arrange(rowNumber) %>%
    select(-rowNumber)
  
  stopCluster(cl)
  
  joinedDFCDEC %>% 
    select(-rowNumber) %>% 
    bind_cols(pulledData) %>% 
    bind_rows(joinedDF %>% 
                filter(is.na(cdecGage))) %>% 
    pivot_wider(names_from = "priority", 
                values_from = c("cdecGage", "SensorNumber", "Duration", "valueCDEC", "closestTime")) %>% 
    relocate(contains("valueCDEC"), .before = Mean)
}
