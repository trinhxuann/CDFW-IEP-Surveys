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
      html_table()
  
    if (nrow(availableData) == 0) {
      availableData <- lapply(tableNames, function(x) data.frame(x = NA) %>% 
                                rename(!!x := x)) %>% 
        bind_cols()
    } else {
      availableData <- availableData %>% 
        setNames(tableNames) %>% 
        arrange(sensorNumber) %>% 
        mutate(duration = gsub("\\(|\\)", "", duration),
               gage = station)
    }
    
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
  if (unique(df$units) %in% "DEG F") {
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

# Function to pull the closest cdec station to a coordinate
calcNearestCDEC <- function(df, cdecGPS, 
                            cdecMetadata = NULL,
                            variable = c("temp", "turbidity", "ec"), 
                            waterColumn = c("top", "bottom")) {

  if (variable == "ec") {
    variableWanted <- "elec.* conduct.* micro"
  } else {
    if (variable == "temp") {
      variableWanted <- "(temp).*(water)"
    } else variableWanted <- variable
  }
  
  waterColumnWanted <- ifelse(waterColumn %in% "bottom", "(lower|bottom)", waterColumn)
  
  variableWanted <- ifelse(waterColumn == "bottom", 
                           paste0(variableWanted, ".*", waterColumnWanted), 
                           variableWanted)
  
  closestMetadata <- lapply(1:nrow(df), function(x) {
    
    distanceMatrix <- distm(data.frame(longitude = df$long[[x]], 
                                       latitude = df$lat[[x]]),
                            data.frame(longitude = cdecGPS$longitude, 
                                       latitude = cdecGPS$latitude),
                            fun = distVincentyEllipsoid)
    
    distanceData <- data.frame(cdecStation = cdecGPS$station,
                               distance = as.vector(distanceMatrix)/1609.344,
                               sktStation = df$Station[x]) %>% 
      arrange(distance)
    
    if (!is.null(cdecMetadata)) {
      
      closestGage <- cdecMetadata %>% 
        filter(grepl(variableWanted, sensorDescription, ignore.case = T))
      
      if (waterColumn != "bottom") {
        closestGage <- closestGage %>% 
          filter(!grepl("(lower|bottom)", x = sensorDescription, ignore.case = T))
      }
      
      closestValidGage <- closestGage %>% 
        left_join(distanceData, by = c("gage" = "cdecStation")) %>% 
        slice_min(distance)
      
      metadata <- cdecMetadata %>% 
        filter(gage == unique(closestValidGage$gage)) %>% 
        mutate(distance = unique(closestValidGage$distance))
    } else {
      
      for (i in 1:nrow(distanceData)) {
        metadata <- pullMetadataCDEC(distanceData$cdecStation[i], list = F) %>% 
          mutate(distance = unique(distanceData$distance[i]))
        
        if (waterColumn == "bottom") {
          waterColumnPass <- any(grepl(variableWanted, 
                                       metadata$sensorDescription, ignore.case = T))
        } else {
          waterColumnPass <- metadata$sensorDescription[which(grepl(variableWanted, 
                                                                    metadata$sensorDescription, 
                                                                    ignore.case = T))] %>% 
            grepl("lower|bottom", x = ., ignore.case = T) %>% 
            {any(!.)}
        }
        if (isTRUE(waterColumnPass)) break()
      }
    }
    
    rownames(metadata) <- NULL
    
    metadata %>% 
      mutate(Station = df$Station[[x]])
  })
}

# Function to populate cdec values (temp, turb, ec) to a list of date time of a survey station
popCDEC <- function(df, 
                    cdec,
                    metadata,
                    variable = c("temp", "turbidity", "ec"), 
                    waterColumn = c("top", "bottom")) {

  if (nrow(df) == 0) {
    message("Data frame has no data.")
    return(data.frame())
  }
  
  if (variable == "ec") {
    variableWanted <- "elec.* conduct.* micro"
  } else {
    if (variable == "temp") {
      variableWanted <- "(temp).*(water)"
    } else variableWanted <- variable
  }
  
  waterColumnWanted <- ifelse(waterColumn %in% "bottom", "(lower|bottom)", waterColumn)
  
  joinedDF <- left_join(df, cdec, by = "Station") %>% 
    data.frame() %>% 
    pivot_longer(c(first, second, third), 
                 names_to = "priority", values_to = "cdecGage")
  
  names(metadata) <- sapply(metadata, function(x) unique(pull(x, gage)))
  
  filteredMetadata <- lapply(na.omit(unique(joinedDF$cdecGage)), function(x) {
    
    dfFiltered <- metadata[[x]] %>%
      filter(grepl(variableWanted, sensorDescription, ignore.case = T))
    
    if (nrow(dfFiltered) > 1) {
      if (waterColumnWanted == "bottom") {
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
      right_join(x, by = c("obsDate" = "SampleDate", "stationId" = "cdecGage"))
   
    if (is.null(df)) return(data.frame(valueCDEC = NA,
                                       closestTime = NA))
    
    towTimeIndex <- sym(names(df)[which(sapply(df, function(x) inherits(x, "POSIXct")) & names(df) %in% "TowTime")])
    
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

  joinedDFCDEC %>% 
    select(-rowNumber) %>% 
    bind_cols(pulledData) %>% 
    bind_rows(joinedDF %>% 
                filter(is.na(cdecGage))) %>% 
    pivot_wider(names_from = "priority", 
                values_from = c("cdecGage", "SensorNumber", "Duration", "valueCDEC", "closestTime")) %>% 
    relocate(contains("valueCDEC"), .before = Mean)
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
  waterColumnWanted <- ifelse(waterColumn %in% "bottom", "(lower|bottom)", waterColumn)
  
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
