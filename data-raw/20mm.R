# This script is meant to bridge between the Access database and Sam's LTMRdata package
# Since the LTMR data package takes in individual flat files as inputs, this script
# will simply connect to Access, pull the relevant tables, export it via text/csv, and finish
# The function will download the Access database from the public FTP website or connect
# To the file located on the UDrive for manipulations. The UDrive option is default.
# To use the UDrive, one must be connected to the UDrive via being in the office or via VPN.
# To get the Access connection to work, you will need the DBI and odbc packages
# You will also need R in the same architecture as your Access database 

# Libraries needed
library(dplyr, warn.conflicts = F)
library(tidyr)
library(DBI)
library(odbc)
library(stringr)

# This will evaluate arguments accompanied with the terminal command to use in this specific script
if (!exists("Args")) {
  Args = commandArgs(T)
} 

# Currently, commandArgs is set up so that:
# first argument is the file arg; 
# the rest will be the names of the table that you want. There isn't leeway for edge cases yet

connectAccess <- function(file = Args[1],
                          exdir = tempdir(),
                          surveyName = "20mm") {
  
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

# Function to start reading data from Access directly
read20mmAccess <- function(surveyName = "20mm",
                           returnDF = F,
                          tablesReturned = Args[-(1)]) {
  pb <- txtProgressBar(min = 0, max = 5, style = 3)
  startTime <- Sys.time()
  cat("\nConnecting to Access \n")
  
  con <- connectAccess()
  
  setTxtProgressBar(pb, 1)
  connectionTime <- Sys.time()
  
  # Pulling just the table names to be used in mapply() below
  tableNames <- odbc::dbListTables(conn = con)
  # Includes system tables which cannot be read, excluding them below with negate
  # tableNames <- stringr::str_subset(tableNames, "MSys", negate = T)
  if (is.null(tablesReturned) | (length(tablesReturned) == 0)) {
    # If no table names are specified, then simply return the names of the possible databases for the user to pick
    DBI::dbDisconnect(con)
    
    if (length(tablesReturned) == 0) message("Specify at least one table to pull from:")
    
    return(tableNames)
  }
  
  cat("\n", paste0("Pulling (", length(tablesReturned), ") tables: "), paste(tablesReturned, collapse = ", "), fill = T)
  
  # Apply the dbReadTable to each readable table in db
  TTmmTables <- mapply(DBI::dbReadTable,
                      name = tablesReturned,
                      MoreArgs = list(conn = con))
  
  # Cleaning up connection
  # The downloaded files will be auto deleted once R shuts down
  DBI::dbDisconnect(con)
  
  # Need to remove extra columns from the database. This is because the files on the UDrive
  # is not 100% the same as the file uploaded to the FTP website. Will only retain columns
  # that are found on the FTP website version available to the public. This is also why
  # the position index is used to identify the tables and not simply using the station name
  # itself--there are instances when the names of the tables are different between ftp and UDrive
  # Will select only the columns that matter:
  
  # Removing extra "JustEdited" from this table
  # Also transforming SampleDate to as.Date instead
  surveyPosition <- which(sapply(TTmmTables,
                                 function(tables) any(grepl("JustEdited",
                                                            x = names(tables),
                                                            ignore.case = T))))
  
  TTmmTables[[surveyPosition]] <- TTmmTables[[surveyPosition]] %>%
    select(-JustEdited) %>% 
    mutate(SampleDate = as.Date(SampleDate))
  
  # Station, changing lat/long to numeric
  stationPosition <- which(sapply(TTmmTables,
                                  function(tables) any(grepl("LatDeg",
                                                             x = names(tables),
                                                             ignore.case = T))))
  
  TTmmTables[[stationPosition]] <- TTmmTables[[stationPosition]] %>% 
    mutate(across(c(LatDeg, LatMin, LatSec, LonDeg, LonMin, LonSec), ~as.numeric(.x)))
  # Tow, changing time to PST/PDT
  towPosition <- which(sapply(TTmmTables,
                              function(tables) any(grepl("TowTime",
                                                         x = names(tables),
                                                         ignore.case = T))))
  
  attr(TTmmTables[[towPosition]]$TowTime, "tzone") <- "America/Los_Angeles"
  
  # Gear
  gearPosition <- which(sapply(TTmmTables,
                               function(tables) any(grepl("ProcessDate",
                                                          x = names(tables),
                                                          ignore.case = T))))
  
  TTmmTables[[gearPosition]] <- TTmmTables[[gearPosition]] %>%
    select(-c(ProcessDate, Processor, ProcessingTime))
  
  # GearCodesLkp Ok
  # MeterCorrections, changing CalibrationDate to Date format
  meterCorrectionsPosition <- which(sapply(TTmmTables,
                                           function(tables) any(grepl("CalibrationDate",
                                                                      x = names(tables),
                                                                      ignore.case = T))))
  
  TTmmTables[[meterCorrectionsPosition]] <- TTmmTables[[meterCorrectionsPosition]] %>%
    mutate(CalibrationDate = as.Date(CalibrationDate))
  
  # There is a "20mmStations" base table in the FTP version; this is NOT available in the most current
  # UDrive local database. Instead, will use the "StationCords" table from local
  # StationCords table ok
  
  # FishSample ok
  # FishLength ok
  
  # Some unicode hiccups like the SLS. \u0092t here for '; also \u0085 for "next line"
  #     mutate(Comments = str_replace(Comments, "\ufffd", "\u0027"))
  # For tables with "Comments", these unicode errors exist
  commentsTables <- sapply(TTmmTables, function(x) any(grepl("Comments", names(x))))
  
  for (i in which(commentsTables)) {
    TTmmTables[[i]] <- TTmmTables[[i]] %>% 
      mutate(Comments = str_replace(Comments, "\ufffd", "\u0027"))
  }
  # Note, this changes ALL unidentified unicodes to have an apostrophe
  
  setTxtProgressBar(pb, 2)
  pullTime <- Sys.time()
  
  # Round all numeric values to 7 digits...This gets rid of "ghost numbers", as known by Bay Study,
  # that are caused by the use of "single" field size in Access. When converted to "double"
  # in R and Excel, the addition of additional decimal points causes non-zero digits to appear
  # to fill in the missing spaces. This is simply a limitation of computer arithmetics
  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
    # Function sourced from: https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r?rq=1
  }
  
  cat("\nChecking for float issues.", fill = T)
  
  floatIssue <- lapply(TTmmTables, function(x) {
    df <- x %>% 
      summarise(across(where(is.numeric), 
                       # Will check for 14 digits, since conversion of "single" to double
                       ~suppressWarnings(sum(as.numeric(substrRight(sprintf("%.14f", .x), 1)), na.rm = T)))) %>% 
      select_if(~ !is.numeric(.) || sum(.) != 0) %>% 
      names()
  })
  
  # Which data frames have this float issue:
  floatIssueDF <- names(which(sapply(floatIssue, function(x) !identical(x, character(0)))))
  # Within the affected DFs, round the affected columns to 7 and then 2 digits
  # I believe the step to round to 7 digits is not needed but does not really affect computational speed
  # Will leave because mechanically, "single" field size is 7 digits long.
  
  for (i in floatIssueDF) {
    
    columnsAffected <- floatIssue[[i]]
    
    cat("Column(s)", paste(columnsAffected, collapse = ", "), "in the", i, "data table experienced float issues. Rounding accordingly.", fill = T)
    
    TTmmTables[[i]] <- TTmmTables[[i]] %>% 
      mutate(across(all_of(columnsAffected), ~round(.x, 7)),
             across(all_of(columnsAffected), ~round(.x, 2)))
  }
  
  setTxtProgressBar(pb, 3)
  floatTime <- Sys.time()
  # For instances where you do not want to write the rds and want to work 
  # entirely in this environment, returnDF will be used
  
  if (returnDF) {
    
    cat("\nReturning dataframe only \n")
    setTxtProgressBar(pb, 5)
    close(pb)
    endTime <- Sys.time()
    
    cat("Connection time: ", (connectionTime - startTime)/60, 
        "; pull time: ", (pullTime - connectionTime)/60,
        "; float time: ", (floatTime - pullTime)/60,
        "; overall time: ", (connectionTime - floatTime)/60, " seconds.", fill = T)
    
    return(TTmmTables)
    
  } else {
    # If not returning the df, will return BOTH the csv files AND RDA file
    
    cat("\nExporting csv flat files \n")
    
    writeFiles <- sapply(seq_along(TTmmTables), function(i) {
      
      if (!file.exists("data-raw")) stop("A `data-raw` folder was not detected in the working directory; please create this folder.")
      
      filePath <- file.path("data-raw", surveyName, paste0(names(TTmmTables[i]), ".csv"))
      
      write.csv(TTmmTables[[i]],
                file = filePath,
                row.names = F,
                fileEncoding = "UTF-8")
      
      file.exists(filePath)
    })
    
    setTxtProgressBar(pb, 4)
    writeTime <- Sys.time()
    
    if (!all(writeFiles)) {
      df <- data.frame(tableNames = tableNames,
                       fileFailed = !writeFiles)
      
      errors <- df$tableNames[which(df$fileFailed == T)]
      stop("Table(s) not export successfully: ", 
           paste0(length(errors), " out of ", length(df$tableNames), " tables \n"),
           paste0(errors, "\n"), call. = F)
    } else {
      cat("\nAll tables exported successfully \n")
    }
    cat("Exporting rds file \n")
    
    # Now returning the rda
    saveRDS(TTmmTables, file = file.path("data-raw", surveyName, "TTmmTables.rds"),
            compress = T)
    
    setTxtProgressBar(pb, 5)
    saveTime <- Sys.time()
    
    if (file.exists(file.path("data-raw", surveyName, "TTmmTables.rds"))) cat("\nRDA file created successfully  \n")
    else (stop("\nRDA file was NOT created, something failed!"))
  }
  close(pb)
  endTime <- Sys.time()
  cat("Done! \n")
  
  cat(paste0("Connection time: ", round(as.numeric(connectionTime - startTime, units = "secs"), 1), 
             "; pull time: ", round(as.numeric(pullTime - connectionTime, units = "secs"), 1),
             "; float time: ", round(as.numeric(floatTime - pullTime, units = "secs"), 1),
             "; write time: ", round(as.numeric(writeTime - floatTime, units = "secs"), 1),
             "; save time: ", round(as.numeric(saveTime - writeTime, units = "secs"), 1),
             "; overall time: ", round(as.numeric(endTime - startTime, units = "secs"), 1), " seconds."), fill = T)
}

# # Run the functions to download and read the database
read20mmAccess()

# # This part of the code is for when you want to run this script directly, which you should not need to
# # FishSample == Catch
# # Station == WaterInfo
# # 20mmStations == Station_Lookup
# read20mmAccess(tablesReturned = c("FishSample", "FishLength", "MeterCorrections",
#                                  "Tow", "Station", "20mmStations"))
# For the UDrive version
# read20mmAccess(file = "U:\\NativeFish\\SmeltData\\DS-DATA\\20-mm local\\20-mm_FishZooData.accdb",
#                tablesReturned = c("FishSample", "FishLength", "MeterCorrections",
#                                   "Tow", "Station", "StationCords"))
