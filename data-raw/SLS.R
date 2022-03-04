# This script will connect to Access, pull the relevant tables, export it via text/csv, and finish
# The function can also download the Access database from the public FTP website or connect
# To the file located on the UDrive for manipulations. The UDrive option is default.
# To use the UDrive, one must be connected to the UDrive via being in the office or via VPN.
# To get the Access connection to work, you will need the DBI and odbc packages
# You will also need R in the same architecture as your Access database 
# I may remove the download from FTP website link option because that is depreciated.

# Libraries needed
library(dplyr, warn.conflicts = F)
library(DBI)
library(odbc)

# This will evaluate arguments accompanied with the terminal command to use in this specific script
# IF you are working in 32-bit R, then this script is called on by the readAccess.R directly and
# this variable will already be defined
if (!exists("Args")) {
  Args = commandArgs(T)
} 

# Currently, commandArgs is set up so that:
# first argument is the bypass arg; 
# second argument is the file arg;
# the rest will be the names of the table that you want. There isn't leeway for edge cases yet

# Function to download the SLS database. Change the ftp website url as required
downloadSLS <- function(url = "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt",
                        surveyName = "SLS",
                        extension = ".zip",
                        bypass = Args[1]) {
  
  if (is.na(bypass)) bypass <- F
  
  if (bypass) {
    return(cat("Bypass was specified, not downloading from the FTP site. \n"))
  }

  # # Does this file already exists? If so, do not download again. Useful for running
  # # this command multiple times during a session. This is because the FTP server
  # # will limit connections to it if it is made too often
  if (file.exists(file.path(tempdir(), paste0(surveyName, ".zip")))) {
    return("File has already been created. Not downloading again.")
  }
  
  library(rvest)
  # Fetch the SLS name from the ftp website
  # Create websession
  startingSession <- xml2::read_html(x = url)
  # Find all the relevant nodes
  nodes <- rvest::html_nodes(startingSession, "a")
  # Pull all text of all links on webpage
  links <- rvest::html_attr(nodes, "href")
  # Subset only the relevant link for the survey of interest
  surveyLink <- stringr::str_subset(links, paste0(surveyName, "*.+", extension))
  
  # surveyName <- surveyName
  fileName <- sub(".*/", "", surveyLink)
  # Doing it this way to hopefully it make more robust to weird changes in the future
  # for eg, 20mm is not just 20mm.zip, but 20mm_New.zip
  
  # Download the SLS.zip file from the ftp website
  # Download to tempfolder
  tempFile <- file.path(tempdir(), fileName)
  # Sets up the URL path here
  dbLink <- paste0(url, "/", fileName)
  
  # Download the zipped database
  # As it is zipped, will set mode = "wb"
  download.file(dbLink, destfile = tempFile,
                mode = "wb")
}

# Function to start reading data from Access directly
readSLSAccess <- function(file = Args[2],
                          exdir = tempdir(),
                          surveyName = "SLS",
                          returnDF = F,
                          tablesReturned = Args[-(1:2)]) {

  cat("\nConnecting to Access \n")
  
  # In order to use this correctly, you need to have the 32-bit version of R installed
  # This function is used with system() below to create an rds file 
  # which is required by the rest of the QAQC script
  
  # If the downloadSLS() function was used to download the SLS files, then the downloaded file will be 
  # stored in the temp directory, of which will pull here
  if (is.na(file) | file == shQuote("NA") | file == "NA") {
    tempFile <- list.files(tempdir())[grep(paste0(surveyName, "*.+zip"), list.files(tempdir()))]
    
    # Extracting the downloaded file from downloadSLS(), which is a zip file
    if (grepl(".zip", tempFile)) {
      localDbFile <- unzip(zipfile = file.path(exdir, tempFile), exdir = exdir)
    }
  } else {
    localDbFile <- file
    # You need to be on VPN to access the U drive, where the local db is housed. This error will also occur
    # IF the name of the db file was changed for some reason.
    if (!file.exists(file)) stop("File path not found. Did you specify right? Are you on VPN?", call. = F)
  }
  
  # Driver and path required to connect from RStudio to Access
  dbString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                     "Dbq=", localDbFile)

  # Connection variable itself
  # tryCatch used here to make potential errors more user friendly
  con <- tryCatch(DBI::dbConnect(drv = odbc::odbc(), .connection_string = dbString),
                  error = function(cond) {
                    if (all(stringr::str_detect(cond$message, c("IM002", "ODBC Driver Manager")))) {
                      message(cond, "\n")
                      message("IM002 and ODBC Driver Manager error generally means a 32-bit R needs to be installed or used.")
                    } else {
                      message(cond)
                    }
                  })

  # Pulling just the table names to be used in mapply() below
  tableNames <- odbc::dbListTables(conn = con)
  # Includes system tables which cannot be read, excluding them below with negate
  # tableNames <- stringr::str_subset(tableNames, "MSys", negate = T)
  if (is.null(tablesReturned)) {
    # If no table names are specified, then simply return the names of the possible databases for the user to pick
    DBI::dbDisconnect(con)
    
    return(odbc::dbListTables(conn = con))
  }
  
  cat("Pulling tables:", tablesReturned, "\n")
  
  # Apply the dbReadTable to each readable table in db
  SLSTables <- mapply(DBI::dbReadTable,
                      name = tablesReturned,
                      MoreArgs = list(conn = con))
  
  # Cleaning up connection
  # The downloaded files (if you are pulling from the FTB) will be auto deleted once R shuts down
  DBI::dbDisconnect(con)
  
  # Need to remove extra columns from the database. Will select only the columns that matter:
  # I determined what columns are required by looking at what the public Access database currently has
  # Catch table = ok as is, for both the FTP and UDrive databases
  # Length table
  lengthPosition <- which(sapply(SLSTables, 
                                 function(tables) any(grepl("Length", 
                                                            x = names(tables), 
                                                            ignore.case = T))))
  # There are strange naming differences of the same column in the two databases...squaring up now
  # Args[[1]] should be the "Bypass" argument
  if (Args[[1]] == "FALSE") {
    EntryOrder = "entryorder"
    YolkSacOrOilPresent = "YolkSacorOilPresent"
  }
  
  SLSTables[[lengthPosition]] <- SLSTables[[lengthPosition]] %>% 
    select(Date, Station, Tow, FishCode, Length, EntryOrder, YolkSacOrOilPresent)
  # Meter Correction = ok as i
  # Tow Info
  towPosition <- which(sapply(SLSTables, 
                              function(tables) any(grepl("BottomDepth", 
                                                         x = names(tables), 
                                                         ignore.case = T))))
  
  SLSTables[[towPosition]] <- SLSTables[[towPosition]] %>% 
    select(Date, Station, Tow, Time, Tide, BottomDepth, CableOut, Duration,
           NetMeterSerial, NetMeterStart, NetMeterEnd, NetMeterCheck, Comments)
  # Water Info
  waterPosition <- which(sapply(SLSTables, 
                              function(tables) any(grepl("Temp|Secchi", 
                                                         x = names(tables), 
                                                         ignore.case = T))))
  
  SLSTables[[waterPosition]] <- SLSTables[[waterPosition]] %>% 
    select(Survey, Date, Station, Temp, TopEC, BottomEC, Secchi, Turbidity,
           Lat, Long, Comments)
  # Station_Lookup
  stationPosition <- which(sapply(SLSTables, 
                                  function(tables) any(grepl("RKI", 
                                                             x = names(tables), 
                                                             ignore.case = T))))
  
  if (!identical(unname(stationPosition), integer(0))) {
    SLSTables[[stationPosition]] <- SLSTables[[stationPosition]] %>% 
      select(Station, LatD, LatM, LatS, LonD, LonM, LonS, 
             RKI, Location, AreaCode, Notes)
  } else {
    stationPosition <- which(names(SLSTables) %in% "Station_Lookup")
    
    SLSTables[[stationPosition]] <- SLSTables[[stationPosition]] %>% 
      select(ID, Station, Description, Lat, Long)
  }
  
  # Round all numeric values to 7 digits...This gets rid of "ghost numbers", as known by Bay Study,
  # that are caused by the use of "single" field size in Access. When converted to "double"
  # in R and Excel, the addition of additional decimal points causes non-zero digits to appear
  # to fill in the missing spaces. This is simply a limitation of computer arithmetics
  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
    # Function sourced from: https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r?rq=1
  }
  
  floatIssue <- lapply(SLSTables, function(x) {
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
    
    cat("\nColumn(s)", columnsAffected, "in the", i, "DF experienced float issues. Rounding accordingly. \n")
    
    SLSTables[[i]] <- SLSTables[[i]] %>% 
      mutate(across(all_of(columnsAffected), ~round(.x, 7)),
             across(all_of(columnsAffected), ~round(.x, 2)))
  }
  
  # For instances where you do not want to write the rds and want to work 
  # entirely in this environment, returnDF will be used
  
  if (returnDF) {
    
    cat("Returning dataframe only \n")
    return(SLSTables)
    
  } else {
    # If not returning the df, will return BOTH the csv files AND RDS file
    
    cat("Exporting csv flat files \n")
    
    writeFiles <- sapply(seq_along(SLSTables), function(i) {
      
      if (!file.exists("data-raw")) stop("A `data-raw` folder was not detected in the working directory; please create this folder.")
      
      filePath <- file.path("data-raw", surveyName, paste0(names(SLSTables[i]), ".csv"))
      
      write.csv(SLSTables[[i]],
                file = filePath,
                row.names = F)
      
      file.exists(filePath)
    })
    
    if (!all(writeFiles)) {
      df <- data.frame(tableNames = tableNames,
                       fileFailed = !writeFiles)
      
      errors <- df$tableNames[which(df$fileFailed == T)]
      stop("Table(s) not export successfully: ", 
           paste0(length(errors), " out of ", length(df$tableNames), " tables \n"),
           paste0(errors, "\n"), call. = F)
    } else {
      cat("All tables exported successfully \n")
    }
    cat("Exporting rds file \n")
    
    # Now returning the rda
    saveRDS(SLSTables, file = file.path("data-raw", surveyName, "SLSTables.rds"),
            compress = T)
    
    if (file.exists(file.path("data-raw", surveyName, "SLSTables.rds"))) cat("RDS file created successfully  \n")
    else (stop("RDS file was NOT created, something failed!"))
  }
  cat("\nDone! \n")
}

# Run the functions to download and read the database
downloadSLS()
readSLSAccess()
