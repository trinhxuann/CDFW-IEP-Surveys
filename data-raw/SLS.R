# This script is meant to bridge between the Access database and Sam's LTMRdata package
# Since the LTMR data package takes in individual flat files as inputs, this script
# will simply connect to Access, pull the relevant tables, export it via text/csv, and finish
# The function will download the Access database from the public FTP website into the working directory
# To get the Access connection to work, you will need the DBI and odbc packages
# You will also need R in the same architecture as your Access database 

# Libraries needed
library(dplyr, warn.conflicts = F)
library(DBI)
library(odbc)

# This will evaluate arguments accompanied with the terminal command to use in this specific script
args = commandArgs(T)
# Currently, commandArgs is set up so that:
# first argument is the bypass arg; 
# second argument is the file arg;
# the rest will be the names of the table that you want. There isn't leeway for edge cases yet

# Function to download the SLS database. Change the ftp website url as required
downloadSLS <- function(url = "https://filelib.wildlife.ca.gov/Public/Delta%20Smelt",
                        surveyName = "SLS",
                        extension = ".zip",
                        bypass = args[1]) {
  
  if (bypass) {
    return(cat("Bypass was specified, not downloading from the FTP site. \n"))
  }
  
  # # Does this file already exists? If so, do not download again. Useful for running
  # # this command multiple times during a session. This is because the FTP server
  # # will limit connections to it if it is made too often
  if (file.exists(file.path(tempdir(), paste0("SLS", ".zip")))) {
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
  surveyName <- surveyName
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
readSLSAccess <- function(file = args[2],
                          exdir = tempdir(),
                          surveyName = "SLS",
                          returnDF = F,
                          tablesReturned = args[-(1:2)]) {

  cat("\nConnecting to Access \n")
  
  # In order to use this correctly, you need to have the 32-bit version of R installed
  # This function is used with system() below to create an rds file 
  # required by the rest of the script
  
  # If the downloadSLS() function was used to download the SLS files, then it will be stored in the 
  # temp directory, of which will pull here
  if (is.null(file)) {
    tempFile <- list.files(tempdir())[grep("*.+zip", list.files(tempdir()))]
    
    # Extracting the downloaded file from downloadSLS()
    if (grepl(".zip", tempFile)) {
      localDbFile <- unzip(zipfile = file.path(exdir, tempFile), exdir = exdir)
    }
  } else {
    localDbFile <- file
    if (!file.exists(file)) stop("File path not found. Did you specify right? Are you on VPN?", call. = F)
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
  # The downloaded files will be auto deleted once R shuts down
  DBI::dbDisconnect(con)
  
  # Need to remove extra columns from the database. Will select only the columns that matter:
  # Catch table = ok as is
  # Length table
  SLSTables$Lengths <- SLSTables$Lengths %>% 
    select(Date, Station, Tow, FishCode, Length, EntryOrder, YolkSacOrOilPresent)
  # Meter Correction = ok as i
  # Tow Info
  SLSTables$TowInfo <- SLSTables$TowInfo %>% 
    select(Date, Station, Tow, Time, Tide, BottomDepth, CableOut, Duration,
           NetMeterSerial, NetMeterStart, NetMeterEnd, NetMeterCheck, Comments)
  # Water Info
  SLSTables$WaterInfo <- SLSTables$WaterInfo %>% 
    select(Survey, Date, Station, Temp, TopEC, BottomEC, Secchi, Turbidity,
           Lat, Long, Comments)
  # Station_Lookup
  SLSTables$Station_Lookup <- SLSTables$Station_Lookup %>% 
    select(ID, Station, Description, Lat, Long)
  
  # For instances where you do not want to write the rda and want to work 
  # entirely in this environment, returnDF will be used
  
  if (returnDF) {
    
    cat("Returning dataframe only \n")
    return(SLSTables)
    
  } else {
    # If not returning the df, will return BOTH the csv files AND RDA file
    
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
    
    if (file.exists(file.path("data-raw", surveyName, "SLSTables.rds"))) cat("RDA file created successfully  \n")
    else (stop("RDA file was NOT created, something failed!"))
  }
  cat("\nDone! \n")
}

# Run the functions to download and read the database
downloadSLS()
readSLSAccess()
