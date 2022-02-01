# K.Alstad 12/10/2021
# This script is a derivative of Trinh's code, but does not download the Access database from the public FTP website
# Rather, this code is intended to be used locally to work with Access db on the U: directory
# This program is run by the readAccess1.R program which identifies which version of R
# and uses 32bit to run DBI and odbc packages
# You will also need R in the same architecture as your Access database
# The code below will set up a subfolder for the output files

# SPECIFY LOCATION OF DB FILE AND SHORT SURVEY NAME FOR OUTPUT FOLDER
localDbFile <- "C:/Users/KAlstad/Kar_Docs/Data_Reporting/BS_Rprocess/FieldData/2021 BS Field Entry.mdb"
# Location source file - DO NOT RUN FROM HERE
# "U:/LTM/BayStudy_Lab/2021_Data/2021 BS Field Entry.mdb"
survey = "Boat"

# Both the Field and the Fish databases had broken links, so all tables could not be extracted together
# Currently have hard coded specific tables to export within the function
# Later will make table listing dynamic #


# Identify and/or create folder for output files
mainDir <- "C:/Users/KAlstad/Kar_Docs/Data_Reporting/BS_Rprocess/AccessOut/"
outfold <- mainDir
outsubf <- paste0(outfold,"data-raw/",survey,"/")


# Clear previously exported CSV and .rda files in this directory
if (!dir.exists(outsubf)){
  dir.create(outsubf)
} else {
  print("Dir already exists!")
  # Clear directory
  file.remove(dir(
    outsubf,
    pattern = "*.csv$",
    full.names = TRUE
  ))
}



# Function to start reading data from Access directly
readBAYAccess <- function(file = localDbFile,
                          surveyName = survey,
                          outloc = outsubf,
                          returnDF = F) {

  # Driver and path required to connect from RStudio to Access
  dbString <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};",
                     "Dbq=", file)

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


  # Pulling just the table names to be used in map() below
  tableNames <- odbc::dbListTables(conn = con)


  # Includes system tables which cannot be read, excluding them below with negate
  tableNames <- stringr::str_subset(tableNames, "MSys", negate = T)
  # pattern2 <- c("Survey_LookUp","TowCodes_Lookup","BoatStation")

  pattern2 <- c("2021 FishCatch","2021 Station Data from Diana","BayWgtFactors","Name AutoCorrect Log",
                "OrganismLookUp","Paste Errors", "Station","Station Numbers","StationConstants",
                "SubstrateLookUp","Tow","Bearing Outliers","Calculated distance and bearing",
                 "Data entry check for Stationv","Data entry check for Tow","Depth_Crosstab","Distance High or Low","Distance Outliers",
                "Meter by date and time","Meters Small or Large","Secchi_Crosstab","Substrate_Crosstab","Tide_Bearing_Crosstab",
                "MWT Catch Matrix","OT Catch Matrix","qry Catch by Species by Year&Net","qry MWT Matrix Master","qry OT Matrix Master",
                "qry Plus Count, NoMeasured and Total Catch","qry Station and Tow Data","qry_3A_LONSME_LF_MWTValidTows","qry_3B_LONSME_LF_OTValidTows",
                "qry_CALHAL_forTagging",  "qry_Catch by Year-Net-Species","qry_Catch&NoMeasured_Year-Net-Species")

  # # "Distance Outliers from Diana",

  tableNames <- tableNames[tableNames %in% pattern2 ]


  print(paste("tableNames: ",tableNames))


  # Apply the dbReadTable to each readable table in db
  BAYTables <- mapply(DBI::dbReadTable,
                      name = tableNames,
                      MoreArgs = list(conn = con))

  # Cleaning up connection
  # The downloaded files will be auto deleted once R shuts down
  DBI::dbDisconnect(con)


  # For instances where you do not want to write the rds and want to work
  # entirely in this environment, returnDF will be used

  if (returnDF) {

    cat("Returning dataframe only \n")
    return(BAYTables)

  } else {
    # If not returning the df, will return BOTH the csv files AND RDS file

    cat("Exporting csv flat files \n")

    writeFiles <- sapply(seq_along(BAYTables), function(i) {

      filePath <- file.path(paste0(outloc,names(BAYTables[i]), ".csv"))

      write.csv(BAYTables[[i]],
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
    cat("Exporting rda file \n")


    ################################################################
    #################### i must has messed up this part ###########
    # Now returning the rda
    saveRDS(BAYTables, file = file.path(paste0(outsubf,"BAYTables.rds")),
            compress = T)

    if (file.exists(file.path(paste0(outsubf,"BAYTables.rds")))) cat("RDS file created successfully  \n")
    else (stop("RDS file was NOT created, something failed!"))
  }
}

# Run the function to read the database
readBAYAccess()



#Notes:
#MDB (short for Microsoft Access Database) was Access's default file format until 2007 when ACCDB replaced it. ...
#MDB allows database replication while ACCDB does not. We can link a table from an MDB to ACCDB, but not vice versa.
















