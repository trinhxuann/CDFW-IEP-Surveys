# K.Alstad 12/10/2021
# This script is a derivative of Trinh's code, but does not download the Access database from the public FTP website
# Rather, this code is intended to be used locally to work with Access db on the U: directory
# This program is run by the readAccess1.R program which identifies which version of R
# and uses 32bit to run DBI and odbc packages
# You will also need R in the same architecture as your Access database
# The code below will set up a subfolder for the output files


# SPECIFY LOCATION OF DB FILE AND SHORT SURVEY NAME FOR OUTPUT FOLDER

localDbFile <- "C:/Users/KAlstad/Kar_Docs/Data_Reporting/BayStudy_Report/BayStudy_DataFlows/DataPublishing/Fish CPUE and Index calc_Sept2021.accdb"
remoteDbFile <- "U:/LTM/Bay Study/FishData/Fish CPUE and Index calc_Sept2021 - Copy.accdb"
survey = "Fish"


# Cannot figure out how to make table listing dynamic #
tables = c("MWT Catch Matrix","OT Catch Matrix")
#pattern <-paste(tables,collapse = ",")
# tblstr <- paste("c(",pattern,")")
# pattern <-paste(shQuote(tables, type="cmd"), collapse=", ")


# Identify and/or create folder for output files
mainDir <- "C:/Users/KAlstad/Kar_Docs/Data_Reporting/BS_Rprocess/AccessOut/"
outfold <- mainDir
outsubf <- paste0(outfold,"data-raw/",survey,"/")
#ifelse(!dir.exists(file.path(outsubf)), dir.create(file.path(outsubf)), FALSE)

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


# tables = tables,
# pattern = pattern,
# tblstr = tblstr,


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

  # pattern2 <- c("BayWeightingFactors","BoatStation","BoatTow","CloudCover_LookUp","CutoffLengths",
  #               "EditLog","EstuarineUseFunctionalGroups_Lookup", "Fish Catch Data","Fish Length Data","Missing Lengths",
  #               "Net_LookUp","Paste Errors","SalinTemp","SpeciesCodes_Lookup","StationConstants_Lookup",
  #               "SubstrateCodes_Lookup","Survey_LookUp","TempCutOff","TideCodes_LookUp","TowCodes_Lookup",
  #               "TowDirection_LookUp","WaveCodes_Lookup")



  pattern2 <- c("MWT Catch Matrix","OT Catch Matrix","qry Catch by Species by Year&Net","qry MWT Matrix Master","qry OT Matrix Master",
                "qry Plus Count, NoMeasured and Total Catch","qry Station and Tow Data","qry_3A_LONSME_LF_MWTValidTows","qry_3B_LONSME_LF_OTValidTows", "qry_CALHAL_forTagging",
                "qry_Catch by Year-Net-Species","qry_Catch&NoMeasured_Year-Net-Species")


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




#MDB (short for Microsoft Access Database) was Access's default file format until 2007 when ACCDB replaced it. ...
#MDB allows database replication while ACCDB does not. We can link a table from an MDB to ACCDB, but not vice versa.
















