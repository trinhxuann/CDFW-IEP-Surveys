# This is the master file to invoke the function to pull data from the Access databases
# Databases incorporated so far: 
# SLS

# This opens a new terminal for 32 bit R to run this first section of the code to then 
# save the necessary out file(s)
# output files will be in the their respective survey folders as csv or rda files

pullAccessDF <- function(script = c("SLS.R", "20mm", "SKT"),
                         architecture = c("32bit", "64bit"),
                         file = c("U:\\NativeFish\\SmeltData\\DS-DATA\\SLS_Query.mdb",
                                  "U:\\NativeFish\\SmeltData\\DS-DATA\\20-mm local\\20-mm_FishZooData.accdb"),
                         tablesReturned) {

  # Are you already on a 32-bit R? If so, will simply source the script since the terminal cannot
  # be opened, as it is already opened here. Size 4 == 32 bit and size 8 == 64 bit
  
  # By default, most CDFW's machines are working with 32bit office. As such, the default
  # method will be to open up 32bit R to connect to the 32bit odbc drivers to connect to Access
  # This architecture argument is only really used if 64bit is specified
  architecture <- match.arg(architecture)
  
  if (.Machine$sizeof.pointer == 4) {
    Args <<- c(file, tablesReturned)
    
    # This is simply a reminder since most people normally work in  64 bit R.
    cat("You are in 32-bit R. \n")
    
    source(file.path("data-raw", script))
  }
  
  if (.Machine$sizeof.pointer == 8) {
    
    if (!grepl("32bit|64bit", architecture)) {
      stop("architecture should be `32bit` or `64bit`", call. = F)
    }
    
    if (architecture == "32bit") {
      if (!file.exists(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"))) {
        stop("A 32-bit R could not be found on this machine.", call. = F)
      }
      
      # File path of the script you want to run
      filePath <- file.path(getwd(), "data-raw", script)
      
      # Does your file of the database have spaces in its name? if so remove them
      file <- shQuote(file)
      # Does your tables have spaces in them? if so remove them
      tablesReturned <- unlist(paste0(lapply(tablesReturned, shQuote), collapse = " "))
      
      # Is there a space in the file path...? This is something that R cannot handle in this specific application
      # Since I cannot account for it here, this error will warn you specifically to change it on your machine.
      if (grepl("\\s", filePath)) stop("The filepath has a space somewhere along the path. Please remove the space.")
      
      # This system command can be finnicky. A lot of information is passed from this command into
      # a new terminal of R, so specific formats must be followed here. Currently, this is set up as
      # user friendly as possible via arguments in this function.
      terminalOutput <- system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", filePath,
                                      " ", file,
                                      " ", tablesReturned), 
                               wait = T)
    } 
    
    if (architecture == "64bit") {
      Args <<- c(file, tablesReturned)
      
      source(file.path("data-raw", script))
    }
  }
}

# # For SLS
# # FTP route
# pullAccessDF(script = "SLS.R",
#              bypass = F,
#              file = NA,
#              tablesReturned = c("Catch", "Lengths", "Meter Corrections",
#                                 "Tow Info", "Water Info", "20mm Stations"))

# UDrive route
pullAccessDF(script = "SLS.R",
             file = "U:\\NativeFish\\SmeltData\\DS-DATA\\SLS_Query.accdb",
             tablesReturned = c("Catch", "Lengths", "MeterCorrections",
                                "TowInfo", "WaterInfo", "Station_Lookup"))



# # For 20mm
# # 20mmStations == StationCords
# # This is for the FTP website route
# pullAccessDF(script = "20mm.R",
#              bypass = F,
#              file = NA,
#              tablesReturned = c("FishSample", "FishLength", "MeterCorrections",
#                                 "Tow", "Station", "20mmStations"))

# UDrive route
pullAccessDF(script = "20mm.R",
             architecture = "64bit",
             file = "U:\\NativeFish\\SmeltData\\DS-DATA\\20-mm local\\20-mm_FishZooData.accdb",
             tablesReturned = c("Survey", "Station", "Tow", "Gear", "GearCodesLkp", "MeterCorrections",
                                "StationCords", "FishSample", "FishLength"))

# For SKT
