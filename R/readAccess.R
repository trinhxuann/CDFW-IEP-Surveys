# This is the master file to invoke the function to pull data from the Access databases
# Databases incorporated so far: 
# SLS

# This opens a new terminal for 32 bit R to run this first section of the code to then 
# save the necessary out file(s)
# output files will be in the their respective survey folders as csv or rda files

pullAccessDF <- function(script = c("SLS.R", "20mm", "SKT"),
                         bypass = c(T, F),
                         file = c("U:\\NativeFish\\SmeltData\\DS-DATA\\SLS_Query.mdb",
                                  "U:\\NativeFish\\SmeltData\\DS-DATA\\20-mm local\\20-mm_FishZooData.accdb"),
                         tablesReturned) {
  
  # Are you already on a 32-bit R? If so, will simply source the script since the terminal cannot
  # be opened, as it is already opened here. Size 4 == 32 bit and size 8 == 64 bit
  if (.Machine$sizeof.pointer == 4) {
    Args <<- c(bypass, file, tablesReturned)
    
    # This is simply a reminder since most people normally work in  64 bit R.
    cat("You are in 32-bit R. \n")
    
    source(file.path("data-raw", script))
  }
  
  if (.Machine$sizeof.pointer == 8) {
    if (!file.exists(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"))) {
      stop("A 32-bit R could not be found on this machine", call. = F)
    }
    
    # File path of the script
    filePath <- file.path(getwd(), "data-raw", script)
    
    # Does your file path of the database have spaces in it?
    file <- shQuote(file)
    # Does your tables have spaces in them?
    tablesReturned <- unlist(paste0(lapply(tablesReturned, shQuote), collapse = " "))
    
    if (grepl("\\s", filePath)) stop("The filepath has a space somewhere along the path. Please remove the space.")
    
    terminalOutput <- system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", filePath,
                                    " ", bypass, 
                                    " ", file,
                                    " ", tablesReturned), 
                             wait = T)
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
             bypass = T,
             file = "U:\\NativeFish\\SmeltData\\DS-DATA\\SLS_Query.mdb",
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
             bypass = T,
             file = "U:\\NativeFish\\SmeltData\\DS-DATA\\20-mm local\\20-mm_FishZooData.accdb",
             tablesReturned = c("Survey", "Station", "Tow", "Gear", "GearCodesLkp", "MeterCorrections",
                                "StationCords", "FishSample", "FishLength"))

# For SKT
