# This is the master file to invoke the function to pull data from the Access databases
# Databases incorporated so far: 
# SLS

# This opens a new terminal for 32 bit R to run this first section of the code to then 
# save the necessary out file(s)
# output files will be in the their respective survey folders as csv or rda files

pullAccessDF <- function(file = c("SLS.R")) {
  
  # file.exists(paste0(Sys.getenv("ProgramFiles"), "/Microsoft Office/root/Office16/ODBC32.DLL"))
  # # Will need to check this on a computer with 64bit Access to see if there is an ODBC64.DLL?
  
  # Are you already on a 32-bit R? If so, will simply source the script since the terminal cannot
  # be opened, as it is already opened here. Size 4 == 32 bit and size 8 == 64 bit
  if (.Machine$sizeof.pointer == 4) {
    source(file.path("data-raw", "SLS.R"))
  }
  
  if (.Machine$sizeof.pointer == 8) {
    if (!file.exists(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"))) {
      stop("A 32-bit R could not be found on this machine", call. = F)
    }
    
    filePath <- file.path(getwd(), "data-raw", file)
    
    terminalOutput <- system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", filePath), 
                             wait = T)
  }
}

pullAccessDF(file = "SLS.R")
