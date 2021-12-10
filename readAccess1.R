# From Trinh Nguyen's Notes:
# This is the master file to invoke the function to pull data from the Access databases
#
# K.Alstad notes re. modifications 12/10/2021:
# In the case of Bay Study, first level Access databases (after data entry and data entry checks)
# are located on the U: drive
# Databases exports customized so far: Bay Study Fish Survey (Fish2) & Field Data (Boat)

# The below program (from Trinh Nguyen's SLA code)
# opens a new terminal for 32 bit R to run this first section of the code to then
# save the necessary out file(s)
# output files will be in the their respective survey folders as csv or rda files

require(readr)
require(dplyr)
require(tidyr)


# Identify file.path of processing program
mainDir <- "C:/Users/KAlstad/Kar_Docs/Data_Reporting/BS_Rprocess/AccessOut/"
#pprog <- ("Fish2.R")
pprog <- ("Boat.R")
ppath <-  paste0(mainDir,"AccessReads/",pprog)



pullAccessDF <- function(file = ppath) {

  # file.exists(paste0(Sys.getenv("ProgramFiles"), "/Microsoft Office/root/Office16/ODBC32.DLL"))
  # # Will need to check this on a computer with 64bit Access to see if there is an ODBC64.DLL?

  # Are you already on a 32-bit R? If so, will simply source the script since the terminal cannot
  # be opened, as it is already opened here. Size 4 == 32 bit and size 8 == 64 bit


  if (.Machine$sizeof.pointer == 4) {
    source(file.path(ppath))
  }

  if (.Machine$sizeof.pointer == 8) {
    if (!file.exists(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe"))) {
      stop("A 32-bit R could not be found on this machine", call. = F)
    }
    terminalOutput <- system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", ppath),
                             wait = T)
  }
}

pullAccessDF(file = ppath)
