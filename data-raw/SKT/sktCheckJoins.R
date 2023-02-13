# Reversing the joins to find intricacies between the datasets
source(file.path("R", "schemaJoin.R"))

# The schema table follows the schema diagram
schema <- tibble(
  parentTable = c("tblCatch", "tblSample", "tblOrganismCodes", "tblStationCoordinates"),
  childTable = c("tblFishInfo", "tblCatch", "tblCatch", "tblSample"),
  primaryKey = c("CatchRowID", "SampleRowID", "OrganismCode", "StationCode"),
  foreignKey = c("CatchRowID", "SampleRowID", "OrganismCode", "StationCode"),
  joinTypes = c(full_join, full_join, left_join, right_join)
)

# prepare the tables to be joined; these joins should mirror those in the join R file
dataList <- list(data$tblCatch,
                 data$tblFishInfo %>% 
                   select(-c(FishID1, FishID2, ReleasedAlive, 
                             LengthRace, CWTRace, CWTNumber)),
                 data$tblSample %>% 
                   select(-JustEdited),
                 data$tblOrganismCodes %>% 
                   select(OrganismCode, CommonName),
                 data$tblStationCoordinates %>% 
                   transmute(StationCode = Station,
                             LatDegTheoretical = LatDeg, 
                             LatMinTheoretical = LatMin, 
                             LatSecTheoretical = LatSec,
                             LongDegTheoretical = LongDeg, 
                             LongMinTheoretical = LongMin, 
                             LongSecTheoretical = LongSec))

cat(sum(sapply(checkingJoins, function(x) !isTRUE(x))), "of the", length(checkingJoins), "joins produced differences.")

# Now to check the joins
checkingJoins <- dataList%>% 
  setNames(c("tblCatch", "tblFishInfo", "tblSample", "tblOrganismCodes", "tblStationCoordinates")) %>% 
  schemaJoin(schema = schema) %>% 
  setNames(c(paste(schema$parentTable[[1]], schema$childTable[[1]], sep = "_"), 
             paste(schema$parentTable[[2]], schema$childTable[[2]], sep = "_"),
             paste(schema$parentTable[[3]], schema$childTable[[3]], sep = "_"),
             paste(schema$parentTable[[4]], schema$childTable[[4]], sep = "_")))

# Exploring the differences
# tblCatch_tblFishInfo; caused by NAs in the LengthRowID; catch with NA lengths are not recorded in the length dataset
all.equal(what$tblCatch_tblFishInfo$tblFishInfo, 
          what$tblCatch_tblFishInfo$childTableCheck %>% 
            filter(!is.na(LengthRowID)))

# tblSample_tblCatch is ok

# tblOrganismCodes_tblCatch; caused by species that are never caught by the survey; likely remnant of FMWT species coding
all.equal(what$tblOrganismCodes_tblCatch$tblCatch, what$tblOrganismCodes_tblCatch$childTableCheck %>% 
            filter(!is.na(CatchRowID)))

# tblStationCoordinates_tblSample; caused by stations sampled by the survey beyond the 40 reported stations
all.equal(what$tblStationCoordinates_tblSample$tblStationCoordinates, 
          what$tblStationCoordinates_tblSample$parentTableCheck %>% 
            filter(!is.na(LatDegTheoretical)))

# Are all differences resolved:
differences <- c(all.equal(what$tblCatch_tblFishInfo$tblFishInfo, 
                              what$tblCatch_tblFishInfo$childTableCheck %>% 
                                filter(!is.na(LengthRowID))),
                    checkingJoins$tblSample_tblCatch,
                    all.equal(what$tblOrganismCodes_tblCatch$tblCatch, what$tblOrganismCodes_tblCatch$childTableCheck %>% 
                                filter(!is.na(CatchRowID))),
                    all.equal(what$tblStationCoordinates_tblSample$tblStationCoordinates, 
                              what$tblStationCoordinates_tblSample$parentTableCheck %>% 
                                filter(!is.na(LatDegTheoretical))))
if (all(differences)) {
  cat("All differences accounted for.")
} else {
  warning("Differences still remain", call. = F)
}
