# https://cdec.water.ca.gov/dynamicapp/staMeta?station_id

library(rvest)
session <- session("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id")

links <- session %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  {.[which(grepl("/dynamicapp/staMeta\\?station_id=", .))]}

pullCoordinates <- function(string) {
  dataString <- session(paste0("https://cdec.water.ca.gov", string)) %>% 
    html_element("table") %>% 
    html_text()
  
  data.frame(station = regmatches(dataString, regexpr("(?<=Station ID)(.*)(?=Elevation)", dataString, perl = T)),
             latitude = regmatches(dataString, regexpr("(?<=Latitude)([\\d.-]+)", dataString, perl = T)),
             longitude = regmatches(dataString, regexpr("(?<=Longitude)([\\d.-]+)", dataString, perl = T)))
}

cdecStations <- lapply(links, pullCoordinates) %>% 
  bind_rows() %>% 
  mutate(across(c(latitude, longitude), ~as.numeric(.x))) %>% 
  filter(!latitude %in% c(0.00000, 99.99900) | !longitude %in% c(0.00000, -999.9990))

cdecMetadata <- pullMetadataCDEC(cdecStations$station)

save(cdecStations, cdecMetadata, file = "cdecStations_20221215.RData")

