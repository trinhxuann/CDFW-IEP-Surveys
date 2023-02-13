mapDF <- read.csv(file.path("StationCords_20mmTN.csv")) %>% 
  separate(Lat, into = c("LatD", "LatM", "LatS"), sep = " ") %>%
  separate(Long, into = c("LonD", "LonM", "LonS"), sep = " ") %>%
  mutate(across(c(LatD, LatM, LatS, LonD, LonM, LonS), ~as.numeric(.x)),
         Latitude = (LatD + LatM/60 + LatS/3600),
         Longitude = -(LonD + LonM/60 + LonS/3600),
         Status = factor(Status,
                         levels = c("indexStation", "indexStationNew", "highOutflow")),
         offsetX = case_when(Station %in% 334 ~ 10,
                             Station %in% c(335) ~ 0,
                             Station %in% 329 ~ -2,
                             Station %in% 336 ~ -5,
                             Station %in% 328 ~ c(0),
                             Station %in% 723 ~ c(-18),
                             TRUE ~ c(18)),
         offsetY = case_when(Station %in% c(334, 335, 336) ~ c(15),
                             Station %in% 329 ~ 13,
                             Station %in% 328 ~ c(-15),
                             Station %in% 723 ~ c(0),
                             TRUE ~ c(0)))

# To color the stations on the map accordingly
# A bit strange how the order of the colors do not seem to follow the levels of the domain?
pal <- colorFactor(c("#EE0000FF", # highOutflow
                                "#3B4992FF", # indexStation
                                "#008B45FF"), # indexStationNew
                                domain = levels(mapDF$Status))

m <- leaflet(mapDF) %>%
  addProviderTiles(providers$Esri.OceanBasemap) 

stationList <- mapDF %>% 
  split(., .$Station)

names(stationList) %>%
  purrr::walk(function(mapDF) {
    m <<- m %>%
      addCircleMarkers(data = stationList[[mapDF]],
                       lng=~Longitude, lat=~Latitude,
                       label = ~as.character(Station),
                       color = ~pal(Status),
                       radius = 6,
                       stroke = F, fillOpacity = 0.8,
                       labelOptions = labelOptions(noHide = T,
                                                   offset = ~c(offsetX, offsetY),
                                                   direction = "center",
                                                   textOnly = T,
                                                   textsize = "12px"))
  })

m %>%
  addLegend(pal = pal, values = ~Status, opacity = 1) %>% 
  setView(lat = 38.09690863981991, lng = -121.89539973367937, zoom = 10)