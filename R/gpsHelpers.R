# GPS plotting function ---------------------------------------------------

# This function is meant to visualize gps coordinates. There are various filters,
# station, year, survey. 
plotGPS <- function(df, year = NULL, survey = NULL, station = NULL, ...) {
  
  df <- df %>% 
    mutate(Survey = ifelse(group == "TheoreticalCoords", group, Survey))
  
  pal <- colorFactor("viridis", domain = c(df$group))
  
  if (is.null(year)) {
    warning("This will plot all tows in the requested data table and may take a substantial amount of resources to complete.", call. = F)
  } else {
    df <- df %>% 
      filter(Year == year | group == "TheoreticalCoords" | is.numeric(group))
  }
  
  if (!is.null(station)) {
    df <- df %>% 
      filter(Station == station)
  }
  
  if (!is.null(survey)) {
    df <- df %>% 
      filter(Survey == survey | group == "TheoreticalCoords" | is.numeric(group))
  }
  
  dfSplit <- split(df, df$Survey)
  
  l <- leaflet(width = "100%", height = "1200") %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels)
  
  names(dfSplit) %>%
    purrr::walk( function(df) {
      l <<- l %>%
        addCircleMarkers(data = dfSplit[[df]],
                         ~StartLong, ~StartLat,
                         label = ~as.character(Station),
                         color = ~pal(group),
                         group = df,
                         radius = 7,
                         stroke = F, fillOpacity = 0.8,
                         labelOptions = labelOptions(noHide = T,
                                                     offset = c(18,0),
                                                     textOnly = T,
                                                     textsize = "12px",
                                                     direction = "center"),
                         popup = ~paste("Survey", Survey,
                                        "<br>Year", Year))
    })
  
  l %>%
    addLegend(pal = pal, values = unique(df$group), opacity = 1, ...) %>% 
    addLayersControl(
      overlayGroups = names(dfSplit),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# Outlying GPS points -----------------------------------------------------

gpsOutlier <- function(df, d = 0.5, station = NULL, year = NULL, survey = NULL,
                       returnDF = F) {
  
  if (!is.null(survey)) df <- filter(df, Survey %in% survey | group %in% "TheoreticalCoords")
  
  if (!is.null(year)) df <- filter(df, Year %in% year | group %in% "TheoreticalCoords")
  
  if (!is.null(station)) df <- filter(df, Station %in% station)
  
  theoretical <- df %>% 
    filter(group %in% "TheoreticalCoords")
  
  outlierDF <- lapply(unique(df$Station), function(x) {
    tows <- df %>% 
      filter(group %in% "Tow", Station == x)
    
    theoretical <- df %>% 
      filter(group %in% "TheoreticalCoords", Station == x)
    
    if (nrow(theoretical) > 0 & nrow(tows) > 0) {
      df <- tows %>% 
        mutate(longTheoretical = theoretical$Long,
               latTheoretical = theoretical$Lat,
               distance = distVincentyEllipsoid(cbind(Long, Lat),
                                                cbind(longTheoretical, latTheoretical))/1609.34,
               outlier = ifelse(distance > d, T, F)) %>% 
        filter(outlier == T)
    } else df <- NULL
    
    df
  }) %>% 
    bind_rows()
  
  fin <- outlierDF %>% 
    bind_rows(df %>% 
                filter(group %in% "TheoreticalCoords",
                       Station %in% unique(outlierDF$Station)))
  
  if (isTRUE(returnDF)) {
    fin %>% 
      transmute(StationID, SurveyID, SampleDate, Year, Survey, Station, 
                LatD, LatM, LatS, LonD, LonM, LonS, Comments.Station,
                group, Lat, Lon = Long, longTheoretical, latTheoretical,
                distance, outlier)
  } else plotGPS(df = fin)
}
