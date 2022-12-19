# GPS plotting function ---------------------------------------------------

# This function is meant to visualize gps coordinates. There are various filters,
# station, year, survey. 
plotGPS <- function(df, year = NULL, survey = NULL, station = NULL, ...) {
  
  names(df) <- tolower(names(df))
  
  if (sum(grepl("survey|year|station|group", names(df))) != 4) {
    stop("Four required columns: year, survey, station, and group. `group` must have a `TheoreticalCoords` label.",
         call. = F)
  }
  
  df <- df %>% 
    mutate(survey = ifelse(group == "TheoreticalCoords", group, survey))
  
  pal <- colorFactor("viridis", domain = c(df$group))
  
  if (is.null(year)) {
    warning("This will plot all tows in the requested data table and may take a substantial amount of resources to complete.", call. = F)
  } else {
    df <- df %>% 
      filter(year == !!year | group == "TheoreticalCoords" | is.numeric(group))
  }
  
  if (!is.null(station)) {
    df <- df %>% 
      filter(station == !!station)
  }
  
  if (!is.null(survey)) {
    df <- df %>% 
      filter(survey == !!survey | group == "TheoreticalCoords" | is.numeric(group))
  }
  
  dfSplit <- split(df, df$survey)
  
  l <- leaflet(width = "100%", height = "1200") %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels)
  
  names(dfSplit) %>%
    purrr::walk( function(df) {
      l <<- l %>%
        addCircleMarkers(data = dfSplit[[df]],
                         ~long, ~lat,
                         label = ~as.character(station),
                         color = ~pal(group),
                         group = df,
                         radius = 7,
                         stroke = F, fillOpacity = 0.8,
                         labelOptions = labelOptions(noHide = T,
                                                     offset = c(18,0),
                                                     textOnly = T,
                                                     textsize = "12px",
                                                     direction = "center"),
                         popup = ~paste("Survey", survey,
                                        "<br>Year", year))
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
  
  originalNames <- names(df)
  names(df) <- tolower(names(df))
  
  if (sum(grepl("survey|year|station|group", names(df))) != 4) {
    stop("Four required columns: year, survey, station, and group. `group` must have a `TheoreticalCoords` label.",
         call. = F)
  }
  
  if (!is.null(survey)) df <- filter(df, survey %in% !!survey | group %in% "TheoreticalCoords")
  
  if (!is.null(year)) df <- filter(df, year %in% !!year | group %in% "TheoreticalCoords")
  
  if (!is.null(station)) df <- filter(df, Station %in% !!station)
  
  theoretical <- df %>% 
    filter(group %in% "TheoreticalCoords")
  
  outlierDF <- lapply(unique(df$station), function(x) {
    tows <- df %>% 
      filter(group %in% "Tow", station == x)
    
    theoretical <- df %>% 
      filter(group %in% "TheoreticalCoords", station == x)
    
    if (nrow(theoretical) > 0 & nrow(tows) > 0) {
      df <- tows %>% 
        mutate(longTheoretical = theoretical$long,
               latTheoretical = theoretical$lat,
               distance = distVincentyEllipsoid(cbind(long, lat),
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
                       station %in% unique(outlierDF$station)))
  
  idVariables <- names(df)[which(grepl(".*(ID).*", names(df)))]
  commentsVariables <- names(df)[which(grepl(".*([cC]omments).*", names(df)))]
  
  if (isTRUE(returnDF)) {
    names(fin)[which(!names(fin) %in% c("longTheoretical", "latTheoretical", "distance", "outlier"))] <-
      originalNames
    fin
  } else plotGPS(df = fin)
}

